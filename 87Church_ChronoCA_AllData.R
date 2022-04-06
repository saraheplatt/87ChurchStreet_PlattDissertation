# 87Church_ChronoCA_AllData.R
# Created by:  FDN  8.5.2014
# This code in its original form was authored and developed by Dr. Fraser Neiman of
# Monticello Archaeology and subsequently edited by Sarah Platt for use in
# her dissertation.
# Last update: FDN 8.5.2014  
# Last Edit: SEP 2.11.2022 for dissertation

# load the libraries
library(dplyr)
library(tidyr)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)

#### 1. get the table with the ware type date ranges ####
# get the table with the ware type date ranges, read in CSV
MCDTypeTable<- read.csv(file = "Dissertation_Chronology/DAACS_MCDTypeTable.csv", 
                        fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

#### 2. Load 87 Church Street Ware Date ####
# Note, this step is modified from original DAACS data. All of the diss
# data is stored in a series of CSV files in the github repository
# for the project

wareTypeDataA <- read.csv(file = 'Dissertation_FinalDataCSVs/87Church_CompiledDataset_Fin.csv', 
                         fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

# Here load in Zierden SGs 

ZierdenSGs <- read.csv(file = 'Dissertation_Chronology/87Church_ZierdenSGs.csv', 
                         fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

# And add to file

wareTypeDataB <- merge(wareTypeDataA, ZierdenSGs, by=
                                    c("CONTEXT"))

# AFTER INITIAL RUN OF CA, THEN RUN OF STABLE AND FEATURES, ADD IN PHASED 
# STABLE AND FEATURE DATA

PhasedData <- read.csv(file = 'Dissertation_Chronology/FeaStab_PhasedData.csv', 
                       fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

# Merge with current data

PhasedData <-  PhasedData %>% 
  group_by(Phase, CONTEXT) %>% 
  summarise(Count = sum(COUNT))

PhasedData <- PhasedData %>%
  select(CONTEXT, Phase)

wareTypeData <- merge(PhasedData, wareTypeDataB, by=
                                    c("CONTEXT"))

# Rename phase column to represent Stable Feature phases, not final phases

wareTypeData <- rename(wareTypeData, SFPhase = Phase)

# do a summary
summary1 <- wareTypeData %>%
  group_by(CONTEXT,WARE) %>% 
  summarise(Count = sum(COUNT))
options(tibble.print_min=100)
summary1

#compute the total count of ceramics
AllCeramicCount <- summary1 %>% summarise(Count=sum(Count))

#### 3. Customizations to the Ware Type dates or names####
#3.1 For example, change beginning and end dates for French CEW to 1675-1900
#This is optional -- The CEW types don't have manufacturing dates so they are 
#can be excluded from MCDs but it can be useful to include them in the CA 
#and compare the CA order with them in and the CA order that doesn't include them

#MCDTypeTable <- MCDTypeTable %>% 
#  mutate(BeginDate = replace(BeginDate, Ware %in% c('French Coarse Earthenware',
#                                                    'Vallauris',
#                                                    'Saintonge',
#                                                    'Huveaune'), 1675),
#         EndDate = replace(EndDate, Ware %in% c('French Coarse Earthenware',
#                                                'Vallauris',
#                                                'Saintonge',
#                                                'Huveaune'),1900))

# Set a minimal start date for all types - this may bite you later if new start 
# dates are >= end dates
#MCDTypeTable <- MCDTypeTable %>% 
#  mutate(BeginDate = ifelse(BeginDate < 1675, 1675, BeginDate)) 

### Here SEP added the dates for Slip-Coated Ware, a rare type found in Charleston
#with a tight date range. She also added a unique early Rhenish ware called Hohr ware
#SEP also dropped the enddate for manufacture for North
#Devon slipware by 30 years based on discussions with Jamestown curators

MCDTypeTable <- MCDTypeTable %>% add_row(WareID = NA, Ware = 'Slip-coated Ware', ObjectTypeID = NA, 
                                         BeginDate = 1720, EndDate = 1750, CeramicMaterialID = NA,
                                         id = NA)

MCDTypeTable <- MCDTypeTable %>% add_row(WareID = NA, Ware = 'Hohr ware', ObjectTypeID = NA, 
                                         BeginDate = 1670, EndDate = 1750, CeramicMaterialID = NA,
                                         id = NA)

MCDTypeTable <- MCDTypeTable %>% 
  mutate(EndDate = replace(EndDate, Ware %in% 'North Devon Slipware', 1700))

#### 4. Compute new numeric date variables from original ones #### 
# Compute midpoint, manufacturing span, and inverse variance for each ware type
# and add new columns to house variables.  These are needed to calculate the MCD.
MCDTypeTable <- MCDTypeTable %>% 
  mutate(midPoint = (EndDate+BeginDate)/2,
         span = (EndDate - BeginDate),
         inverseVar = 1/(span/6)^2 
  )

#### 5. Here you have the option to remove contexts with deposit type Cleanup and Surface Collection ####
# Here SEP eliminated all unprovenienced contexts. These contexts were initially flagged
# as yUNPROV for "yes unprov" in the data cleaning R script 87Church_DataCleaning.R.
# These contexts either recognizably lacked vertical and/or horizontal context 
# or they were flagged as unprovenienced/clean up in Zierden's site report
# or field notes. See data cleaning appendix. 

wareTypeData <- subset(wareTypeData, ! wareTypeData$UNPROV  %in% 'yUNPROV') 

#### 6. Create the UNIT Variable ####
# The UNIT variable contains the level at which assemblages are aggregated in 
# the analysis. You will need to customize this logic for YOUR site. There
# are circumstances where it will make the most sense to group by SGs or Features, but in others
# you may need to use contexts. Note, you should only use ONE option, either 6.1, 6.2, 6.3, or 6.4.  The rest
# should be commented out.
# Note that we create a new dataframe: wareTypeData_Unit.

## 6.2 Use this to assign ContextID to the unit. 
wareTypeData_Unit <- wareTypeData %>%  
  mutate(unit = wareTypeData$CONTEXT)

# Check on the content of the unit variable to make sure all is cool.
table(wareTypeData_Unit$unit)

#### 7. Transpose the data for the MCD and CA ####
wareByUnitT <- wareTypeData_Unit %>% group_by(WARE,unit) %>% 
  summarise(count = sum(COUNT)) %>%
  spread(WARE, value=count , fill=0 )

wareByUnitT$unit <- as.character(wareByUnitT$unit)

#### 8. Remove specific ware types (if you must) and set sample size cut off  ####
# 8.1 It is possible at this point to drop types you do not want in the MCD computations
# We do this because some types are historical types and some aren’t
#if a type isn’t helpful in providing a chronological signal then you should take it out
#Ex: American SW -- it has a very different function than a pearlware plate
#and therefore its pattern of occurrence is likely to be affected, can get temporal gradiant for Dim1 (REW) vs Dim 2 that capturing is more utilitarian  
#One thing -- we are doing this before we calculate MCDs NOT the seriation
#Two approaches: 1) leave them in the MCD dataframe and only take them out of the CA and then when you compare CA to the MCDs (i.e. don't
# remove them here,
#2) If you take them out here you will be doing the MCD CA comparison on the same dataset without the ware types

# Here we name the types we do NOT
# want included (NOTE: You will need to add the types that are particular to your site to the select function):
# Note from SEP: These types were removed because the identifications by Herold were uncertain, or were 
# made by a different set of standards than the DAACS data resulting in a inconsistency that
# could not be reconciled.
wareByUnitT1 <- wareByUnitT %>% dplyr::select( 
  - 'Redware',
  - 'Frechen Brown',
  - 'Wedgwood Green',
  - 'British Brown/Fulham Type')

# 8.2  We may also want to enforce a sample size cut off on the MCD analysis.
# MCDs and TPQs are more reliable with larger samples, but may be in 
# useful in small ones. DAACS standard is N > 5.
# Note the use of column numbers as index values to get the type counts, which 
# are assumed to start in col 2.
wareByUnitTTotals<- rowSums(wareByUnitT1[,-1])
table(wareByUnitTTotals)
wareByUnitT1 <-wareByUnitT1[wareByUnitTTotals > 5,]
# And get rid of any types that do not occur in the subset of assemblages that 
# DO  meet the sample size cutoff
wareByUnitT2 <- wareByUnitT1[,c(T,colSums(wareByUnitT1[,-1])
                                > 0)]

#### 9. Define functions to Remove Types w/o Dates and then compute MCDs ####
# 9.1 We build a function that removes types with no dates, either because they 
# have no dates in the MCDTypeTable or because they are not MCD Ware 
# Types (e.g. they are CEW Types). This approach is useful because it returns a 
# dataframe that contains ONLY types that went into the MCDs, which you may 
# want to analyze using using other methods (e.g. CA).
# Two arguments: 
#   unitData: dataframe with counts of ware types in units
#   the left variable IDs the units, while the rest of the varaibles are types
#   typeData: a dataframe with at least three variables named 'Ware', 'midPoint'
#   and 'inversevar' containing the manufacturing midpoints and inverse 
#   variances for the types.
# Returns a list comprised of two dataframes:
#   unitDataWithDates has units with types with dates
#   typeDataWithDates has the types with dates
RemoveTypesNoDates <- function(unitData,typeData){
  #unitData<- WareByUnitT1
  #typeData <-MCDTypeTable
  typesWithNoDates <- typeData$Ware[(is.na(typeData$midPoint))] 
  # types in the MCD table with no dates.
  moreTypesWithNoDates <- colnames(unitData)[-1][! colnames(unitData)[-1] %in% 
                                                   typeData$Ware] 
  # types in the data that are NOT in the MCD Type table.
  typesWithNoDates <- c(typesWithNoDates, moreTypesWithNoDates)
  unitDataWithDates <- unitData[, ! colnames(unitData) %in%  typesWithNoDates]
  typeDataWithDates <- typeData[! typeData$Ware %in%  typesWithNoDates, ]
  unitDataWithDates <- filter(unitDataWithDates, 
                              rowSums(unitDataWithDates[,2:ncol(unitDataWithDates)])>0)
  return(list(unitData = unitDataWithDates, 
              typeData = typeDataWithDates))
}

# run the function
dataForMCD <- RemoveTypesNoDates(wareByUnitT2 , MCDTypeTable)

# Define a function that computes MCDs
# Two arguments: 
#   unitData: a dataframe with the counts of ware types in units. 
#   We assume the first column IDs the units, while the rest of the columns 
#   are counts of types.
#   typeData: a dataframe with at least two variables named 'midPoint' and 
#   'inversevar' containing the manufacturing midpoints and inverse variances 
#   for the types.
# Returns a list comprise of two dataframes: 
#     MCDs has units and the vanilla and BLUE MCDs
#     midPoints has the types and manufacturing midpoints, in the order they 
#     appeared in the input unitData dataframe.  
EstimateMCD<- function(unitData,typeData){
  countMatrix<- as.matrix(unitData[,2:ncol(unitData)])
  originalUnitName <-  colnames(unitData)[1]
  colnames(unitData)[1] <- 'unitID'
  unitID <- (unitData[,1])
  unitID[is.na(unitID)] <-'Unassigned'
  unitData[,1] <- unitID
  nUnits <- nrow(unitData)   
  nTypes<- nrow(typeData)
  nTypesFnd <-ncol(countMatrix)
  typeNames<- colnames(countMatrix)
  # create two col vectors to hold inverse variances and midpoints
  # _in the order in which the type variables occur in the data_.
  invVar<-matrix(data=0,nrow=nTypesFnd, ncol=1)
  mPoint <- matrix(data=0,nrow=nTypesFnd, ncol=1)
  for (i in (1:nTypes)){
    for (j in (1:nTypesFnd)){
      if (typeData$Ware[i]==typeNames[j]) {
        invVar[j,]<-typeData$inverseVar[i] 
        mPoint[j,] <-typeData$midPoint[i]
      }
    }
  }
  # compute the blue MCDs
  # get a unit by type matrix of inverse variances
  invVarMat<-matrix(t(invVar),nUnits,nTypesFnd, byrow=T)
  # a matrix of weights
  blueWtMat<- countMatrix * invVarMat
  # sums of the weight
  sumBlueWts <- rowSums(blueWtMat)
  # the BLUE MCDs
  blueMCD<-(blueWtMat %*% mPoint) / sumBlueWts
  # compute the vanilla MCDs
  sumWts <- rowSums(countMatrix)
  # the vanilla MCDs
  MCD<-(countMatrix %*% mPoint) / sumWts
  # now for the TPQs
  meltedUnitData <- gather(unitData, key = Ware, value=count,- unitID)
  meltedUnitData <- filter(meltedUnitData, count > 0) 
  mergedUnitData <- inner_join(meltedUnitData, typeData, by='Ware')
  # the trick is that to figure out the tpq. it's best to have each record (row) 
  # represent an individual sherd  but in its current state, each record has 
  # a count c:(c > 1). We must generate c records for each original record.
  # Use rep and rownames - rowname is a unique number for each row, kind of 
  # like an index. rep() goes through dataframe mergedUnitData and replicates 
  # based on the count column, i.e. if count is 5 it will create 5 records or 
  # rows and for columns 1 and 6 (col 1 is unit name and 6 is begin date.
  repUnitData <- mergedUnitData[rep(rownames(mergedUnitData ),mergedUnitData$count),c(1,6)]
  # once all the rows have a count of one, we run the quantile function
  TPQ <- tapply(repUnitData$BeginDate,repUnitData$unitID, 
                function(x) quantile(x, probs =1.0, type=3 ))              
  TPQp95 <- tapply(repUnitData$BeginDate,repUnitData$unitID, 
                   function(x) quantile(x, probs = .95 , type=3 ))                 
  TPQp90 <- tapply(repUnitData$BeginDate,repUnitData$unitID, 
                   function(x) quantile(x, probs = .90,  type=3 ))   
  # Finally we assemble the results in to a list
  MCDs<-data.frame(unitID, MCD, blueMCD, TPQ, TPQp95, TPQp90, sumWts )
  colnames(MCDs)<- c(originalUnitName,'MCD','blueMCD', 'TPQ', 'TPQp95', 'TPQp90', 'Count')
  midPoints <- data.frame(typeNames,mPoint)
  MCDs <- list('MCDs'=MCDs,'midPoints'=midPoints)
  return(MCDs)
} 
# end of function EstimateMCD

# apply the function
MCDByUnit<-EstimateMCD(dataForMCD$unitData, dataForMCD$typeData)

# let's see what it looks like
MCDByUnit

# Export this data for basic reference
# write.csv((MCDByUnit[["MCDs"]]),"87Church_MCDS.TPQS.csv", row.names = FALSE)

#### 10. Seriation diagram based on MCDs ####
# First define a  function to sort the rows and cols of a matrix based on the
# orders from two arguments (e.g. MCDs and midpoints)
# arguments:  the name of the variable that contains the unit scores (e.g. MCDs)
#             the name of the variable that contains the type score (e.g. 
#               the midpoints) 
#             the name of the dataframe that contains the counts of ware types 
#               in units
# returns:    the sorted dataframe 

sortData<- function(unitScores,typeScores,unitData){
  sortedData<-unitData[order(unitScores, decreasing=T),]
  sortedData<-sortedData[,c(1,order(typeScores)+1)]
  return(sortedData)
}

unitDataSorted <- sortData(MCDByUnit$MCDs$blueMCD,
                           MCDByUnit$midPoints$mPoint,
                           dataForMCD$unitData)

#### 11.  Make a Ford-style battleship plot  ####
# convert to a matrix, whose cols are the counts
# make the unit name a 'rowname" of the matrix
Mat<-as.matrix(unitDataSorted[,2:ncol(unitDataSorted)])
rownames(Mat)<-unitDataSorted$unit
rSums<- matrix (rowSums(Mat),nrow(Mat),ncol(Mat), byrow=F)
MatProp<-Mat/rSums
# Do the plot
battleship.plot(MatProp,
                mar=c(2,6,10,1),
                main = 'Seriation by Blue MCD',
                xlab='Ware Type',
                ylab= 'Context',
                col='grey')
#Export Plot

#### 12. Now let's try some Correspondence Analysis ####
# You need to decide if you want to use exactly the same data that
# went into the MCD analysis (dataForMCD$unitData), or the 
# data with all the ware types (e.g. if you want to include CEWs/ware types that don’t have dates).
# To chose, commment out one of these two lines:
#wareByUnitT_forCA <- wareByUnitT2 # use all the data
wareByUnitT_forCA <- dataForMCD$unitData # use ONLY the data used for MCDs

# 12.1 USE THIS SECTION AFTER AN INITIAL CA RUN TO remove types and units from the analysis that are outliers

#Remove units
#wareByUnitT_forCA <- wareByUnitT_forCA %>% filter(unit != '02-03-087')


# When you have removed a unit check to see if any of the column totals is now 0.  If any exist you will need to 
# remove the ware type, otherwise the CA won't run
#colSums(wareByUnitT_forCA[,-1])

# Remove types
#wareByUnitT_forCA <- wareByUnitT_forCA %>% select( 
#  - 'Red Agate, refined',
#)

#12.2 USE THIS SECTION TO RUN THE INITIAL CA AND THEN USE AGAIN AFTER REMOVING OUTLIERS                   
#Run the CA

matX <- as.matrix(wareByUnitT_forCA[,-1]) 
rownames(matX) <- wareByUnitT_forCA$unit

ca1<-ca(matX)
# Summary(ca1) - inertia plot with broken stick

# put the result in dataframes
inertia <- data.frame('Inertia' = prop.table(ca1$sv^2))


rowScores <- data.frame(ca1$rowcoord[,1:5], unit =ca1$rownames)
colScores <- data.frame(ca1$colcoord[,1:5], type =ca1$colnames)


# 12.3 Compute the broken stick model inertia
# Broken stick model hows inertia percentage values would look if distribution was random -- i.e. null hypothesis.  
# If the inertia values returned by the CA function are showing Dim 1 is capturing significant variation then Dim 1 values from CA 
# will be higher than Dim 1 values from BS and Dim 2 values will be lower than values from BS
# If Dim1 and Dim2 are significant CA, values in CA for both dimensions will be higher than both values in BS.
# One piece of evidence that’s helping you evaluate whether patterning is real 
# see https://rdrr.io/rforge/PCDimension/man/brokenStick.html for more information on the broken stick model

broken.stick <- function(p)
  # Compute the expected values of the broken-stick distribution for 'p' pieces.
  # Example: broken.stick.out.20 = broken.stick(20)
  #             Pierre Legendre, April 2007
{
  result = matrix(0,p,2)
  colnames(result) = c("Dim","Expected.Inertia")
  for(j in 1:p) {
    E = 0
    for(x in j:p) E = E+(1/x)
    result[j,1] = j
    result[j,2] = E/p
  }
  result <- result
  return(data.frame(result))
}

bs <- broken.stick(nrow(inertia))



# plot the proportion of inertia
theme_set(theme_classic(base_size = 20))

p <- ggplot(data=inertia , aes(x= 1:length(Inertia), y=Inertia)) +
  # geom_bar(stat="identity", fill="grey") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(col= "cornflower blue", size=1) +
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue") +
  #make sure to update title to proper site name
  labs( title="87 Church Street - All Data", x="Dimension", y='Proportion of Inertia' ) +
  geom_line(aes(y = bs[,2], x= bs[,1]), color = "black", linetype = "dashed", 
            size=1)
p

# Once CA is settled and outliers removed, save plot

# 12.4 plots of row and column scores for Dim 1 vs. Dim 2 and Dim 1 vs. Dim 3 
# We are looking to see if Dim1 captures time, keep in mind that time may reside in two dimensions 
# and you may be better off looking for clusters in the Dim1Dim2 scatterplot, removing outliers and running again.
# It is an iterative process

# First set max overlaps for labels

options(ggrepel.max.overlaps = Inf)

# ggplot version of row scores dim 1 and dim 2
library(ggrepel)
set.seed(42)
p1 <- ggplot(rowScores, aes(x=Dim1,y=Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label= unit,vjust=-.6, cex=5) +
  theme(plot.title = element_text(hjust = 0.5))+
  #geom_text_repel(aes(label= unit), cex = 4) +
  labs(title="87 Church Street - All Data", 
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')
  )
p1

#ggplot version of col scores dim 1 and dim 2
p2 <- ggplot(colScores, aes(x = Dim1,y = Dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label= type),vjust=-.6, cex=5)+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text_repel(aes(label=type), cex= 3) +
  labs(title="87 Church Street - All Data", 
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep='')
  )  
p2

#add Stable/Feature phases
rowScoresCompA <- select(wareTypeData_Unit, unit, SFPhase, COUNT)

rowScoresCompA1 <- rowScoresCompA %>% group_by(unit, SFPhase) %>% 
  summarise(Count = sum(COUNT))

rowScoresCompA2 <- rowScoresCompA1 %>% select(unit, SFPhase)

rowScoresCompB <- left_join (rowScores, rowScoresCompA2, by = 'unit') %>%
  mutate(Component = ifelse(is.na(SFPhase),'',SFPhase))

rowScoresCompB[rowScoresCompB == ""] <- "NotAssign"

##Plot it

p1a <- ggplot(rowScoresCompB,aes(x = Dim1, y = Dim2, 
                                 fill= SFPhase)) +
  #scale_y_continuous(limits=c(1750, 1950)) +
  geom_point(shape=21,  alpha = .75, size= 6)  + 
  scale_fill_manual(values = c(
                              "P01" = "red",
                              "P01B" = "blue",
                              "P02" = "green",
                              "P02B" = "purple",
                              "P03" = "orange",
                              "P03B" = "yellow",
                              "P04" = "brown",
                              "P05" = "pink",
                              "NotAssign" = "NA")) +
  #geom_text_repel(aes(label= unit), cex=4) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="87 Church Street - All Data", 
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep=''))
p1a

p1a2 <- ggplot(rowScoresCompB,aes(x = Dim1, y = Dim2, 
                                 fill= SFPhase)) +
  #scale_y_continuous(limits=c(1750, 1950)) +
  geom_point(shape=21,  alpha = .75, size= 6)  + 
  scale_fill_brewer(name="SFPhase",
                    palette = 'Set1')+
  #geom_text_repel(aes(label= unit), cex=4) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="87 Church Street - All Data", 
       x = paste ("Dimension 1",":  ", round(inertia[1,]*100),'%', sep=''), 
       y= paste ("Dimension 2",":  ", round(inertia[2,]*100),'%', sep=''))
p1a2

#### 13. Compare MCD and CA dim scores ####
# create a data frame of units, counts, and mcds
CA_MCD <- inner_join(MCDByUnit$MCDs, rowScores, by='unit' )

# Plot CA Dim 1 vs. MCDs
#ggplot version of CA Dim 1 vs. MCDs
p3 <- ggplot(CA_MCD, aes(x=Dim1,y=blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=unit),vjust=-.6, cex=5)+
  theme(plot.title = element_text(hjust = 0.5))+
  #geom_text_repel(aes(label=unit), cex=6) +
  labs(title="87 Church Street - Stable/Herold Features", 
       x="Dimension 1", 
       y="BLUE MCD") 
p3

#ggplot version of CA Dim 2 vs. MCDs
p4 <- ggplot(CA_MCD, aes(x = Dim2,y = blueMCD))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  #geom_text(aes(label=unit),vjust=-.6, cex=5)+
  #geom_text_repel(aes(label=unit), cex=6) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="87 Church Street", 
       x="Dimension 2", 
       y="BLUE MCD") 
p4 

#### 14. Histogram of Dim 1 scores for Phasing ####
# Dim 1 Scores Weighted Histogram, you may need to change scale
dim1ForHist<- data.frame(dim1 = rep(CA_MCD$Dim1, CA_MCD$Count))
p5a <- ggplot(dim1ForHist, aes(x = dim1)) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_histogram(aes(y=..density..), colour="black", fill="tan", binwidth=0.1, 
                 boundary= .1) +
  scale_x_continuous(breaks=seq(-3, 3, .25))+
  labs(title="87 Church Street - All Data", x="Dimension 1", y="Density") +
  geom_density(fill=NA)
p5a

# Here is the 2d density estimate
dim1dim2forHist <- data.frame(dim1 = rep(CA_MCD$Dim1, CA_MCD$Count), 
                              dim2 = rep(CA_MCD$Dim2, CA_MCD$Count))
library(ggrepel)
set.seed(42)
p5c <- ggplot(dim1dim2forHist, aes(x=dim1,y=dim2))+
  geom_point(shape=21, size=5, colour="black", fill="cornflower blue")+
  # geom_text(aes(label= unit,vjust=-.6, cex=5) +
  theme(plot.title = element_text(hjust = 0.5))+
  #geom_text_repel(aes(label= unit), cex = 4) +
  labs(title="87 Church Street - All Data", 
       x = paste ("Dimension 1"), 
       y= paste ("Dimension 2")
  )
p5c

p5c + geom_density2d_filled(alpha = 0.5, binwidth=0.5) +
  geom_density2d(size = 0.25, colour = "black")

#Dim 2 Scores weighted histogram
#### 14. Histogram of Dim 2 scores for Phasing ####
# Dim 2 Scores Weighted Histogram, you may need to change scale
dim2ForHist<- data.frame(dim2 = rep(CA_MCD$Dim2, CA_MCD$Count))
p5b <- ggplot(dim2ForHist, aes(x = dim2)) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_histogram(aes(y=..density..), colour="black", fill="tan", binwidth=0.1, 
                 boundary= .1) +
  scale_x_continuous(breaks=seq(-3, 9, .5))+
  labs(title="87 Church Street - All Data", x="Dimension 2", y="Density") +
  geom_density(fill=NA)
p5b


# Add lines for phase breaks
# Note -- don't do this until after you have removed outliers
p5a1 <- p5a + geom_vline(xintercept=c(-1.15, -1, -.5, 0, .6), colour = "gray", linetype = "dashed",
                         size=1)      
p5a1

p5b1 <- p5b + geom_vline(xintercept=c(.2, 2.5), colour = "gray", linetype = "dashed",
                         size=1)      
p5b1

#### 15.  Do the Dim 1 -  MCD scatterplot with Phase assignments  ####
# Do the Phase assigments, based on the Dim1 scores
# Note, depending on how many phases you have you will need to add or comment out lines and update boundaries
CA_MCD_PhaseB <- CA_MCD %>% mutate(Phase = case_when (((Dim1 >= 0.6) & (Dim2 <= 0.4)) ~ 'P01',
                                                     ((Dim1 >= 0.6) & (Dim2 > 0.4)) ~ 'P01b',
                                                     (((Dim1 >= 0) &(Dim1 < 0.6)) & (Dim2 <= 0.2)) ~ 'P02', 
                                                     (((Dim1 >= 0) &(Dim1 < 0.6)) & (Dim2 > 0.2)) ~ 'P02b',
                                                     (((Dim1 >= -0.5) &(Dim1 < 0)) & (Dim2 <= 0.2)) ~ 'P03',
                                                     (((Dim1 >= -0.5) &(Dim1 < 0)) & (Dim2 > 0.2)) ~ 'P03b',
                                                     ((Dim1 < -0.5) & (Dim2 <= 0.2)) ~ 'P04',
                                                     ((Dim1 <= -1.2) & (Dim2 <= 2.5)) ~ 'P04b',
                                                     ((Dim1 <= -1.3) & (Dim2 > 2.5)) ~ 'P04c'
))

CA_MCD_Phase <- within(CA_MCD_PhaseB, Phase[unit == 'C03 | 02 | '] <- 'P04b')
CA_MCD_Phase <- within(CA_MCD_Phase, Phase[unit == 'HWK03 | 02 | '] <- 'P04b')

CA_MCD_Phase <- within(CA_MCD_Phase, Phase[is.na(Phase)] <- 'P05')

# BlueMCD By Dim1 plot by Phase
# This one uses DAACS Website colors 
# Be sure to adjust labels and and values accordingly, value for P03 is "darkblue"

p6 <- ggplot(CA_MCD_Phase,aes(x = Dim1, y = blueMCD, 
                              fill= Phase)) +
  #scale_y_continuous(limits=c(1750, 1950)) +
  geom_point(shape=21,  alpha = .75, size= 6)  +
  scale_fill_brewer(name="DAACS Phase",
                    palette = 'Set1') + 
  #geom_text_repel(aes(label= unit), cex=4) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="87 Church Street - All Data", x="Dimension 1", y="BLUE MCD")
p6

#Try this with Dim1 Dim 2

p6z <- ggplot(CA_MCD_Phase,aes(x = Dim1, y = Dim2, 
                               fill= Phase)) +
  #scale_y_continuous(limits=c(1750, 1950)) +
  geom_point(shape=21,  alpha = .75, size= 6)  + 
  scale_fill_brewer(name="DAACS Phase",
                    palette = 'Set1') + 
  #geom_text_repel(aes(label= unit), cex=4) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="87 Church Street - All Data", x="Dimension 1", y="Dimension 2")
p6z

##### 16. Compute the MCDs and TPQs for the phases ####

# join the Phases to the ware by unit data
unitPhase <- select(CA_MCD_Phase, unit, Phase) 

unitPhaseb <- unitPhase %>% rename(CONTEXT = unit)

wareByUnit_Phase<- left_join (wareTypeDataA, unitPhaseb, by = 'CONTEXT') %>%
  mutate(Phase = ifelse(is.na(Phase),'',Phase))

###Save this data as a CSV
#write.csv(wareByUnit_Phase,"87Church_PhasedData.csv", row.names = FALSE)

# Transpose the data for the MCD and CA 
wareByPhaseT <- wareByUnit_Phase %>% group_by(WARE, Phase) %>% 
  summarise(count = sum(COUNT)) %>%
  spread(WARE, value=count , fill=0 )


dataForMCD_Phase <- RemoveTypesNoDates(wareByPhaseT, MCDTypeTable)

# apply the Estimate MCD function
MCDByPhase<-EstimateMCD(dataForMCD_Phase$unitData,
                        dataForMCD_Phase$typeData)

# let's see what it looks like
MCDByPhase

# Exported this data for The Charleston Museum
#write.csv((MCDByPhase[["MCDs"]]),"87Church_MCDsByPhase.csv", row.names = FALSE)

