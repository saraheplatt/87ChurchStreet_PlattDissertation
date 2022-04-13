# 87Church_Feature26Ceramics

# load the libraries
library(dplyr)
library(tidyr)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)
library(data.table)

#Theme set for ggplot

theme_set(theme_classic())

#Load dissertation palette

diss_colors <- c(
  "#06263D",
  "#004E1C",
  "#56B942",
  "#2AA9C2",
  "#C5F2F7",
  "#7C0405",
  "#C02614",
  "#E87F19",
  "#F9EB04")

# Load in final dataset

wareTypeDataA <- read.csv(file = 'Dissertation_FinalDataCSVs/87Church_PhasedData.csv', 
                          fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)


#Summarize

Wares <- wareTypeDataA %>% 
  group_by(WARE, CEW) %>% 
  summarise(Count = sum(COUNT))


#Read in CSV of material equivalencies 

MaterialType <- read.csv(file = 'Dissertation_UrbanSlavery/DAACSWARES_Materials.csv', 
                          fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

#Merge in material data

wareTypeDataB <- merge(wareTypeDataA,MaterialType,by="WARE")

#### Calculate all relative frequencies for feature 26####

# Pull just Feature 26

Feat26RelativeFreqData <- subset(wareTypeDataB, FEATURE == 'F026')

#Summarize data into just needed columns

Feat26RelativeFreqDataC <- Feat26RelativeFreqData %>% 
  group_by(LEVEL, WARE) %>% 
  summarise(Count = sum(COUNT))

#Combine all rows with data lacking vertical provenience (NA) into a single 
#UNPROV designation

Feat26RelativeFreqDataC[is.na(Feat26RelativeFreqDataC)] = 'UNPROV'

#### Calculate Relative Frequencies of Feature 26 Levels

#Transpose the data set
Feat26RelativeFreqDataD <- Feat26RelativeFreqDataC %>% group_by(LEVEL, WARE) %>%
  summarise(Count=sum(Count)) %>%
  spread(WARE,
         value=Count ,
         fill=0 )

#Combine British brown and Frechen into "Brown Stoneware" 
#and Saintonge and French Coarse Earthenware into "French CEW"

Feat26RelativeFreqDataE <- Feat26RelativeFreqDataD %>% mutate(BrownStoneware = 
                                                                `British Brown/Fulham Type` +
                                                                `Frechen Brown`)

Feat26RelativeFreqDataF <- Feat26RelativeFreqDataE %>% mutate(`French CEW` = 
                                                                `Saintonge` +
                                                                `French Coarse Earthenware`)

Feat26RelativeFreqDataF$`British Brown/Fulham Type` <- NULL
Feat26RelativeFreqDataF$`Frechen Brown` <- NULL

Feat26RelativeFreqDataF$`French Coarse Earthenware` <- NULL
Feat26RelativeFreqDataF$`Saintonge` <- NULL

#Calculate sums of ware types
Feat26RelativeFreqDataG <- Feat26RelativeFreqDataF %>% mutate(sumrow= `Agate, refined (Whieldon-type)` +
                                                `Astbury Type` + 
                                                BrownStoneware +
                                                  `Buckley-type` +
                                                `Coarse Earthenware, unidentified` +
                                                `Colonoware` +
                                                `Creamware` +
                                                `Delftware, Dutch/British` +
                                                  `French CEW` +
                                                `Hohr ware` +
                                                `Jackfield Type` +
                                                `Native American` +
                                                `North Devon Gravel Tempered` +
                                                `North Devon Slipware` +
                                                `Nottingham` + 
                                                `Pearlware` +
                                                `Porcelain, Chinese` +
                                                `Porcellaneous/Hard Paste` +
                                                `Slip Dip` +
                                                `Slip-Coated` +
                                                `Slipware, North Midlands/Staffordshire` +
                                                `Spanish Coarse Earthenware` +
                                                `Staffordshire Mottled Glaze` +
                                                `Stoneware, unidentifiable` +
                                                `Unidentifiable` +
                                                `Westerwald/Rhenish` +
                                                `Whieldon-type Ware` +
                                                `White Salt Glaze` +
                                                `Whiteware` 
)

###Calculate relative frequencies 

Feat26RelativeFreqDataH <- Feat26RelativeFreqDataG %>% mutate(AgateRF=`Agate, refined (Whieldon-type)`/sumrow,
                                                     AstburyRF=`Astbury Type`/sumrow,
                                                     BrownStonewareRF=BrownStoneware/sumrow,
                                                     BuckleyRF=`Buckley-type`/sumrow,
                                                     CEWunidRF=`Coarse Earthenware, unidentified`/sumrow,
                                                     ColonowareRF=`Colonoware`/sumrow,
                                                     CreamwareRF=`Creamware`/sumrow,
                                                     DelftRF=`Delftware, Dutch/British`/sumrow,
                                                     FrenchCEWRF=`French CEW`/sumrow,
                                                     HohrRF=`Hohr ware`/sumrow,
                                                     JackfieldRF=`Jackfield Type`/sumrow,
                                                     MangMottRF=`Staffordshire Mottled Glaze`/sumrow,
                                                     NativeAmericanRF=`Native American`/sumrow,
                                                     NorthDevonGTRF=`North Devon Gravel Tempered`/sumrow,
                                                     NorthDevonSlipRF=`North Devon Slipware`/sumrow,
                                                     NottinghamRF=`Nottingham`/sumrow,
                                                     PearlwareRF=`Pearlware`/sumrow,
                                                     ChinesePorcelainRF=`Porcelain, Chinese`/sumrow,
                                                     PorcellaneousRF=`Porcellaneous/Hard Paste`/sumrow,
                                                     SlipDipRF=`Slip Dip`/sumrow,
                                                     SlipCoatedRF=`Slip-Coated`/sumrow,
                                                     SpanishCEWRF=`Spanish Coarse Earthenware`/sumrow,
                                                     StaffSlipRF=`Slipware, North Midlands/Staffordshire`/sumrow,
                                                     StonewareUnidRF=`Stoneware, unidentifiable`/sumrow,
                                                     UnidRF=`Unidentifiable`/sumrow,
                                                     WesterwaldRF=`Westerwald/Rhenish`/sumrow,
                                                     WhieldonRF=`Whieldon-type Ware`/sumrow,
                                                     WhiteSaltGlazedRF=`White Salt Glaze`/sumrow,
                                                     WhitewareRF=`Whiteware`/sumrow
)

#Strip down to just relative Frequencies

Feat26RelativeFreqDataI <- Feat26RelativeFreqDataH %>% 
  select(sumrow, 
         AgateRF,
         AstburyRF,
         BrownStonewareRF,
         BuckleyRF,
         CEWunidRF,
         ColonowareRF,
         CreamwareRF,
         DelftRF,
         FrenchCEWRF,
         HohrRF,
         JackfieldRF,
         MangMottRF,
         NativeAmericanRF,
         NorthDevonGTRF,
         NorthDevonSlipRF,
         NottinghamRF,
         PearlwareRF,
         ChinesePorcelainRF,
         PorcellaneousRF,
         SlipDipRF,
         SlipCoatedRF,
         SpanishCEWRF,
         StaffSlipRF,
         StonewareUnidRF,
         UnidRF,
         WesterwaldRF,
         WhieldonRF,
         WhiteSaltGlazedRF,
         WhitewareRF
  )

#Swap rows and columns

Feat26RelativeFreqDataRFa<-as.data.frame(t(Feat26RelativeFreqDataI))

#Convert row names into a column

Feat26RelativeFreqDataRF <- cbind(rownames(Feat26RelativeFreqDataRFa), 
           data.frame(Feat26RelativeFreqDataRFa, row.names=NULL))

#Swap rows and columns on earlier dataset for sums of ware types

Feat26RelativeFreqDataSumsa<-as.data.frame(t(Feat26RelativeFreqDataG))

#Convert row names into a column

Feat26RelativeFreqDataSums <- cbind(rownames(Feat26RelativeFreqDataSumsa), 
                                  data.frame(Feat26RelativeFreqDataSumsa, row.names=NULL))

#Export 2 CSVs, one final relative frequencies and one with the ware type sums

#Make a csv for tables
write.csv(Feat26RelativeFreqDataRF,
          "Feat26RelativeFreqDataRF.csv", row.names = FALSE)

write.csv(Feat26RelativeFreqDataSums,
          "Feat26RelativeFreqDataSums.csv", row.names = FALSE)

#Outside of R, these two datasets will be combined into a single table.

### Compile results into new columns (analytical material types) for plot

Feat26RelativeFreqPlotA <- Feat26RelativeFreqDataI %>% mutate(Porcelain = 
                                                                ChinesePorcelainRF)
Feat26RelativeFreqPlotA <- Feat26RelativeFreqPlotA %>% mutate("Late Porcelain" = 
                                                                 PorcellaneousRF)
Feat26RelativeFreqPlotA <- Feat26RelativeFreqPlotA %>% mutate("REW Early 18th" = 
                                                                DelftRF)
Feat26RelativeFreqPlotA <- Feat26RelativeFreqPlotA %>% mutate("REW Late 18th" = 
                                                                AgateRF +
                                                                AstburyRF +
                                                                CreamwareRF +
                                                                JackfieldRF +
                                                                WhieldonRF)
Feat26RelativeFreqPlotA <- Feat26RelativeFreqPlotA %>% mutate("REW 19th" = 
                                                                PearlwareRF +
                                                                WhitewareRF)
Feat26RelativeFreqPlotA <- Feat26RelativeFreqPlotA %>% mutate("CEW" = 
                                                                BuckleyRF +
                                                                CEWunidRF + 
                                                                ColonowareRF +
                                                                FrenchCEWRF +
                                                                NativeAmericanRF +
                                                                NorthDevonGTRF +
                                                                SpanishCEWRF)
Feat26RelativeFreqPlotA <- Feat26RelativeFreqPlotA %>% mutate("CEW Common Table" = 
                                                                MangMottRF +
                                                                NorthDevonSlipRF + 
                                                                SlipCoatedRF +
                                                                StaffSlipRF)
Feat26RelativeFreqPlotA <- Feat26RelativeFreqPlotA %>% mutate("Stoneware" = 
                                                                BrownStonewareRF +
                                                                StonewareUnidRF)
Feat26RelativeFreqPlotA <- Feat26RelativeFreqPlotA %>% mutate("Stoneware Common Table" = 
                                                                HohrRF +
                                                                WesterwaldRF + 
                                                                NottinghamRF +
                                                                SlipDipRF +
                                                                WhiteSaltGlazedRF)

#Subset to just needed columns

Feat26RelativeFreqPlotB <- Feat26RelativeFreqPlotA[c('LEVEL', 
                                                     'Porcelain', 'Late Porcelain', "REW Early 18th",
                                                     "REW Late 18th", "REW 19th", 'CEW',
                                                     "CEW Common Table", "Stoneware",
                                                     "Stoneware Common Table")]

#Turn tibble into dataframe
Feat26RelativeFreqPlotC <- as.data.frame(Feat26RelativeFreqPlotB)

#Reshape
Feat26RelativeFreqPlot <- reshape(Feat26RelativeFreqPlotC, 
                                   varying = c('Porcelain', 'Late Porcelain', "REW Early 18th",
                                               "REW Late 18th", "REW 19th", 'CEW',
                                               "CEW Common Table", "Stoneware",
                                               "Stoneware Common Table"), 
                                   v.names = "RelativeFreq",
                                   timevar = "Ware", 
                                   times = c('Porcelain', 'Late Porcelain', "REW Early 18th",
                                             "REW Late 18th", "REW 19th", 'CEW',
                                             "CEW Common Table", "Stoneware",
                                             "Stoneware Common Table"), 
                                   new.row.names = 1:1000,
                                   direction = "long")

####Barplot!
library(RColorBrewer)
ggplot(data=Feat26RelativeFreqPlot, aes(x=LEVEL, y=RelativeFreq, fill=Ware)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c(
    "#06263D",
    "#004E1C",
    "#56B942",
    "#2AA9C2",
    "#C5F2F7",
    "#7C0405",
    "#C02614",
    "#E87F19",
    "#F9EB04")) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(face="bold", 
                               size=14),
    axis.text.y = element_text(face="bold", 
                               size=14)) +
  ylab("Relative Frequency") +
  xlab("Context") +
  ggtitle("87 Church Street - Feature 26 Ceramics")

#### Re run relative frequencies by occupation

# Compile dataset based on occupation categories

Feat26OccRelFreqA <- within(Feat26RelativeFreqDataC, LEVEL[LEVEL == 'L01' | 
                                                          LEVEL == 'L02' |
                                                          LEVEL == 'L03' ] <- "IVtoV")

Feat26OccRelFreqA <- within(Feat26OccRelFreqA, LEVEL[LEVEL == 'L04' | 
                                                             LEVEL == 'L05' ] <- "IIItoV")

Feat26OccRelFreqA <- within(Feat26OccRelFreqA, LEVEL[LEVEL == 'L06' | 
                                                             LEVEL == 'L07' |
                                                             LEVEL == 'L08' |
                                                             LEVEL == 'L09' |
                                                             LEVEL == 'L10' |
                                                             LEVEL == 'L11' |
                                                             LEVEL == 'L12' |
                                                             LEVEL == 'L13' |
                                                             LEVEL == 'L14' |
                                                             LEVEL == 'L15' |
                                                             LEVEL == 'L16' ] <- "III")

#Summarize

Feat26OccRelFreqB <- Feat26OccRelFreqA %>% 
  group_by(LEVEL, WARE) %>% 
  summarise(Count = sum(Count))

#Rename LEVEL to Occupation
Feat26OccRelFreqC <- Feat26OccRelFreqB %>% 
  rename(
    Occupation = LEVEL
  )

#Transpose the data set
Feat26OccRelFreqD <- Feat26OccRelFreqC %>% group_by(Occupation, WARE) %>%
  summarise(Count=sum(Count)) %>%
  spread(WARE,
         value=Count ,
         fill=0 )

#Combine British brown and Frechen into "Brown Stoneware" 
#and Saintonge and French Coarse Earthenware into "French CEW"

Feat26OccRelFreqE <- Feat26OccRelFreqD %>% mutate(BrownStoneware = 
                                                                `British Brown/Fulham Type` +
                                                                `Frechen Brown`)

Feat26OccRelFreqF <- Feat26OccRelFreqE %>% mutate(`French CEW` = 
                                                                `Saintonge` +
                                                                `French Coarse Earthenware`)

Feat26OccRelFreqF$`British Brown/Fulham Type` <- NULL
Feat26OccRelFreqF$`Frechen Brown` <- NULL

Feat26OccRelFreqF$`French Coarse Earthenware` <- NULL
Feat26OccRelFreqF$`Saintonge` <- NULL

#Calculate sums of ware types
Feat26OccRelFreqG <- Feat26OccRelFreqF %>% mutate(sumrow= `Agate, refined (Whieldon-type)` +
                                                                `Astbury Type` + 
                                                                BrownStoneware +
                                                                `Buckley-type` +
                                                                `Coarse Earthenware, unidentified` +
                                                                `Colonoware` +
                                                                `Creamware` +
                                                                `Delftware, Dutch/British` +
                                                                `French CEW` +
                                                                `Hohr ware` +
                                                                `Jackfield Type` +
                                                                `Native American` +
                                                                `North Devon Gravel Tempered` +
                                                                `North Devon Slipware` +
                                                                `Nottingham` + 
                                                                `Pearlware` +
                                                                `Porcelain, Chinese` +
                                                                `Porcellaneous/Hard Paste` +
                                                                `Slip Dip` +
                                                                `Slip-Coated` +
                                                                `Slipware, North Midlands/Staffordshire` +
                                                                `Spanish Coarse Earthenware` +
                                                                `Staffordshire Mottled Glaze` +
                                                                `Stoneware, unidentifiable` +
                                                                `Unidentifiable` +
                                                                `Westerwald/Rhenish` +
                                                                `Whieldon-type Ware` +
                                                                `White Salt Glaze` +
                                                                `Whiteware` 
)

###Calculate relative frequencies 

Feat26OccRelFreqH <- Feat26OccRelFreqG %>% mutate(AgateRF=`Agate, refined (Whieldon-type)`/sumrow,
                                                              AstburyRF=`Astbury Type`/sumrow,
                                                              BrownStonewareRF=BrownStoneware/sumrow,
                                                              BuckleyRF=`Buckley-type`/sumrow,
                                                              CEWunidRF=`Coarse Earthenware, unidentified`/sumrow,
                                                              ColonowareRF=`Colonoware`/sumrow,
                                                              CreamwareRF=`Creamware`/sumrow,
                                                              DelftRF=`Delftware, Dutch/British`/sumrow,
                                                              FrenchCEWRF=`French CEW`/sumrow,
                                                              HohrRF=`Hohr ware`/sumrow,
                                                              JackfieldRF=`Jackfield Type`/sumrow,
                                                              MangMottRF=`Staffordshire Mottled Glaze`/sumrow,
                                                              NativeAmericanRF=`Native American`/sumrow,
                                                              NorthDevonGTRF=`North Devon Gravel Tempered`/sumrow,
                                                              NorthDevonSlipRF=`North Devon Slipware`/sumrow,
                                                              NottinghamRF=`Nottingham`/sumrow,
                                                              PearlwareRF=`Pearlware`/sumrow,
                                                              ChinesePorcelainRF=`Porcelain, Chinese`/sumrow,
                                                              PorcellaneousRF=`Porcellaneous/Hard Paste`/sumrow,
                                                              SlipDipRF=`Slip Dip`/sumrow,
                                                              SlipCoatedRF=`Slip-Coated`/sumrow,
                                                              SpanishCEWRF=`Spanish Coarse Earthenware`/sumrow,
                                                              StaffSlipRF=`Slipware, North Midlands/Staffordshire`/sumrow,
                                                              StonewareUnidRF=`Stoneware, unidentifiable`/sumrow,
                                                              UnidRF=`Unidentifiable`/sumrow,
                                                              WesterwaldRF=`Westerwald/Rhenish`/sumrow,
                                                              WhieldonRF=`Whieldon-type Ware`/sumrow,
                                                              WhiteSaltGlazedRF=`White Salt Glaze`/sumrow,
                                                              WhitewareRF=`Whiteware`/sumrow
)

#Strip down to just relative Frequencies

Feat26OccRelFreqI <- Feat26OccRelFreqH %>% 
  select(sumrow, 
         AgateRF,
         AstburyRF,
         BrownStonewareRF,
         BuckleyRF,
         CEWunidRF,
         ColonowareRF,
         CreamwareRF,
         DelftRF,
         FrenchCEWRF,
         HohrRF,
         JackfieldRF,
         MangMottRF,
         NativeAmericanRF,
         NorthDevonGTRF,
         NorthDevonSlipRF,
         NottinghamRF,
         PearlwareRF,
         ChinesePorcelainRF,
         PorcellaneousRF,
         SlipDipRF,
         SlipCoatedRF,
         SpanishCEWRF,
         StaffSlipRF,
         StonewareUnidRF,
         UnidRF,
         WesterwaldRF,
         WhieldonRF,
         WhiteSaltGlazedRF,
         WhitewareRF
  )

### Compile results into new columns (analytical material types) for plot

Feat26OccRelFreqPlotA <- Feat26OccRelFreqI %>% mutate(Porcelain = 
                                                                ChinesePorcelainRF)
Feat26OccRelFreqPlotA <- Feat26OccRelFreqPlotA %>% mutate("Late Porcelain" = 
                                                                PorcellaneousRF)
Feat26OccRelFreqPlotA <- Feat26OccRelFreqPlotA %>% mutate("REW Early 18th" = 
                                                                DelftRF)
Feat26OccRelFreqPlotA <- Feat26OccRelFreqPlotA %>% mutate("REW Late 18th" = 
                                                                AgateRF +
                                                                AstburyRF +
                                                                CreamwareRF +
                                                                JackfieldRF +
                                                                WhieldonRF)
Feat26OccRelFreqPlotA <- Feat26OccRelFreqPlotA %>% mutate("REW 19th" = 
                                                                PearlwareRF +
                                                                WhitewareRF)
Feat26OccRelFreqPlotA <- Feat26OccRelFreqPlotA %>% mutate("CEW" = 
                                                                BuckleyRF +
                                                                CEWunidRF + 
                                                                ColonowareRF +
                                                                FrenchCEWRF +
                                                                NativeAmericanRF +
                                                                NorthDevonGTRF +
                                                                SpanishCEWRF)
Feat26OccRelFreqPlotA <- Feat26OccRelFreqPlotA %>% mutate("CEW Common Table" = 
                                                                MangMottRF +
                                                                NorthDevonSlipRF + 
                                                                SlipCoatedRF +
                                                                StaffSlipRF)
Feat26OccRelFreqPlotA <- Feat26OccRelFreqPlotA %>% mutate("Stoneware" = 
                                                                BrownStonewareRF +
                                                                StonewareUnidRF)
Feat26OccRelFreqPlotA <- Feat26OccRelFreqPlotA %>% mutate("Stoneware Common Table" = 
                                                                HohrRF +
                                                                WesterwaldRF + 
                                                                NottinghamRF +
                                                                SlipDipRF +
                                                                WhiteSaltGlazedRF)

#Subset to just needed columns

Feat26OccRelFreqPlotB <- Feat26OccRelFreqPlotA[c('Occupation', 
                                                     'Porcelain', 'Late Porcelain', "REW Early 18th",
                                                     "REW Late 18th", "REW 19th", 'CEW',
                                                     "CEW Common Table", "Stoneware",
                                                     "Stoneware Common Table")]

#Remove Unprov
Feat26OccRelFreqPlotB2 <- Feat26OccRelFreqPlotB[-c(4),]

#Turn tibble into dataframe
Feat26OccRelFreqPlotC <- as.data.frame(Feat26OccRelFreqPlotB2)

#Reshape
Feat26OccRelFreqPlot <- reshape(Feat26OccRelFreqPlotC, 
                                  varying = c('Porcelain', 'Late Porcelain', "REW Early 18th",
                                              "REW Late 18th", "REW 19th", 'CEW',
                                              "CEW Common Table", "Stoneware",
                                              "Stoneware Common Table"), 
                                  v.names = "RelativeFreq",
                                  timevar = "Ware", 
                                  times = c('Porcelain', 'Late Porcelain', "REW Early 18th",
                                            "REW Late 18th", "REW 19th", 'CEW',
                                            "CEW Common Table", "Stoneware",
                                            "Stoneware Common Table"), 
                                  new.row.names = 1:1000,
                                  direction = "long")

#
####Barplot!
library(RColorBrewer)
ggplot(data=Feat26OccRelFreqPlot, aes(x=Occupation, y=RelativeFreq, fill=Ware)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c(
    "#06263D",
    "#004E1C",
    "#56B942",
    "#2AA9C2",
    "#C5F2F7",
    "#7C0405",
    "#C02614",
    "#E87F19",
    "#F9EB04")) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(face="bold", 
                               size=14),
    axis.text.y = element_text(face="bold", 
                               size=14)) +
  ylab("Relative Frequency") +
  xlab("Occupation") +
  ggtitle("87 Church Street - Feature 26 Ceramics")





















#Need to turn tibble to dataframe first

Feat26RelativeFreqDataJ <- as.data.frame(Feat26RelativeFreqDataI)

#Reshape

Feat26RelativeFreqDataFIN <- reshape(Feat26RelativeFreqDataJ, 
                                   varying = c("AgateRF",
                                               "AstburyRF",
                                               "BrownStonewareRF",
                                               "BuckleyRF",
                                               "CEWunidRF",
                                               "ColonowareRF",
                                               "CreamwareRF",
                                               "DelftRF",
                                               "FrenchCEWRF",
                                               "HohrRF",
                                               "JackfieldRF",
                                               "MangMottRF",
                                               "NativeAmericanRF",
                                               "NorthDevonGTRF",
                                               "NorthDevonSlipRF",
                                               "NottinghamRF",
                                               "PearlwareRF",
                                               "ChinesePorcelainRF",
                                               "PorcellaneousRF",
                                               "SlipDipRF",
                                               "SlipCoatedRF",
                                               "StaffSlipRF",
                                               "SpanishCEWRF",
                                               "StonewareUnidRF",
                                               "UnidRF",
                                               "WesterwaldRF",
                                               "WhieldonRF",
                                               "WhiteSaltGlazedRF",
                                               "WhitewareRF"), 
                                   v.names = "RelativeFreq",
                                   timevar = "Ware", 
                                   times = c("AgateRF",
                                             "AstburyRF",
                                             "BrownStonewareRF",
                                             "BuckleyRF",
                                             "CEWunidRF",
                                             "ColonowareRF",
                                             "CreamwareRF",
                                             "DelftRF",
                                             "FrenchCEWRFb",
                                             "HohrRF",
                                             "JackfieldRF",
                                             "MangMottRF",
                                             "NativeAmericanRF",
                                             "NorthDevonGTRF",
                                             "NorthDevonSlipRF",
                                             "NottinghamRF",
                                             "PearlwareRF",
                                             "ChinesePorcelainRF",
                                             "PorcellaneousRF",
                                             "SlipDipRF",
                                             "SlipCoatedRF",
                                             "StaffSlipRF",
                                             "SpanishCEWRF",
                                             "StonewareUnidRF",
                                             "UnidRF",
                                             "WesterwaldRF",
                                             "WhieldonRF",
                                             "WhiteSaltGlazedRF",
                                             "WhitewareRF"), 
                                   new.row.names = 1:1000,
                                   direction = "long")

#Create CSV to generate table for chapter

write.csv(Feat26RelativeFreqDataFIN,
          "Feat26RelativeFreqDataFIN.csv", row.names = FALSE)



#Make a csv for tables
write.csv(Feat26RelativeFreqDataI,
          "Feat26RelativeFreqData_Fin.csv", row.names = FALSE)










#### Levels and Feature Plots ####

# Pull just data from needed contexts for 87 Church III

# Feature 26

wareTypeData87CIIIa <- subset(wareTypeDataB, FEATURE == 'F026')

wareTypeData87CIIIa2 <- subset(wareTypeData87CIIIa, (LEVEL == 'L07')|
                                 (LEVEL == 'L08')|
                                 (LEVEL == 'L09')|
                                 (LEVEL == 'L10')|
                                 (LEVEL == 'L11')|
                                 (LEVEL == 'L12')|
                                 (LEVEL == 'L13')|
                                 (LEVEL == 'L14')|
                                 (LEVEL == 'L15')|
                                 (LEVEL == 'L16'))

wareTypeData87CIIIa2 <- subset(wareTypeData87CIIIa,                           
                                   (LEVEL != 'UNPROV') &
                                     !is.na(LEVEL))

wareTypeData87CIIIa3 <- wareTypeData87CIIIa2 %>% 
  group_by(WARE, FEATURE, MATERIAL) %>% 
  summarise(Count = sum(COUNT))

wareTypeData87CIIIaFin <- rename(wareTypeData87CIIIa3, Context = FEATURE)

# Workyard Levels
                              
wareTypeData87CIIIb <- subset(wareTypeDataB,                           
                            (LEVEL == 'L03' & COMPONENT == 'WORKYARD')|
                            (LEVEL == 'L03A' & COMPONENT == 'WORKYARD')|
                            (LEVEL == 'L03B' & COMPONENT == 'WORKYARD')) 

wareTypeData87CIIIb2 <- subset(wareTypeData87CIIIb, is.na(FEATURE))

wareTypeData87CIIIb3 <- wareTypeData87CIIIb2 %>% 
  group_by(WARE, LEVEL, MATERIAL) %>% 
  summarise(Count = sum(COUNT))

wareTypeData87CIIIbFin <- rename(wareTypeData87CIIIb3, Context = LEVEL)

### Combine these into a 87 Church III Dataset

wareTypeData87CIII <- rbind(wareTypeData87CIIIaFin, wareTypeData87CIIIbFin)

## Generate 87 Church I/II dataset

# Pull features
wareTypeData87CIIa <- subset(wareTypeDataB, 
                                (FEATURE %like% '^F065')|
                                (FEATURE %like% '^F166'))

# Collapse features into a single designation

wareTypeData87CIIa2 <- within(wareTypeData87CIIa, FEATURE[FEATURE %like% '^F065'] <- 'F065')
wareTypeData87CIIa3 <- within(wareTypeData87CIIa2, FEATURE[FEATURE %like% '^F166'] <- 'F166')

# Create feature dataset

wareTypeData87CIIa4 <- wareTypeData87CIIa3 %>% 
  group_by(WARE, FEATURE, MATERIAL) %>% 
  summarise(Count = sum(COUNT))

wareTypeData87CIIaFin <- rename(wareTypeData87CIIa4, Context = FEATURE)

### Pull levels

wareTypeData87CIIb <- subset(wareTypeDataB,                           
                              (LEVEL == 'L04' & COMPONENT == 'WORKYARD')|
                                (LEVEL == 'L05' & COMPONENT == 'WORKYARD')|
                                (LEVEL == 'L06' & COMPONENT == 'WORKYARD')|
                               (LEVEL == 'L07' & COMPONENT == 'WORKYARD')|
                               (LEVEL == 'L08' & COMPONENT == 'WORKYARD')) 

wareTypeData87CIIb2 <- subset(wareTypeData87CIIb, is.na(FEATURE))

wareTypeData87CIIb3 <- wareTypeData87CIIb2 %>% 
  group_by(WARE, LEVEL, MATERIAL) %>% 
  summarise(Count = sum(COUNT))

wareTypeData87CIIbFin <- rename(wareTypeData87CIIb3, Context = LEVEL)

### Combine these into a 87 Church III Dataset

wareTypeData87CII <- rbind(wareTypeData87CIIaFin, wareTypeData87CIIbFin)

#### Combine these into a single dataset for relative frequencies

wareTypeData87CMil <- rbind(wareTypeData87CII, wareTypeData87CIII)

#### Relative Frequencies of wares ####

# Summarize

MilTotalsWare <- wareTypeData87CMil %>% 
  group_by(Context, WARE) %>% 
  summarise(Count = sum(Count))

#### Calculate Relative Frequencies 

#Transpose the data set
MilWareRelFreqA <- MilTotalsWare %>% group_by(Context, WARE) %>%
  summarise(Count=sum(Count)) %>%
  spread(WARE,
         value=Count ,
         fill=0 )

#Calculate sums of ware types
MilWareRelFreqB <- MilWareRelFreqA %>% mutate(sumrow= `Agate, refined (Whieldon-type)` + 
                                                `American Stoneware` +
                                                `Astbury Type` + 
                                                `Bennington/Rockingham` +
                                                `Black Basalt` +
                                                `British Brown/Fulham Type` +
                                                `Buckley-type` +
                                                `Caribbean Coarse Earthenware` +
                                                `Coarse Earthenware, unidentified` +
                                                `Colonoware` +
                                                `Creamware` +
                                                `Delftware, Dutch/British` +
                                                `Frechen Brown` +
                                                `French Coarse Earthenware` +
                                                `Hohr ware` +
                                                `Jackfield Type` +
                                                `Majolica` +
                                                `Native American` +
                                                `North Devon Gravel Tempered` +
                                                `North Devon Slipware` +
                                                `Nottingham` + 
                                                `Pearlware` +
                                                `Porcelain, Chinese` +
                                                `Porcelain, unidentified` +
                                                `Porcellaneous/Hard Paste` +
                                                `Post-Medieval London-area Redware` +
                                                `Rosso Antico` +
                                                `Saintonge` +
                                                `Slip Dip` +
                                                `Slip-Coated` +
                                                `Slipware, North Midlands/Staffordshire` +
                                                `Spanish Coarse Earthenware` +
                                                `Staffordshire Mottled Glaze` +
                                                `Stoneware, unidentifiable` +
                                                `Unidentifiable` +
                                                `Westerwald/Rhenish` +
                                                `Whieldon-type Ware` +
                                                `White Salt Glaze` +
                                                `Whiteware` +
                                                `Yellow Ware`
                                               )

###Calculate relative frequencies 

MilWareRelativeFreqFIN <- MilWareRelFreqB %>% mutate(AgateRF=`Agate, refined (Whieldon-type)`/sumrow,
                                                    AmericanStonewareRF=`American Stoneware`/sumrow,
                                                    AstburyRF=`Astbury Type`/sumrow,
                                                    RockinghamRF=`Bennington/Rockingham`/sumrow,
                                                    BlackBasaltRF=`Black Basalt`/sumrow,
                                                    CaribCEWRF=`Caribbean Coarse Earthenware`/sumrow,
                                                    BuckleyRF=`Buckley-type`/sumrow,
                                                    BritishBrownRF=`British Brown/Fulham Type`/sumrow,
                                                    CEWunidRF=`Coarse Earthenware, unidentified`/sumrow,
                                                    ColonowareRF=`Colonoware`/sumrow,
                                                    CreamwareRF=`Creamware`/sumrow,
                                                    DelftRF=`Delftware, Dutch/British`/sumrow,
                                                    FrechenRF=`Frechen Brown`/sumrow,
                                                    FrenchCEWRF=`French Coarse Earthenware`/sumrow,
                                                    HohrRF=`Hohr ware`/sumrow,
                                                    JackfieldRF=`Jackfield Type`/sumrow,
                                                    MajolicaRF=`Majolica`/sumrow,
                                                    NativeAmericanRF=`Native American`/sumrow,
                                                    NorthDevonGTRF=`North Devon Gravel Tempered`/sumrow,
                                                    NorthDevonSlipRF=`North Devon Slipware`/sumrow,
                                                    NottinghamRF=`Nottingham`/sumrow,
                                                    PearlwareRF=`Pearlware`/sumrow,
                                                    ChinesePorcelainRF=`Porcelain, Chinese`/sumrow,
                                                    PorcelainUnidRF=`Porcelain, unidentified`/sumrow,
                                                    PorcellaneousRF=`Porcellaneous/Hard Paste`/sumrow,
                                                    PMLRedRF=`Post-Medieval London-area Redware`/sumrow,
                                                    RossoAnticoRF=`Rosso Antico`/sumrow,
                                                    SaintongeRF=`Saintonge`/sumrow,
                                                    SlipDipRF=`Slip Dip`/sumrow,
                                                    SlipCoatedRF=`Slip-Coated`/sumrow,
                                                    StaffSlipRF=`Slipware, North Midlands/Staffordshire`/sumrow,
                                                    StonewareUnidRF=`Stoneware, unidentifiable`/sumrow,
                                                    UnidRF=`Unidentifiable`/sumrow,
                                                    WesterwaldRF=`Westerwald/Rhenish`/sumrow,
                                                    WhieldonRF=`Whieldon-type Ware`/sumrow,
                                                    WhiteSaltGlazedRF=`White Salt Glaze`/sumrow,
                                                    WhitewareRF=`Whiteware`/sumrow,
                                                    YellowWareRF=`Yellow Ware`/sumrow,
                                                    MangMottRF=`Staffordshire Mottled Glaze`/sumrow,
                                                    SpanishCEWRF=`Spanish Coarse Earthenware`/sumrow,
                                                    )
#Create CSV to generate table for chapter

write.csv(MilWareRelativeFreqFIN,
          "MilWareRelativeFrequencies.csv", row.names = FALSE)










#### Relative Frequencies of material ####

# Summarize

MilTotalsMaterial <- wareTypeData87CMil %>% 
  group_by(Context, MATERIAL) %>% 
  summarise(Count = sum(Count))

#### Calculate Relative Frequencies 

#Transpose the data set
MilMatRelFreqA <- MilTotalsMaterial %>% group_by(Context, MATERIAL) %>%
  summarise(Count=sum(Count)) %>%
  spread(MATERIAL,
         value=Count ,
         fill=0 )

#Calculate sums of ware types
MilMatRelFreqB <- MilMatRelFreqA %>% mutate(sumrow= ChinesePorcelain + CoarseEarthenware +
                                                           CoarseEarthenwareTable + Porcelain + RefinedEarthenware
                                                         + RefinedStoneware + Stoneware +
                                                           TinEnameled + Unidentifiable + Colonoware +
                                                           NativeAmerican)

###Calculate relative frequencies 

MilMatRelFreqFin <- MilMatRelFreqB %>% mutate(ChinesePorcelainRF=ChinesePorcelain/sumrow,
                                                    CoarseEarthenwareRF=CoarseEarthenware/sumrow,
                                                    PorcelainRF=Porcelain/sumrow,
                                                    CoarseEarthenwareTabRF=CoarseEarthenwareTable/sumrow,
                                                    RefinedEarthenwareRF=RefinedEarthenware/sumrow,
                                                    StonewareRefRF=RefinedStoneware/sumrow,
                                                    StonewareRF=Stoneware/sumrow,
                                                    TinEnameledRF=TinEnameled/sumrow,
                                                    UnidentifiableRF=Unidentifiable/sumrow,
                                                    NativeAmericanRF=NativeAmerican/sumrow,
                                                    ColonowareRF=Colonoware/sumrow)

#Create CSV to generate table for chapter

#write.csv(MilMatRelFreqFin,
         # "MilMatRelativeFrequencies.csv", row.names = FALSE)

####Pull just needed contexts

MilMatRelativeFreqsFinb <- MilMatRelFreqFin %>%
  select(Context, ChinesePorcelainRF, RefinedEarthenwareRF, CoarseEarthenwareRF,
         CoarseEarthenwareTabRF, StonewareRF, StonewareRefRF, PorcelainRF, UnidentifiableRF, TinEnameledRF,
         ColonowareRF, NativeAmericanRF)

#Remove relative frequency designation
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, ChinesePorcelain = ChinesePorcelainRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, RefinedEarthenware = RefinedEarthenwareRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, CoarseEarthenware = CoarseEarthenwareRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, CoarseEarthenwareTable = CoarseEarthenwareTabRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, Stoneware = StonewareRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, RefinedStoneware = StonewareRefRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, Porcelain = PorcelainRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, Unidentifiable = UnidentifiableRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, TinEnameled = TinEnameledRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, Colonoware = ColonowareRF)
MilMatRelativeFreqsFinb<- rename(MilMatRelativeFreqsFinb, NativeAmerican = NativeAmericanRF)

#Reshape data

#Need to turn tibble to dataframe first

MilMatRelativeFreqsFinc <- as.data.frame(MilMatRelativeFreqsFinb)

#Reshape

MilMatRelativeFreqsPlot <- reshape(MilMatRelativeFreqsFinc, 
                                varying = c("ChinesePorcelain", "RefinedEarthenware", "CoarseEarthenware", "CoarseEarthenwareTable", "Stoneware",
                                            "RefinedStoneware", "Porcelain", "Unidentifiable", "TinEnameled", "Colonoware",
                                            "NativeAmerican"), 
                                v.names = "RelativeFreq",
                                timevar = "Ware", 
                                times = c("ChinesePorcelain", "RefinedEarthenware", "CoarseEarthenware", "CoarseEarthenwareTable", "Stoneware",
                                          "RefinedStoneware", "Porcelain", "Unidentifiable", "TinEnameled", "Colonoware",
                                          "NativeAmerican"), 
                                new.row.names = 1:1000,
                                direction = "long")

# Select just needed columns

MilMatRelativeFreqsPlotC <- MilMatRelativeFreqsPlot %>%
  select(Context, RelativeFreq, Ware)

# Eliminate unid. and porcelain

MilMatRelativeFreqsPlotD <- subset(MilMatRelativeFreqsPlotC,                           
                             (Ware != 'Porcelain') &
                               (Ware != 'Unidentifiable') &
                               (Ware != 'Stoneware'))

####Barplot!
library(RColorBrewer)
ggplot(data=MilMatRelativeFreqsPlotD, aes(x=Context, y=RelativeFreq, fill=Ware)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c(
    "#06263D",
    "#004E1C",
    "#56B942",
    "#2AA9C2",
    "#C5F2F7",
    "#7C0405",
    "#C02614",
    "#E87F19",
    "#F9EB04")) +
theme(
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  axis.text.x = element_text(face="bold", 
                             size=14),
  axis.text.y = element_text(face="bold", 
                             size=14)) +
  ylab("Relative Frequency") +
  xlab("Context") +
  scale_x_discrete(limits = c("L08", "F166", "F065", "L07", "L06", "L05", "L04", "F026",
                              "L03B", "L03A", "L03"))


#### Feature 26 ####

# Pull just feature 26 records

wareTypeDataC <- subset(wareTypeDataB, FEATURE == 'F026') 

# Summarize

f26Totals <- wareTypeDataC %>% 
  group_by(LEVEL, MATERIAL) %>% 
  summarise(Count = sum(COUNT))

#### Calculate Relative Frequencies 

#Transpose the data set
f26FINALFREQDATASETA <- f26Totals %>% group_by(LEVEL,MATERIAL) %>%
  summarise(Count=sum(Count)) %>%
  spread(MATERIAL,
         value=Count ,
         fill=0 )

#Calculate sums of ware types
f26FINALFREQDATASETAZ <- f26FINALFREQDATASETA %>% mutate(sumrow= CoarseEarthenware + Porcelain
                                                         + RefinedEarthenware + Stoneware + 
                                                           Unidentifiable)

###Calculate relative frequencies 

f26RelativeFreqs <-f26FINALFREQDATASETAZ %>% mutate(PorcelainRF=Porcelain/sumrow,
                                                    CoarseEarthenwareRF=CoarseEarthenware/sumrow,
                                                    StonewareRF=Stoneware/sumrow,
                                                    UnidentifiableRF=Unidentifiable/sumrow,
                                                    RefinedEarthenwareRF=RefinedEarthenware/sumrow)

#Create CSV to generate table for chapter

write.csv(f26RelativeFreqs,
          "Fea26MaterialRelativeFrequencies.csv", row.names = FALSE)

#Need to turn tibble to dataframe first

f26RelativeFreqsB <- as.data.frame(f26RelativeFreqs)

#Select needed columns

f26RelativeFreqsC <- f26RelativeFreqsB %>%
  select(LEVEL, CoarseEarthenwareRF, PorcelainRF, RefinedEarthenwareRF,
         StonewareRF, UnidentifiableRF)

#Reshape

f26MatRelativeFreqsPlot <- reshape(f26RelativeFreqsC, 
                                   varying = c("PorcelainRF", "RefinedEarthenwareRF", "CoarseEarthenwareRF", "StonewareRF",
                                               "UnidentifiableRF"), 
                                   v.names = "RelativeFreq",
                                   timevar = "Material", 
                                   times = c("PorcelainRF", "RefinedEarthenwareRF", "CoarseEarthenwareRF", "StonewareRF",
                                             "UnidentifiableRF"), 
                                   new.row.names = 1:1000,
                                   direction = "long")

f26MatRelativeFreqsPlotB <- subset(f26MatRelativeFreqsPlot,                           
                                   (LEVEL != 'UNPROV') &
                                     !is.na(LEVEL))

####Barplot!
library(RColorBrewer)
ggplot(data=f26MatRelativeFreqsPlotB, aes(x=LEVEL, y=RelativeFreq, fill=Material)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c(
    "#06263D",
    "#004E1C",
    "#56B942",
    "#2AA9C2",
    "#C5F2F7",
    "#7C0405",
    "#C02614",
    "#E87F19",
    "#F9EB04")) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(face="bold", 
                               size=14),
    axis.text.y = element_text(face="bold", 
                               size=14)) +
  ylab("Relative Frequency") +
  xlab("Context") 



#scale_x_discrete(limits = c("L08", "F166", "F065", "L07", "L06", "L05", "L04", "F026",
#"L03B", "L03A", "L03"))

