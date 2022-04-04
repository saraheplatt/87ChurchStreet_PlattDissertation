# 87Church_UrbanSlaveryCeramics

# load the libraries
library(dplyr)
library(tidyr)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)

# Load in final dataset

wareTypeDataA <- read.csv(file = '87Church_PhasedData.csv', 
                          fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

# Create CSV with all ware types for material type assignments

#Summarize

Wares <- wareTypeDataA %>% 
  group_by(WARE, CEW) %>% 
  summarise(Count = sum(COUNT))

#Create CSV

write.csv(Wares,
          "DAACSWARES.csv", row.names = FALSE)

#Read in CSV

MaterialType <- read.csv(file = 'DAACSWARES_Materials.csv', 
                          fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

#Merge in material data

wareTypeDataB <- merge(wareTypeDataA,MaterialType,by="WARE")

### Feature 26 

# Pull just feature 26 records

wareTypeDataC <- subset(wareTypeDataB, FEATURE == 'F026') 

# Summarize

Feature26Totals <- wareTypeDataC %>% 
  group_by(LEVEL, MATERIAL) %>% 
  summarise(Count = sum(COUNT))

#### Calculate Relative Frequencies

#Transpose the data set
f26FINALFREQDATASETA <- Feature26Totals %>% group_by(LEVEL,MATERIAL) %>%
  summarise(Count=sum(Count)) %>%
  spread(MATERIAL,
         value=Count ,
         fill=0 )

#Calculate sums of ware types
f26FINALFREQDATASETAZ <- f26FINALFREQDATASETA %>% mutate(sumrow= ChinesePorcelain + CoarseEarthenware +
                                                     CoarseEarthenwareTable + Porcelain + RefinedEarthenware
                                                   + RefinedStoneware + Stoneware +
                                                     TinEnameled + Unidentifiable)

###Calculate relative frequencies 

f26RelativeFreqs <-f26FINALFREQDATASETAZ %>% mutate(ChinesePorcelainRF=ChinesePorcelain/sumrow,
                                              CoarseEarthenwareRF=CoarseEarthenware/sumrow,
                                              PorcelainRF=Porcelain/sumrow,
                                              CoarseEarthenwareTabRF=CoarseEarthenwareTable/sumrow,
                                              RefinedEarthenwareRF=RefinedEarthenware/sumrow,
                                              StonewareRefRF=RefinedStoneware/sumrow,
                                              StonewareRF=Stoneware/sumrow,
                                              TinEnameledRF=TinEnameled/sumrow,
                                              UnidentifiableRF=Unidentifiable/sumrow)

####Pull just needed contexts

f26RelativeFreqsFinal <- f26RelativeFreqs %>%
  select(LEVEL, ChinesePorcelainRF, RefinedEarthenwareRF, CoarseEarthenwareRF,
         CoarseEarthenwareTabRF, StonewareRF, StonewareRefRF, PorcelainRF, UnidentifiableRF, TinEnameledRF)

#Remove relative frequency designation
f26RelativeFreqsFinal<- rename(f26RelativeFreqsFinal, ChinesePorcelain = ChinesePorcelainRF)
f26RelativeFreqsFinal<- rename(f26RelativeFreqsFinal, RefinedEarthenware = RefinedEarthenwareRF)
f26RelativeFreqsFinal<- rename(f26RelativeFreqsFinal, CoarseEarthenware = CoarseEarthenwareRF)
f26RelativeFreqsFinal<- rename(f26RelativeFreqsFinal, CoarseEarthenwareTable = CoarseEarthenwareTabRF)
f26RelativeFreqsFinal<- rename(f26RelativeFreqsFinal, Stoneware = StonewareRF)
f26RelativeFreqsFinal<- rename(f26RelativeFreqsFinal, RefinedStoneware = StonewareRefRF)
f26RelativeFreqsFinal<- rename(f26RelativeFreqsFinal, Porcelain = PorcelainRF)
f26RelativeFreqsFinal<- rename(f26RelativeFreqsFinal, Unidentifiable = UnidentifiableRF)
f26RelativeFreqsFinal<- rename(f26RelativeFreqsFinal, TinEnameled = TinEnameledRF)

#Reshape data

#Need to turn tibble to dataframe first

f26RelativeFreqsFinal <- as.data.frame(f26RelativeFreqsFinal)

#Reshape

f26RelativeFreqsPlot <- reshape(f26RelativeFreqsFinal, 
             varying = c("ChinesePorcelain", "RefinedEarthenware", "CoarseEarthenware", "CoarseEarthenwareTable", "Stoneware",
                         "RefinedStoneware", "Porcelain", "Unidentifiable", "TinEnameled"), 
             v.names = "RelativeFreq",
             timevar = "Ware", 
             times = c("ChinesePorcelain", "RefinedEarthenware", "CoarseEarthenware", "CoarseEarthenwareTable", "Stoneware",
                       "RefinedStoneware", "Porcelain", "Unidentifiable", "TinEnameled"), 
             new.row.names = 1:1000,
             direction = "long")

# Select just needed columns

f26RelativeFreqsPlotB <- f26RelativeFreqsPlot %>%
  select(LEVEL, RelativeFreq, Ware)

# Eliminate UNPROV and NA

f26RelativeFreqsPlotB <- f26RelativeFreqsPlotB %>% filter()

f26RelativeFreqsPlotB <- subset(f26RelativeFreqsPlotB, LEVEL != is.na(LEVEL)) 
f26RelativeFreqsPlotB <- subset(f26RelativeFreqsPlotB, LEVEL != 'UNPROV') 
f26RelativeFreqsPlotB <- subset(f26RelativeFreqsPlotB, LEVEL != 'L01') 
f26RelativeFreqsPlotB <- subset(f26RelativeFreqsPlotB, LEVEL != 'L02') 


####Barplot!
library(RColorBrewer)
ggplot(data=f26RelativeFreqsPlotB, aes(x=LEVEL, y=RelativeFreq, fill=Ware)) +
  geom_bar(stat="identity", position="dodge") +
  scale_colour_brewer(palette = "Dark2")+
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  ) + theme(axis.text.x = element_text(face="bold", 
                                       size=14),
            axis.text.y = element_text(face="bold", 
                                       size=14))+
  ylab("Relative Frequency") +
  xlab("")


#+
#  scale_x_discrete(limits = c("F166", "F065", "L08", "L07", "L06", "L05", "L04", "L03"))
