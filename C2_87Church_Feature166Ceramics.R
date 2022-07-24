#87Church_Feature166Ceramics

# load the libraries
library(dplyr)
library(tidyr)
library(reshape2)
library (ca)
library (plotrix)
library(ggplot2)
library(viridis)
library(data.table)

#Load dissertation palette

diss_colors <- c(
  "#06263D",
  "#004E1C",
  "#56B942",
  "#2AA9C2",
  "#A6CDD1",
  "#7C0405",
  "#C02614",
  "#E87F19",
  "#D8CD02")

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

# Pull feature 65
wareTypeDataF166A <- subset(wareTypeDataB, 
                           (FEATURE %like% '^F166'))

# Summarize into dataset

wareTypeDataF166B <- wareTypeDataF166A %>% 
  group_by(WARE, FEATURE) %>% 
  summarise(Count = sum(COUNT))

#Transpose the data set
wareTypeDataF166C <- wareTypeDataF166B %>% group_by(FEATURE, WARE) %>%
  summarise(Count=sum(Count)) %>%
  spread(WARE,
         value=Count ,
         fill=0 )

#Combine British brown and Frechen into "Brown Stoneware" 
#and Saintonge and French Coarse Earthenware into "French CEW"

wareTypeDataF166D <- wareTypeDataF166C %>% mutate(BrownStoneware = 
                                                        `British Brown/Fulham Type` +
                                                        `Frechen Brown`)

wareTypeDataF166E <- wareTypeDataF166D %>% mutate(`French CEW` = 
                                                        `Saintonge` +
                                                        `French Coarse Earthenware`)

wareTypeDataF166E$`British Brown/Fulham Type` <- NULL
wareTypeDataF166E$`Frechen Brown` <- NULL

wareTypeDataF166E$`French Coarse Earthenware` <- NULL
wareTypeDataF166E$`Saintonge` <- NULL

#Calculate sums of ware types
wareTypeDataF166F <- wareTypeDataF166E %>% mutate(sumrow= 
                                                        `BrownStoneware` +
                                                        `Buckley-type` +
                                                        `Coarse Earthenware, unidentified` +
                                                        `Colonoware` +
                                                        `Delftware, Dutch/British` +
                                                        `French CEW` +
                                                        `Hohr ware` +
                                                        `Majolica` +
                                                        `Native American` +
                                                        `North Devon Gravel Tempered` +
                                                        `North Devon Slipware` +
                                                        `Nottingham` + 
                                                        `Porcelain, Chinese` +
                                                        `Post-Medieval London-area Redware` +
                                                        `Slip Dip` +
                                                        `Slip-Coated` +
                                                        `Slipware, North Midlands/Staffordshire` +
                                                        `Spanish Coarse Earthenware` +
                                                        `Staffordshire Mottled Glaze` +
                                                        `Stoneware, unidentifiable` +
                                                        `Westerwald/Rhenish` +
                                                        `White Salt Glaze`
)

###Calculate relative frequencies 
wareTypeDataF166G <- wareTypeDataF166F %>% mutate(
                                                      BuckleyRF=`Buckley-type`/sumrow,
                                                      BrownStonewareRF=`BrownStoneware`/sumrow,
                                                      CEWunidRF=`Coarse Earthenware, unidentified`/sumrow,
                                                      ColonowareRF=`Colonoware`/sumrow,
                                                      DelftRF=`Delftware, Dutch/British`/sumrow,
                                                      FrenchCEWRF=`French CEW`/sumrow,
                                                      HohrRF=`Hohr ware`/sumrow,
                                                      MajolicaRF=`Majolica`/sumrow,
                                                      NativeAmericanRF=`Native American`/sumrow,
                                                      NorthDevonGTRF=`North Devon Gravel Tempered`/sumrow,
                                                      NorthDevonSlipRF=`North Devon Slipware`/sumrow,
                                                      NottinghamRF=`Nottingham`/sumrow,
                                                      ChinesePorcelainRF=`Porcelain, Chinese`/sumrow,
                                                      PMLRedRF=`Post-Medieval London-area Redware`/sumrow,
                                                      SlipDipRF=`Slip Dip`/sumrow,
                                                      SlipCoatedRF=`Slip-Coated`/sumrow,
                                                      StaffSlipRF=`Slipware, North Midlands/Staffordshire`/sumrow,
                                                      StonewareUnidRF=`Stoneware, unidentifiable`/sumrow,
                                                      WesterwaldRF=`Westerwald/Rhenish`/sumrow,
                                                      WhiteSaltGlazedRF=`White Salt Glaze`/sumrow,
                                                      MangMottRF=`Staffordshire Mottled Glaze`/sumrow,
                                                      SpanishCEWRF=`Spanish Coarse Earthenware`/sumrow,
)

#Strip down to just relative frequencies 

wareTypeDataF166H <- wareTypeDataF166G %>% 
  select(sumrow, 
         BuckleyRF,
         BrownStonewareRF,
         CEWunidRF,
         ColonowareRF,
         DelftRF,
         FrenchCEWRF,
         HohrRF,
         MajolicaRF,
         NativeAmericanRF,
         NorthDevonGTRF,
         NorthDevonSlipRF,
         NottinghamRF,
         ChinesePorcelainRF,
         PMLRedRF,
         SlipDipRF,
         SlipCoatedRF,
         StaffSlipRF,
         StonewareUnidRF,
         WesterwaldRF,
         WhiteSaltGlazedRF,
         MangMottRF,
         SpanishCEWRF
  )

#Swap rows and columns

wareTypeDataF166I<-as.data.frame(t(wareTypeDataF166H))

#Convert row names into a column

wareTypeDataF166J <- cbind(rownames(wareTypeDataF166I), 
                             data.frame(wareTypeDataF166I, row.names=NULL))

#Swap rows and columns on earlier dataset for sums of ware types

#Feat26RelativeFreqDataSumsa<-as.data.frame(t(Feat26RelativeFreqDataG))

#Convert row names into a column

#Feat26RelativeFreqDataSums <- cbind(rownames(Feat26RelativeFreqDataSumsa), 
# data.frame(Feat26RelativeFreqDataSumsa, row.names=NULL))

#Export 2 CSVs, one final relative frequencies and one with the ware type sums

#Make a csv for tables
write.csv(wareTypeDataF166J,
          "Feature166RelativeFreqDataCeramics.csv", row.names = FALSE)

#write.csv(Feat26RelativeFreqDataSums,
# "Feat26RelativeFreqDataSums.csv", row.names = FALSE)

#Outside of R, these two datasets will be combined into a single table.

### Compile results into new columns (analytical material types) for plot

F166PlotDataA <- wareTypeDataF166H %>% mutate(Porcelain = 
                                                       ChinesePorcelainRF)
F166PlotDataA <- F166PlotDataA %>% mutate("REW Early 18th" = 
                                                      DelftRF +
                                                      MajolicaRF)

F166PlotDataA <- F166PlotDataA %>% mutate("CEW" = 
                                                      BuckleyRF +
                                                      CEWunidRF + 
                                                      ColonowareRF +
                                                      FrenchCEWRF +
                                                      NativeAmericanRF +
                                                      NorthDevonGTRF +
                                                      SpanishCEWRF +
                                                      PMLRedRF)
F166PlotDataA <- F166PlotDataA %>% mutate("CEW Common Table" = 
                                                      MangMottRF +
                                                      NorthDevonSlipRF + 
                                                      StaffSlipRF)
F166PlotDataA <- F166PlotDataA %>% mutate("Stoneware" = 
                                                      BrownStonewareRF +
                                                      StonewareUnidRF )
F166PlotDataA <- F166PlotDataA %>% mutate("Stoneware Common Table" = 
                                                      HohrRF +
                                                      WesterwaldRF + 
                                                      NottinghamRF +
                                                      SlipDipRF +
                                                      WhiteSaltGlazedRF)

#Subset to just needed columns

F166PlotDataB <- F166PlotDataA[c('FEATURE', 
                                           'Porcelain', "REW Early 18th"
                                           , 'CEW',
                                           "CEW Common Table", "Stoneware",
                                           "Stoneware Common Table")]

#Turn tibble into dataframe
F166PlotDataC <- as.data.frame(F166PlotDataB)

#Reshape
F166PlotDataD <- reshape(F166PlotDataC, 
                              varying = c('Porcelain', "REW Early 18th",
                                          'CEW',
                                          "CEW Common Table", "Stoneware",
                                          "Stoneware Common Table"), 
                              v.names = "RelativeFreq",
                              timevar = "Ware", 
                              times = c('Porcelain', "REW Early 18th",
                                        'CEW',
                                        "CEW Common Table", "Stoneware",
                                        "Stoneware Common Table"), 
                              new.row.names = 1:1000,
                              direction = "long")

####Barplot!
ggplot(data=F166PlotDataD, aes(x=FEATURE, y=RelativeFreq, fill=Ware)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c(
    "#06263D",
    "#004E1C",
    "#56B942",
    "#2AA9C2",
    "#A6CDD1",
    "#7C0405",
    "#C02614",
    "#E87F19",
    "#D8CD02")) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(face="bold", 
                               size=14),
    axis.text.y = element_text(face="bold", 
                               size=14)) +
  ylab("Relative Frequency") +
  xlab("Context") +
  ggtitle("87 Church Street - Feature 166 Ceramics") 


