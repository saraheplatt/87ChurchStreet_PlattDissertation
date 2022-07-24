#87Church_Feature65Ceramics

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
wareTypeDataF65A <- subset(wareTypeDataB, 
                             (FEATURE %like% '^F065'))

# Summarize into dataset

wareTypeDataF65B <- wareTypeDataF65A %>% 
  group_by(WARE, FEATURE) %>% 
  summarise(Count = sum(COUNT))

#Transpose the data set
wareTypeDataF65C <- wareTypeDataF65B %>% group_by(FEATURE, WARE) %>%
  summarise(Count=sum(Count)) %>%
  spread(WARE,
         value=Count ,
         fill=0 )

#Combine British brown and Frechen into "Brown Stoneware" 

wareTypeDataF65D <- wareTypeDataF65C %>% mutate(BrownStoneware = 
                                                        `British Brown/Fulham Type`)

wareTypeDataF65D$`British Brown/Fulham Type` <- NULL

#Calculate sums of ware types
wareTypeDataF65E <- wareTypeDataF65D %>% mutate(sumrow= 
                                                        `Astbury Type` + 
                                                        `BrownStoneware` +
                                                        `Buckley-type` +
                                                        `Coarse Earthenware, unidentified` +
                                                        `Colonoware` +
                                                        `Creamware` +
                                                        `Delftware, Dutch/British` +
                                                        `Hohr ware` +
                                                        `Majolica` +
                                                        `Native American` +
                                                        `North Devon Gravel Tempered` +
                                                        `North Devon Slipware` +
                                                        `Nottingham` + 
                                                        `Porcelain, Chinese` +
                                                        `Slip Dip` +
                                                        `Slipware, North Midlands/Staffordshire` +
                                                        `Spanish Coarse Earthenware` +
                                                        `Stoneware, unidentifiable` +
                                                        `Westerwald/Rhenish` +
                                                        `Whieldon-type Ware` +
                                                        `White Salt Glaze`
)

###Calculate relative frequencies 
wareTypeDataF65F <- wareTypeDataF65E %>% mutate(
                                                      AstburyRF=`Astbury Type`/sumrow,
                                                      BuckleyRF=`Buckley-type`/sumrow,
                                                      BrownStonewareRF=`BrownStoneware`/sumrow,
                                                      CEWunidRF=`Coarse Earthenware, unidentified`/sumrow,
                                                      ColonowareRF=`Colonoware`/sumrow,
                                                      CreamwareRF=`Creamware`/sumrow,
                                                      DelftRF=`Delftware, Dutch/British`/sumrow,
                                                      HohrRF=`Hohr ware`/sumrow,
                                                      MajolicaRF=`Majolica`/sumrow,
                                                      NativeAmericanRF=`Native American`/sumrow,
                                                      NorthDevonGTRF=`North Devon Gravel Tempered`/sumrow,
                                                      NorthDevonSlipRF=`North Devon Slipware`/sumrow,
                                                      NottinghamRF=`Nottingham`/sumrow,
                                                      ChinesePorcelainRF=`Porcelain, Chinese`/sumrow,
                                                      SlipDipRF=`Slip Dip`/sumrow,
                                                      StaffSlipRF=`Slipware, North Midlands/Staffordshire`/sumrow,
                                                      StonewareUnidRF=`Stoneware, unidentifiable`/sumrow,
                                                      WesterwaldRF=`Westerwald/Rhenish`/sumrow,
                                                      WhieldonRF=`Whieldon-type Ware`/sumrow,
                                                      WhiteSaltGlazedRF=`White Salt Glaze`/sumrow,
                                                      SpanishCEWRF=`Spanish Coarse Earthenware`/sumrow,
)

#Strip down to just relative frequencies 

wareTypeDataF65G <- wareTypeDataF65F %>% 
  select(sumrow, 
         AstburyRF,
         BuckleyRF,
         BrownStonewareRF,
         CEWunidRF,
         ColonowareRF,
         CreamwareRF,
         DelftRF,
         HohrRF,
         MajolicaRF,
         NativeAmericanRF,
         NorthDevonGTRF,
         NorthDevonSlipRF,
         NottinghamRF,
         ChinesePorcelainRF,
         SlipDipRF,
         StaffSlipRF,
         StonewareUnidRF,
         WesterwaldRF,
         WhieldonRF,
         WhiteSaltGlazedRF,
         SpanishCEWRF
  )

#Swap rows and columns

wareTypeDataF65H<-as.data.frame(t(wareTypeDataF65G))

#Convert row names into a column

wareTypeData87CMilI <- cbind(rownames(wareTypeDataF65H), 
                             data.frame(wareTypeDataF65H, row.names=NULL))

#Make a csv for tables
write.csv(wareTypeData87CMilI,
          "Feature65CeramicsRelativeFreq.csv", row.names = FALSE)








