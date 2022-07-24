# 87Church_87ChurchItoIIICeramics

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

#### Calculate all relative frequencies for select 
#87 Church I to III contexts ####

# Pull just data from needed contexts for 87 Church III

# Feature 26

wareTypeData87CIIIa <- subset(wareTypeDataB, FEATURE == 'F026')

wareTypeData87CIIIa2 <- subset(wareTypeData87CIIIa, (LEVEL == 'L06')|
                                 (LEVEL == 'L07')|
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

wareTypeData87CIIIa4 <- rename(wareTypeData87CIIIa3, Context = FEATURE)

# Workyard Levels

wareTypeData87CIIIb <- subset(wareTypeDataB,                           
                              (LEVEL == 'L03' & COMPONENT == 'WORKYARD')|
                                (LEVEL == 'L03A' & COMPONENT == 'WORKYARD')|
                                (LEVEL == 'L03B' & COMPONENT == 'WORKYARD')) 

wareTypeData87CIIIb2 <- subset(wareTypeData87CIIIb, is.na(FEATURE))

wareTypeData87CIIIb3 <- wareTypeData87CIIIb2 %>% 
  group_by(WARE, LEVEL, MATERIAL) %>% 
  summarise(Count = sum(COUNT))

wareTypeData87CIIIb4 <- rename(wareTypeData87CIIIb3, Context = LEVEL)

### Combine these into a 87 Church III Dataset

wareTypeData87CIII <- rbind(wareTypeData87CIIIa4, wareTypeData87CIIIb4)

## Generate 87 Church I/II dataset

# Pull features
wareTypeData87CIIa <- subset(wareTypeDataB, 
                             (FEATURE %like% '^F065')|
                               (FEATURE %like% '^F166'))

# Collapse features into a single designation

wareTypeData87CIIa2 <- within(wareTypeData87CIIa, FEATURE[FEATURE =='F065a' |
                                                            FEATURE == 'F065b' |
                                                            FEATURE == 'F065c' |
                                                            FEATURE == 'F065d' |
                                                            FEATURE == 'F065e'] <- 'F065Early')

wareTypeData87CIIa2b <- within(wareTypeData87CIIa2, FEATURE[FEATURE =='F065' |
                                                            FEATURE == 'F065x'] <- 'F065Late')

wareTypeData87CIIa3 <- within(wareTypeData87CIIa2b, FEATURE[FEATURE %like% '^F166'] <- 'F166')

# Create feature dataset

wareTypeData87CIIa4 <- wareTypeData87CIIa3 %>% 
  group_by(WARE, FEATURE, MATERIAL) %>% 
  summarise(Count = sum(COUNT))

wareTypeData87CIIa5 <- rename(wareTypeData87CIIa4, Context = FEATURE)

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

wareTypeData87CIIb4 <- rename(wareTypeData87CIIb3, Context = LEVEL)

### Combine these into a 87 Church III Dataset

wareTypeData87CII <- rbind(wareTypeData87CIIa5, wareTypeData87CIIb4)

#### Combine these into a single dataset for relative frequencies

wareTypeData87CMil <- rbind(wareTypeData87CII, wareTypeData87CIII)

#### Calculate Relative Frequencies

#Transpose the data set
wareTypeData87CMilA <- wareTypeData87CMil %>% group_by(Context, WARE) %>%
  summarise(Count=sum(Count)) %>%
  spread(WARE,
         value=Count ,
         fill=0 )

#Combine British brown and Frechen into "Brown Stoneware" 
#and Saintonge and French Coarse Earthenware into "French CEW"

wareTypeData87CMilB <- wareTypeData87CMilA %>% mutate(BrownStoneware = 
                                                                `British Brown/Fulham Type` +
                                                                `Frechen Brown`)

wareTypeData87CMilC <- wareTypeData87CMilB %>% mutate(`French CEW` = 
                                                                `Saintonge` +
                                                                `French Coarse Earthenware`)

wareTypeData87CMilC$`British Brown/Fulham Type` <- NULL
wareTypeData87CMilC$`Frechen Brown` <- NULL

wareTypeData87CMilC$`French Coarse Earthenware` <- NULL
wareTypeData87CMilC$`Saintonge` <- NULL

#Calculate sums of ware types
wareTypeData87CMilD <- wareTypeData87CMilC %>% mutate(sumrow= `Agate, refined (Whieldon-type)` + 
                                                                `American Stoneware` +
                                                                `Astbury Type` + 
                                                                `Bennington/Rockingham` +
                                                                `Black Basalt` +
                                                                `BrownStoneware` +
                                                                `Buckley-type` +
                                                                `Caribbean Coarse Earthenware` +
                                                                `Coarse Earthenware, unidentified` +
                                                                `Colonoware` +
                                                                `Creamware` +
                                                                `Delftware, Dutch/British` +
                                                                `French CEW` +
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
wareTypeData87CMilE <- wareTypeData87CMilD %>% mutate(AgateRF=`Agate, refined (Whieldon-type)`/sumrow,
                                                     AmericanStonewareRF=`American Stoneware`/sumrow,
                                                     AstburyRF=`Astbury Type`/sumrow,
                                                     RockinghamRF=`Bennington/Rockingham`/sumrow,
                                                     BlackBasaltRF=`Black Basalt`/sumrow,
                                                     CaribCEWRF=`Caribbean Coarse Earthenware`/sumrow,
                                                     BuckleyRF=`Buckley-type`/sumrow,
                                                     BrownStonewareRF=`BrownStoneware`/sumrow,
                                                     CEWunidRF=`Coarse Earthenware, unidentified`/sumrow,
                                                     ColonowareRF=`Colonoware`/sumrow,
                                                     CreamwareRF=`Creamware`/sumrow,
                                                     DelftRF=`Delftware, Dutch/British`/sumrow,
                                                     FrenchCEWRF=`French CEW`/sumrow,
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

#Strip down to just relative frequencies 

wareTypeData87CMilF <- wareTypeData87CMilE %>% 
  select(sumrow, 
         AgateRF,
         AmericanStonewareRF,
         AstburyRF,
         RockinghamRF,
         BlackBasaltRF,
         CaribCEWRF,
         BuckleyRF,
         BrownStonewareRF,
         CEWunidRF,
         ColonowareRF,
         CreamwareRF,
         DelftRF,
         FrenchCEWRF,
         HohrRF,
         JackfieldRF,
         MajolicaRF,
         NativeAmericanRF,
         NorthDevonGTRF,
         NorthDevonSlipRF,
         NottinghamRF,
         PearlwareRF,
         ChinesePorcelainRF,
         PorcelainUnidRF,
         PorcellaneousRF,
         PMLRedRF,
         RossoAnticoRF,
         SlipDipRF,
         SlipCoatedRF,
         StaffSlipRF,
         StonewareUnidRF,
         UnidRF,
         WesterwaldRF,
         WhieldonRF,
         WhiteSaltGlazedRF,
         WhitewareRF,
         YellowWareRF,
         MangMottRF,
         SpanishCEWRF
  )

#Swap rows and columns

wareTypeData87CMilG<-as.data.frame(t(wareTypeData87CMilF))

#Convert row names into a column

wareTypeData87CMilH <- cbind(rownames(wareTypeData87CMilG), 
                                  data.frame(wareTypeData87CMilG, row.names=NULL))

#Swap rows and columns on earlier dataset for sums of ware types

#Feat26RelativeFreqDataSumsa<-as.data.frame(t(Feat26RelativeFreqDataG))

#Convert row names into a column

#Feat26RelativeFreqDataSums <- cbind(rownames(Feat26RelativeFreqDataSumsa), 
                                   # data.frame(Feat26RelativeFreqDataSumsa, row.names=NULL))

#Export 2 CSVs, one final relative frequencies and one with the ware type sums

#Make a csv for tables
write.csv(wareTypeData87CMilH,
          "87CItoIIIRelativeFreqDataCeramics.csv", row.names = FALSE)

#write.csv(Feat26RelativeFreqDataSums,
         # "Feat26RelativeFreqDataSums.csv", row.names = FALSE)

#Outside of R, these two datasets will be combined into a single table.

### Compile results into new columns (analytical material types) for plot

Mil87ChurchRFPlotA <- wareTypeData87CMilF %>% mutate(Porcelain = 
                                                                ChinesePorcelainRF)
Mil87ChurchRFPlotA <- Mil87ChurchRFPlotA %>% mutate("Late Porcelain" = 
                                                                PorcellaneousRF +
                                                                PorcelainUnidRF)
Mil87ChurchRFPlotA <- Mil87ChurchRFPlotA %>% mutate("REW Early 18th" = 
                                                                DelftRF +
                                                                MajolicaRF)
Mil87ChurchRFPlotA <- Mil87ChurchRFPlotA %>% mutate("REW Late 18th" = 
                                                                AgateRF +
                                                                AstburyRF +
                                                                CreamwareRF +
                                                                JackfieldRF +
                                                                WhieldonRF)
Mil87ChurchRFPlotA <- Mil87ChurchRFPlotA %>% mutate("REW 19th" = 
                                                                PearlwareRF +
                                                                WhitewareRF +
                                                                RockinghamRF +
                                                                YellowWareRF)
Mil87ChurchRFPlotA <- Mil87ChurchRFPlotA %>% mutate("CEW" = 
                                                                BuckleyRF +
                                                                CEWunidRF + 
                                                                ColonowareRF +
                                                                FrenchCEWRF +
                                                                NativeAmericanRF +
                                                                NorthDevonGTRF +
                                                                SpanishCEWRF +
                                                                CaribCEWRF +
                                                                PMLRedRF)
Mil87ChurchRFPlotA <- Mil87ChurchRFPlotA %>% mutate("CEW Common Table" = 
                                                                MangMottRF +
                                                                NorthDevonSlipRF + 
                                                                SlipCoatedRF +
                                                                StaffSlipRF)
Mil87ChurchRFPlotA <- Mil87ChurchRFPlotA %>% mutate("Stoneware" = 
                                                                BrownStonewareRF +
                                                                StonewareUnidRF +
                                                                AmericanStonewareRF)
Mil87ChurchRFPlotA <- Mil87ChurchRFPlotA %>% mutate("Stoneware Common Table" = 
                                                                HohrRF +
                                                                WesterwaldRF + 
                                                                NottinghamRF +
                                                                SlipDipRF +
                                                                WhiteSaltGlazedRF +
                                                                RossoAnticoRF)

#Subset to just needed columns

Mil87ChurchRFPlotB <- Mil87ChurchRFPlotA[c('Context', 
                                                     'Porcelain', 'Late Porcelain', "REW Early 18th",
                                                     "REW Late 18th", "REW 19th", 'CEW',
                                                     "CEW Common Table", "Stoneware",
                                                     "Stoneware Common Table")]

#Turn tibble into dataframe
Mil87ChurchRFPlotC <- as.data.frame(Mil87ChurchRFPlotB)

#Reshape
Mil87ChurchRFPlotD <- reshape(Mil87ChurchRFPlotC, 
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
ggplot(data=Mil87ChurchRFPlotD, aes(x=Context, y=RelativeFreq, fill=Ware)) +
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
  ggtitle("87 Church Street - Occupations I through III Ceramics") +
  scale_x_discrete(limits = c("L08", "F065Early", "F166", "F065Late", 
                              "L07", "L06", "L05", "L04",
                               "F026",
                              "L03B", "L03A", "L03"))


#### Re run relative frequencies by occupation

# Compile dataset based on occupation categories

MilOccRelFreqA <- within(wareTypeData87CMil, Context[Context == 'F065Early' | 
                                                             Context == 'L08' |
                                                             Context == 'F166' ] <- "ItoII")

MilOccRelFreqB <- within(MilOccRelFreqA, Context[Context == 'L07' | 
                                                   Context == 'L06' |
                                                   Context == 'L05' |
                                                   Context == 'L04'] <- "II")

MilOccRelFreqC <- within(MilOccRelFreqB, Context[
                                                       Context == 'L03B' |
                                                       Context == 'F026' |
                                                       Context == 'F065Late' 
                                                       ] <- "III")

MilOccRelFreqD <- within(MilOccRelFreqC, Context[Context == 'L03A' |
                                                   Context == 'L03' 
                                                    ] <- "IIItoV")


#Summarize

MilOccRelFreqE  <- MilOccRelFreqD  %>% 
  group_by(Context, WARE) %>%
  summarise(Count=sum(Count))

#Rename LEVEL to Occupation
MilOccRelFreqF <- MilOccRelFreqE %>% 
  rename(
    Occupation = Context
  )

#Transpose the data set
MilOccRelFreqG <- MilOccRelFreqF %>% group_by(Occupation, WARE) %>%
  summarise(Count=sum(Count)) %>%
  spread(WARE,
         value=Count ,
         fill=0 )

#Combine British brown and Frechen into "Brown Stoneware" 
#and Saintonge and French Coarse Earthenware into "French CEW"

MilOccRelFreqH <- MilOccRelFreqG %>% mutate(BrownStoneware = 
                                                    `British Brown/Fulham Type` +
                                                    `Frechen Brown`)

MilOccRelFreqI <- MilOccRelFreqH %>% mutate(`French CEW` = 
                                                    `Saintonge` +
                                                    `French Coarse Earthenware`)

MilOccRelFreqI$`British Brown/Fulham Type` <- NULL
MilOccRelFreqI$`Frechen Brown` <- NULL

MilOccRelFreqI$`French Coarse Earthenware` <- NULL
MilOccRelFreqI$`Saintonge` <- NULL

#Calculate sums of ware types
MilOccRelFreqJ <- MilOccRelFreqI %>% mutate(sumrow= `Agate, refined (Whieldon-type)` + 
                                                        `American Stoneware` +
                                                        `Astbury Type` + 
                                                        `Bennington/Rockingham` +
                                                        `Black Basalt` +
                                                        `BrownStoneware` +
                                                        `Buckley-type` +
                                                        `Caribbean Coarse Earthenware` +
                                                        `Coarse Earthenware, unidentified` +
                                                        `Colonoware` +
                                                        `Creamware` +
                                                        `Delftware, Dutch/British` +
                                                        `French CEW` +
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
MilOccRelFreqK <- MilOccRelFreqJ %>% mutate(AgateRF=`Agate, refined (Whieldon-type)`/sumrow,
                                                      AmericanStonewareRF=`American Stoneware`/sumrow,
                                                      AstburyRF=`Astbury Type`/sumrow,
                                                      RockinghamRF=`Bennington/Rockingham`/sumrow,
                                                      BlackBasaltRF=`Black Basalt`/sumrow,
                                                      CaribCEWRF=`Caribbean Coarse Earthenware`/sumrow,
                                                      BuckleyRF=`Buckley-type`/sumrow,
                                                      BrownStonewareRF=`BrownStoneware`/sumrow,
                                                      CEWunidRF=`Coarse Earthenware, unidentified`/sumrow,
                                                      ColonowareRF=`Colonoware`/sumrow,
                                                      CreamwareRF=`Creamware`/sumrow,
                                                      DelftRF=`Delftware, Dutch/British`/sumrow,
                                                      FrenchCEWRF=`French CEW`/sumrow,
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

#Strip down to just relative frequencies 

MilOccRelFreqL <- MilOccRelFreqK %>% 
  select(sumrow, 
         AgateRF,
         AmericanStonewareRF,
         AstburyRF,
         RockinghamRF,
         BlackBasaltRF,
         CaribCEWRF,
         BuckleyRF,
         BrownStonewareRF,
         CEWunidRF,
         ColonowareRF,
         CreamwareRF,
         DelftRF,
         FrenchCEWRF,
         HohrRF,
         JackfieldRF,
         MajolicaRF,
         NativeAmericanRF,
         NorthDevonGTRF,
         NorthDevonSlipRF,
         NottinghamRF,
         PearlwareRF,
         ChinesePorcelainRF,
         PorcelainUnidRF,
         PorcellaneousRF,
         PMLRedRF,
         RossoAnticoRF,
         SlipDipRF,
         SlipCoatedRF,
         StaffSlipRF,
         StonewareUnidRF,
         UnidRF,
         WesterwaldRF,
         WhieldonRF,
         WhiteSaltGlazedRF,
         WhitewareRF,
         YellowWareRF,
         MangMottRF,
         SpanishCEWRF
  )

#Swap rows and columns

MilOccRelFreqM<-as.data.frame(t(MilOccRelFreqL))

#Convert row names into a column

MilOccRelFreqO <- cbind(rownames(MilOccRelFreqM), 
                             data.frame(MilOccRelFreqM, row.names=NULL))

#Swap rows and columns on earlier dataset for sums of ware types

#Feat26RelativeFreqDataSumsa<-as.data.frame(t(Feat26RelativeFreqDataG))

#Convert row names into a column

#Feat26RelativeFreqDataSums <- cbind(rownames(Feat26RelativeFreqDataSumsa), 
# data.frame(Feat26RelativeFreqDataSumsa, row.names=NULL))

#Export 2 CSVs, one final relative frequencies and one with the ware type sums

#Make a csv for tables
#write.csv(wareTypeData87CMilH,
          #"87CItoIIIRelativeFreqDataCeramics.csv", row.names = FALSE)

#write.csv(Feat26RelativeFreqDataSums,
# "Feat26RelativeFreqDataSums.csv", row.names = FALSE)

#Outside of R, these two datasets will be combined into a single table.

### Compile results into new columns (analytical material types) for plot

MilOccRelFreqM <- MilOccRelFreqL %>% mutate(Porcelain = 
                                                       ChinesePorcelainRF)
MilOccRelFreqM <- MilOccRelFreqM %>% mutate("Late Porcelain" = 
                                                      PorcellaneousRF +
                                                      PorcelainUnidRF)
MilOccRelFreqM <- MilOccRelFreqM %>% mutate("REW Early 18th" = 
                                                      DelftRF +
                                                      MajolicaRF)
MilOccRelFreqM <- MilOccRelFreqM %>% mutate("REW Late 18th" = 
                                                      AgateRF +
                                                      AstburyRF +
                                                      CreamwareRF +
                                                      JackfieldRF +
                                                      WhieldonRF)
MilOccRelFreqM <- MilOccRelFreqM %>% mutate("REW 19th" = 
                                                      PearlwareRF +
                                                      WhitewareRF +
                                                      RockinghamRF +
                                                      YellowWareRF)
MilOccRelFreqM <- MilOccRelFreqM %>% mutate("CEW" = 
                                                      BuckleyRF +
                                                      CEWunidRF + 
                                                      ColonowareRF +
                                                      FrenchCEWRF +
                                                      NativeAmericanRF +
                                                      NorthDevonGTRF +
                                                      SpanishCEWRF +
                                                      CaribCEWRF +
                                                      PMLRedRF)
MilOccRelFreqM <- MilOccRelFreqM %>% mutate("CEW Common Table" = 
                                                      MangMottRF +
                                                      NorthDevonSlipRF + 
                                                      SlipCoatedRF +
                                                      StaffSlipRF)
MilOccRelFreqM <- MilOccRelFreqM %>% mutate("Stoneware" = 
                                                      BrownStonewareRF +
                                                      StonewareUnidRF +
                                                      AmericanStonewareRF)
MilOccRelFreqM <- MilOccRelFreqM %>% mutate("Stoneware Common Table" = 
                                                      HohrRF +
                                                      WesterwaldRF + 
                                                      NottinghamRF +
                                                      SlipDipRF +
                                                      WhiteSaltGlazedRF +
                                                      RossoAnticoRF)

#Subset to just needed columns

MilOccRelFreqN <- MilOccRelFreqM[c('Occupation', 
                                           'Porcelain', 'Late Porcelain', "REW Early 18th",
                                           "REW Late 18th", "REW 19th", 'CEW',
                                           "CEW Common Table", "Stoneware",
                                           "Stoneware Common Table")]

#Turn tibble into dataframe
MilOccRelFreqO <- as.data.frame(MilOccRelFreqN)

#Reshape
MilOccRelFreqP <- reshape(MilOccRelFreqO, 
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
ggplot(data=MilOccRelFreqP, aes(x=Occupation, y=RelativeFreq, fill=Ware)) +
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
  xlab("Occupation") +
  ggtitle("87 Church Street - Occupations I through III Ceramics") +
  scale_x_discrete(limits = c("ItoII", "II", "III", "IIItoV"))
