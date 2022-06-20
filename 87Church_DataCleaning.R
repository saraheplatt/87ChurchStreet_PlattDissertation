# 87Church_DataCleaning.R
# Created by:  SEP 12.3.21

# load the libraries
library(dplyr)
library(tidyr)
library(reshape2)
library (plotrix)
library(ggplot2)
library(viridis)
library(data.table)

#### 1. Initial PastPerfect data upload and streamlining 

# Read in CSV of Past Perfect data

InitialImportData<- read.csv(file = '87Church_Raw_PastPerfect_Feb2020_Final.csv', 
                             fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

# Eliminate all columns except for the variables I will be using in the dissertation

StreamlinedData <- InitialImportData %>%
  select(OBJECTID, PARENT, OBJNAME, DESCRIP, UNIT, LEVEL, FEATURE, COUNT)

#### 2. Create a type dataframe to examine what is there, and what needs to be eliminated

CMTypesA <- StreamlinedData %>%
  select(PARENT, OBJNAME, COUNT)

CMTypesB <- CMTypesA %>% group_by(PARENT, OBJNAME) %>% 
  summarise(Count=sum(COUNT))

# Eliminate non-ceramics and non-vessel ceramic records. This data was not wholly collected 
# and will not be used in any large dissertation analyses except for singular artifacts.


StreamlinedDataB <- filter(StreamlinedData, PARENT != 'Accessories, writing')
StreamlinedDataC <- filter(StreamlinedDataB, PARENT != 'Brick')
StreamlinedDataD <- filter(StreamlinedDataC, PARENT != 'Container glass')
StreamlinedDataE <- filter(StreamlinedDataD, PARENT != 'Gun part')
StreamlinedDataF <- filter(StreamlinedDataE, PARENT != 'Roof tile')
StreamlinedDataG <- filter(StreamlinedDataF, PARENT != 'Toys and children')
StreamlinedDataH <- filter(StreamlinedDataG, PARENT != "Ceramics, children's dishes" )

#### 3. Check for counts of zero. Due to the nature of legacy data, there were
# instances of ARL numbers not being able to be broken up by count- often this was 
# cataloger error. In initial data entry, no count was assigned for these 
# ARL numbers. For the purposes of analyses, in these instances a count of 1
# was assigned to indicate presence

Zeros <- subset(StreamlinedDataH, is.na(COUNT), select=OBJECTID)

# Here the NAs are replaced by a count of 1 to indicate presence

StreamlinedDataHb <- StreamlinedDataH %>% replace_na(list('COUNT' = 1))

#### 4. Clean up context variable data entry errors

### 4a. Check and clean up the unit variable

CMContextsUnitSum <-  StreamlinedDataHb %>% 
  group_by(UNIT) %>% 
  summarise(Count = sum(COUNT))

# Start cleaning, line by line

# A Block

StreamlinedDataH1 <- StreamlinedDataHb %>% 
  mutate(NewUNIT=ifelse(UNIT %in% 'A', 'A00', UNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A 14', 'A14', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A 19', 'A19', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A 24', 'A24', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A?', 'AUNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A1', 'A01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A 24', 'A24', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A2', 'A02', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A3', 'A03', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A3b', 'A03b', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A 24', 'A24', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A4', 'A04', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A5', 'A05', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A6', 'A06', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A7', 'A07', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A8', 'A08', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'A9', 'A09', NewUNIT))

# B Block

StreamlinedDataH1 <- StreamlinedDataH1 %>% 
  mutate(NewUNIT=ifelse(UNIT %in% 'B', 'B00', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B 13', 'B13', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B 21', 'B21', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B1', 'B01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B1', 'B01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B13m', 'B13UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B2', 'B02', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B3', 'B03', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B4', 'B04', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B5', 'B05', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B6', 'B06', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B7', 'B07', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B8', 'B08', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'B9', 'B09', NewUNIT))

# Check the results

CMContextsUnitSum2 <-  StreamlinedDataH1 %>% 
  group_by(NewUNIT) %>% 
  summarise(Count = sum(COUNT))

# C Units

StreamlinedDataH1 <- StreamlinedDataH1 %>% 
  mutate(NewUNIT=ifelse(UNIT %in% 'C', 'C00', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'C1', 'C01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'C2', 'C02', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'C3', 'C03', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'C4', 'C04', NewUNIT))

# Check the results

CMContextsUnitSum2 <-  StreamlinedDataH1 %>% 
  group_by(NewUNIT) %>% 
  summarise(Count = sum(COUNT))

# D Units

StreamlinedDataH1 <- StreamlinedDataH1 %>% 
  mutate(NewUNIT=ifelse(UNIT %in% 'D', 'D00', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D Illegible', 'DUNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D1', 'D01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D2', 'D02', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D3', 'D03', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D4', 'D04', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D5', 'D05', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D6', 'D06', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D7', 'D07', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D8', 'D08', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'D9', 'D09', NewUNIT)) 

# Check the results

CMContextsUnitSum2 <-  StreamlinedDataH1 %>% 
  group_by(NewUNIT) %>% 
  summarise(Count = sum(COUNT))

# E Units

StreamlinedDataH1 <- StreamlinedDataH1 %>% 
  mutate(NewUNIT=ifelse(UNIT %in% 'E', 'E00', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'E1', 'E01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'E2', 'E02', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'E3', 'E03', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'E4', 'E04', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'E5', 'E05', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'E6', 'E06', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'E7', 'E07', NewUNIT))

# Check the results

CMContextsUnitSum2 <-  StreamlinedDataH1 %>% 
  group_by(NewUNIT) %>% 
  summarise(Count = sum(COUNT))

# Eliminate any feature designations from this column and replace with NA

StreamlinedDataH1 <- StreamlinedDataH1 %>% 
  mutate(NewUNIT=ifelse(UNIT %in% 'F 26', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'F 65', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'F 77', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'F. 26', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'F. 93', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'F136T', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'F166', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'F26', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'F77', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea 65', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea 65a', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea 65b', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea 65d', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea 65e', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 183', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 25', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 46', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 48', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 65', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 65c', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 65e', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 65x', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 77', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 89', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 93', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 95', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Fea. 97', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Feature 26', NA, NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Feature 77', NA, NewUNIT))

# Check the results

CMContextsUnitSum2 <-  StreamlinedDataH1 %>% 
  group_by(NewUNIT) %>% 
  summarise(Count = sum(COUNT))

# Kitchen units

StreamlinedDataH1 <- StreamlinedDataH1 %>% 
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK', 'HWK00', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK I', 'HWK01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK II', 'HWK02', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK III', 'HWK03', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK illegible', 'HWKUNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK IV', 'HWK04', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK IX', 'HWK09', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK surface', 'HWKSURF', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK unmarked', 'HWKUNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK Unmarked', 'HWKUNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK V', 'HWK05', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK VI', 'HWK06', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK VII', 'HWK07', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK VIII', 'HWK08', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK X', 'HWK10', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XI', 'HWK11', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XII', 'HWK12', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XIII', 'HWK13', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XIV', 'HWK14', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XIX', 'HWK19', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XV', 'HWK15', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XVI', 'HWK16', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XVII', 'HWK17', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XVIII', 'HWK18', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XX', 'HWK20', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XXI', 'HWK21', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XXII', 'HWK22', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK XXIII', 'HWK23', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWK? II', 'HWK02', NewUNIT))

# Check the results

CMContextsUnitSum2 <-  StreamlinedDataH1 %>% 
  group_by(NewUNIT) %>% 
  summarise(Count = sum(COUNT))

# Privy

StreamlinedDataH1 <- StreamlinedDataH1 %>% 
  mutate(NewUNIT=ifelse(UNIT %in% 'HWN', 'HWNUNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWN 1', 'HWN01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWN I', 'HWN01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWN I misc', 'HWN01', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWN IB', 'HWN01B', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWN II', 'HWN02', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWN Square II', 'HWN02', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'HWN unmarked', 'HWNUNPROV', NewUNIT))

# Check the results

CMContextsUnitSum2 <-  StreamlinedDataH1 %>% 
  group_by(NewUNIT) %>% 
  summarise(Count = sum(COUNT))

# Unprovenienced materials and stray HWK units

StreamlinedDataH1 <- StreamlinedDataH1 %>% 
  mutate(NewUNIT=ifelse(UNIT %in% 'Illegible', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Illegible/unmarked', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'IV', 'HWK04', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'misc', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Misc', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'misc.', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Misc.', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Misc/illegible', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'misc/no #', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Misc/no label', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Miscellaneous', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Surface', 'UNPROVSURF', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Miscellaneous', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Trench 1', 'TRENCH', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Unknown', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Unlabeled', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Unmarked', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'unmarked', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Unmarked/illegible', 'UNPROV', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'VII', 'HWK07', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'X', 'HWK10', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'XIV', 'HWK14', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Garden Trench', 'GT', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'Garden Trench W', 'GTW', NewUNIT)) %>%
  mutate(NewUNIT=ifelse(UNIT %in% 'H', 'UNPROV', NewUNIT))

# Check the results

CMContextsUnitSum2 <-  StreamlinedDataH1 %>% 
  group_by(NewUNIT) %>% 
  summarise(Count = sum(COUNT))

# Make Unit blanks "NA", first make copy of data in case it gets messed up

StreamlinedDataH2 <- StreamlinedDataH1

StreamlinedDataH2[StreamlinedDataH2 == ""] <- NA 

# Check the results

CMContextsUnitSum2 <-  StreamlinedDataH2 %>% 
  group_by(NewUNIT) %>% 
  summarise(Count = sum(COUNT))

### 4b. Check and clean up the level variable

CMContextsLevelSum <-  StreamlinedDataH2 %>% 
  group_by(LEVEL) %>% 
  summarise(Count = sum(COUNT))

# Start cleaning, line by line. Start with known levels.

StreamlinedDataH3 <- StreamlinedDataH2 %>% 
  mutate(NewLEVEL=ifelse(LEVEL %in% '?', 'UNPROV', LEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '1', '01', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '1 and 2', '0102', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '1,2', '0102', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '1,6', '0106', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '1/2/2003', '010203', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '166b', NA, NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '18/23/3', '010203', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '2', '02', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '2-Jan', '0102', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '3', '03', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '3-Feb', '0203', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '3,4', '0304', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '3a', '03A', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '3A', '03A', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '3b', '03B', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '3B', '03B', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '4', '04', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '4-Mar', '0304', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '3,4', '0304', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '4a', '04A', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '4b', '04B', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '5', '05', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '5-Feb', '0205', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '5a', '05A', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '5b', '05B', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '6', '06', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '6-Feb', '0206', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '6-Jan', '0106', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '6-May', '0506', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '6 - below forge', '06BF', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '6n', '06N', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '7', '07', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '7-Apr', '0407', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '7-Jun', '0607', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '7,8', '0708', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '78', '0708', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '7a', '07A', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '7b', '07B', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '8', '08', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '8-Jul', '0708', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '9', '09', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '9, 10', '0910', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% '37', '03B', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'Illegible', 'UNPROV', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'illegible', 'UNPROV', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'misc', 'UNPROV', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'Misc.', 'UNPROV', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'miscellaneous', 'UNPROV', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'Miscellaneous', 'UNPROV', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'Patio 3', 'PIII', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'Patio II', 'PII', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'Patio III', 'PIII', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'Surface', 'SURF', NewLEVEL)) %>%
  mutate(NewLEVEL=ifelse(LEVEL %in% 'surface', 'SURF', NewLEVEL))

# Check results

CMContextsLevelSum <-  StreamlinedDataH3 %>% 
  group_by(NewLEVEL) %>% 
  summarise(Count = sum(COUNT))

# Here pause to take a look at relationship between levels and units/features

CMContextsSum <-  StreamlinedDataH3 %>% 
  group_by(NewUNIT, NewLEVEL, FEATURE) %>% 
  summarise(Count = sum(COUNT))

### Here start correcting some of these levels based on the relationships

StreamlinedDataH4 <- within(StreamlinedDataH3, FEATURE[NewLEVEL == 'b' & FEATURE == '65'] <- '065b')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'b' & FEATURE == '065b'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'a' & NewUNIT == 'B15'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, NewUNIT[NewLEVEL == 'B' & NewUNIT == 'HWN01'] <- 'HWN01B')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'B' & NewUNIT == 'HWN01B'] <- NA)

StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'B116/3b' & NewUNIT == 'B14'] <- 'UNPROV')
StreamlinedDataH4 <- within(StreamlinedDataH4, FEATURE[NewLEVEL == 'D' & FEATURE == '65'] <- '065d')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'D' & FEATURE == '065d'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, FEATURE[NewLEVEL == 'F' & FEATURE == '45'] <- '045f')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'F' & FEATURE == '045f'] <- NA)

StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'g' & NewUNIT == 'A07'] <- 'UNPROV')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'I' & NewUNIT == 'HWN01'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, FEATURE[NewLEVEL == 'I' & FEATURE == '45'] <- '045I')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'I' & FEATURE == '045I'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'II' & NewUNIT == 'HWK00'] <- '02')
StreamlinedDataH4 <- within(StreamlinedDataH4, FEATURE[NewLEVEL == 'II' & FEATURE == '45'] <- '045II')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'II' & FEATURE == '045II'] <- NA)

StreamlinedDataH4 <- within(StreamlinedDataH4, FEATURE[NewLEVEL == 'L' & FEATURE == '7'] <- '007L')
StreamlinedDataH4 <- within(StreamlinedDataH4, FEATURE[NewLEVEL == 'Lower' & FEATURE == '7'] <- '007L')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'L' & FEATURE == '007L'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'Lower' & FEATURE == '007L'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, NewUNIT[NewUNIT == 'HWK00' & FEATURE == '007L'] <- NA)

StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'm' & NewUNIT == 'B13'] <- 'UNPROV')
StreamlinedDataH4 <- within(StreamlinedDataH4, FEATURE[NewLEVEL == 'W' & FEATURE == '45'] <- '045w')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'W' & FEATURE == '045w'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, FEATURE[NewLEVEL == 'Z' & FEATURE == '45'] <- '045z')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'Z' & FEATURE == '045z'] <- NA)

StreamlinedDataH4 <- within(StreamlinedDataH4, FEATURE[NewLEVEL == 'U' & FEATURE == '45'] <- '045u')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'U' & FEATURE == '045u'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'Unmarked' & NewUNIT == 'HWNUNPROV'] <- NA)
StreamlinedDataH4 <- within(StreamlinedDataH4, NewUNIT[NewLEVEL == 'West End' & NewUNIT == 'GT'] <- 'GTW')
StreamlinedDataH4 <- within(StreamlinedDataH4, NewLEVEL[NewLEVEL == 'West End' & NewUNIT == 'GTW'] <- NA)

#Check results periodically
CMContextsSumB <-  StreamlinedDataH4 %>% 
  group_by(NewUNIT, NewLEVEL, FEATURE) %>% 
  summarise(Count = sum(COUNT))


### 4c. Check and clean up the feature variable

# First check the variable
CMContextsSumFea <-  StreamlinedDataH4 %>% 
  group_by(FEATURE) %>% 
  summarise(Count = sum(COUNT))

# Start to update as needed

StreamlinedDataH5 <- StreamlinedDataH4 %>% 
  mutate(NewFEATURE=ifelse(FEATURE %in% '1', '001', FEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '127B', '127b', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '131A', '131a', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '131B', '131b', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '136A', '136a', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '136T', '136t', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '19', '019', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '131B', '131b', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '2', '002', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '21', '021', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '24', '024', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '25', '025', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '26', '026', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '27', '027', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '28', '028', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '3', '003', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '30', '030', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '31', '031', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '32', '032', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '33', '033', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '34', '034', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '35', '035', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '36', '036', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '37', '037', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '37', '037', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '37 below forge', '037bf', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '38', '038', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '3b', '003b', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '4', '004', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '40', '040', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '41', '041', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '43', '043', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '45', '045', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '46', '046', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '47', '047', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '48', '048', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '49', '049', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '50', '050', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '5', '005', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '51', '051', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '54', '054', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '55', '055', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '56', '056', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '57', '057', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '59', '059', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '6', '006', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '60', '060', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '61', '061', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '62', '062', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '63', '063', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '65', '065', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '65a', '065a', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '65b', '065b', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '65c', '065c', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '65d', '065d', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '65E', '065e', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '65e', '065e', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '65I', '065i', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% "651", '065i', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '65x', '065x', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '69', '069', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '7', '007', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '71', '071', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '72', '072', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '73', '073', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '74', '074', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '75', '075', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '76', '076', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '77', '077', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '79', '079', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '7L', '007L', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '8', '008', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '80', '080', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '81', '081', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '82', '082', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '83', '083', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '84', '084', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '85', '085', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '86', '086', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '87', '087', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '88', '088', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '89', '089', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '9', '009', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '90', '090', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '91', '091', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '92', '092', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '93', '093', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '94', '094', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '95', '095', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '96', '096', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '97', '097', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '98', '098', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% '99', '099', NewFEATURE)) 

StreamlinedDataH5 <- StreamlinedDataH5 %>% 
  mutate(NewFEATURE=ifelse(FEATURE %in% 'F166', '166', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Fea 166', '166', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 113', '113', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 136T', '136t', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 166', '166', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 166a', '166a', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 166b', '166b', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 178', '178', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 178a', '178a', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 178b', '178b', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 183', '183', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 19', '019', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 201', '201', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 25', '025', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 26', '026', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 27', '027', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 33', '033', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 37', '037', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 38', '038', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 46', '046', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 5', '005', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 50', '050', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 55', '055', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 61', '061', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 62', '062', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 65', '065', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 65a', '065a', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 65b', '065b', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 65c', '065c', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 65d', '065d', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 65e', '065e', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 65', '065x', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 7', '007', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 72', '072', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 75', '075', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 77', '077', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 78', '078', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 7L', '007L', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 8', '008', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 81', '081', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 86', '086', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 89', '089', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 91', '091', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 93', '093', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 95', '095', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 97', '097', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 26 level 13', '026', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 48', '048', NewFEATURE)) %>%
  mutate(NewFEATURE=ifelse(FEATURE %in% 'Feature 65x', '065x', NewFEATURE))

# Check the results periodically

CMContextsSumNFea <-  StreamlinedDataH5 %>% 
  group_by(NewFEATURE) %>% 
  summarise(Count = sum(COUNT))

# Eliminate feature details from level variable, first check what's there

CMContextsSumNFeaB <-  StreamlinedDataH5 %>% 
  group_by(NewLEVEL, NewFEATURE) %>% 
  summarise(Count = sum(COUNT))

# Eliminate feature details

StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 91'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 86'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 81'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 78'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 75'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 72'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 62'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 61'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 55'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 50'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 38'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 37'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 33'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 27'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 25'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 201'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 19'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 183'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 178b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 178a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 178'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 166b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 166a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea. 166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea 178'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'Fea 166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F8'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F7L'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F7'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F183'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F166b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'f166b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F113'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F. 166b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F 8'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F 7L'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F 7'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F 5'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F 48'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F 46'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == 'F 166b'] <- NA)

CMContextsSumNFeaC <-  StreamlinedDataH5 %>% 
  group_by(NewLEVEL, NewFEATURE) %>% 
  summarise(Count = sum(COUNT))

# Check all the results against the old variable

CMContextsSumV <-  StreamlinedDataH5 %>% 
  group_by(UNIT, LEVEL, FEATURE, NewUNIT, NewLEVEL, NewFEATURE) %>% 
  summarise(Count = sum(COUNT))

# Make individual fixes

StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewFEATURE == 'Illegible' & NewUNIT == 'B14'] <- 'UNPROV')
StreamlinedDataH5 <- within(StreamlinedDataH5, NewFEATURE[NewLEVEL == 'UNPROV' & NewUNIT == 'B14'] <- NA)

# A Unit checks. Here, I am primarily eliminating any level or unit designations from features.

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewLEVEL == '02' & NewUNIT == 'A00' & NewFEATURE 
                                                       == '038'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == '02' & NewFEATURE == '038'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewLEVEL == '05' & NewUNIT == 'A04' & NewFEATURE 
                                                       == '093'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == '05' & NewFEATURE == '093'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A04' & NewFEATURE == '049'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewLEVEL == '06' & NewUNIT == 'A05' & NewFEATURE 
                                                       == '037'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == '06' & NewFEATURE == '037'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewLEVEL == '06' & NewUNIT == 'A05' & NewFEATURE 
                                                       == '037bf'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == '06' & NewFEATURE == '037bf'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewFEATURE[NewLEVEL == '06BF' & NewUNIT == 'A05' & NewFEATURE 
                                                          == '037'] <- '037bf')
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewLEVEL == '06BF' & NewUNIT == 'A05' & NewFEATURE 
                                                       == '037bf'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == '06BF' & NewFEATURE == '037bf'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A06' & NewFEATURE == '112'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A06' & NewFEATURE == '030'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A08' & NewFEATURE == '034'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A08' & NewFEATURE == '045'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A08' & NewFEATURE == '051'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewLEVEL == '09' & NewUNIT == 'A05' & NewFEATURE 
                                                       == '045'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == '09' & NewFEATURE == '045'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A09' & NewFEATURE == '048'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewLEVEL == '01' & NewUNIT == 'A10' & NewFEATURE 
                                                       == '045'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == '01' & NewFEATURE == '045'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewLEVEL == '09' & NewUNIT == 'A05' & NewFEATURE 
                                                       == '045'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewLEVEL == '09' & NewFEATURE == '045'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A14' & NewFEATURE == '166a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A16' & NewFEATURE == '045'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A17' & NewFEATURE == '127a'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A18' & NewFEATURE == '046'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A18' & NewFEATURE == '131b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A18' & NewFEATURE == '090'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A22' & NewFEATURE == '148'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'A24' & NewFEATURE == '136'] <- NA)

# Check these results

CMContextsSumV2 <-  StreamlinedDataH5 %>% 
  group_by(UNIT, LEVEL, FEATURE, NewUNIT, NewLEVEL, NewFEATURE) %>% 
  summarise(Count = sum(COUNT))

# B units, same thing

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B00' & NewFEATURE == '131a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B00' & NewFEATURE == '165'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B00' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B00' & NewFEATURE == '183'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B00' & NewFEATURE == '036'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B00' & NewFEATURE == '038'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B00' & NewFEATURE == '089'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B03' & NewFEATURE == '065'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B06' & NewFEATURE == '073'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B06' & NewFEATURE == '076'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B07' & NewFEATURE == '031'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B10' & NewFEATURE == '142'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B10' & NewFEATURE == '144'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B10' & NewFEATURE == '043'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B11' & NewFEATURE == '077'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B12' & NewFEATURE == '139'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B12' & NewFEATURE == '140'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B13' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B13' & NewFEATURE == '166b'] <- NA)

# Brief pause to check

CMContextsSumV3 <-  StreamlinedDataH5 %>% 
  group_by(UNIT, LEVEL, FEATURE, NewUNIT, NewLEVEL, NewFEATURE) %>% 
  summarise(Count = sum(COUNT))

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B13' & NewFEATURE == '163'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B13' & NewFEATURE == '164'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B13' & NewFEATURE == '165'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B13' & NewFEATURE == '166a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B13' & NewFEATURE == '168'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B13/14' & NewFEATURE == '166b'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewFEATURE[NewUNIT == 'B14' & NewFEATURE == '003b'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B14' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B14' & NewFEATURE == '166a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B14' & NewFEATURE == '166b'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B15' & NewFEATURE == '166b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B15' & NewFEATURE == '170'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B16' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B16' & NewFEATURE == '166a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B16' & NewFEATURE == '166b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B16' & NewFEATURE == '173'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B16' & NewFEATURE == '075'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B17' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B17' & NewFEATURE == '166a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B17' & NewFEATURE == '166b'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B17' & NewFEATURE == '176'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B17' & NewFEATURE == '184'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B17' & NewFEATURE == '185'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B18' & NewFEATURE == '176'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B18' & NewFEATURE == '184'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B18' & NewFEATURE == '185'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B19' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B19' & NewFEATURE == '166b'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B20' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B20' & NewFEATURE == '166b'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B20' & NewFEATURE == '161'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B21' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B21' & NewFEATURE == '166a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B21' & NewFEATURE == '166b'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B21' & NewFEATURE == '191'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B22' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B22' & NewFEATURE == '166a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B22' & NewFEATURE == '166b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B22' & NewFEATURE == '178'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B22' & NewFEATURE == '178a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B22' & NewFEATURE == '178b'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B22' & NewFEATURE == '201'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewFEATURE[NewUNIT == 'B23' & NewFEATURE == '003'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B23' & NewFEATURE == '093'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B23' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B23' & NewFEATURE == '083'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B23' & NewFEATURE == '183'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B24' & NewFEATURE == '166'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B24' & NewFEATURE == '166a'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'B24' & NewFEATURE == '166b'] <- NA)

# Check results again

CMContextsSumV4 <-  StreamlinedDataH5 %>% 
  group_by(UNIT, LEVEL, FEATURE, NewUNIT, NewLEVEL, NewFEATURE) %>% 
  summarise(Count = sum(COUNT))

# Anything that's left

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'D13' & NewFEATURE == '128'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'E03' & NewFEATURE == '113'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'E03' & NewFEATURE == '119'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK00' & NewFEATURE == '005'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK00' & NewFEATURE == '007'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK00' & NewFEATURE == '007L'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK00' & NewFEATURE == '008'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK00' & NewFEATURE == '001'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK00' & NewFEATURE == '002'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK00' & NewFEATURE == '003'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK00' & NewFEATURE == '004'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK00' & NewFEATURE == '009'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewLEVEL[NewUNIT == 'HWK00' & NewFEATURE == '047'] <- 'UNPROV')
StreamlinedDataH5 <- within(StreamlinedDataH5, NewFEATURE[NewUNIT == 'HWK00' & NewFEATURE == '047'] <- NA)

StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK02' & NewFEATURE == '007'] <- NA)
StreamlinedDataH5 <- within(StreamlinedDataH5, NewUNIT[NewUNIT == 'HWK06' & NewFEATURE == '004'] <- NA)

# Check results again

CMContextsSumV5 <-  StreamlinedDataH5 %>% 
  group_by(UNIT, LEVEL, FEATURE, NewUNIT, NewLEVEL, NewFEATURE) %>% 
  summarise(Count = sum(COUNT))

# Look at overall counts minus old contexts

CMContextsSumV6 <-  StreamlinedDataH5 %>% 
  group_by(NewUNIT, NewLEVEL, NewFEATURE) %>% 
  summarise(Count = sum(COUNT))

# Create a new dataframe with copied data in case I mess up

StreamlinedDataH6 <- StreamlinedDataH5
StreamlinedDataH6b <- StreamlinedDataH5

### 4d. Create new collapsed variable called "DAACS CONTEXT" using all of the cleaned context
# variables

# First replaced NAs JUST for the DAACS context variable
StreamlinedDataH6b[is.na(StreamlinedDataH6b)] <- ""

# Then use these two dataframes to create compressed DAACS context variable

StreamlinedDataH6$DAACSCONT <- paste(StreamlinedDataH6b$NewUNIT, "|", 
                                     StreamlinedDataH6b$NewLEVEL, "|",
                                     StreamlinedDataH6b$NewFEATURE, sep = "")

# Added leading Ls and Fs to levels and features because excel sucks

StreamlinedDataH6$NewFEATURE <- paste('F', StreamlinedDataH6$NewFEATURE, sep = "")
StreamlinedDataH6$NewLEVEL <- paste('L', StreamlinedDataH6$NewLEVE, sep = "")

StreamlinedDataH6 <- within(StreamlinedDataH6, NewLEVEL[NewLEVEL == 'LNA'] <- NA)
StreamlinedDataH6 <- within(StreamlinedDataH6, NewFEATURE[NewFEATURE == 'FNA'] <- NA)
StreamlinedDataH6 <- within(StreamlinedDataH6, NewLEVEL[NewLEVEL == 'LUNPROV'] <- 'UNPROV')
StreamlinedDataH6 <- within(StreamlinedDataH6, NewFEATURE[NewFEATURE == 'FUNPROV'] <- 'UNPROV')

HeroldContextsFixed <- StreamlinedDataH6

#### 5. Create new variables called DAACSWARE, DAACSCEW, and DAACSDECGENRE. These variables
# represent redcoding the Charleston Museum type names and designations as those used by 
# DAACS. I will deploy the DAACS terminology in the dissertation.

# Load in CSV of created DAACS/Charleston Museum equivalencies 

WareEquivalencies<- read.csv(file = '87Church_DAACSWareEquivalencies.csv', 
                             fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

# Unite the Ware Equivalencies with the cleaned Herold Data

HeroldContextsFixedWARES <- merge(WareEquivalencies, HeroldContextsFixed, by=
                                    c("PARENT", "OBJNAME"))

# Export CV of cleaned Herold data to archive

write.csv(HeroldContextsFixedWARES,"87Church_Cleaned_PastPerfect.csv", row.names = TRUE)

# Strip down spreadsheet to just the contexts I will be using for analysis

FINALHeroldData <- HeroldContextsFixedWARES %>%
  select(OBJECTID, DAACSware, DAACScew, DAACSdec, NewUNIT, NewLEVEL, 
         NewFEATURE, DAACSCONT, COUNT)

# Rename to the final column names

FINALHeroldDataB <- FINALHeroldData %>% 
  rename(
    WARE = DAACSware,
    CEW = DAACScew,
    DEC = DAACSdec,
    UNIT = NewUNIT,
    LEVEL = NewLEVEL,
    FEATURE = NewFEATURE,
    CONTEXT = DAACSCONT
  )

#### 5. Read in and clean DAACS data

# Read in DAACS data 

DAACSInitialImportData<- read.csv(file = '87Church_Raw_DAACSCeramics_Aug2021.csv', 
                                  fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

# Strip down to just needed contexts

DAACSDataStreamlined <- DAACSInitialImportData %>%
  select(Context, Artifact.ID, Count, Coarse.Earthenware.Type, Decorative.Genre,
         Ware)

# Rename variables to final names

DAACSDataStreamlinedB <- DAACSDataStreamlined %>% 
  rename(
    WARE = Ware,
    CEW = Coarse.Earthenware.Type,
    DEC = Decorative.Genre,
    CONTEXT = Context,
    OBJECTID = Artifact.ID,
    COUNT = Count
  )

# Replace blanks with Not Applicable 

DAACSDataStreamlinedB[DAACSDataStreamlinedB==""]<-'Not Applicable'

# Add UNIT, LEVEL, and FEATURE. Unused for this dataset (could be in the future)
# but for now just add NA

DAACSDataStreamlinedB$UNIT <- NA
DAACSDataStreamlinedB$LEVEL <- NA
DAACSDataStreamlinedB$FEATURE <- NA

### 6. Merge both datasets and export!

FINALCompiled<- rbind(DAACSDataStreamlinedB, FINALHeroldDataB)

# Check the ware, cew, and dec columns and make any corrections as needed

FINALchecks <-  FINALCompiled %>% 
  group_by(WARE) %>% 
  summarise(Count = sum(COUNT))

FINALchecks <-  FINALCompiled %>% 
  group_by(CEW) %>% 
  summarise(Count = sum(COUNT))

FINALchecks <-  FINALCompiled %>% 
  group_by(DEC) %>% 
  summarise(Count = sum(COUNT))

# Make final fixes

FINALCompiled$CEW <- with(FINALCompiled, ifelse(CEW=='Lesesne Colono (CMT)', 'Lesesne',
                                                CEW))
FINALCompiled$CEW <- with(FINALCompiled, ifelse(CEW=='River-burnished ColonoCMT', 'River Burnished',
                                                CEW))
FINALCompiled$CEW <- with(FINALCompiled, ifelse(CEW== 'Stobo Colono (CMT)', 'Stobo',
                                                CEW))
FINALCompiled$CEW <- with(FINALCompiled, ifelse(CEW== 'Yaughan Colono (CMT)', 'Yaughan',
                                                CEW))
FINALCompiled$CEW <- with(FINALCompiled, ifelse(CEW=='River-burnished ColonoCMT', 'River-Burnished',
                                                CEW))

# Add site components

FINALCompiledB <-FINALCompiled
FINALCompiledB$COMPONENT <- NA

FINALCompiledB <- within(FINALCompiledB, COMPONENT[UNIT %like% '^A'] <- 'WORKYARD')
FINALCompiledB <- within(FINALCompiledB, COMPONENT[UNIT %like% '^B'] <- 'WORKYARD')
FINALCompiledB <- within(FINALCompiledB, COMPONENT[UNIT %like% '^C'] <- 'CELLAR')
FINALCompiledB <- within(FINALCompiledB, COMPONENT[UNIT %like% '^D'] <- 'DRIVEWAY')
FINALCompiledB <- within(FINALCompiledB, COMPONENT[UNIT %like% '^E'] <- 'SIDEWALK')
FINALCompiledB <- within(FINALCompiledB, COMPONENT[UNIT %like% '^HWK'] <- 'KITCHEN')
FINALCompiledB <- within(FINALCompiledB, COMPONENT[UNIT %like% '^HWN'] <- 'PRIVY')
FINALCompiledB <- within(FINALCompiledB, COMPONENT[OBJECTID %like% '^1307'] <- 'STABLE')

# Add excavation blocks

FINALCompiledB$BLOCK <- NA

FINALCompiledB <- within(FINALCompiledB, BLOCK[UNIT %like% '^A'] <- 'A')
FINALCompiledB <- within(FINALCompiledB, BLOCK[UNIT %like% '^B'] <- 'B')
FINALCompiledB <- within(FINALCompiledB, BLOCK[UNIT %like% '^C'] <- 'B')
FINALCompiledB <- within(FINALCompiledB, BLOCK[UNIT %like% '^D'] <- 'C')
FINALCompiledB <- within(FINALCompiledB, BLOCK[UNIT %like% '^E'] <- 'D')

# Add unprov column 

FINALCompiledB$UNPROV <- NA

FINALCompiledB <- within(FINALCompiledB, UNPROV[UNIT %ilike% 'UNPROV' | LEVEL %ilike% 
                                                 'UNPROV'] <- 'yUNPROV')

#Flag individual contexts as unprov, these are all stable contexts that are 
# out of context or are cleanup contexts as noted in Zierden's 2007
# site report

FINALCompiledB <- within(FINALCompiledB, UNPROV[CONTEXT == '02-01-040' | CONTEXT ==
                                                  '02-02-043' | CONTEXT ==
                                                  '02-02-075' | CONTEXT ==
                                                  '02-01-082' | CONTEXT ==
                                                  '02-03-089' | CONTEXT ==
                                                  '02-04-092' | CONTEXT ==
                                                  '02-02-093' | CONTEXT ==
                                                  '02-04-101' | CONTEXT ==
                                                  '02-03-106' | CONTEXT ==
                                                  '02-05-117' | CONTEXT ==
                                                  '02-04-137' | CONTEXT ==
                                                  '02-06-139' | CONTEXT ==
                                                  '02-06-145' | CONTEXT ==
                                                  '02-06-162' | CONTEXT ==
                                                  '02-07-164' | CONTEXT ==
                                                  '02-06-170' | CONTEXT ==
                                                  '02-030507-175'] <- 'yUNPROV')

# Flag units lacking vertical context as unprov

FINALCompiledB <- within(FINALCompiledB, UNPROV[UNIT %like% '^A' & 
                                                  is.na(LEVEL)] <- 'yUNPROV')
FINALCompiledB <- within(FINALCompiledB, UNPROV[UNIT %like% '^B' & 
                                                  is.na(LEVEL)] <- 'yUNPROV')
FINALCompiledB <- within(FINALCompiledB, UNPROV[UNIT %like% '^C' & 
                                                  is.na(LEVEL)] <- 'yUNPROV')
FINALCompiledB <- within(FINALCompiledB, UNPROV[UNIT %like% '^D' & 
                                                  is.na(LEVEL)] <- 'yUNPROV')
FINALCompiledB <- within(FINALCompiledB, UNPROV[UNIT %like% '^E' & 
                                                  is.na(LEVEL)] <- 'yUNPROV')
FINALCompiledB <- within(FINALCompiledB, UNPROV[UNIT %like% '^HWK' & 
                                                  is.na(LEVEL)] <- 'yUNPROV')
FINALCompiledB <- within(FINALCompiledB, UNPROV[UNIT %like% '^HWN' & 
                                                  is.na(LEVEL)] <- 'yUNPROV')

# Check results

FINALchecksC <-  FINALCompiledB %>% 
  group_by(CONTEXT, UNIT, LEVEL, FEATURE, UNPROV) %>% 
  summarise(Count = sum(COUNT))

# Add component designation to features where known

FINALCompiledB <- within(FINALCompiledB, COMPONENT[FEATURE %like% 
                                                    '^F021' | FEATURE %like%
                                                  '^F026' | FEATURE %like%
                                                  '^F027' | FEATURE %like%
                                                  '^F028' | FEATURE %like%
                                                  '^F030' | FEATURE %like%
                                                  '^F031' | FEATURE %like%
                                                  '^F032' | FEATURE %like%
                                                  '^F033' | FEATURE %like%
                                                  '^F034' | FEATURE %like%
                                                  '^F035' | FEATURE %like%
                                                  '^F036' | FEATURE %like%
                                                  '^F037' | FEATURE %like%
                                                  '^F038' | FEATURE %like%
                                                  '^F041' | FEATURE %like%
                                                  '^F042' | FEATURE %like%
                                                  '^F043' | FEATURE %like%
                                                  '^F044' | FEATURE %like%
                                                  '^F045' | FEATURE %like%
                                                    '^F047' | FEATURE %like%
                                                    '^F048' | FEATURE %like%
                                                    '^F049' | FEATURE %like%
                                                    '^F051' | FEATURE %like%
                                                    '^F053'| FEATURE %like%
                                                    '^F055' | FEATURE %like%
                                                    '^F056' | FEATURE %like%
                                                    '^F057' | FEATURE %like%
                                                    '^F058' | FEATURE %like%
                                                    '^F060' | FEATURE %like%
                                                    '^F061' | FEATURE %like%
                                                    '^F063' | FEATURE %like%
                                                    '^F064' | FEATURE %like%
                                                    '^F065'| FEATURE %like%
                                                    '^F067' | FEATURE %like%
                                                    '^F072' | FEATURE %like%
                                                    '^F073' | FEATURE %like%
                                                    '^F074' | FEATURE %like%
                                                    '^F075' | FEATURE %like%
                                                    '^F076' | FEATURE %like%
                                                    '^F077' | FEATURE %like%
                                                    '^F078'| FEATURE %like%
                                                    '^F079' | FEATURE %like%
                                                    '^F089' | FEATURE %like%
                                                    '^F090' | FEATURE %like%
                                                    '^F091' | FEATURE %like%
                                                    '^F093' | FEATURE %like%
                                                    '^F094' | FEATURE %like%
                                                    '^F098' | FEATURE %like%
                                                    '^F099' | FEATURE %like%
                                                    '^F100'| FEATURE %like%
                                                    '^F101' | FEATURE %like%
                                                    '^F102' | FEATURE %like%
                                                    '^F103' | FEATURE %like%
                                                    '^F104' | FEATURE %like%
                                                    '^F110' | FEATURE %like%
                                                    '^F111' | FEATURE %like%
                                                    '^F112' | FEATURE %like%
                                                    '^F119' | FEATURE %like%
                                                    '^F123' | FEATURE %like%
                                                    '^F124'| FEATURE %like%
                                                    '^F125' | FEATURE %like%
                                                    '^F127' | FEATURE %like%
                                                    '^F128' | FEATURE %like%
                                                    '^F129' | FEATURE %like%
                                                    '^F132' | FEATURE %like%
                                                    '^F133' | FEATURE %like%
                                                    '^F136' | FEATURE %like%
                                                    '^F137' | FEATURE %like%
                                                    '^F139'| FEATURE %like%
                                                    '^F140' | FEATURE %like%
                                                    '^F142' | FEATURE %like%
                                                    '^F143' | FEATURE %like%
                                                    '^F144' | FEATURE %like%
                                                    '^F145' | FEATURE %like%
                                                    '^F148' | FEATURE %like%
                                                    '^F150' | FEATURE %like%
                                                    '^F154' | FEATURE %like%
                                                    '^F157' | FEATURE %like%
                                                    '^F159' | FEATURE %like%
                                                    '^F161'| FEATURE %like%
                                                    '^F163' | FEATURE %like%
                                                    '^F164' | FEATURE %like%
                                                    '^F165' | FEATURE %like%
                                                    '^F166' | FEATURE %like%
                                                    '^F168' | FEATURE %like%
                                                    '^F170' | FEATURE %like%
                                                    '^F171' | FEATURE %like%
                                                    '^F172' | FEATURE %like%
                                                    '^F173'| FEATURE %like%
                                                    '^F176' | FEATURE %like%
                                                    '^F179' | FEATURE %like%
                                                    '^F180'| FEATURE %like%
                                                    '^F181' | FEATURE %like%
                                                    '^F183' | FEATURE %like%
                                                    '^F184' | FEATURE %like%
                                                    '^F185' | FEATURE %like%
                                                    '^F188' | FEATURE %like%
                                                    '^F190' | FEATURE %like%
                                                    '^F191' | FEATURE %like%
                                                    '^F192' | FEATURE %like%
                                                    '^F193' | FEATURE %like%
                                                    '^F195' | FEATURE %like%
                                                    '^F198'| FEATURE %like%
                                                    '^F199' | FEATURE %like%
                                                    '^F200' | FEATURE %like%
                                                    '^F201' | FEATURE %like%
                                                    '^F131' ] <- 'WORKYARD')

FINALCompiledB <- within(FINALCompiledB, COMPONENT[FEATURE %like% 
                                                     '^F001' | FEATURE %like%
                                                     '^F002' | FEATURE %like%
                                                     '^F003' | FEATURE %like%
                                                     '^F004' | FEATURE %like%
                                                     '^F005' | FEATURE %like%
                                                     '^F006' | FEATURE %like%
                                                     '^F007' | FEATURE %like%
                                                     '^F008' | FEATURE %like%
                                                     '^F009' ] <- 'KITCHEN')

# Quick check on kitchen features

FINALchecksD <-  FINALCompiledB %>% 
  group_by(UNIT, LEVEL, FEATURE, UNPROV) %>% 
  summarise(Count = sum(COUNT))

# Quick unprov fix

FINALCompiledB <- within(FINALCompiledB, UNPROV[FEATURE == 'F006'] <- NA)
FINALCompiledB <- within(FINALCompiledB, UNIT[FEATURE == 'F006'] <- NA)

# Download CSV

write.csv(FINALCompiledB,"87Church_CompiledDataset.csv", row.names = TRUE)

# Please note, a series of transformations occurred outside of R due to ease
# and available time. These include the updating of a number of DAACS artifact
# IDs to reflect the addition of the ware type Slip-Coated. There are also two 
# artifact records (ARL 50453.1 and .2 ) I updated to UNPROV by 
# hand due to available time. One critical addition is the vertical provenience 
# information for F26 Staffordshire slipware ARL 22603.2 through .13. This data was added 
# after the fact and was done by hand in the excel spreadsheet. These changes
# are reflected in the "87Church_CompiledDataset_Fin.csv" file. 
