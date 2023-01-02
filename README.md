# 87ChurchStreet_PlattDissertation

The following includes code and data file for final analyses used in Sarah E. Platt's 2022 dissertation (Syracuse University - Anthropology) _Entangled Lives in a Southern Metropolis: An Archaeology of Legacies at 87 Church Street, Charleston, South Carolina (1734-1771)_. All files are also available from the author by request. It is highly recommended by the author that all users of the following datasets reach out to the archaeology department at The Charleston Museum to indicate their plans for use.

The following is a reproduction of Appendix 2 in the dissertation, which outlines each analysis, their associated code scripts, and associated data files. These designations correspond to the included files in the repository. 

## Data Cleaning (A files)

### A1_87Church_DataCleaning.R
This R Script documents the data cleaning steps undertaken on the raw data files downloaded from the DAACS database and The Charleston Museum's Past Perfect database. For this code to run, it requires files A2, A3, and A4.

### A2_87Church_Raw_PastPerfect_Feb2020_Final.csv
This CSV contains all of the raw Past Perfect data downloaded from The Charleston Museum for use in this dissertation. It was downloaded in February 2020, and has likely changed in past years due to ongoing research on site. 

### A3_87Church_Raw_DAACSCeramics_Aug2021.csv
This CSV contains all of the raw DAACS ceramic data used in this dissertation. It was downloaded in August 2021, and has likely changed in past years due to ongoing research on site. The finalized Heyward-Washington House (87 Church Street) dataset will be available for download at daacs.org in 2024-2025. 

### A4_87ChurchDAACSWareEquivalencies.csv
This CSV contains The Charleston Museum ware types and their DAACS ware type equivalents as applied in this dissertation.

### A5_87Church_CompiledDataset.csv
This CSV is the result of file A1 R script, see code comments lines 1210 to 1217.

### A6_87Church_CompiledDataset_Fin.csv
This CSV is a modified version of file A5, see code comments in file 1 R script code comments line 1210 to 1217. 

## Chronology and Correspondence Analysis (B files)

Please note, the CA code (files B1 and B2) was developed based on a script initially developed and authored by Dr. Fraser Neiman, director of the Archaeology Department at Thomas Jefferson's Monticello at the time of the dissertation's writing. 

### B1_87Church_ChronoCA_AllData.R
This is the primary CA script, and relies on results from file B2 R script, ensure you follow all code comments closely.

### B2_87Church_ChronoCA_FeaStab.R
This is the secondary CA script based on the initial results of file 1, ensure you follow all code comments cloesly.

### B3_DAACS_MCDTypeTable.csv
A CSV of ware types and corresponding types as deployed by DAACS, this file is needed for the B1 and B2 R scripts. 

### B4_87Church_ZierdenSGs.csv
Equivalencies of DAACS designated contexts to the stratigraphic groups published in Zierden and Reitz's 2007 Heyward-Washington House site report.

### B5_87Church_PhasedData.csv
This csv is the result of the script B1 and B2, and is necessary for all future code scripts in the rest of the dissertation.

### B6_87Church_MCDS.TPQS.csv
This csv is the result of script B2, and is provided as a reference to future researchers.

### A6_87Church_CompiledDataset_Fin.csv
See above description, required to run scripts B1 and B2

## Workyard Ceramics - Relative Frequencies (C files)

### C1_87Church_87ChurchItoIICeramics.R
This script produces barplots in chapter 4 and appendix 4, including relative frequencies of early occupational phases.

### C2_87Church_Feature166Ceramics.R
This script produces barplots in chapter 4 and appendix 4 pertinent to feature 166.

### C3_87Church_Feature26Ceramics.R
This script produces barplots in chapter 4 and appendix 4 pertinent to feature 26.

### C4_87Church_Feature65Ceramics.R
This script produces barplots in chapter 4 and appendix 4 pertinent to feature 65.

### C5_DAACSWARES_Materials.R
This file includes material type equivalencies to DAACS ware types.

### B5_87Church_PhasedData.csv
See above description, required to run all scripts in this section.

## Reference files (D files)

### D1_87Church_HeroldTobaccoPipes.csv
A csv file with imbedded excel formulas for calculating Binford dates, based on compiled data from Elaine Herold on file at The Charleston Museum.

### D2_87Church_HeroldSmallFinds.csv
A digitized list of small finds, based on compiled data from Elaine Herold on file at The Charleston Museum.

