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
