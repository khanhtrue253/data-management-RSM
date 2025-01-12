## Hi 👋

This repository contains the files used for the Group Assignment for the course Analyzing Digital Footprints given by prof. Martinovici.

Files include:

- `data_retrieval`: All the files needed to construct the final Kiva dataset used to answer our research question. These are saved in `RData` files.

  - `collect_data_uniqueIDCollect.R`: Collect unique IDs based on query;
  - `collect_data_API.R`: Collect more data based on unique IDs;
  - `process_data.R`: Process collected data. This includes feature engineering and plot creation. 
  - `aux_functions.R`: Functions that are used to unpack nested lists in the `process_data_R` file.
  
  
- `plots`: Contains all plot figures captured from the `process_data.R` file.

- `sections`: Contains all sections for the `written_report.Rmd`.

  - `section_1`: The introduction

  - `section_2`: The Data part. This consists of:
    
    - `section_2_main`: Main child-document document
    
    - `section_2_variableExploration`: Child-document containing descriptive analysis on chosen `10` variables.
    
    - `section_2_clusterAnalysis_code`: Child-document containing the code and explanation to perfrom the cluster analysis. This builds upon:
    
      - `section_2_clusterAnalysis_clustering.R`: This code performs the clustering. Output is saved under `clusters.RData`.
      
        - `clusters.RData`: Resulting data from our cluster analysis.
        
    
      - `ClusterAnalysis_clusterSummaries.RData`: Data object that saves the summarized categorical features of cluster analysis.
      
    - `section_2_clusterAnalysis_interpretation`: Contains interpretation of gathered clusters.
  
  - `section_3`: The discussion.
  
  - `section_4`: References.

- `style.css`: CSS parameters for rmarkdown files.

- `written_report.Rmd` & `written_report.html`: Contains our final report. This is build upon the sections 1-4.

