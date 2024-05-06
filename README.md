# Background

This repository contains the code base for Richard Barad's capstone project for MUSA 8020 Capstone Project. 

The project also involved publishing the results of the capstone project fo BigQuery and creating an interactive map using Carto. This piece of the capstone project is also used as a a final project for MUSA 5090 Geospatial Cloud Computing and Visualization and was completed by Richard Barad and Dave Dreenan. The Carto Dashboard is available [here](https://clausa.app.carto.com/map/39f234f9-2648-42cf-a2c1-f0ba9b860aec). 

# Project Goals

The project aims to explore how cluster analysis techniques, specifically k-means cluster analysis can be used to map and identify environmental justice areas, and group together areas that face similar environmental and socioeconomic vulnerabilities and risks.

# Repository Overview

This repository includes a file called capston_rb.Rmd which is a markdown file that contains the code base, along with annotations describing by methodology and workflow. The markdown file also includes background research, limitations of existing initiatives, research questions, results, discussion and limitations, and ideas for future analysis. The R markdown is also knitted to an html file which is also included in the repository. **The code chunk of the R Markdown file which is relevant to MUSA 5090 Geospatial Cloud Computing and Visualization starts on line 1005 and is titled bigquery.**

The repository also includes the following additional files:

* The outputs folder contains png images of the visuals which are included in the R markdown.
* The SQL folder includes the .sql query which is used to create the table used in the Carto Dashboard. 
* The Capstone_flow_chart.png file is a flow chart showing the steps involved in my analysis, and is included in the Markdown. 
* RB_Capstone: R project
* Data Folder: Contains data files used in the code

Note that the repository contains pre-calculated data on the wildfire likelihood by census tract and data on the percent of each census tract that is located in the floodplain. In order to replicate this analysis for another study area, data on the flood hazard zone for states within the study area would need to be downloaded from [FEMA](https://msc.fema.gov/portal/advanceSearch). The code to calculate the percent of each census tract that is located within the floodplain is included in the R Markdown. Additionally, data on likelihood of a fire occurring would also need to be downloaded from [USDA](https://wildfirerisk.org/). The code to calculate the mean wildfire risk for a census tract is included in the markdown.    



