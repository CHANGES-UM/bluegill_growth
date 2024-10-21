# bluegill_growth
The following scripts and data support the manuscript: **Elise E. Grabda, Peter J. Flood, Katelyn B.S. King, James E. Breck, Kevin E. Wehrly, Karen M. Alofs. "Mismatch between climated-based bioenergetics model of fish growth and long-term and regional-scale empirical data" currently under review.**

The manuscript investigates Bluegill growth with climate warming. We used both mathematical simulation and Boosted Regression Tree modeling. These data revealed that older Bluegill (ages 5-8) were either increasing as expected or not changing, while younger Bluegill (ages 1-4) were growing more slowly. Since simulations showed an increased consumption with higher temperatures to maintain the same growth, it is likely that young Bluegill are not able to find enough food at warmer temperatures, due to an increase in density, which leads to slower growth. 



**The 'bluegill_code' folder includes the following scripts:** \
**01_data_cleaning** includes code for combining disperate data \
**02_data_exploration** includes exploring driver variables and code for many of the appenix figures and tables\
**03_linear_models** includes code for the linear model and plot\
**04_BRT_models** includes the methods for running BRT models for all age classes and plots \
**05_reducedDOY_BRT_model** includes the methods for running BRT models for all age classes and plots using the reduced day of year dataset \
**06_map_code** includes code for mapping sample sites \
**07_plotting bioenergetics_output** includes the plots for the bioenergetics models  


**The 'bluegill_data' folder includes the following datasets for the historical and contemporary data:** \
**backcast lulc folder** includes historical land use and cover data \
**Bioenergetics Output folder** includes data tables with the bioenergetics output \
**Census_Data_County folder** includes the data tables for the US Census data by county \
**all_grow_21NOV2023.csv** the historical data with bluegill age and length information \
**fishc_bluegill2.csv**   historical data with the manually calculated catch per unit effort  \
**fishc_qaqc_added_lakes.csv**  the historical catch data including gear and effort  \
**IFR_lake_points.csv**   the lake lat/lon information \
**lagos_join_ifr.csv**   the LAGOS lake ids matched to the IFR lake ids \
**lake_degree_days_year.csv** has the modeled degree days for each year for each lake from James Breck \
**lake_depth_2021.csv** contemporary lake depth information  \
**lake_summ_fish_pres_qaqc.csv** has the historical presence and absence data for predator fishes  \
**lake_summary_qaqc.csv** historical lake Secchi depth and max depth measurements  \
**lake_surface_temp.csv** the modeled yearly lake temperatures for each lake from James Breck \
**model_data.csv**  this is the clean dataset of all combined variables that were used for the models \
**new_key_nhd_link.csv**  table to link the IFR lake ids to the NHD (National Hydrography Dataset) \
**snt_blg_ages_2002_2020.csv** contemporary (Status and Trends) data with bluegill age and length information \
**snt_catch_data_mar2021.csv** contemporary (Status and Trends) catch data data \
**snt_effort_data.csv** contemporary (Status and Trends) effort data \
**snt_secchi_2002_202.csv** contemporary (Status and Trends) Secchi depth data \
**sntlulc**  contemporary (Status and Trends) land use and cover data \


**The 'Figures' folder includes the figures made in R for the manuscript**

**The 'Tables' folder includes output for tables in the manuscript**
