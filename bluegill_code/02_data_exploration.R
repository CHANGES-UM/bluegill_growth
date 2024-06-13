# code for investigating correlations between preditor variables and summary stats 
#written by Peter Flood and Katelyn King 

#----Load Libraries-------------------------------------------------------------
library(tidyverse) #data manipulation
library(Hmisc) #calculate correlations
library(corrplot) #plot correlations

#---Load Data-------------------------------------------------------------------
binded <- read.csv("bluegill_data/model_data.csv") %>% 
  mutate(cpue = log(cpue+1)) %>% 
  mutate(AGE = as.factor(AGE))

#----Check Correlations---------------------------------------------------------
names(binded)

#look at all potential covariates
covariates <- binded %>% 
  dplyr::select(LAT_DD, LONG_DD, agriculture, area_ha, cpue, DD_mean, dd_year, depth_m,
         doy, forests, lmb, logarea, logcounty, logdepth, mean_secchi_m, perch,
         pike, surf_temp_year, Total.Population, urban, walleye, wetlands)

var.cor <- rcorr(as.matrix(covariates), type = "pearson")
var.cor

#write.csv(var.cor[[1]], file = "Tables/var_cor.csv")

#rotate table to long format
var.cor.long <- var.cor[[1]] %>% as.data.frame() %>% 
  rownames_to_column(var = "var1") %>% 
  pivot_longer(cols = c(LAT_DD:wetlands), names_to = "var2", values_to = "pearsons_r") %>% 
  arrange(desc(pearsons_r)) %>% 
  filter(var1 != var2)

#set up for plotting
diag(var.cor$P) <- 0

#correlation plot
var.corrplot <- corrplot(var.cor$r, type = "upper", order = "hclust",
                         p.mat = var.cor$P, sig.level = 0.05, insig = "blank")

#tiff(filename = "Figures/all_covariates_correlations.tiff", width = 1200, height = 900,
 #    units = "px")

corrplot(var.cor$r, type = "upper", order = "hclust",
         p.mat = var.cor$P, sig.level = 0.05, insig = "blank")

dev.off()

#correlations among variables included in boosted regression tree models
brt.covariates <- covariates %>% 
   dplyr::select(logcounty, logdepth, DD_mean, doy, logarea, surf_temp_year, wetlands, 
          cpue, forests, agriculture, urban, mean_secchi_m, walleye, pike, perch,
          lmb)
 
 
brt.var.cor <- rcorr(as.matrix(brt.covariates), type = "pearson")
brt.var.cor
 
brt.var.cor.long <- brt.var.cor[[1]] %>% as.data.frame() %>% 
   rownames_to_column(var = "var1") %>% 
   pivot_longer(cols = c(logcounty:lmb), names_to = "var2", values_to = "pearsons_r") %>% 
   arrange(desc(pearsons_r)) %>% 
   filter(var1 != var2)
 
 #set up for plotting
diag(brt.var.cor$P) <- 0
 
 #correlation plot
brt.var.corrplot <- corrplot(brt.var.cor$r, type = "upper", order = "hclust",
                          p.mat = brt.var.cor$P, sig.level = 0.05, insig = "blank")
 
#tiff(filename = "Figures/brt_covariates_correlations.tiff", width = 1200, height = 900,
 #     units = "px")
 
corrplot(brt.var.cor$r, type = "upper", order = "hclust",
          p.mat = brt.var.cor$P, sig.level = 0.05, insig = "blank")
 
dev.off()

#### summary statistics of predictor variables #### 

#summary by lake year 
summary_data_lakeyear<-read.csv("bluegill_data/model_data.csv") %>% 
  select('new_key', 'year', 'mean_secchi_m', 'perch', 'lmb', 'pike', 'walleye',  
         'urban', 'agriculture', 'forests', 'wetlands', 'depth_m', 
         'area_ha', 'doy', 'DD_mean', 'cpue', 'surf_temp_year') %>% 
  distinct(new_key, year, .keep_all = TRUE)

#summary stats 
skimr::skim(summary_data_lakeyear)

#summary by lake  
summary_data_lake<-read.csv("bluegill_data/model_data.csv") %>% 
  select('new_key', 'year', 'mean_secchi_m', 'perch', 'lmb', 'pike', 'walleye',  
         'urban', 'agriculture', 'forests', 'wetlands', 'depth_m', 
         'area_ha', 'doy', 'DD_mean', 'cpue', 'surf_temp_year') %>% 
  distinct(new_key, .keep_all = TRUE)

#summary stats 
skimr::skim(summary_data_lake)

#### look at day of year ####  
summary(binded$doy)
binded%>% 
  group_by(type)%>% 
  summarise(mean = mean(doy), 
            med = median(doy),
            Q25 = quantile (doy, 0.25, na.rm=TRUE), 
            Q75 = quantile(doy, 0.75, na.rm=TRUE)
            )

#frequency plot
ggplot(binded, aes(doy,  colour = type)) + 
  geom_freqpoly()

#density plot 
ggplot(binded, aes(doy)) + 
  geom_density(aes(fill = type), alpha=0.4)


 
 