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

tiff(filename = "Figures/all_covariates_correlations.tiff", width = 1200, height = 900,
     units = "px")

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
 
 tiff(filename = "Figures/brt_covariates_correlations.tiff", width = 1200, height = 900,
      units = "px")
 
 corrplot(brt.var.cor$r, type = "upper", order = "hclust",
          p.mat = brt.var.cor$P, sig.level = 0.05, insig = "blank")
 
 dev.off()
 

 
 
 