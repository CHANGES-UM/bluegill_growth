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

#---summary statistics of predictor variables ----------------------

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

#preditor stats 
sum(summary_data_lakeyear$walleye, na.rm = TRUE)/2454 * 100
sum(summary_data_lakeyear$lmb, na.rm = TRUE)/2454 * 100
sum(summary_data_lakeyear$pike, na.rm = TRUE)/2454 * 100
sum(summary_data_lakeyear$perch, na.rm = TRUE)/2454 * 100

#--- look at day of year ------------------------------------------
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


#----Frequency histograms for covariates--------------------------
###DOY
#full data set
(doy.age.hist.full <- ggplot(data = binded %>% 
                          mutate(Year = as.factor(year)), 
                        aes(x = doy, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Day of Year")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/doy_freq_hist_full.tiff", 
       plot = doy.age.hist.full, 
       width = 200, height = 150, units = "mm", dpi = 300)
 
#restricted doy values 
(doy.age.hist.restricted <- ggplot(data = binded %>% 
                          filter(doy >= 141 & doy <=208) %>% 
                          mutate(Year = as.factor(year)), 
                        aes(x = doy, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Day of Year")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/doy_freq_hist_restricted.tiff", 
       plot = doy.age.hist.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Degree days
#full data set
(dd.age.hist.full <- ggplot(data = binded %>% 
                          mutate(Year = as.factor(year)), 
                        aes(x = DD_mean, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Cohort Degree Days")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/dd_freq_hist_full.tiff", 
       plot = dd.age.hist.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(dd.age.hist.restricted <- ggplot(data = binded %>% 
                          filter(doy >= 141 & doy <=208) %>% 
                          mutate(Year = as.factor(year)), 
                        aes(x = DD_mean, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Cohort Degree Days")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/dd_freq_hist_restricted.tiff", 
       plot = dd.age.hist.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Lake Depth
#full data set
(lakedepth.age.hist.full <- ggplot(data = binded %>% 
                              mutate(Year = as.factor(year)), 
                            aes(x = logdepth, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Log10 Lake Area")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/lakedepth_freq_hist_full.tiff", 
       plot = lakedepth.age.hist.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(lakedepth.age.hist.restricted <- ggplot(data = binded %>% 
                                    filter(doy >= 141 & doy <=208) %>% 
                                    mutate(Year = as.factor(year)), 
                                  aes(x = logdepth, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Log10 Lake Area")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/lakedepth_freq_hist_restricted.tiff", 
       plot = lakedepth.age.hist.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Surface Temperature
#full data set
(surftemp.age.hist.full <- ggplot(data = binded %>% 
                              mutate(Year = as.factor(year)), 
                            aes(x = surf_temp_year, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Mean Annual Surface Temperature")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/surftemp_freq_hist_full.tiff", 
       plot = surftemp.age.hist.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(surftemp.age.hist.restricted <- ggplot(data = binded %>% 
                                    filter(doy >= 141 & doy <=208) %>% 
                                    mutate(Year = as.factor(year)), 
                                  aes(x = surf_temp_year, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Mean Annual Surface Temperature")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/surftemp_freq_hist_restricted.tiff", 
       plot = surftemp.age.hist.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Lake Area
#full data set
(lakearea.age.hist.full <- ggplot(data = binded %>% 
                                     mutate(Year = as.factor(year)), 
                                   aes(x = logarea, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Log10 Lake Area")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/lakearea_freq_hist_full.tiff", 
       plot = lakearea.age.hist.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(lakearea.age.hist.restricted <- ggplot(data = binded %>% 
                                           filter(doy >= 141 & doy <=208) %>% 
                                           mutate(Year = as.factor(year)), 
                                         aes(x = logarea, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Log10 Lake Area")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/lakearea_freq_hist_restricted.tiff", 
       plot = lakearea.age.hist.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Bluegill CPUE
#full data set
(bgcpue.age.hist.full <- ggplot(data = binded %>% 
                                     mutate(Year = as.factor(year)), 
                                   aes(x = cpue, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Bluegill CPUE")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/bgcpue_freq_hist_full.tiff", 
       plot = bgcpue.age.hist.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(bgcpue.age.hist.restricted <- ggplot(data = binded %>% 
                                           filter(doy >= 141 & doy <=208) %>% 
                                           mutate(Year = as.factor(year)), 
                                         aes(x = cpue, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Bluegill CPUE")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/bgcpue_freq_hist_restricted.tiff", 
       plot = bgcpue.age.hist.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Secchi Depth
#full data set
(secchi.age.hist.full <- ggplot(data = binded %>% 
                                  mutate(Year = as.factor(year)), 
                                aes(x = mean_secchi_m, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Secchi Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/secchi_freq_hist_full.tiff", 
       plot = secchi.age.hist.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(secchi.age.hist.restricted <- ggplot(data = binded %>% 
                                        filter(doy >= 141 & doy <=208) %>% 
                                        mutate(Year = as.factor(year)), 
                                      aes(x = mean_secchi_m, fill = Year))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d()+
    labs(y = "Count", x = "Secchi Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/secchi_freq_hist_restricted.tiff", 
       plot = secchi.age.hist.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)
