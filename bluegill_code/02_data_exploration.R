# code for investigating correlations between predictor variables and summary stats 
#written by Peter Flood and Katelyn King 

#----Load Libraries-------------------------------------------------------------
library(tidyverse) #data manipulation
library(Hmisc) #calculate correlations
library(corrplot) #plot correlations
library(ggpubr) #stacking plots

#---Load Data-------------------------------------------------------------------
binded <- read.csv("bluegill_data/model_data.csv") %>% 
    mutate(cpue = log10(cpue+1), 
           logarea = log10(area_ha),
           logdepth = log10(depth_m)
    ) %>%
  mutate(AGE = as.factor(AGE)) %>% 
  mutate(Type = case_when(
    year <= 1998 ~ "Historic",
    year > 1998 ~ "Contemporary"
  ))

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
 
#dev.off()

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

#predator stats 
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

#Number of observations per type
binded %>% group_by(type) %>% tally()

#----Frequency histograms for covariates--------------------------
###DOY
#full data set
(doy.age.hist.full <- ggplot(data = binded %>% 
                          mutate(Year = as.factor(year)), 
                        aes(x = doy, fill = Type, color = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                        aes(x = doy, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                        aes(x = DD_mean, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                        aes(x = DD_mean, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                            aes(x = logdepth, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                                  aes(x = logdepth, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                            aes(x = surf_temp_year, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                                  aes(x = surf_temp_year, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                                   aes(x = logarea, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                                         aes(x = logarea, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                                   aes(x = cpue, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                                         aes(x = cpue, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                                aes(x = mean_secchi_m, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
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
                                      aes(x = mean_secchi_m, fill = Type))+
    geom_histogram()+
    facet_wrap(~AGE)+
    scale_fill_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Secchi Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()
)

ggsave(filename = "Figures/Frequency Histograms/secchi_freq_hist_restricted.tiff", 
       plot = secchi.age.hist.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

#----Freqpoly for covariates--------------------------
###All age classes combined
###DOY
#full data set
(doy.age.freqpoly.combined.ages.full <- ggplot(data = binded %>% 
                                   mutate(Year = as.factor(year)), 
                                 aes(x = doy, color = Type))+
   geom_freqpoly()+
   scale_color_viridis_d(end = 0.9)+
   labs(y = "Count", x = "Day of Year")+
   scale_y_continuous(expand = c(0,0))+
   theme_bw()+
   theme(axis.text = element_text(size = 8),
         axis.text.x = element_text(angle = 45))
)

#restricted doy values 
(doy.age.freqpoly.combined.ages.restricted <- ggplot(data = binded %>% 
                                         filter(doy >= 141 & doy <=208) %>% 
                                         mutate(Year = as.factor(year)), 
                                       aes(x = doy, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Day of Year")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)


###Degree days
#full data set
(dd.age.freqpoly.combined.ages.full <- ggplot(data = binded %>% 
                                  mutate(Year = as.factor(year)), 
                                aes(x = DD_mean, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Cohort Degree Days")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

#restricted doy values 
(dd.age.freqpoly.combined.ages.restricted <- ggplot(data = binded %>% 
                                        filter(doy >= 141 & doy <=208) %>% 
                                        mutate(Year = as.factor(year)), 
                                      aes(x = DD_mean, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Cohort Degree Days")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

###Lake Depth
#full data set
(lakedepth.age.freqpoly.combined.ages.full <- ggplot(data = binded %>% 
                                         mutate(Year = as.factor(year)), 
                                       aes(x = logdepth, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Log10 Lake Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

#restricted doy values 
(lakedepth.age.freqpoly.combined.ages.restricted <- ggplot(data = binded %>% 
                                               filter(doy >= 141 & doy <=208) %>% 
                                               mutate(Year = as.factor(year)), 
                                             aes(x = logdepth, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Log10 Lake Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

###Surface Temperature
#full data set
(surftemp.age.freqpoly.combined.ages.full <- ggplot(data = binded %>% 
                                        mutate(Year = as.factor(year)), 
                                      aes(x = surf_temp_year, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Mean Annual Surface Temperature")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

#restricted doy values 
(surftemp.age.freqpoly.combined.ages.restricted <- ggplot(data = binded %>% 
                                              filter(doy >= 141 & doy <=208) %>% 
                                              mutate(Year = as.factor(year)), 
                                            aes(x = surf_temp_year, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Mean Annual Surface Temperature")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

###Lake Area
#full data set
(lakearea.age.freqpoly.combined.ages.full <- ggplot(data = binded %>% 
                                        mutate(Year = as.factor(year)), 
                                      aes(x = logarea, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Log10 Lake Area")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

#restricted doy values 
(lakearea.age.freqpoly.combined.ages.restricted <- ggplot(data = binded %>% 
                                              filter(doy >= 141 & doy <=208) %>% 
                                              mutate(Year = as.factor(year)), 
                                            aes(x = logarea, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Log10 Lake Area")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

###Bluegill CPUE
#full data set
(bgcpue.age.freqpoly.combined.ages.full <- ggplot(data = binded %>% 
                                      mutate(Year = as.factor(year)), 
                                    aes(x = cpue, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Bluegill CPUE")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

#restricted doy values 
(bgcpue.age.freqpoly.combined.ages.restricted <- ggplot(data = binded %>% 
                                            filter(doy >= 141 & doy <=208) %>% 
                                            mutate(Year = as.factor(year)), 
                                          aes(x = cpue, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Bluegill CPUE")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

###Secchi Depth
#full data set
(secchi.age.freqpoly.combined.ages.full <- ggplot(data = binded %>% 
                                      mutate(Year = as.factor(year)), 
                                    aes(x = mean_secchi_m, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Secchi Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

#restricted doy values 
(secchi.age.freqpoly.combined.ages.restricted <- ggplot(data = binded %>% 
                                            filter(doy >= 141 & doy <=208) %>% 
                                            mutate(Year = as.factor(year)), 
                                          aes(x = mean_secchi_m, color = Type))+
    geom_freqpoly()+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Secchi Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

###----Stacked Combined Ages Freqpoly------------------------------------------------------
all.freqpoly.combined.ages <- ggarrange(doy.age.freqpoly.combined.ages.full, doy.age.freqpoly.combined.ages.restricted,
                          dd.age.freqpoly.combined.ages.full, dd.age.freqpoly.combined.ages.restricted,
                          surftemp.age.freqpoly.combined.ages.full, surftemp.age.freqpoly.combined.ages.restricted,
                          lakearea.age.freqpoly.combined.ages.full, lakearea.age.freqpoly.combined.ages.restricted,
                          lakedepth.age.freqpoly.combined.ages.full, lakedepth.age.freqpoly.combined.ages.restricted,
                          secchi.age.freqpoly.combined.ages.full, secchi.age.freqpoly.combined.ages.restricted,
                          bgcpue.age.freqpoly.combined.ages.full, bgcpue.age.freqpoly.combined.ages.restricted,
                          ncol = 2, nrow = 7, labels = "AUTO", 
                          common.legend = T, legend = "right")

all.freqpoly.combined.ages

ggsave(filename = "Figures/Frequency Histograms/all_freqpoly_combined_ages.jpeg",
       plot = all.freqpoly.combined.ages,
       width = 300, height = 550, units = "mm", dpi = 300)
###----Freqpolys Per Age Class----------------------------------------------------
###DOY
#full data set
(doy.age.freqpoly.full <- ggplot(data = binded %>% 
                               mutate(Year = as.factor(year)), 
                             aes(x = doy, color = Type))+
   geom_freqpoly()+
   facet_wrap(~AGE)+
   scale_color_viridis_d(end = 0.9)+
   labs(y = "Count", x = "Day of Year")+
   scale_y_continuous(expand = c(0,0))+
   theme_bw()+
   theme(axis.text = element_text(size = 8),
         axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/doy_freq_freqpoly_full.tiff", 
       plot = doy.age.freqpoly.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(doy.age.freqpoly.restricted <- ggplot(data = binded %>% 
                                     filter(doy >= 141 & doy <=208) %>% 
                                     mutate(Year = as.factor(year)), 
                                   aes(x = doy, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Day of Year")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/doy_freq_freqpoly_restricted.tiff", 
       plot = doy.age.freqpoly.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Degree days
#full data set
(dd.age.freqpoly.full <- ggplot(data = binded %>% 
                              mutate(Year = as.factor(year)), 
                            aes(x = DD_mean, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Cohort Degree Days")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/dd_freq_freqpoly_full.tiff", 
       plot = dd.age.freqpoly.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(dd.age.freqpoly.restricted <- ggplot(data = binded %>% 
                                    filter(doy >= 141 & doy <=208) %>% 
                                    mutate(Year = as.factor(year)), 
                                  aes(x = DD_mean, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Cohort Degree Days")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/dd_freq_freqpoly_restricted.tiff", 
       plot = dd.age.freqpoly.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Lake Depth
#full data set
(lakedepth.age.freqpoly.full <- ggplot(data = binded %>% 
                                     mutate(Year = as.factor(year)), 
                                   aes(x = logdepth, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Log10 Lake Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/lakedepth_freq_freqpoly_full.tiff", 
       plot = lakedepth.age.freqpoly.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(lakedepth.age.freqpoly.restricted <- ggplot(data = binded %>% 
                                           filter(doy >= 141 & doy <=208) %>% 
                                           mutate(Year = as.factor(year)), 
                                         aes(x = logdepth, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Log10 Lake Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/lakedepth_freq_freqpoly_restricted.tiff", 
       plot = lakedepth.age.freqpoly.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Surface Temperature
#full data set
(surftemp.age.freqpoly.full <- ggplot(data = binded %>% 
                                    mutate(Year = as.factor(year)), 
                                  aes(x = surf_temp_year, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Mean Annual Surface Temperature")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/surftemp_freq_freqpoly_full.tiff", 
       plot = surftemp.age.freqpoly.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(surftemp.age.freqpoly.restricted <- ggplot(data = binded %>% 
                                          filter(doy >= 141 & doy <=208) %>% 
                                          mutate(Year = as.factor(year)), 
                                        aes(x = surf_temp_year, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Mean Annual Surface Temperature")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/surftemp_freq_freqpoly_restricted.tiff", 
       plot = surftemp.age.freqpoly.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Lake Area
#full data set
(lakearea.age.freqpoly.full <- ggplot(data = binded %>% 
                                    mutate(Year = as.factor(year)), 
                                  aes(x = logarea, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Log10 Lake Area")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/lakearea_freq_freqpoly_full.tiff", 
       plot = lakearea.age.freqpoly.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(lakearea.age.freqpoly.restricted <- ggplot(data = binded %>% 
                                          filter(doy >= 141 & doy <=208) %>% 
                                          mutate(Year = as.factor(year)), 
                                        aes(x = logarea, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Log10 Lake Area")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/lakearea_freq_freqpoly_restricted.tiff", 
       plot = lakearea.age.freqpoly.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Bluegill CPUE
#full data set
(bgcpue.age.freqpoly.full <- ggplot(data = binded %>% 
                                  mutate(Year = as.factor(year)), 
                                aes(x = cpue, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Bluegill CPUE")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/bgcpue_freq_freqpoly_full.tiff", 
       plot = bgcpue.age.freqpoly.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(bgcpue.age.freqpoly.restricted <- ggplot(data = binded %>% 
                                        filter(doy >= 141 & doy <=208) %>% 
                                        mutate(Year = as.factor(year)), 
                                      aes(x = cpue, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Bluegill CPUE")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/bgcpue_freq_freqpoly_restricted.tiff", 
       plot = bgcpue.age.freqpoly.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###Secchi Depth
#full data set
(secchi.age.freqpoly.full <- ggplot(data = binded %>% 
                                  mutate(Year = as.factor(year)), 
                                aes(x = mean_secchi_m, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Secchi Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/secchi_freq_freqpoly_full.tiff", 
       plot = secchi.age.freqpoly.full, 
       width = 200, height = 150, units = "mm", dpi = 300)

#restricted doy values 
(secchi.age.freqpoly.restricted <- ggplot(data = binded %>% 
                                        filter(doy >= 141 & doy <=208) %>% 
                                        mutate(Year = as.factor(year)), 
                                      aes(x = mean_secchi_m, color = Type))+
    geom_freqpoly()+
    facet_wrap(~AGE)+
    scale_color_viridis_d(end = 0.9)+
    labs(y = "Count", x = "Secchi Depth")+
    scale_y_continuous(expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45))
)

ggsave(filename = "Figures/Frequency Histograms/secchi_freq_freqpoly_restricted.tiff", 
       plot = secchi.age.freqpoly.restricted, 
       width = 200, height = 150, units = "mm", dpi = 300)

###----Stacked Freqpoly per age class------------------------------------------------------
all.freqpoly <- ggarrange(doy.age.freqpoly.full, doy.age.freqpoly.restricted,
                          dd.age.freqpoly.full, dd.age.freqpoly.restricted,
                          surftemp.age.freqpoly.full, surftemp.age.freqpoly.restricted,
                          lakearea.age.freqpoly.full, lakearea.age.freqpoly.restricted,
                          lakedepth.age.freqpoly.full, lakedepth.age.freqpoly.restricted,
                          secchi.age.freqpoly.full, secchi.age.freqpoly.restricted,
                          bgcpue.age.freqpoly.full, bgcpue.age.freqpoly.restricted,
                          ncol = 2, nrow = 7, labels = "AUTO", 
                          common.legend = T, legend = "right")

all.freqpoly

ggsave(filename = "Figures/Frequency Histograms/all_freqpoly.jpeg",
       plot = all.freqpoly,
       width = 300, height = 550, units = "mm", dpi = 300)

#-----Individual Lakes-----------------------------------------------------------
#Exploring the data to look for individual lakes with repeated observations
#Number of observations per lake
lake.obs <- binded %>% 
  group_by(new_key, Type) %>% 
  tally() %>% 
  arrange(desc(n))

lake.obs.wide <- lake.obs %>% 
  pivot_wider(names_from = Type, values_from = n) %>% 
  arrange(desc(Contemporary), desc(Historic))

lake.obs.top <- lake.obs %>% 
  filter(n > 49)

#plot observations through time forlakes with the most observations
(lake.obs.year <- ggplot(data = binded %>% 
                              filter(new_key %in% c(lake.obs.top$new_key)) %>% 
                              mutate(Year = as.factor(year)), 
                            aes(x = year, fill = AGE))+
   geom_histogram()+
   facet_wrap(~new_key)+
   scale_fill_viridis_d(end = 0.9)+
   labs(y = "Frequency", x = "Year")+
   scale_y_continuous(expand = c(0,0))+
   theme_bw()+
   theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
)

ggsave(filename = "Figures/Frequency Histograms/lakes_observations_year.tiff", 
       plot = lake.obs.year, 
       width = 200, height = 150, units = "mm", dpi = 300)



#-----cpue and DD-----------------------------------------------------------
#summary by lake year 
summary_data_lakeyear<-binded %>% 
  distinct(new_key, year, .keep_all = TRUE)

dd_cpue_plot<-ggplot(summary_data_lakeyear, aes(dd_year, cpue)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw() + 
  labs(x = "Degree Days", y="Bluegill CPUE")

ggsave(filename = "Figures/dd_cpue_plot.tiff", 
       plot = dd_cpue_plot, 
       width = 200, height = 150, units = "mm", dpi = 300)


