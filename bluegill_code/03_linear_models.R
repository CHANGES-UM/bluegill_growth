library(tidyverse) #data manipulation
library(RVAideMemoire) #used for testing lm model assumptions
library(DHARMa) #checking model assumptions and output
library(fitdistrplus) #insight into which distribution to use
library(logspline) #k-s test for selected distribution 
library(effectsize) #calculate effect sizes
library(ggeffects) #marginal effect plots
library(emmeans) #posthoc analyses
library(ggpmisc) 
library(ggpubr) #arranging multipanel plots
#-----------------------Load Data---------------------------------------------------
binded <- read.csv("bluegill_data/model_data.csv") %>% 
  mutate(cpue = log10(cpue+1), 
         logarea = log10(area_ha),
         logdepth = log10(depth_m)) %>% 
  mutate(AGE = as.factor(AGE))


#-----------------------Linear Regressions-------------------------------------------
#We want to check if when sampling occurred meaningfully varies through time
#so regressing doy and year

#check model assumptions
#normality

#histograms
(doy.age.hist <- ggplot(data = binded %>% 
                          mutate(Year = as.factor(year)), 
                        aes(x = doy, fill = Year))+
   geom_histogram()+
   facet_wrap(~AGE)+
   scale_fill_viridis_d()+
   labs(y = "Count", x = "Day of Year")+
   scale_y_continuous(expand = c(0,0))+
   theme_bw()
)

ggsave(filename = "Figures/doy_freq_hist.tiff", plot = doy.age.hist, 
       width = 200, height = 150, units = "mm", dpi = 300)

#shapiro test by factor level
byf.shapiro(doy ~ AGE, data = binded)
#not normal

#looking at it anyway just for visualization purposes
lm.doy.yr <- lm(doy ~ year*AGE, data = binded)
summary(lm.doy.yr)

(doy.yr.plot <- ggplot(data = binded %>% 
                         mutate(Age  = as.factor(AGE)), 
                       aes(x = year, y = doy,
                           color = Age, fill = Age, group = Age))+
    geom_smooth(method = "lm")+
    #facet_wrap(~Age, scales = "free")+
    scale_color_viridis_d()+
    scale_fill_viridis_d()+
    labs(y = "Day of Year", x = "Year")+
    theme_bw()
)

#How do our climate variables vary through time?
climate_plot_data<-binded%>% 
  distinct(year, new_key, .keep_all = TRUE)

(temp.yr.plot <- ggplot(data = climate_plot_data, aes(x = year, y = surf_temp_year))+
    geom_point()+
    geom_smooth()+
    labs(y = "Surface Temperature", x = "Year")+
    theme_bw()
)

(dd.mean.yr.plot <- ggplot(data = binded, aes(x = year, y = DD_mean))+
    geom_point()+
    geom_smooth()+
    labs(y = "Mean Degree Days", x = "Year")+
    theme_bw()
)

(dd.yr.plot <- ggplot(data = climate_plot_data, aes(x = year, y = dd_year))+
    geom_point()+
    geom_smooth()+
    labs(y = "Degree Days", x = "Year")+
    theme_bw()
)

(lat.yr.plot <- ggplot(data = climate_plot_data, aes(x = year, y = LAT_DD))+
    geom_point()+
    geom_smooth()+
    labs(y = "Latitude", x = "Year")+
    theme_bw()
)

climate.plot <- ggarrange(temp.yr.plot, dd.yr.plot, dd.mean.yr.plot, lat.yr.plot,
                          ncol = 1, nrow = 4, labels = "AUTO")
climate.plot

ggsave(filename = "Figures/climate_vars_year.tiff", climate.plot,
       height = 200, width = 150, units = "mm", dpi = 300)

#-----------Multiple regression of length through time-------------------------------
#partial to several important variables

(year.age.hist <- ggplot(data = binded, aes(x = year))+
   geom_histogram()+
   facet_wrap(~AGE)
)

(length.age.hist <- ggplot(data = binded, aes(x = length_mean_mm))+
    geom_histogram()+
    facet_wrap(~AGE)
)

#check normality
byf.shapiro(length_mean_mm ~ as.factor(AGE), data = binded)
#not normal, log transformation doesn't fix this

#running lm anyway just to see what it looks like 
lm.length.year <- lm(log(length_mean_mm) ~ year*AGE + logarea + logdepth + doy, data = binded)
summary(lm.length.year)
eta_squared(lm.length.year)
interpret(eta_squared(lm.length.year), rules = "cohen1992")
AIC(lm.length.year)


testDispersion(lm.length.year)

simulation.output <- simulateResiduals(fittedModel = lm.length.year)

residuals(simulation.output)
residuals(simulation.output, quantileFunction = qnorm)

plot(simulation.output)

#post hoc comparisons
emm <- emmeans(lm.length.year, ~ AGE*year)
pairs(emm, simple = "AGE")
test(pairs(emm, by = "year"), by = NULL, adjust = "mvt")

#export tables
interpret(eta_squared(lm.length.year), rules = "cohen1992") %>% 
  write.csv(file = "Tables/eta_squared.csv")

pairs(emm, simple = "AGE") %>% 
  write.csv(file = "Tables/emmeans.csv")

#plot raw data
(length.year.plot <- ggplot(data = binded %>% 
                              mutate(Age = as.factor(AGE)), 
                            aes(x = year, y = length_mean_mm, 
                                color = Age, fill = Age, group = Age))+
    #geom_point()+
    geom_smooth(method = "lm")+
    stat_poly_eq()+
    scale_color_viridis_d()+
    scale_fill_viridis_d()+
    labs(y = "Length (mm)", x = "Year")+
    theme_bw()
)

#plot marginal effects predicted over range of x and y values in the data
(length.year.marginal.plot <- ggpredict(lm.length.year, terms = c("year", "AGE")) %>%  
    plot()+
    #stat_poly_eq(use_label("eq"))+
    annotate(geom = "text", label = "y = 0.24x - 273", x = 2000, y = 215)+
    annotate(geom = "text", label = "y = 0.27x - 344", x = 2000, y = 205)+
    annotate(geom = "text", label = "y = 0.31x - 440", x = 2000, y = 192)+
    annotate(geom = "text", label = "y = 0.16x - 143", x = 2000, y = 175)+
    annotate(geom = "text", label = "y = -0.06x + 260", x = 2000, y = 152)+
    annotate(geom = "text", label = "y = -0.24x + 593", x = 2000, y = 130)+
    annotate(geom = "text", label = "y = -0.41x + 921", x = 2000, y = 100)+
    annotate(geom = "text", label = "y = -0.70x + 1460", x = 2000, y = 70)+
    scale_color_viridis_d()+
    scale_fill_viridis_d()+
    labs(title = NULL,
         y = "Mean Length-at-Age (mm)",
         x = "Year")+
    guides(fill = guide_legend(title = "Age"), color = guide_legend(title = "Age"))+
    theme_bw()
)

ggsave(filename = "~/GitHub/bluegill_growth/Figures/length_year_marginal_effects_plot.jpg", length.year.marginal.plot, 
       dpi = 300, width = 200, height = 150, units = "mm")

#----------Test alternate distributions---------------------------------------------------------------------------
#try to find a distribution to use other than normal
#generate Cullen and Frey plot to get ideas for distributions to test
descdist(binded$length_mean_mm)

#fit several potential distributions
fit.weibull <- fitdist(binded$length_mean_mm, "weibull")
fit.norm <- fitdist(binded$length_mean_mm, "norm")
fit.log.norm <- fitdist(log(binded$length_mean_mm), "norm")
fit.gamma <- fitdist(binded$length_mean_mm, "gamma")

#plot the fit of those distributions
plot(fit.weibull)
plot(fit.norm)
plot(fit.log.norm)
plot(fit.gamma)

#compare aic values
fit.weibull$aic
fit.norm$aic
fit.log.norm$aic
fit.gamma$aic

#log normal has the lowest aic by an order of magnitude
#so we're going to stick with a multiple regression using log transformed lengths

#Kolmogorov-Smirnov test simulation
#weibull has lowest AIC and best looking plots
# n.sims <- 5e4
# 
# stats <- replicate(n.sims, {      
#   r <- rweibull(n = length(binded$length_mean_mm)
#                 , shape= fit.weibull$estimate["shape"]
#                 , scale = fit.weibull$estimate["scale"]
#   )
#   estfit.weibull <- fitdist(r, "weibull") # added to account for the estimated parameters
#   as.numeric(ks.test(r
#                      , "pweibull"
#                      , shape= estfit.weibull$estimate["shape"]
#                      , scale = estfit.weibull$estimate["scale"])$statistic
#   )      
# })
# 
# #plot
# plot(ecdf(stats), las = 1, main = "KS-test statistic simulation (CDF)", col = "darkorange", lwd = 1.7)
# grid()
# 
# #generate a p-value
# fit <- logspline(stats)
# 
# 1 - plogspline(ks.test(x
#                        , "pweibull"
#                        , shape= fit.weibull$estimate["shape"]
#                        , scale = fit.weibull$estimate["scale"])$statistic
#                , fit
# )

