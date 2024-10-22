library(tidyverse) #data manipulation
library(RVAideMemoire) #used for testing lm model assumptions
library(DHARMa) #checking model assumptions and output
library(fitdistrplus) #insight into which distribution to use
library(logspline) #k-s test for selected distribution 
library(car) #partial regression plots
library(effectsize) #calculate effect sizes
library(ggeffects) #marginal effect plots
library(marginaleffects) #marginal effects
library(emmeans) #posthoc analyses
library(ggpmisc) 
library(ggpubr) #arranging multipanel plots
#-----------------------Load Data---------------------------------------------------
binded <- read.csv("bluegill_data/model_data.csv") %>% 
  mutate(cpue = log(cpue+1), 
         logarea = log10(area_ha),
         logdepth = log10(depth_m)) %>% 
  mutate(AGE = as.factor(AGE),
         log_length = log10(length_mean_mm))
#-----------------------Linear Regressions-------------------------------------------
#We want to check if when sampling occurred meaningfully varies through time
#so regressing doy and year

#check model assumptions
#normality


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

(temp.yr.plot <- ggplot(data = binded, aes(x = year, y = surf_temp_year, 
                                           color = LAT_DD, fill = LAT_DD))+
    geom_point()+
    geom_smooth()+
    scale_color_viridis_c(direction = -1)+
    scale_fill_viridis_c(direction = -1)+
    labs(y = "Surface Temperature", x = "Year")+
    theme_bw()+
    guides(fill = guide_legend(title = "Latitude"),
           color = guide_legend(title = "Latitude"))
)

(dd.mean.yr.plot <- ggplot(data = binded, aes(x = year, y = DD_mean, 
                                              color = LAT_DD, fill = LAT_DD))+
    geom_point()+
    geom_smooth()+
    scale_color_viridis_c(direction = -1)+
    scale_fill_viridis_c(direction = -1)+
    labs(y = "Mean Degree Days", x = "Year")+
    theme_bw()+
    guides(fill = guide_legend(title = "Latitude"),
           color = guide_legend(title = "Latitude"))
)

(dd.yr.plot <- ggplot(data = binded, aes(x = year, y = dd_year, 
                                         color = LAT_DD, fill = LAT_DD))+
    geom_point()+
    geom_smooth()+
    scale_color_viridis_c(direction = -1)+
    scale_fill_viridis_c(direction = -1)+
    labs(y = "Degree Days", x = "Year")+
    theme_bw()+
    guides(fill = guide_legend(title = "Latitude"),
           color = guide_legend(title = "Latitude"))
)

(lat.yr.plot <- ggplot(data = binded, aes(x = year, y = LAT_DD,
                                          color = surf_temp_year, 
                                          fill = surf_temp_year))+
    geom_point()+
    geom_smooth()+
    scale_color_viridis_c()+
    scale_fill_viridis_c()+
    labs(y = "Latitude", x = "Year")+
    theme_bw()+
    guides(fill = guide_legend(title = "Surface Temperature"),
           color = guide_legend(title = "Surface Temperature"))
)

climate.plot <- ggarrange(temp.yr.plot, dd.yr.plot, 
                          dd.mean.yr.plot, lat.yr.plot,
                          ncol = 1, nrow = 4, labels = "AUTO")
climate.plot

ggsave(filename = "Figures/climate_vars_year.tiff", climate.plot,
       height = 250, width = 150, units = "mm", dpi = 300)

#-----------Multiple regression of length through time-------------------------------
###-------Full Dataset-----------------------------------------------------------------
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
lm.length.year <- lm(length_mean_mm ~ year*AGE + I(logarea^2) + I(logdepth^2), data = binded)
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

######---------------------Marginal Effects------------------------------------------
#plot marginal effects predicted over range of x and y values in the data
result <- predict_response(lm.length.year, c("year", "AGE"))
plot(result)
test_predictions(result) %>% 
  write.csv(file = "Tables/pairwise_length_time_slopes.csv", row.names = F)

(length.year.marginal.plot <- ggpredict(lm.length.year, terms = c("year", "AGE")) %>%  
    plot()+
    #stat_poly_eq(use_label("eq"))+
    #stat_regline_equation()+
    stat_poly_line()+
    annotate(geom = "text", label = "y = 77 + 0.07x", x = 2000, y = 224)+
    annotate(geom = "text", label = "y = 21 + 0.093x", x = 2000, y = 212)+
    annotate(geom = "text", label = "y = -64 + 0.13x", x = 2000, y = 199)+
    annotate(geom = "text", label = "y = 230 - 0.028x", x = 2000, y = 182)+
    annotate(geom = "text", label = "y = 610 - 0.23x", x = 2000, y = 160)+
    annotate(geom = "text", label = "y = 920 - 0.39x", x = 2000, y = 137)+
    annotate(geom = "text", label = "y = 1200 - 0.55x", x = 2000, y = 110)+
    annotate(geom = "text", label = "y = 1700 - 0.83x", x = 2000, y = 80)+
    scale_color_viridis_d()+
    scale_fill_viridis_d()+
    labs(title = NULL,
         y = "Mean Length-at-Age (mm)",
         x = "Year")+
    guides(fill = guide_legend(title = "Age", reverse = T), color = guide_legend(title = "Age", reverse = T))+
    theme_bw()
)


ggsave(filename = "~/GitHub/bluegill_growth/Figures/length_year_marginal_effects_plot.jpg", 
       length.year.marginal.plot, 
       dpi = 300, width = 200, height = 150, units = "mm")
####----------Test alternate distributions---------------------------------------------------------------------------
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

###-------Reduced Dataset-----------------------------------------------------------------
#restricted day of year (doy) range
#partial to several important variables
binded.doy.restricted <- binded %>% filter(doy >= 141 & doy <=208)
#binded.doy.restricted<-filter(binded, doy >= 139 & doy <=162) #is the IQR across the SNT dataset

(year.age.hist.restricted <- ggplot(data = binded.doy.restricted, aes(x = year))+
   geom_histogram()+
   facet_wrap(~AGE)
)

(length.age.hist.restricted <- ggplot(data = binded.doy.restricted, aes(x = length_mean_mm))+
    geom_histogram()+
    facet_wrap(~AGE)
)

#check normality
byf.shapiro(length_mean_mm ~ as.factor(AGE), data = binded.doy.restricted)
#not normal for all age classes, log transformation makes it worse

#running lm anyway just to see what it looks like 
#candidate models:
lm.length.year.restricted <- lm(length_mean_mm ~ year*AGE + I(logarea^2) + I(logdepth^2), 
                                     data = binded.doy.restricted)

summary(lm.length.year.restricted)
eta_squared(lm.length.year.restricted)
interpret(eta_squared(lm.length.year.restricted), rules = "cohen1992")
AIC(lm.length.year.restricted)


testDispersion(lm.length.year.restricted)

simulation.output.restricted <- simulateResiduals(fittedModel = lm.length.year.restricted)

residuals(simulation.output.restricted)
residuals(simulation.output.restricted, quantileFunction = qnorm)

plot(simulation.output.restricted)

#post hoc comparisons
emm.restricted <- emmeans(lm.length.year.restricted, ~ AGE*year)
pairs(emm.restricted, simple = "AGE")
test(pairs(emm.restricted, by = "year"), by = NULL, adjust = "mvt")

#export tables
interpret(eta_squared(lm.length.year.restricted), rules = "cohen1992") %>% 
  write.csv(file = "Tables/eta_squared_doy_restricted.csv")

pairs(emm.restricted, simple = "AGE") %>% 
  write.csv(file = "Tables/emmeans_doy_restricted.csv")

#plot raw data
(length.year.plot.restricted <- ggplot(data = binded.doy.restricted %>% 
                              mutate(Age = as.factor(AGE)), 
                            aes(x = year, y = length_mean_mm, 
                                color = Age, fill = Age, group = Age))+
    #geom_point()+
    geom_smooth(method = "lm")+
    stat_poly_eq(use_label("eq"))+
    scale_color_viridis_d()+
    scale_fill_viridis_d()+
    labs(y = "Length (mm)", x = "Year")+
    theme_bw()
)

####---Marginal Effects------------------------------------------------------
#plot marginal effects predicted over range of x and y values in the data
result.restricted <- predict_response(lm.length.year.restricted, c("year", "AGE"))
plot(result.restricted)
test_predictions(result.restricted) %>% 
  write.csv(file = "Tables/pairwise_length_time_slopes_restricted.csv", row.names = F)


(length.year.marginal.plot.restricted <- ggpredict(lm.length.year.restricted, terms = c("year", "AGE")) %>%  
    plot()+
    #stat_poly_eq(use_label("eq"))+
    #stat_regline_equation()+
    stat_poly_line()+
    annotate(geom = "text", label = "y = 310 - 0.046x", x = 2000, y = 224)+
    annotate(geom = "text", label = "y = 70 + 0.069x", x = 2000, y = 212)+
    annotate(geom = "text", label = "y = -24 + 0.11x", x = 2000, y = 199)+
    annotate(geom = "text", label = "y = 210 - 0.015x", x = 2000, y = 182)+
    annotate(geom = "text", label = "y = 560 - 0.2x", x = 2000, y = 159)+
    annotate(geom = "text", label = "y = 790 - 0.33x", x = 2000, y = 135)+
    annotate(geom = "text", label = "y = 980 - 0.44x", x = 2000, y = 105)+
    annotate(geom = "text", label = "y = 1200 - 0.55x", x = 2000, y = 75)+
    scale_color_viridis_d()+
    scale_fill_viridis_d()+
    labs(title = NULL,
         y = "Mean Length-at-Age (mm)",
         x = "Year")+
    guides(fill = guide_legend(title = "Age", reverse = T), 
           color = guide_legend(title = "Age", reverse = T))+
    theme_bw()
)

ggsave(filename = "~/GitHub/bluegill_growth/Figures/length_year_marginal_effects_plot_restricted.jpg", 
       length.year.marginal.plot.restricted, 
       dpi = 300, width = 200, height = 150, units = "mm")

####----------Test alternate distributions---------------------------------------------------------------------------
#try to find a distribution to use other than normal
#generate Cullen and Frey plot to get ideas for distributions to test
descdist(binded.doy.restricted$length_mean_mm)

#fit several potential distributions
fit.weibull.restricted <- fitdist(binded.doy.restricted$length_mean_mm, "weibull")
fit.norm.restricted <- fitdist(binded.doy.restricted$length_mean_mm, "norm")
fit.log.norm.restricted <- fitdist(log(binded.doy.restricted$length_mean_mm), "norm")
fit.gamma.restricted <- fitdist(binded.doy.restricted$length_mean_mm, "gamma")

#plot the fit of those distributions
plot(fit.weibull.restricted)
plot(fit.norm.restricted)
plot(fit.log.norm.restricted)
plot(fit.gamma.restricted)

#compare aic values
fit.weibull.restricted$aic
fit.norm.restricted$aic
fit.log.norm.restricted$aic
fit.gamma.restricted$aic

#log normal has the lowest aic by an order of magnitude
#so we're going to stick with a multiple regression using log transformed lengths

#Kolmogorov-Smirnov test simulation
#weibull has lowest AIC and best looking plots
# n.sims <- 5e4
# 
# stats <- replicate(n.sims, {      
#   r <- rweibull(n = length(binded.doy.restricted$length_mean_mm)
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

#### plot depth and area #### 
ggplot(data = binded.doy.restricted, aes(x = year, y = logdepth))+
    geom_point()+
    geom_smooth()+
    theme_bw()

ggplot(data = binded.doy.restricted, aes(x = year, y = logarea))+
  geom_point()+
  geom_smooth()+
  theme_bw()

#log transformed
area_mod= lm( logarea ~ year, data = binded.doy.restricted )
summary(area_mod)

depth_mod =lm( logdepth ~ year, data = binded.doy.restricted )
summary(depth_mod)

#untransformed 
area_mod= lm( area_ha ~ year, data = binded.doy.restricted )
summary(area_mod)

depth_mod =lm( depth_m ~ year, data = binded.doy.restricted )
summary(depth_mod)



