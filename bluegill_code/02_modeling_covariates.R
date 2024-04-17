library(tidyverse) #data manipulation
library(ggeffects) #marginal effect plots
#-----------------------Load Data---------------------------------------------------
binded <- read.csv("bluegill_data/model_data.csv") %>% 
  mutate(cpue = log(cpue+1))


#-----------------------Linear Regressions-------------------------------------------
#We want to check if when sampling occurred meaningfully varies through time
#so regressing doy and year

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

#multiple regression to examine change through time 
#partial to several important variables

lm.length.year <- lm(length_mean_mm ~ year*AGE + logarea + logdepth + doy, data = binded)
summary(lm.length.year)

(length.year.plot <- ggplot(data = binded %>% 
                              mutate(Age = as.factor(AGE)), 
                            aes(x = year, y = length_mean_mm, 
                                color = Age, fill = Age, group = Age))+
    #geom_point()+
    geom_smooth(method = "lm")+
    scale_color_viridis_d()+
    scale_fill_viridis_d()+
    labs(y = "Length (mm)", x = "Year")+
    theme_bw()
)

(length.year.marginal.plot <- ggpredict(lm.length.year, terms = c("year", "AGE")) %>%  
    plot()+
    scale_color_viridis_d()+
    scale_fill_viridis_d()+
    labs(title = NULL,
         y = "Length (mm)",
         x = "Year")+
    guides(fill = guide_legend(title = "Age"), color = guide_legend(title = "Age"))+
    theme_bw()
)

