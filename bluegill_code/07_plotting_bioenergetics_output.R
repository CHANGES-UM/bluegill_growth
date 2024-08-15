#load libraries
library(tidyverse) #data manipulation and plotting
library(ggpubr) #stacking plots

#load data
constant.consumption <- read.csv("bluegill_data/Bioenergetics Output/version6_same_p.csv") %>% 
  mutate(DD = as.factor(DD))

constant.growth <- read.csv("bluegill_data/Bioenergetics Output/version6_same_growth.csv") %>% 
  mutate(DD = as.factor(DD))

#plot constant consumption length by age
(constant.consumption.plot <- ggplot(data = constant.consumption, 
                                     aes(x = Age, y = Initial.Length, color = DD, group = DD))+
    geom_smooth(se = F)+
    scale_color_viridis_d(option = "A", end = 0.95)+
    scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
    labs(y = "Length (mm)")+
    guides(fill = guide_legend(title = "Degree Days", reverse = T), 
           color = guide_legend(title = "Degree Days", reverse = T))+
    theme_bw()+
    theme()
)

# ggsave("Figures/constant_consumption_length_by_age.tiff",
#        plot = constant.consumption.plot,
#        dpi = 300, width = 200, height = 150, units = "mm")

#plot constant growth consumption by age
(constant.growth.plot <- ggplot(data = constant.growth, 
                                     aes(x = Age, y = Total.Consumption, color = DD, group = DD))+
    geom_smooth(se = F)+
    scale_color_viridis_d(option = "A", end = 0.95)+
    scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
    labs(y = "Total Consumption (g)")+
    guides(fill = guide_legend(title = "Degree Days", reverse = T), 
           color = guide_legend(title = "Degree Days", reverse = T))+
    theme_bw()+
    theme()
)

# ggsave("Figures/constant_growth_consumption_by_age.tiff", 
#        plot = constant.growth.plot,
#        dpi = 300, width = 200, height = 150, units = "mm")


#stack plots
bioenergetics.stacked <- ggarrange(constant.consumption.plot, constant.growth.plot, 
                                   nrow = 2, ncol = 1, labels = "AUTO", 
                                   common.legend = T, legend = "right")
bioenergetics.stacked

ggsave("Figures/bioenergetics_stacked.tiff", 
       plot = bioenergetics.stacked,
       dpi = 300, width = 200, height = 150, units = "mm")
