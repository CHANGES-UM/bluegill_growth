##### written by Katelyn King 
library(spData)
library(ggplot2)
library(sf)
library(dplyr)

#read in data 
bg_data <- read.csv("bluegill_data/model_data.csv") %>% 
  filter(doy >= 141 & doy <=208)

bg_data_hist<-filter(bg_data, type == 'historical')%>%
  distinct(new_key, .keep_all = TRUE)
bg_data_cont<-filter(bg_data, type == 'contemporary')%>%
  distinct(new_key, .keep_all = TRUE)
n_distinct(bg_data_hist$new_key)
n_distinct(bg_data_cont$new_key)

#read in MI map
# get US inset 
data("us_states", package = "spData")

us_states = st_transform(us_states, crs = "+proj=longlat +datum=WGS84 +no_defs") #match the FMU shapefile
MI<-dplyr::filter(us_states, NAME == 'Michigan')
plot(MI)

# plot
(map_data<-ggplot() +
  geom_sf(data=MI, aes(), fill = "white") + 
  geom_point(data=bg_data, aes(x = LONG_DD, y = LAT_DD, color=c(type) )) +
  theme_bw() +  
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +
 #clean_theme() + 
  theme_void() +
  scale_colour_manual(values = c("darkblue", "lightsalmon"), 
                      name= '') + 
  guides(colour  = guide_legend(position = "inside")) +
  theme(legend.justification.inside = c(0.1, 0.3)) + 
  theme(legend.text=element_text(size=15))
)

ggsave( map_data, 
        device = "png", 
        filename = "Figures/map_of_data.png", 
        dpi = 600, height = 8, width = 8, units = "in",
        bg="#ffffff") #sets background to white 

#get MI basemap 
MI<-map_data("state") %>%
  subset(region %in% c("michigan")) # select michigan 
MI_basemap<-ggplot(data = MI) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + #this fill MI white and outline is black
  coord_fixed(1.3) 

#create a map #you can change the color based on the variable by changing "colour" part
MI_basemap + 
  geom_point(data=bg_data, aes(x = LONG_DD, y = LAT_DD, colour = c(type))) +  #
  labs(color="")  + #changes the labels on the legend 
  theme_bw() +
  scale_colour_manual(values = c("darkblue", "lightsalmon"), 
                      name= '')  + 
  guides(colour  = guide_legend(position = "inside")) +
  theme(legend.justification.inside = c(0, 0))


