
# R script for: Surveying Historical Growth Data: The Effects of Climate and Other Environmental Condition Factors on Bluegill Growth
# Dataset Creation
# Written by Elise Grabda and modified by Katelyn King 

#### load libraries #### 
library(tidyr) #data munging 
library(dplyr) #data munging 
library(skimr) #summary statistics 
library(readxl) #reading in excel files 
library(data.table) #needed for year day (doy)
library(rqdatatable) #library for natural_join

#### datasets for linking ####
#add lake link file to link new keys to nhdids (to join with temp data)
new_key_comid_translate<-read.csv("bluegill_data/new_key_nhd_link.csv") %>%  
  dplyr::select(IHDLKID,  NEW_KEY ) %>%
  dplyr::rename(nhdid = IHDLKID, new_key=NEW_KEY) %>%
  mutate_at(c('new_key'), ~na_if(., ''))  %>% # turn blank spaces into NA
  mutate_at(c('nhdid'), ~na_if(.,-99)) %>% #turn -99 into NA 
  drop_na()  #remove NA

#lakes with lat lon
IFRLakes <- read.csv("bluegill_data/IFR_lake_points.csv") %>% 
  dplyr::rename(new_key = NEW_KEY) %>% 
  dplyr::select(new_key, LAT_DD, LONG_DD)

#linking to lagos - needed for LULC data  
#joining the historic lake coordinates to the dataset of LAGOS lakes - Elise did this join in GIS - might be missing some
LAGOSjoinIFR <- read.csv("bluegill_data/lagos_join_ifr.csv") %>% 
  dplyr::select(new_key, lagoslakei) %>% 
  dplyr::rename(lagoslakeid = lagoslakei) %>% 
  distinct(new_key, .keep_all = TRUE)

#snt_lagoslink <- read_excel("bluegill_data/snt_lagoslink.xls") #? ignore for now   - the same as above? -apparently i gave this to her

#### historical datasets ####
BLG_dropNA <- read.csv("bluegill_data/all_grow_21NOV2023.csv") %>% 
  dplyr::filter(species == "bluegill")  %>%
  mutate(length_mean_mm = ifelse(is.na(length_mean_mm) & is.na(length_max_mm), length_min_mm, length_mean_mm)) %>%  #use min length if there is no max length 
  drop_na(c(new_key, begin_date_year, begin_date_month, begin_date_day, age_group, length_mean_mm )) %>% #need year, month, day for DOY calc 
  dplyr:: select(new_key, begin_date_year, begin_date_month, begin_date_day, age_group, length_mean_mm, county, subject_id)
BLG_dropNA$month <- match(BLG_dropNA$begin_date_month,month.name)

#get Secchi 
lake_summary_qaqc <- read.csv("bluegill_data/lake_summary_qaqc.csv") %>% 
  dplyr::select(new_key, begin_date_year, secchi_min_m)%>% 
  distinct(new_key, begin_date_year, .keep_all = TRUE)

#get pres/abs of other fishes 
lake_summ_fish_pres_qaqc <- read.csv("bluegill_data/lake_summ_fish_pres_qaqc.csv") %>% 
  dplyr::select(new_key, begin_date_year, yellow_perch, largemouth_bass, northern_pike, walleye) %>%  #want unique cases of lake and year (there are duplicates here)
  drop_na(new_key, begin_date_year) %>% 
  distinct(new_key, begin_date_year, .keep_all = TRUE)
# Replace NAs with 0s, these species were not found in these lakes
lake_summ_fish_pres_qaqc[is.na(lake_summ_fish_pres_qaqc)] <- 0

### legacy land use land cover 
file.list <- list.files("bluegill_data/backcast_lulc", pattern='*.xls', full.names = T) 
df.list <- lapply(file.list, FUN = read_excel)
names(df.list[[1]])
df.list[1]
year_names <- c(1938:1992)

for (i in 1:55) {
  
  names(df.list[[i]])[4] <- 'Open_Water'
  names(df.list[[i]])[5] <- 'Developed'
  names(df.list[[i]])[6] <- 'Mining'
  names(df.list[[i]])[7] <- 'Barren'
  names(df.list[[i]])[8] <- 'Deciduous'
  names(df.list[[i]])[9] <- 'Evergreen'
  names(df.list[[i]])[10] <- 'Mixed'
  names(df.list[[i]])[11] <- 'Grassland'
  names(df.list[[i]])[12] <- 'Shrubland'
  names(df.list[[i]])[13] <- 'AgLand'
  names(df.list[[i]])[14] <- 'Pasture'
  names(df.list[[i]])[15] <- 'HerbWetland'
  names(df.list[[i]])[16] <- 'WoodWetland'
  
  df.list[[i]]$sum <- rowSums(df.list[[i]][,3:16])
  df.list[[i]]$year <- year_names[i]
  
  
}

historic_lulc <- bind_rows(df.list)
historic_lulc <- historic_lulc %>% 
  mutate(urban  = Developed + Mining+ Barren,
          agriculture = AgLand+ Pasture + Grassland,
          forests =  Deciduous+ Mixed+ Evergreen+ Shrubland,
          wetlands =  WoodWetland + HerbWetland + Open_Water+ VALUE_0,
          sum =  forests+ wetlands+ urban+ agriculture,
          urban2 = urban/sum,
          agriculture2 = agriculture/sum,
          forests2 = forests/sum,
          wetlands2 = wetlands/sum  #change to a proportion
         ) %>%
  dplyr::select(LAGOSLAKEI, year, urban2, agriculture2, forests2, wetlands2) %>% 
  dplyr::rename(lagoslakeid = LAGOSLAKEI, urban = urban2, agriculture =agriculture2, forests = forests2, wetlands = wetlands2)

#join historical datasets 
all_historical <- left_join(BLG_dropNA, lake_summary_qaqc, by= c('new_key', 'begin_date_year')) %>% 
                  left_join(lake_summ_fish_pres_qaqc, by= c('new_key', 'begin_date_year')) %>% 
 left_join(LAGOSjoinIFR, by= 'new_key') %>% 
  left_join(IFRLakes, by= 'new_key') %>% 
  left_join(new_key_comid_translate, by= 'new_key') %>%
  left_join(historic_lulc, by=c('lagoslakeid', 'begin_date_year' = 'year' )) %>% 
  dplyr::rename(year=begin_date_year, day = begin_date_day, Survey_Number = subject_id, AGE=age_group, mean_secchi_m = secchi_min_m, 
                lmb=largemouth_bass ,pike=northern_pike,perch=yellow_perch)  %>% 
  dplyr::select(-c(begin_date_month)) %>% # keep just month with is numerical 
  mutate(type = 'historical') #add a column with the time period 

#### contemporary datasets ####
snt_blg <- read.csv("bluegill_data/snt_blg_ages_2002_2020.csv") %>% 
  dplyr::rename(new_key = New_Key) %>% 
  tidyr::separate(Collection_Date, 
                  into = c('month', 'day', 'year'), 
                  remove = FALSE) %>% 
  mutate(year = as.integer(paste0("20", year))) %>% #add the year 2000 in front of years
  filter(year<2066) %>% #there is a 1966? Ask Kevin about this 
  group_by(new_key,year,day,month,AGE,Survey_Number) %>%
  summarise_at(vars(LENGTH_IN), list(mean_length_in = mean)) %>% 
  ungroup() %>% 
  mutate(length_mean_mm = mean_length_in*25.4 ) %>%#convert to mm 
  left_join(IFRLakes, by= 'new_key') %>% 
  left_join(LAGOSjoinIFR, by= 'new_key')

nlcdyears <- c(2001,2006,2011,2016,2019)
a <- as.numeric(snt_blg$year)
b <- nlcdyears
snt_blg$lulcyear <- sapply(a, function(a, b) {b[which.min(abs(a-b))]}, b)

#catch data to include perch, walleye, pike, lmb 
snt_catch_data <- read.csv("bluegill_data/snt_catch_data_mar2021.csv") %>% 
  tidyr::pivot_wider(id_cols = c(NEW_KEY, SURVEY_YEAR, COUNTY),
                     names_from = SPECIES,
                     values_from = c(FISH_COUNT),
                     values_fn = sum,
                     values_fill = list(FISH_COUNT = 0))%>%
  dplyr::rename(new_key=NEW_KEY,
                   lmb=LMB,
                   pike=NOP,
                   perch=YEP,
                   walleye=WAE,
                year= SURVEY_YEAR, 
                county = COUNTY) %>% 
  dplyr::select(new_key, year, lmb, pike, perch, walleye, county)

#*predictor variables #### 
sntlulc <- read.csv("bluegill_data/sntlulc.csv") %>% #ask Elise where this came from
    mutate(urban = Developed, 
       agriculture =Cultivated_Crops+ Hay_Pasture + Herbaceous, 
       forests = Deciduous_Forest + Mixed_Forest+ Evergreen_Forest+ Shrub_Scrub, 
       wetlands = Woody_Wetlands + Emergent_Herbaceous_Wetlands + Open_Water + Unclassified, 
       sum = urban + agriculture + forests + wetlands, 
       urban2 = urban/sum,
       agriculture2 = agriculture/sum,
       forests2 = forests/sum,
       wetlands2 = wetlands/sum  #change to a proportion
) %>% 
  dplyr::select(lagoslakei, Year, urban2, agriculture2, forests2, wetlands2) %>% 
  dplyr::rename(lagoslakeid = lagoslakei, year = Year , urban = urban2, agriculture =agriculture2, forests = forests2, wetlands = wetlands2)

snt_secchi <- read.csv("bluegill_data/snt_secchi_2002_2020.csv") %>% 
  dplyr::rename(new_key = New_Key) %>% 
  dplyr::mutate(secchi_m = SECCHI_FT/3.281) %>% 
  dplyr::select(-c(SECCHI_FT)) %>% 
  group_by(new_key, Year) %>%
  summarise_at(vars(secchi_m), list(mean_secchi_m = mean)) #get rid of onen duplicate 

all_snt<- left_join(snt_blg, snt_catch_data, by=c('new_key','year')) %>% 
          left_join(sntlulc, by=c('lagoslakeid','lulcyear' = "year")) %>% 
        left_join(snt_secchi, by=c('new_key','year' = 'Year')) %>% 
  left_join(new_key_comid_translate, by= 'new_key') %>%
  dplyr::select(-c(mean_length_in, lulcyear)) %>% 
  mutate(type = 'contemporary') %>%  #add a column with the time period 
  mutate(perch = ifelse(perch >= 1, 1, 0), 
         pike = ifelse(pike >= 1, 1, 0),
         walleye = ifelse(walleye >= 1, 1, 0),
         lmb = ifelse(lmb >= 1, 1, 0),
         ) #change predator info to 0s and 1s 
 
#bind historical and contemporary data
binded<-rbind(all_historical, all_snt) 

#need a decade column to join with census data ; join by decade and county name
binded$decade <- round(binded$year/10)*10


#### data for both datasets ####
#* lake depth and area ####
lake_depth_2021 <- read.csv("bluegill_data/lake_depth_2021.csv") %>% 
dplyr::rename(new_key = New_Key) %>% 
  dplyr::mutate(depth_m = MeanDepth_ft/3.281, 
                area_ha = Area_ac/2.471) %>% 
  dplyr::select(new_key,area_ha, depth_m)

#*lake degree days #### 
lake_degree_days_year <- read.csv("bluegill_data/lake_degree_days_year.csv")%>%
  dplyr::select(-c(HECTARES, FETCH_M, ZMAX_M, ZAVE_M, D)) %>% 
  group_by(IHDLKID) %>%
  summarise_all( mean) %>% #some nhdids have more than one row... so take the mean 
  dplyr::rename(nhdid = IHDLKID)%>% 
  pivot_longer( #pivot so that year is its own column 
    cols= starts_with("DD_"),
    names_to = "year", 
    names_prefix = "DD_",
    values_to = "dd_year") %>% 
  mutate(year = as.integer(as.character(year)))


#* census data ####
countypop_1940 <- read.csv("bluegill_data/Census_Data_County/R13037017_SL050.csv") %>%
  dplyr::select(Area.Name, Total.Population, Year)
countypop_1950 <- read.csv("bluegill_data/Census_Data_County/R13037020_SL050.csv")%>%
  dplyr::select(Area.Name, Total.Population, Year)
countypop_1960 <- read.csv("bluegill_data/Census_Data_County/R13037022_SL050.csv") %>%
  dplyr::select(Area.name, Total.Population, Year) %>% 
  dplyr::rename(Area.Name = Area.name)
countypop_1970 <- read.csv("bluegill_data/Census_Data_County/R13037023_SL4601.csv")%>%
  dplyr::select(Area.Name, Total.Population, Year)
countypop_1980 <- read.csv("bluegill_data/Census_Data_County/R13037024_SL11.csv")%>%
  dplyr::select(Area.Name, Total.Population, Year)
countypop_1990 <- read.csv("bluegill_data/Census_Data_County/R13037025_SL050.csv")%>%
  dplyr::select(Area.Name, Total.Population, Year)
countypop_2000 <- read.csv("bluegill_data/Census_Data_County/R13037026_SL050.csv")%>%
  dplyr::select(Area.Name, Total.Population, Year)
countypop_2010 <- read.csv("bluegill_data/Census_Data_County/R13037027_SL050.csv")%>%
  dplyr::select(Area.Name, Total.Population, Year)
countypop_2020 <- read.csv("bluegill_data/Census_Data_County/R13037029_SL050.csv")%>%
  dplyr::select(Area.Name, X2020.Total.Population, Year) %>% 
  dplyr::rename(Total.Population = X2020.Total.Population)
#bind all the decades 
countypop_all<-rbind(countypop_1940, countypop_1950, countypop_1960, countypop_1970, countypop_1980, countypop_1990, countypop_2000, countypop_2010, countypop_2020) %>% 
  mutate(Area.Name = gsub(" County", "", Area.Name)) %>% 
  mutate(county = tolower(Area.Name)) %>% 
  dplyr::select(-c(Area.Name))

### need to make county all lowercase before join
# join by decade and county name
binded2<- left_join(binded, lake_depth_2021, by= 'new_key') %>% 
  mutate(county = tolower(county)) %>% 
  left_join(lake_degree_days_year, by =c('nhdid', 'year')) %>% 
  left_join(countypop_all, by =c('decade' = 'Year', 'county' = "county")) %>% 
  mutate(logdepth = log(depth_m),  #log transform 
         logarea = log(area_ha), 
         logcounty = log(as.numeric(Total.Population)), 
         date = paste(year,month,day,sep = "-"), 
         doy = yday(date) #day of year 
) 

#creating a variable that is the average dd of the lifetime of the fish 
#DD has 3 columns nhdid, year, and DD 
#take the sum of Degree Days of the equal nhdids AND the year before the year of the binded data AND all the years greater than the year - the age group THEN divide the sum DD by age  
binded2$DD_mean <- NA #create a new column called "DD_mean"

for (i in 1: 14454) {
  
  binded2[i,33] <- sum(lake_degree_days_year[lake_degree_days_year[,1]==binded2[i,17] & lake_degree_days_year[,2] < binded2[i,2] & lake_degree_days_year[,2] >= (binded[i,2]-binded[i,4]) ,3]) / binded[i,4]
  
}

#filter out age 0, and 9-18 
summary(as.factor(binded2$AGE))
model_data<-filter(binded2, AGE >=1 &  AGE <=8 )

#Add CPUE of trap/fyke
#### historical trap/fyke catch data #### 
fishc_data<-read.csv("bluegill_data/fishc_qaqc_added_lakes.csv")
bluegill_historical<-read.csv("bluegill_data/model_data.csv")%>% 
  filter(type == "historical") %>% 
  distinct(new_key, year) #just get the lake and the year, don't need all of the ages 

#try to match lakes and pull out trap/fyke observations 
match<-inner_join(bluegill_historical, fishc_data, by = c('new_key', 'year' = 'begin_date_year')) %>% 
  filter(type_gear1 == "trap" | type_gear2 == "trap" | type_gear3 == "trap" | type_gear4 == "trap" | 
           type_gear1 == "fyke" | type_gear2 == "fyke" | type_gear3 == "fyke" | type_gear4 == "fyke" ) %>% 
  filter(bluegill >0)

#look at card images to determine effort 
#write.csv(match, "/Users/katelynking/Desktop/fishc_bluegill.csv", row.names = FALSE)
cpue_hist<-read.csv("bluegill_data/fishc_bluegill2.csv") %>% 
  drop_na(cpue) %>% 
  filter(cpue >=1 & cpue <=100) %>%  #remove two outliers 
  mutate(new_key = ifelse(new_key == '1-Apr', '4-1', new_key)) %>%  #fix one new key that got changed to a date 
  select(new_key, year, bluegill, effort_num_lifts) %>%
  group_by(new_key, year) %>%
  mutate(effort_num_lifts = as.numeric(as.character(effort_num_lifts))) %>% 
  summarize_all(sum) %>% #sum across cards
  mutate(cpue=round((bluegill/effort_num_lifts),1)
  ) %>% 
  select(new_key, year, cpue)
  
n_distinct(cpue_hist$new_key) #24 distinct lakes with cpue data 

#### read in SNT CPUE data #### 
#catch data for bluegill 
snt_blg_catch <- read.csv("bluegill_data/snt_catch_data_mar2021.csv") %>% 
  tidyr::pivot_wider(id_cols = c(NEW_KEY, Survey_Number, SURVEY_YEAR, GEAR),
                     names_from = SPECIES,
                     values_from = c(FISH_COUNT),
                     values_fn = sum,
                     values_fill = list(FISH_COUNT = 0))%>%
  dplyr::rename(new_key=NEW_KEY) %>% 
  dplyr::select(new_key, Survey_Number, SURVEY_YEAR, GEAR, BLG)

#effort from snt
snt_effort<- read.csv("bluegill_data/snt_effort_data.csv") %>% 
  dplyr::select(NEW_KEY, Survey_Number, GEAR, EFFORT) %>% 
  rename(new_key = NEW_KEY)

#join catch and effort # chose only fyke and trap , effort in lifts 
snt_blg_cpue<-left_join(snt_blg_catch, snt_effort,  by=c('new_key','Survey_Number', 'GEAR')) %>% 
  filter(GEAR == "LMFYKE" | GEAR == "SMFYKE" | GEAR == "TRAPNET") %>% 
  group_by(new_key, Survey_Number, SURVEY_YEAR) %>% 
  select(-c('GEAR')) %>%
  summarize_all(sum) %>% #sum across all gears 
  mutate(cpue=round((BLG/EFFORT),1)
         ) %>% 
  select(new_key, Survey_Number, cpue)

n_distinct(snt_blg_cpue$new_key) #463 distinct lakes with cpue data (some have multiple surveys)
n_distinct(snt_blg_cpue$Survey_Number) 

#join snt data by the survey number and join with historical by year and lake
model_data_final<-left_join(model_data, snt_blg_cpue, by=c('new_key', 'Survey_Number')) %>% 
  natural_join(cpue_hist, by=c('new_key', 'year'),  jointype = "LEFT")


write.csv(model_data_final, "bluegill_data/model_data.csv", row.names = FALSE)

#### FINAL PREDICTOR VARIABLES 15 variables not including blg density 
#perch, lmb, pike, walleye, secchi_m,  urban, ag, forest, wetlands, year, doy, logcounty, logarea, logdepth, DD_mean

#number of unique lakes 
n_distinct(model_data$new_key) 

#summary stats 
skimr::skim(model_data)
  
