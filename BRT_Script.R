###
# 
# Elise Grabda
# R script for: Surveying Historical Growth Data: The Effects of Climate and Other Environmental Condition Factors on Bluegill Growth
# Dataset Creation, and BRT Analyses
###

library(tidyr)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggfortify)
library(data.table)
library(FSA)
library(car)
library(factoextra)
library(vegan)
library(psych)
library(cowplot)
library(LAGOSNE)
library(dismo)
library(gbm)
library(lubridate)
library(readr)
library(FSA)
install.packages("readxl")
library(readxl)

library(dismo)
library(gbm)
###Datasets
BLGrow <- read.csv("bluegill_data/all_grow_21NOV2023.csv") %>% 
  dplyr::filter(species == "bluegill")  %>%
  dplyr::select(-c(species))
IFR_Lake_Points_1_ <- read.csv("bluegill_data/IFR_lake_points.csv")
LAGOSjoinIFR <- read.csv("bluegill_data/lagos_join_ifr.csv")
lake_depth_2021 <- read.csv("bluegill_data/lake_depth_2021.csv")
lake_summary_qaqc <- read.csv("bluegill_data/lake_summary_qaqc.csv")
lake_summ_fish_pres_qaqc <-  read.csv("bluegill_data/lake_summ_fish_pres_qaqc.csv")
new_key_comid_translate <- read.csv("new_key_comid_translate.csv")
lake_degree_days_year <-  read.csv("bluegill_data/lake_degree_days_year.csv")
snt_blg_ages_2002_2020_1_ <- read.csv("bluegill_data/snt_blg_ages_2002_2020.csv")
sntlulc <- read.csv("bluegill_data/sntlulc.csv") 
snt_lagoslink <- read_excel("bluegill_data/snt_lagoslink.xls")
snt_secchi <- read.csv("bluegill_data/snt_secchi_2002_2020.csv")
snt_catch_data <- read.csv("bluegill_data/snt_catch_data_mar2021.csv") 

countypop_1940 <- read.csv("bluegill_data/Census_Data_County/R13037017_SL050.csv", 
                           skip = 1)
countypop_1950 <- read.csv("bluegill_data/Census_Data_County/R13037020_SL050.csv", 
                           skip = 1)
countypop_1960 <- read.csv("bluegill_data/Census_Data_County/R13037022_SL050.csv", 
                           skip = 1)
countypop_1970 <- read.csv("bluegill_data/Census_Data_County/R13037023_SL4601.csv", 
                           skip = 1)
countypop_1980 <- read.csv("bluegill_data/Census_Data_County/R13037024_SL11.csv", 
                           skip = 1)
countypop_1990 <- read.csv("bluegill_data/Census_Data_County/R13037025_SL050.csv", 
                           skip = 1)
countypop_2000 <- read.csv("bluegill_data/Census_Data_County/R13037026_SL050.csv", 
                           skip = 1)
countypop_2010 <- read.csv("bluegill_data/Census_Data_County/R13037027_SL050.csv",
                           skip = 1)
countypop_2020 <- read.csv("bluegill_data/Census_Data_County/R13037029_SL050.csv",
                           skip = 1)

#Plotting Function
PubPlot <- function (gbm.object, variable.no = 0, smooth = FALSE, rug = TRUE, 
                     n.plots = length(pred.names), common.scale = TRUE, write.title = TRUE, 
                     y.label = "fitted function", x.label = NULL, show.contrib = TRUE, 
                     plot.layout = c(3, 4), ...) 
{
  if (!requireNamespace("gbm")) {
    stop("you need to install the gbm package to run this function")
  }
  requireNamespace("splines")
  gbm.call <- gbm.object$gbm.call
  gbm.x <- gbm.call$gbm.x
  pred.names <- gbm.call$predictor.names
  response.name <- gbm.call$response.name
  data <- gbm.call$dataframe
  max.plots <- plot.layout[1] * plot.layout[2]
  plot.count <- 0
  n.pages <- 1
  if (length(variable.no) > 1) {
    stop("only one response variable can be plotted at a time")
  }
  if (variable.no > 0) {
    n.plots <- 1
  }
  max.vars <- length(gbm.object$contributions$var)
  if (n.plots > max.vars) {
    n.plots <- max.vars
    warning("reducing no of plotted predictors to maximum available (", 
            max.vars, ")")
  }
  predictors <- list(rep(NA, n.plots))
  responses <- list(rep(NA, n.plots))
  for (j in c(1:n.plots)) {
    if (n.plots == 1) {
      k <- variable.no
    }
    else {
      k <- match(gbm.object$contributions$var[j], pred.names)
    }
    if (is.null(x.label)) {
      var.name <- gbm.call$predictor.names[k]
    }
    else {
      var.name <- x.label
    }
    pred.data <- data[, gbm.call$gbm.x[k]]
    response.matrix <- gbm::plot.gbm(gbm.object, k, return.grid = TRUE)
    predictors[[j]] <- response.matrix[, 1]
    if (is.factor(data[, gbm.call$gbm.x[k]])) {
      predictors[[j]] <- factor(predictors[[j]], levels = levels(data[, 
                                                                      gbm.call$gbm.x[k]]))
    }
    responses[[j]] <- response.matrix[, 2] - mean(response.matrix[, 
                                                                  2])
    if (j == 1) {
      ymin = min(responses[[j]])
      ymax = max(responses[[j]])
    }
    else {
      ymin = min(ymin, min(responses[[j]]))
      ymax = max(ymax, max(responses[[j]]))
    }
  }
  op <- graphics::par(no.readonly = TRUE)
  for (j in c(1:n.plots)) {
    if (plot.count == max.plots) {
      plot.count = 0
      n.pages <- n.pages + 1
    }
    plot.count <- plot.count + 1
    if (n.plots == 1) {
      k <- match(pred.names[variable.no], gbm.object$contributions$var)
      if (show.contrib) {
        x.label <- paste(var.name, "  (", round(gbm.object$contributions[k, 
                                                                         2], 1), "%)", sep = "")
      }
    }
    else {
      k <- match(gbm.object$contributions$var[j], pred.names)
      var.name <- gbm.call$predictor.names[k]
      if (show.contrib) {
        x.label <- paste(var.name, "  (", round(gbm.object$contributions[j, 
                                                                         2], 1), "%)", sep = "")
      }
      else x.label <- var.name
    }
    if (common.scale) {
      plot(predictors[[j]], responses[[j]], ylim = c(ymin, 
                                                     ymax), type = "l", xlab = x.label, ylab = y.label, 
           ...)
    }
    else {
      plot(predictors[[j]], responses[[j]], type = "l", 
           xlab = x.label, ylab = y.label, ...)
    }
    if (smooth & is.vector(predictors[[j]])) {
      temp.lo <- loess(responses[[j]] ~ predictors[[j]], 
                       span = 0.3)
      lines(predictors[[j]], fitted(temp.lo), lty = 2, 
            col = 2)
    }
    if (plot.count == 1 & n.plots == 1) {
      if (write.title) {
        title("")
      }
      if (rug & is.vector(data[, gbm.call$gbm.x[variable.no]])) {
        rug(quantile(data[, gbm.call$gbm.x[variable.no]], 
                     probs = seq(0, 1, 0.1), na.rm = TRUE), col = "Blue")
      }
    }
    else {
      if (write.title & j == 1) {
        title(response.name)
      }
      if (rug & is.vector(data[, gbm.call$gbm.x[k]])) {
        rug(quantile(data[, gbm.call$gbm.x[k]], probs = seq(0, 
                                                            1, 0.1), na.rm = TRUE))
      }
    }
  }
  
  
}



###BEGIN


setwd("C:/Thesis/Growth")



#NA Management
BLG_dropNA <- BLGrow %>%
  drop_na(c(new_key, begin_date_year, fish_count, length_mean_mm, length_max_mm, begin_date_month))

#Setting up Left Join for georeferencing coordinates

IFRLakes <- IFR_Lake_Points_1_ %>%
  rename(
    new_key = NEW_KEY
  )

LeftJoin1 <- merge(x = BLG_dropNA, y = IFRLakes[,c(2,8,9)], by = "new_key", all.x = T)
BLG.trim <- LeftJoin1[,c(1,2,3,4,5,6,9,10,11,12,19,20)]



#sent to ARCGISPro to Spatially Associate points with their lagosID
#Returns Data w/ associated lagoslakeID:


BLGtrim.LAGOS <- LAGOSjoinIFR[,c(3:15,19,20)]



BLGtrim.LAGOS <- BLGtrim.LAGOS %>%
  rename(
    lagoslakeid = lagoslakei
  )
###LULC Management
setwd("C:/Thesis/Growth/GIS/USGS_rasters/Clipped_Rasters/AnalysisTables")

#For Loop to Rename LULC variables, and Generate sums
file.list <- list.files("bluegill_data/backcast_lulc", pattern='*.xls', full.names = T) 
df.list <- lapply(file.list, read_excel)
names(df.list[[1]])
df.list[1]
gunkle <- c(1938:1992)

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
  df.list[[i]]$year <- gunkle[i]
 
  
}



### legacy parameter

ja <- bind_rows(df.list)

#Attaching Environmental Factors
setwd("C:/Thesis/Growth")

lake_depth_2021$New_Key <- as.factor(lake_depth_2021$New_Key)
BLGtrim.LAGOS$year <- BLGtrim.LAGOS$begin_date_year
ja$LAGOSLAKEI <- as.factor(ja$LAGOSLAKEI)

BLG.lulc <- merge(x = BLGtrim.LAGOS, y = ja[,2:18],
                  by.x = c("lagoslakeid", "year"),
                  by.y = c("LAGOSLAKEI", "year"),
                  all.x = T)

BLG.lulc <- BLG.lulc[complete.cases(BLG.lulc),]


BLG.lake <-  merge(BLG.lulc, lake_depth_2021[,c(4,5,7)],
                   by.x = "new_key",
                   by.y = "New_Key",
                   all.x = T)
BLG.lake <- merge(x = BLG.lake, y = lake_summary_qaqc[,c(2,7,15)],
                  by.x = c("new_key", "year"),
                  by.y = c("new_key", "begin_date_year"),
                  all.x = T)
#fISH PRESENCE iNDICATORs

fish.group <- lake_summ_fish_pres_qaqc %>% group_by(new_key, begin_date_year)

fish.groupyear <- dplyr::mutate(fish.group, lakeyear = cur_group_id())
fish.groupyear$new_key <- as.factor(fish.groupyear$new_key)

sardine <- fish.groupyear[,-c(13:18,20:23,25:46)]
sardine <- sardine[complete.cases(sardine),]


peek <- merge(x = BLG.lake, y = sardine[,c(2,7,11:14)],
              by.x = c("new_key", "year"),
              by.y = c("new_key", "begin_date_year"),
              all.x = T)

aboo <- unique(peek)

aboo <- aboo[,-c(6,9)]

###Attaching Temperature Data

lake_degree_days_year$COMID <- lake_degree_days_year$IHDLKID

DD_testy <- merge(x = lake_degree_days_year, y = new_key_comid_translate,
                  by.x = "COMID",
                  by.y = "COMID",
                  all.x = T)
DD_testy2 <- DD_testy %>%
  drop_na(NEW_KEY)

dd_names <- names(DD_testy2)[8:131]

DD_testy3 <- melt(DD_testy2,
                  measure.vars = dd_names)
DD_testy3$variable <- gsub("DD_","",as.character(DD_testy3$variable))

DD_testy3$variable <- as.numeric(DD_testy3$variable)
DD_testy3$NEW_KEY <- as.factor(DD_testy3$NEW_KEY)

DD_lulc <- merge(x = aboo, y = DD_testy3[,c(15,26,27)],
                 by.x = c("new_key", "year"),
                 by.y = c("NEW_KEY", "variable"),
                 all.x = T)
DD_lulc <- unique(DD_lulc)


DD_lulc$Urban <- DD_lulc$Developed +DD_lulc$Mining+DD_lulc$Barren
DD_lulc$Agriculture <- DD_lulc$AgLand+DD_lulc$Pasture +DD_lulc$Grassland
DD_lulc$Forests <- DD_lulc$Deciduous+DD_lulc$Mixed+DD_lulc$Evergreen+DD_lulc$Shrubland
DD_lulc$Wetlands <- DD_lulc$WoodWetland +DD_lulc$HerbWetland +DD_lulc$Open_Water+DD_lulc$VALUE_0
DD_lulc$sum <- DD_lulc$Forests+DD_lulc$Wetlands+DD_lulc$Urban+DD_lulc$Agriculture
DD_lulc[,c(38:41)] <- DD_lulc[,c(38:41)]/DD_lulc$sum
DD_lulc <- DD_lulc[,-c(5,7,11,15:29)]

###IFR SNT DATA
snt_blg <- as.data.frame(snt_blg_ages_2002_2020_1_) %>% 
  tidyr::separate(Collection_Date, 
               into = c('month', 'day', 'year'), 
                remove = FALSE) %>% 
  mutate(year = as.integer(paste0("20", year)), 
         day = as.numeric(day), 
         month = as.numeric(month) 
         )%>% 
  filter(year<2066)

#below code didnt work so I did the above 
snt_blg$year <- as.numeric(substr(snt_blg$Collection_Date,1,4))
snt_blg$month <- as.numeric(substr(snt_blg$Collection_Date,6,7))
snt_blg$day <- as.numeric(substr(snt_blg$Collection_Date,9,10))

snt_blg <- subset(snt_blg, year > 1990)


snt_Zoon.format <- snt_blg %>%
  group_by(New_Key,year,day,month,AGE,Survey_Number) %>%
  summarise(mean_length_in = mean(LENGTH_IN))


snt_Zoon.format$length_mean_mm <- snt_Zoon.format$mean_length_in*25.4

snt.coor <- merge(snt_Zoon.format, IFRLakes[,c(2,8,9)],
                    by.x = "New_Key",
                    by.y = "new_key",
                    all.x = T)
snt.format <- merge(snt.coor, snt_lagoslink[,c(10,45,53)],
                    by.x = "New_Key",
                    by.y = "New_Key",
                    all.x = T)


nlcdyears <- c(2001,2006,2011,2016,2019)
a <- snt.format$year
b <- nlcdyears


snt.format$lulcyear <- sapply(a, function(a, b) {b[which.min(abs(a-b))]}, b)

snt.lulc <- merge(snt.format, sntlulc[,3:22],
                  by.x = c("lagoslakei","lulcyear"),
                  by.y = c("lagoslakei", "Year"),
                  all.x=T)

snt.depth <- merge(snt.lulc, lake_depth_2021[,c(4,5,7)],
                   by.x = "New_Key",
                   by.y = "New_Key",
                   all.x = T)

snt.secchi <- merge(snt.depth, snt_secchi,
                by.x = c("New_Key","year"),
                by.y = c("New_Key", "Year"),
                all.x = T)

snt.dd <- merge(snt.secchi, DD_testy3[,c(15,26,27)],
                by.x = c("New_Key","year"),
                by.y = c("NEW_KEY", "variable"),
                all.x = T)





species.tab <- dcast(snt_catch_data, NEW_KEY+SURVEY_YEAR ~ SPECIES)

snt.species <- merge(snt.dd,species.tab,
                     by.x = c("New_Key", "year"),
                     by.y = c("NEW_KEY","SURVEY_YEAR"),
                     
                     
                     all.x = T)


snt.species2 <- snt.species[,-c(4,9,37:39,42)]

snt.species2 <- snt.species2 %>%
  rename(new_key=New_Key,
         age_group=AGE,
         VALUE_0 = Unclassified,
         lmb=LMB,
         pike=NOP,
         perch=YEP,
         walleye=WAE,
         WoodWetland=Woody_Wetlands,
         HerbWetland=Emergent_Herbaceous_Wetlands,
         Barren=Barren_Land,
         Evergreen=Evergreen_Forest,
         Mixed=Mixed_Forest,
         Deciduous=Deciduous_Forest,
         Shrubland=Shrub_Scrub,
         AgLand=Cultivated_Crops,
         Pasture=Hay_Pasture,
         Grassland=Herbaceous,
         lagosID=lagoslakei,
         SurveyID=Survey_Number.x,
         secchi_m=SECCHI_FT
         )



snt.species2$Forests <- snt.species2$Deciduous+snt.species2$Mixed+snt.species2$Evergreen+snt.species2$Shrubland
snt.species2$Wetlands <- snt.species2$WoodWetland +snt.species2$HerbWetland+snt.species2$Open_Water+snt.species2$VALUE_0
snt.species2$Urban <- snt.species2$Developed
snt.species2$Agriculture <- snt.species2$AgLand+snt.species2$Pasture+snt.species2$Grassland

snt.species2$secchi_m <- snt.species2$secchi_m/3.281



for (i in 1:5624) {
  snt.species2$sum[i] <- sum(snt.species2[i,c(39:42)])
  
}
snt.species2[,c(39:42)] <- snt.species2[,c(39:42)]/ snt.species2$sum



snt.species2 <- snt.species2[,-c(12:29,32,43)]

DD_lulc <- DD_lulc %>%
  rename(lagosID=lagoslakeid,
         SurveyID=subject_id,
         month=begin_date_month,
         day=begin_date_day,
         lmb=largemouth_bass,
         pike=northern_pike,
         perch=yellow_perch,
         secchi_m=secchi_min_m
         )
DD_lulc$month <- match(DD_lulc$month,month.name)

binded <- rbind(DD_lulc,snt.species2)


######
#Linking avg. degree days

DD_testy4 <- DD_testy3 %>% group_by(NEW_KEY,variable) %>% summarise(DD = mean(value))


binded[,24] <- 0 

for (i in 1: 14042) {
  
  binded[i,24] <- sum(DD_testy4[DD_testy4[,1]==binded[i,1] & DD_testy4[,2] < binded[i,2] & DD_testy4[,2] >= (binded[i,2]-binded[i,5]) ,3]) / binded[i,5]
  
}

binded["V24"][binded["V24"]==0] <- NA
names(binded)[24] <- "AvgCoDD"






#### county populations
setwd("C:/Thesis/Growth")
binded$decade <- round(binded$year/10)*10


countypop_1940$Geo_QName <- gsub(", Michigan","",as.character(countypop_1940$Geo_QName))
countypop_1950$Geo_QName <- gsub(", Michigan","",as.character(countypop_1950$Geo_QName))
countypop_1960$Geo_qname <- gsub(", Michigan","",as.character(countypop_1960$Geo_qname))
countypop_1970$Geo_QName <- gsub(", Michigan","",as.character(countypop_1970$Geo_QName))
countypop_1980$Geo_QName <- gsub(", Michigan","",as.character(countypop_1980$Geo_QName))
countypop_1990$Geo_QName <- gsub(", Michigan","",as.character(countypop_1990$Geo_QName))
countypop_2000$Geo_QName <- gsub(", Michigan","",as.character(countypop_2000$Geo_QName))
countypop_2010$Geo_QName <- gsub(", Michigan","",as.character(countypop_2010$Geo_QName))
countypop_2020$Geo_QNAME <- gsub(", Michigan","",as.character(countypop_2020$Geo_QNAME))





pop40 <- merge(x= binded, y= countypop_1940[,c(2,7,8)],
              by.x = c("COUNTY_Nam", "decade"),
              by.y = c("Geo_QName", "X1940"),
              all.x = T)
pop50 <- merge(x= pop40, y= countypop_1950[,c(2,7,8)],
              by.x = c("COUNTY_Nam", "decade"),
              by.y = c("Geo_QName", "X1950"),
              all.x = T)
pop60 <- merge(x= pop50, y= countypop_1960[,c(2,11,12)],
              by.x = c("COUNTY_Nam", "decade"),
              by.y = c("Geo_qname", "X1960"),
              all.x = T)

pop60$cpop1 <- coalesce(pop60[,26],pop60[,27],pop60[,28])
pop60 <- pop60[,-c(26:28)]

pop70 <- merge(x= pop60, y= countypop_1970[,c(3,9,10)],
              by.x = c("COUNTY_Nam", "decade"),
              by.y = c("Geo_QName", "X1970"),
              all.x = T)
pop80 <- merge(x= pop70, y= countypop_1980[,c(2,9,10)],
              by.x = c("COUNTY_Nam", "decade"),
              by.y = c("Geo_QName", "X1980"),
              all.x = T)
pop90 <- merge(x= pop80, y= countypop_1990[,c(2,10,11)],
              by.x = c("COUNTY_Nam", "decade"),
              by.y = c("Geo_QName", "X1990"),
              all.x = T)

pop90$cpop2 <- coalesce(pop90[,27],pop90[,28],pop90[,29])
pop90 <- pop90[,-c(27:29)]

pop00 <- merge(x= pop90, y= countypop_2000[,c(2,12,13)],
              by.x = c("COUNTY_Nam", "decade"),
              by.y = c("Geo_QName", "X2000"),
              all.x = T)
pop10 <- merge(x= pop00, y= countypop_2010[,c(2,12,13)],
              by.x = c("COUNTY_Nam", "decade"),
              by.y = c("Geo_QName", "X2010"),
              all.x = T)
pop20 <- merge(x= pop10, y= countypop_2020[,c(3,11,14)],
              by.x = c("COUNTY_Nam", "decade"),
              by.y = c("Geo_QNAME", "X2020"),
              all.x = T)

pop20$countypop <- coalesce(pop20[,28],pop20[,29],pop20[,30])

pop20 <- pop20[,-c(28:30)]
pop20$CountyPopulation <- coalesce(pop20[,26],pop20[,27],pop20[,28])
binded <- pop20[,-c(26:28)]


#log transformation of factors
binded$logdepth <- log(binded$MeanDepth_ft)
binded$logarea <- log(binded$Area_ac)
binded$logcounty <- log(binded$CountyPopulation)


binded <- subset(binded, year >= 1950)

binded$Date <- paste(binded$year,binded$month,binded$day,sep = "-")
binded$DoY <- yday(binded$Date)

##BRT MODELS



#Comprehensive

###AGE1
xx.list <- c(4,15:19,21:25,27:29,31)


brt.a1 = gbm.step(data = subset(binded, age_group == 1), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)
summary(brt.a1)
gbm.plot(brt.a1) #partial dependence plots 
brt.int1=gbm.interactions(brt.a1)
brt.int1$interactions
brt.int1$rank.list
brt.simp1 = gbm.simplify(brt.a1)

if (min(brt.simp1$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp1$deviance.summary$mean,index.return=TRUE) 
  xlist_simp1=brt.simp1$pred.list[[droplist$ix[1]]] 
} else {xlist_simp1=xx.list}

brt.a1simp = gbm.step(data = subset(binded, age_group == 1), gbm.x = xlist_simp1, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)


ni1=nrow(subset(binded, age_group == 1))
predset1=sample(1:ni1,100) 
predbrt1=predict(brt.a1, 
                 subset(binded, age_group == 1)[predset1,xx.list], 
                 n.trees=brt.a1$gbm.call$best.trees, 
                 type="response") 
plot(predbrt1, subset(binded, age_group == 1)[predset1,8]) # i think predicted lengths vs observed 

summary(lm(subset(binded, age_group == 1)[predset1,8] ~ predbrt1))

####
#### Age 2
brt.a2 = gbm.step(data = subset(binded, age_group == 2), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int2=gbm.interactions(brt.a2)
brt.simp2 = gbm.simplify(brt.a2)

if (min(brt.simp2$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp2$deviance.summary$mean,index.return=TRUE) 
  xlist_simp2=brt.simp2$pred.list[[droplist$ix[1]]] 
} else {xlist_simp2=xx.list}

brt.a2simp = gbm.step(data = subset(binded, age_group == 2), gbm.x = xlist_simp2, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)


ni2=nrow(subset(binded, age_group == 2))
predset2=sample(1:ni2,100) 
predbrt2=predict(brt.a2simp,subset(binded, age_group == 2)[predset2,xlist_simp2],n.trees=brt.a2simp$gbm.call$best.trees, type="response") 
plot(predbrt2,subset(binded, age_group == 2)[predset2,8])

summary(lm(subset(binded, age_group == 2)[predset2,8] ~ predbrt2))
####


### Age3
brt.a3 = gbm.step(data = subset(binded, age_group == 3), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int3 = gbm.interactions(brt.a3)
brt.simp3 = gbm.simplify(brt.a3)

if (min(brt.simp3$deviance.summary$mean)<0) { 
  droplist = sort(brt.simp3$deviance.summary$mean,index.return=TRUE) 
  xlist_simp3 = brt.simp3$pred.list[[droplist$ix[1]]] 
} else {xlist_simp3=xx.list}

brt.a3simp = gbm.step(data = subset(binded, age_group == 3), gbm.x = xlist_simp3, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

summary(brt.a3simp)

ni3=nrow(subset(binded, age_group == 3))
predset3=sample(1:ni3,100) 
predbrt3=predict(brt.a3simp,subset(binded, age_group == 3)[predset3,xlist_simp3],n.trees=brt.a3simp$gbm.call$best.trees, type="response") 
plot(predbrt3,subset(binded, age_group == 3)[predset3,8])

summary(lm(subset(binded, age_group == 3)[predset3,8] ~ predbrt3))
###


### Age 4

brt.a4 = gbm.step(data = subset(binded, age_group == 4), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int4=gbm.interactions(brt.a4)
brt.simp4 = gbm.simplify(brt.a4)

if (min(brt.simp4$deviance.summary$mean)<0) { #if dropping any predictor improves model fit
  droplist=sort(brt.simp4$deviance.summary$mean,index.return=TRUE) #list the number of drops (used to find the best number)
  xlist_simp4=brt.simp4$pred.list[[droplist$ix[1]]] #returns the list of predictors after dropping the least important, which
  #can be used to fit a new model
} else {xlist_simp4=xx.list}

brt.a4simp = gbm.step(data = subset(binded, age_group == 4), gbm.x = xlist_simp4, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)



ni4=nrow(subset(binded, age_group == 4)) #number of rows in the data
predset4=sample(1:ni4,100) #random draw of 100 lakes
predbrt4=predict(brt.a4simp,subset(binded, age_group == 4)[predset4,xlist_simp4],n.trees=brt.a4simp$gbm.call$best.trees, type="response") #predictions from the model to the 100 lakes
plot(predbrt4,subset(binded, age_group == 4)[predset4,8])

summary(lm(subset(binded, age_group == 4)[predset4,8] ~ predbrt4))



###

### Age 5
brt.a5 = gbm.step(data = subset(binded, age_group == 5), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int5=gbm.interactions(brt.a5)
brt.simp5 = gbm.simplify(brt.a5)

if (min(brt.simp5$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp5$deviance.summary$mean,index.return=TRUE) 
  xlist_simp5=brt.simp5$pred.list[[droplist$ix[1]]] 
} else {xlist_simp5=xx.list}

brt.a5simp = gbm.step(data = subset(binded, age_group == 5), gbm.x = xlist_simp5, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)




ni5=nrow(subset(binded, age_group == 5))
predset5=sample(1:ni5,100) 
predbrt5=predict(brt.a5simp,subset(binded, age_group == 5)[predset5,xlist_simp5],n.trees=brt.a5simp$gbm.call$best.trees, type="response") 
plot(predbrt5,subset(binded, age_group == 5)[predset5,8])

summary(lm(subset(binded, age_group == 5)[predset5,8] ~ predbrt5))
###

### Age 6
brt.a6 = gbm.step(data = subset(binded, age_group == 6), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int6=gbm.interactions(brt.a6)
brt.simp6 = gbm.simplify(brt.a6)

if (min(brt.simp6$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp6$deviance.summary$mean,index.return=TRUE) 
  xlist_simp6=brt.simp6$pred.list[[droplist$ix[1]]] 
} else {xlist_simp6=xx.list}

brt.a6simp = gbm.step(data = subset(binded, age_group == 6), gbm.x = xlist_simp6, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)



ni6=nrow(subset(binded, age_group == 6))
predset6=sample(1:ni6,100) 
predbrt6=predict(brt.a6simp,subset(binded, age_group == 6)[predset6,xlist_simp6],n.trees=brt.a6simp$gbm.call$best.trees, type="response") 
plot(predbrt6,subset(binded, age_group == 6)[predset6,8])

summary(lm(subset(binded, age_group == 6)[predset6,8] ~ predbrt6))
###

### Age 7
brt.a7 = gbm.step(data = subset(binded, age_group == 7), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int7=gbm.interactions(brt.a7)
brt.simp7 = gbm.simplify(brt.a7)

if (min(brt.simp7$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp7$deviance.summary$mean,index.return=TRUE) 
  xlist_simp7=brt.simp7$pred.list[[droplist$ix[1]]] 
} else {xlist_simp7=xx.list}

brt.a7simp = gbm.step(data = subset(binded, age_group == 7), gbm.x = xlist_simp7, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)



ni7=nrow(subset(binded, age_group == 7))
predset7=sample(1:ni7,100) 
predbrt7=predict(brt.a7simp,subset(binded, age_group == 7)[predset7,xlist_simp7],n.trees=brt.a7simp$gbm.call$best.trees, type="response") 
plot(predbrt7,subset(binded, age_group == 7)[predset7,8])

summary(lm(subset(binded, age_group == 7)[predset7,8] ~ predbrt7))
###

### Age 8
brt.a8 = gbm.step(data = subset(binded, age_group == 8), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int8=gbm.interactions(brt.a8)
brt.simp8 = gbm.simplify(brt.a8)

if (min(brt.simp1$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp8$deviance.summary$mean,index.return=TRUE) 
  xlist_simp8=brt.simp8$pred.list[[droplist$ix[1]]] 
} else {xlist_simp8 = xx.list}

brt.a8simp = gbm.step(data = subset(binded, age_group == 8), gbm.x = xlist_simp8, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)



ni8=nrow(subset(binded, age_group == 8))
predset8=sample(1:ni8,100) 
predbrt8=predict(brt.a8simp,subset(binded, age_group == 8)[predset8,xlist_simp8],n.trees=brt.a8simp$gbm.call$best.trees, type="response") 
plot(predbrt8,subset(binded, age_group == 8)[predset8,8])

summary(lm(subset(binded, age_group == 8)[predset8,8] ~ predbrt8))

##Fewer Factors
#year, DD, depth, DoY  
######################################
###AGE1
xx.list <- c(4,25,27,31)


brt.a1dd = gbm.step(data = subset(binded, age_group == 1), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                    n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)
####
#### Age 2
brt.a2dd = gbm.step(data = subset(binded, age_group == 2), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                    n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)
####


### Age3
brt.a3dd = gbm.step(data = subset(binded, age_group == 3), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                    n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)
###


### Age 4

brt.a4dd = gbm.step(data = subset(binded, age_group == 4), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                    n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)

###

### Age 5
brt.a5dd = gbm.step(data = subset(binded, age_group == 5), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                    n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)
###

### Age 6
brt.a6dd = gbm.step(data = subset(binded, age_group == 6), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                    n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)
###

### Age 7
brt.a7dd = gbm.step(data = subset(binded, age_group == 7), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                    n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)

###

### Age 8
brt.a8dd = gbm.step(data = subset(binded, age_group == 8), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                    n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)



######################################
#year, depth, DoY 
###AGE1
xx.list <-c(4,27,31)
brt.a1c = gbm.step(data = subset(binded, age_group == 1), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                   n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)


####
#### Age 2
brt.a2c = gbm.step(data = subset(binded, age_group == 2), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                   n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)


####


### Age3
brt.a3c = gbm.step(data = subset(binded, age_group == 3), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                   n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)


###

### Age 4

brt.a4c = gbm.step(data = subset(binded, age_group == 4), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                   n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)


###

### Age 5
brt.a5c = gbm.step(data = subset(binded, age_group == 5), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                   n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)


###

### Age 6
brt.a6c = gbm.step(data = subset(binded, age_group == 6), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                   n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)


###

### Age 7
brt.a7c = gbm.step(data = subset(binded, age_group == 7), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                   n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)



### Age 8
brt.a8c = gbm.step(data = subset(binded, age_group == 8), gbm.x = xx.list, gbm.y = 8, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                   n.folds = 10, n.trees = 50, step.size = 10, max.trees = 1000000, tolerance = 0.001)

###
#Model Comparisons
Age_Group <- rep(1:8,3)
PR2 <- c(
  1-(brt.a1simp$self.statistics$mean.resid/brt.a1simp$self.statistics$mean.null),
  1-(brt.a2simp$self.statistics$mean.resid/brt.a2simp$self.statistics$mean.null),
  1-(brt.a3simp$self.statistics$mean.resid/brt.a3simp$self.statistics$mean.null),
  1-(brt.a4simp$self.statistics$mean.resid/brt.a4simp$self.statistics$mean.null),
  1-(brt.a5simp$self.statistics$mean.resid/brt.a5simp$self.statistics$mean.null),
  1-(brt.a6simp$self.statistics$mean.resid/brt.a6simp$self.statistics$mean.null),
  1-(brt.a7simp$self.statistics$mean.resid/brt.a7simp$self.statistics$mean.null),
  1-(brt.a8simp$self.statistics$mean.resid/brt.a8simp$self.statistics$mean.null)
)

PR3 <- c(
  1-(brt.a1c$self.statistics$mean.resid/brt.a1c$self.statistics$mean.null),
  1-(brt.a2c$self.statistics$mean.resid/brt.a2c$self.statistics$mean.null),
  1-(brt.a3c$self.statistics$mean.resid/brt.a3c$self.statistics$mean.null),
  1-(brt.a4c$self.statistics$mean.resid/brt.a4c$self.statistics$mean.null),
  1-(brt.a5c$self.statistics$mean.resid/brt.a5c$self.statistics$mean.null),
  1-(brt.a6c$self.statistics$mean.resid/brt.a6c$self.statistics$mean.null),
  1-(brt.a7c$self.statistics$mean.resid/brt.a7c$self.statistics$mean.null),
  1-(brt.a8c$self.statistics$mean.resid/brt.a8c$self.statistics$mean.null)
)

PR2DD <- c(
  1-(brt.a1dd$self.statistics$mean.resid/brt.a1dd$self.statistics$mean.null),
  1-(brt.a2dd$self.statistics$mean.resid/brt.a2dd$self.statistics$mean.null),
  1-(brt.a3dd$self.statistics$mean.resid/brt.a3dd$self.statistics$mean.null),
  1-(brt.a4dd$self.statistics$mean.resid/brt.a4dd$self.statistics$mean.null),
  1-(brt.a5dd$self.statistics$mean.resid/brt.a5dd$self.statistics$mean.null),
  1-(brt.a6dd$self.statistics$mean.resid/brt.a6dd$self.statistics$mean.null),
  1-(brt.a7dd$self.statistics$mean.resid/brt.a7dd$self.statistics$mean.null),
  1-(brt.a8dd$self.statistics$mean.resid/brt.a8dd$self.statistics$mean.null)
)

Performance <- c(PR2,PR2DD,PR3)

ModN <- as.factor(c(3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2))

Model.comp <- cbind(Age_Group,Performance,ModN)


Model.comp <- as.data.frame(Model.comp)
Model.comp[,3] <- as.factor(Model.comp[,3])
Model.comp %>%
  ggplot(aes(x= Age_Group, y= Performance, shape = ModN))+
  xlab("Age Group")+
  ylab("Deviance Explained")+
  ggtitle("BRT Model Explanatory Ability Comparison")+
  geom_point()+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_fill_manual( name = "Model Number",values = ModN)

###
#Plots
#Regression Plots
##

subset(binded, age_group <= 8 & age_group > 0) %>%
  ggplot(aes(x=year,y=length_mean_mm))+
  geom_point(shape = 3, size = 0.01)+
  geom_smooth(method=lm, formula = y~x)+
  facet_wrap(~age_group, nrow = 4, ncol = 2)+
  xlab("Year")+
  ylab("Mean Length (mm)")+
  ggtitle("Changes in Body Size by Age Class Over Time")+
  theme_update(plot.title = element_text(hjust = 0.5))+
  theme_pubr()

#Relative Influence Plots
###
par(mar=c(4,8,2,2),las=2)
a1.rel.inf <- as.data.frame(cbind(summary.gbm(brt.a1simp,plotit=F)$var,summary.gbm(brt.a1simp,plotit=F)$rel.inf))
names(a1.rel.inf)[1] <- "Factors"
names(a1.rel.inf)[2] <- "RelativeInfluence"

a1.rel.inf$Factors <- as.factor(a1.rel.inf$Factors)
a1.rel.inf$RelativeInfluence <- as.numeric(a1.rel.inf$RelativeInfluence)
a1.rel.inf <- a1.rel.inf[order(a1.rel.inf$RelativeInfluence),]

a1plot <- a1.rel.inf %>%
  ggplot(aes(y=RelativeInfluence, x= reorder(Factors,RelativeInfluence)))+
  geom_point(stat = "identity",size=2)+
  coord_flip( ylim = c(0, 60))+
  xlab("Factors")+
  ylab("Relative Influence")+
  theme_bw()

a2.rel.inf <- as.data.frame(cbind(summary.gbm(brt.a2simp,plotit=F)$var,summary.gbm(brt.a2simp,plotit=F)$rel.inf))
names(a2.rel.inf)[1] <- "Factors"
names(a2.rel.inf)[2] <- "RelativeInfluence"

a2.rel.inf$Factors <- as.factor(a2.rel.inf$Factors)
a2.rel.inf$RelativeInfluence <- as.numeric(a2.rel.inf$RelativeInfluence)
a2.rel.inf <- a2.rel.inf[order(a2.rel.inf$RelativeInfluence),]

a2plot <- a2.rel.inf %>%
  ggplot(aes(y=RelativeInfluence, x= reorder(Factors,RelativeInfluence)))+
  geom_point(stat = "identity",size=2)+
  coord_flip( ylim = c(0, 60))+
  xlab("Factors")+
  ylab("Relative Influence")+
  theme_bw()


a3.rel.inf <- as.data.frame(cbind(summary.gbm(brt.a3simp,plotit=F)$var,summary.gbm(brt.a3simp,plotit=F)$rel.inf))
names(a3.rel.inf)[1] <- "Factors"
names(a3.rel.inf)[2] <- "RelativeInfluence"

a3.rel.inf$Factors <- as.factor(a3.rel.inf$Factors)
a3.rel.inf$RelativeInfluence <- as.numeric(a3.rel.inf$RelativeInfluence)
a3.rel.inf <- a3.rel.inf[order(a3.rel.inf$RelativeInfluence),]

a3plot <- a3.rel.inf %>%
  ggplot(aes(y=RelativeInfluence, x= reorder(Factors,RelativeInfluence)))+
  geom_point(stat = "identity",size=2)+
  coord_flip( ylim = c(0, 60))+
  xlab("Factors")+
  ylab("Relative Influence")+
  theme_bw()

a4.rel.inf <- as.data.frame(cbind(summary.gbm(brt.a4simp,plotit=F)$var,summary.gbm(brt.a4simp,plotit=F)$rel.inf))
names(a4.rel.inf)[1] <- "Factors"
names(a4.rel.inf)[2] <- "RelativeInfluence"
a4.rel.inf$Factors <- as.factor(a4.rel.inf$Factors)
a4.rel.inf$RelativeInfluence <- as.numeric(a4.rel.inf$RelativeInfluence)
a4.rel.inf <- a4.rel.inf[order(a4.rel.inf$RelativeInfluence),]

a4plot <- a4.rel.inf %>%
  ggplot(aes(y=RelativeInfluence, x= reorder(Factors,RelativeInfluence)))+
  geom_point(stat = "identity",size=2)+
  coord_flip( ylim = c(0, 60))+
  xlab("Factors")+
  ylab("Relative Influence")+
  theme_bw()

a5.rel.inf <- as.data.frame(cbind(summary.gbm(brt.a5simp,plotit=F)$var,summary.gbm(brt.a5simp,plotit=F)$rel.inf))
names(a5.rel.inf)[1] <- "Factors"
names(a5.rel.inf)[2] <- "RelativeInfluence"

a5.rel.inf$Factors <- as.factor(a5.rel.inf$Factors)
a5.rel.inf$RelativeInfluence <- as.numeric(a5.rel.inf$RelativeInfluence)
a5.rel.inf <- a5.rel.inf[order(a5.rel.inf$RelativeInfluence),]

a5plot <- a5.rel.inf %>%
  ggplot(aes(y=RelativeInfluence, x= reorder(Factors,RelativeInfluence)))+
  geom_point(stat = "identity",size=2)+
  coord_flip( ylim = c(0, 60))+
  xlab("Factors")+
  ylab("Relative Influence")+
  theme_bw()

a6.rel.inf <- as.data.frame(cbind(summary.gbm(brt.a6simp,plotit=F)$var,summary.gbm(brt.a6simp,plotit=F)$rel.inf))
names(a6.rel.inf)[1] <- "Factors"
names(a6.rel.inf)[2] <- "RelativeInfluence"

a6.rel.inf$Factors <- as.factor(a6.rel.inf$Factors)
a6.rel.inf$RelativeInfluence <- as.numeric(a6.rel.inf$RelativeInfluence)
a6.rel.inf <- a6.rel.inf[order(a6.rel.inf$RelativeInfluence),]

a6plot <- a6.rel.inf %>%
  ggplot(aes(y=RelativeInfluence, x= reorder(Factors,RelativeInfluence)))+
  geom_point(stat = "identity",size=2)+
  coord_flip( ylim = c(0, 60))+
  xlab("Factors")+
  ylab("Relative Influence")+
  theme_bw()

a7.rel.inf <- as.data.frame(cbind(summary.gbm(brt.a7simp,plotit=F)$var,summary.gbm(brt.a7simp,plotit=F)$rel.inf))
names(a7.rel.inf)[1] <- "Factors"
names(a7.rel.inf)[2] <- "RelativeInfluence"

a7.rel.inf$Factors <- as.factor(a7.rel.inf$Factors)
a7.rel.inf$RelativeInfluence <- as.numeric(a7.rel.inf$RelativeInfluence)
a7.rel.inf <- a7.rel.inf[order(a7.rel.inf$RelativeInfluence),]

a7plot <- a7.rel.inf %>%
  ggplot(aes(y=RelativeInfluence, x= reorder(Factors,RelativeInfluence)))+
  geom_point(stat = "identity",size=2)+
  coord_flip( ylim = c(0, 60))+
  xlab("Factors")+
  ylab("Relative Influence")+
  theme_bw()

a8.rel.inf <- as.data.frame(cbind(summary.gbm(brt.a8simp,plotit=F)$var,summary.gbm(brt.a8simp,plotit=F)$rel.inf))
names(a8.rel.inf)[1] <- "Factors"
names(a8.rel.inf)[2] <- "RelativeInfluence"
a8.rel.inf$Factors <- as.factor(a8.rel.inf$Factors)
a8.rel.inf$RelativeInfluence <- as.numeric(a8.rel.inf$RelativeInfluence)
a8.rel.inf <- a8.rel.inf[order(a8.rel.inf$RelativeInfluence),]

a8plot <- a8.rel.inf %>%
  ggplot(aes(y=RelativeInfluence, x= reorder(Factors,RelativeInfluence)))+
  geom_point(stat = "identity",size=2)+
  coord_flip( ylim = c(0, 60))+
  xlab("Factors")+
  ylab("Relative Influence")+
  theme_bw()

plot_grid(a1plot,a2plot,a3plot,a4plot,a5plot,a6plot,a7plot,a8plot, labels="auto",ncol = 2,align = c("hv"))

#PDP Plots



PubPlot(brt.a1, variable.no =1, plot.layout = c(1,1),y.lab= "Fitted Response",x.lab= "Year",las=1)
PubPlot(brt.a1simp, variable.no =2, plot.layout=c(1,1), y.lab= "Fitted Response", x.lab= "Secchi Depth (m)",las=1)
PubPlot(brt.a1simp, variable.no =3, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Yellow Perch",las=1)
PubPlot(brt.a1simp, variable.no =4, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Largemouth Bass",las=1)
PubPlot(brt.a1simp, variable.no =5, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Northern Pike",las=1)
PubPlot(brt.a1simp, variable.no =6, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Urban Land Cover",las=1)
PubPlot(brt.a1simp, variable.no =7, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Agriculture Land Cover",las=1)
PubPlot(brt.a1simp, variable.no =8, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Forested Land Cover",las=1)
PubPlot(brt.a1simp, variable.no =9, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Wetland Land Cover",las=1)
PubPlot(brt.a1simp, variable.no =10,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Average Cohort Degree Days",las=1)
PubPlot(brt.a1simp, variable.no =11,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Mean Depth (ft.) - Log Transformed",las=1)
PubPlot(brt.a1simp, variable.no =12,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Lake Area (ac) - Log Transformed",las=1)
PubPlot(brt.a1simp, variable.no =13,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "County Population - Log Transformed",las=1)
PubPlot(brt.a1simp, variable.no =14,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Day of Year",las=1)


###

############

PubPlot(brt.a2simp, variable.no =1, plot.layout = c(1,1),y.lab= "Fitted Response",x.lab= "Year",las=1)
PubPlot(brt.a2simp, variable.no =2, plot.layout=c(1,1), y.lab= "Fitted Response", x.lab= "Secchi Depth (m)",las=1)
PubPlot(brt.a2simp, variable.no =3, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Yellow Perch",las=1)
PubPlot(brt.a2simp, variable.no =4, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Largemouth Bass",las=1)
PubPlot(brt.a2simp, variable.no =5, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Northern Pike",las=1)
PubPlot(brt.a2simp, variable.no =6, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Urban Land Cover",las=1)
PubPlot(brt.a2simp, variable.no =7, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Agriculture Land Cover",las=1)
PubPlot(brt.a2simp, variable.no =8, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Forested Land Cover",las=1)
PubPlot(brt.a2simp, variable.no =9, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Wetland Land Cover",las=1)
PubPlot(brt.a2simp, variable.no =10,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Average Cohort Degree Days",las=1)
PubPlot(brt.a2simp, variable.no =11,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Mean Depth (ft.) - Log Transformed",las=1)
PubPlot(brt.a2simp, variable.no =12,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Lake Area (ac) - Log Transformed",las=1)
PubPlot(brt.a2simp, variable.no =13,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "County Population - Log Transformed",las=1)
PubPlot(brt.a2simp, variable.no =14,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Day of Year",las=1)

###

############

PubPlot(brt.a3simp, variable.no =1, plot.layout = c(1,1),y.lab= "Fitted Response",x.lab= "Year",las=1)
PubPlot(brt.a3simp, variable.no =2, plot.layout=c(1,1), y.lab= "Fitted Response", x.lab= "Secchi Depth (m)",las=1)
PubPlot(brt.a3simp, variable.no =3, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Yellow Perch",las=1)
PubPlot(brt.a3simp, variable.no =4, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Largemouth Bass",las=1)
PubPlot(brt.a3simp, variable.no =5, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Northern Pike",las=1)
PubPlot(brt.a3simp, variable.no =6, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Walleye",las=1)
PubPlot(brt.a3simp, variable.no =7, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Urban Land Cover",las=1)
PubPlot(brt.a3simp, variable.no =8, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Agriculture Land Cover",las=1)
PubPlot(brt.a3simp, variable.no =9, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Forested Land Cover",las=1)
PubPlot(brt.a3simp, variable.no =10, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Wetland Land Cover",las=1)
PubPlot(brt.a3simp, variable.no =11,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Average Cohort Degree Days",las=1)
PubPlot(brt.a3simp, variable.no =12,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Mean Depth (ft.) - Log Transformed",las=1)
PubPlot(brt.a3simp, variable.no =13,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Lake Area (ac) - Log Transformed",las=1)
PubPlot(brt.a3simp, variable.no =14,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "County Population - Log Transformed",las=1)
PubPlot(brt.a3simp, variable.no =15,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Day of Year",las=1)

###

############
PubPlot(brt.a4simp, variable.no =1, plot.layout = c(1,1),y.lab= "Fitted Response",x.lab= "Year",las=1)
PubPlot(brt.a4simp, variable.no =2, plot.layout=c(1,1), y.lab= "Fitted Response", x.lab= "Secchi Depth (m)",las=1)
PubPlot(brt.a4simp, variable.no =3, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Yellow Perch",las=1)
PubPlot(brt.a4simp, variable.no =4, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Largemouth Bass",las=1)
PubPlot(brt.a4simp, variable.no =5, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Northern Pike",las=1)
PubPlot(brt.a4simp, variable.no =6, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Walleye",las=1)
PubPlot(brt.a4simp, variable.no =7, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Urban Land Cover",las=1)
PubPlot(brt.a4simp, variable.no =8, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Agriculture Land Cover",las=1)
PubPlot(brt.a4simp, variable.no =9, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Forested Land Cover",las=1)
PubPlot(brt.a4simp, variable.no =10, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Wetland Land Cover",las=1)
PubPlot(brt.a4simp, variable.no =11,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Average Cohort Degree Days",las=1)
PubPlot(brt.a4simp, variable.no =12,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Mean Depth (ft.) - Log Transformed",las=1)
PubPlot(brt.a4simp, variable.no =13,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Lake Area (ac) - Log Transformed",las=1)
PubPlot(brt.a4simp, variable.no =14,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "County Population - Log Transformed",las=1)
PubPlot(brt.a4simp, variable.no =15,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Day of Year",las=1)

###

############
PubPlot(brt.a5simp, variable.no =1, plot.layout = c(1,1),y.lab= "Fitted Response",x.lab= "Year",las=1)
PubPlot(brt.a5simp, variable.no =2, plot.layout=c(1,1), y.lab= "Fitted Response", x.lab= "Secchi Depth (m)",las=1)
PubPlot(brt.a5simp, variable.no =3, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Yellow Perch",las=1)
PubPlot(brt.a5simp, variable.no =4, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Largemouth Bass",las=1)
PubPlot(brt.a5simp, variable.no =5, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Northern Pike",las=1)
PubPlot(brt.a5simp, variable.no =6, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Walleye",las=1)
PubPlot(brt.a5simp, variable.no =7, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Urban Land Cover",las=1)
PubPlot(brt.a5simp, variable.no =8, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Agriculture Land Cover",las=1)
PubPlot(brt.a5simp, variable.no =9, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Forested Land Cover",las=1)
PubPlot(brt.a5simp, variable.no =10, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Wetland Land Cover",las=1)
PubPlot(brt.a5simp, variable.no =11,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Average Cohort Degree Days",las=1)
PubPlot(brt.a5simp, variable.no =12,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Mean Depth (ft.) - Log Transformed",las=1)
PubPlot(brt.a5simp, variable.no =13,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Lake Area (ac) - Log Transformed",las=1)
PubPlot(brt.a5simp, variable.no =14,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "County Population - Log Transformed",las=1)
PubPlot(brt.a5simp, variable.no =15,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Day of Year",las=1)

###

############

PubPlot(brt.a6simp, variable.no =1, plot.layout = c(1,1),y.lab= "Fitted Response",x.lab= "Year",las=1)
PubPlot(brt.a6simp, variable.no =2, plot.layout=c(1,1), y.lab= "Fitted Response", x.lab= "Secchi Depth (m)",las=1)
PubPlot(brt.a6simp, variable.no =3, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Largemouth Bass",las=1)
PubPlot(brt.a6simp, variable.no =4, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Walleye",las=1)
PubPlot(brt.a6simp, variable.no =5, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Urban Land Cover",las=1)
PubPlot(brt.a6simp, variable.no =6, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Agriculture Land Cover",las=1)
PubPlot(brt.a6simp, variable.no =7, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Forested Land Cover",las=1)
PubPlot(brt.a6simp, variable.no =8, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Wetland Land Cover",las=1)
PubPlot(brt.a6simp, variable.no =9,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Average Cohort Degree Days",las=1)
PubPlot(brt.a6simp, variable.no =10,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Mean Depth (ft.) - Log Transformed",las=1)
PubPlot(brt.a6simp, variable.no =11,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Lake Area (ac) - Log Transformed",las=1)
PubPlot(brt.a6simp, variable.no =12,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "County Population - Log Transformed",las=1)
PubPlot(brt.a6simp, variable.no =13,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Day of Year",las=1)

###

############

PubPlot(brt.a7simp, variable.no =1, plot.layout = c(1,1),y.lab= "Fitted Response",x.lab= "Year",las=1)
PubPlot(brt.a7simp, variable.no =2, plot.layout=c(1,1), y.lab= "Fitted Response", x.lab= "Secchi Depth (m)",las=1)
PubPlot(brt.a7simp, variable.no =3, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Largemouth Bass",las=1)
PubPlot(brt.a7simp, variable.no =4, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Urban Land Cover",las=1)
PubPlot(brt.a7simp, variable.no =5, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Agriculture Land Cover",las=1)
PubPlot(brt.a7simp, variable.no =6, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Forested Land Cover",las=1)
PubPlot(brt.a7simp, variable.no =7, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Wetland Land Cover",las=1)
PubPlot(brt.a7simp, variable.no =8,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Average Cohort Degree Days",las=1)
PubPlot(brt.a7simp, variable.no =9,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Mean Depth (ft.) - Log Transformed",las=1)
PubPlot(brt.a7simp, variable.no =10,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Lake Area (ac) - Log Transformed",las=1)
PubPlot(brt.a7simp, variable.no =11,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "County Population - Log Transformed",las=1)
PubPlot(brt.a7simp, variable.no =12,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Day of Year",las=1)


###

############
PubPlot(brt.a8simp, variable.no =1, plot.layout = c(1,1),y.lab= "Fitted Response",x.lab= "Year",las=1)
PubPlot(brt.a8simp, variable.no =2, plot.layout=c(1,1), y.lab= "Fitted Response", x.lab= "Secchi Depth (m)",las=1)
PubPlot(brt.a8simp, variable.no =3, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Largemouth Bass",las=1)
PubPlot(brt.a8simp, variable.no =4, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Northern Pike",las=1)
PubPlot(brt.a8simp, variable.no =5, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Presence of Walleye",las=1)

PubPlot(brt.a8simp, variable.no =6, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Urban Land Cover",las=1)
PubPlot(brt.a8simp, variable.no =7, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Agriculture Land Cover",las=1)
PubPlot(brt.a8simp, variable.no =8, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Forested Land Cover",las=1)
PubPlot(brt.a8simp, variable.no =9, plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Proportion of Wetland Land Cover",las=1)
PubPlot(brt.a8simp, variable.no =10,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Average Cohort Degree Days",las=1)
PubPlot(brt.a8simp, variable.no =11,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Mean Depth (ft.) - Log Transformed",las=1)
PubPlot(brt.a8simp, variable.no =12,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Lake Area (ac) - Log Transformed",las=1)
PubPlot(brt.a8simp, variable.no =13,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "County Population - Log Transformed",las=1)
PubPlot(brt.a8simp, variable.no =14,plot.layout = c(1,1),y.lab= "Fitted Response", x.lab= "Day of Year",las=1)

###Interaction Plots

gbm.perspec(brt.a1simp,13,14, z.range = c(40,105))

gbm.perspec(brt.a2simp,12,14, z.range = c(70,130))

gbm.perspec(brt.a3simp,11,14, z.range = c(90,135))

gbm.perspec(brt.a4simp,11,14, z.range = c(120,170))

gbm.perspec(brt.a5simp,11,14, z.range = c(140,180))

gbm.perspec(brt.a6simp,13,2,z.range = c(160,210))

gbm.perspec(brt.a7simp,12,10, z.range = c(170,210))

gbm.perspec(brt.a8simp,11,10, z.range = c(180,230))

###
