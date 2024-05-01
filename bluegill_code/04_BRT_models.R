
#Boosted Regression Tree Code 
# Written by Elise Grabda and modified by Katelyn King and Peter Flood 

#### load libraries ####
library(dismo) #for BRT modeling
library(vip) #for plotting 
#devtools::install_github("JBjouffray/ggBRT") # only need to install once
library(ggBRT) #BRT plotting and summary statistics
library(cowplot)
library(tidyverse) #data manipulation

### read in data that was created from 01_data cleaning scripts # 
binded<-read.csv("bluegill_data/model_data.csv") %>% 
  mutate(cpue = log(cpue+1))

#### BRT models for individual ages #### 

#set list of variables (16 predictors)
##year, Secchi, perch, lmb, pike, walleye,  urban, ag, forest, wetlands, logdepth, logarea, logcounty, doy, DD_mean, cpue
xx.list <- c('mean_secchi_m', 'perch', 'lmb', 'pike', 'walleye',  
             'urban', 'agriculture', 'forests', 'wetlands', 'logdepth', 
             'logarea', 'logcounty', 'doy', 'DD_mean', 'cpue', 'surf_temp_year')

### AGE1 ####
brt.a1 = gbm.step(data = subset(binded, AGE == 1), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", 
                  tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)
gbm.plot(brt.a1) #partial dependence plots 
vi(brt.a1)
vip(brt.a1, 
    num_features = 15L,
    geom = c( "point")) + 
  ggtitle("Age 1") +  
  theme_light() + 
  geom_hline(yintercept=6.67,linetype="dashed", lwd=0.5,colour="blue") #adding critical threshold 1/15 = 0.0667*100

#ggBRT plots and summary
#Per. Expl is interpretted like an R^2
#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a1) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a1, main = "Age 1")


#interaction investigation 
brt.int1=gbm.interactions(brt.a1) 
brt.int1$interactions
brt.int1$rank.list

#predict 
ni1=nrow(subset(binded, AGE == 1))
predset1=sample(1:ni1,100) 
predbrt1=predict(brt.a1, 
                 subset(binded, AGE == 1)[predset1,xx.list], 
                 n.trees=brt.a1$gbm.call$best.trees, 
                 type="response") 
plot(predbrt1, subset(binded, AGE == 1)[predset1,5]) #predicted lengths vs observed column 5

summary(lm(subset(binded, AGE == 1)[predset1,5] ~ predbrt1)) #predicted lengths vs observed column 5


#### Age 2 #### 
brt.a2 = gbm.step(data = subset(binded, AGE == 2), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a2) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a2, main = "Age 2")

 #interaction
brt.int2=gbm.interactions(brt.a2)
brt.simp2 = gbm.simplify(brt.a2)

#predict 
ni2=nrow(subset(binded, AGE == 2))
predset2=sample(1:ni2,100) 
predbrt2=predict(brt.a2simp,subset(binded, AGE == 2)[predset2,xlist_simp2],n.trees=brt.a2simp$gbm.call$best.trees, type="response") 
plot(predbrt2,subset(binded, AGE == 2)[predset2,5])

summary(lm(subset(binded, AGE == 2)[predset2,5] ~ predbrt2))

### Age3 ####
brt.a3 = gbm.step(data = subset(binded, AGE == 3), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a3) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a3, main = "Age 3")

#interactions 
brt.int3 = gbm.interactions(brt.a3)
brt.simp3 = gbm.simplify(brt.a3)

#predict
ni3=nrow(subset(binded, AGE == 3))
predset3=sample(1:ni3,100) 
predbrt3=predict(brt.a3simp,subset(binded, AGE == 3)[predset3,xlist_simp3],n.trees=brt.a3simp$gbm.call$best.trees, type="response") 
plot(predbrt3,subset(binded, AGE == 3)[predset3,5])

summary(lm(subset(binded, AGE == 3)[predset3,5] ~ predbrt3))


### Age 4 #### 

brt.a4 = gbm.step(data = subset(binded, AGE == 4), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a4) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a4, main = "Age 4")

#interaction
# brt.int4=gbm.interactions(brt.a4)
# #brt.simp4 = gbm.simplify(brt.a4)
# 
# #predict
# ni4=nrow(subset(binded, AGE == 4)) #number of rows in the data
# predset4=sample(1:ni4,100) #random draw of 100 lakes
# predbrt4=predict(brt.a4simp,subset(binded, AGE == 4)[predset4,xlist_simp4],n.trees=brt.a4simp$gbm.call$best.trees, type="response") #predictions from the model to the 100 lakes
# plot(predbrt4,subset(binded, AGE == 4)[predset4,5])
# 
# #summary(lm(subset(binded, AGE == 4)[predset4,5] ~ predbrt4))


### Age 5 ####
brt.a5 = gbm.step(data = subset(binded, AGE == 5), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a5) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a5, main = "Age 5")

#interaction
# brt.int5=gbm.interactions(brt.a5)
# brt.simp5 = gbm.simplify(brt.a5)

#predict
# ni5=nrow(subset(binded, AGE == 5))
# predset5=sample(1:ni5,100) 
# predbrt5=predict(brt.a5simp,subset(binded, AGE == 5)[predset5,xlist_simp5],n.trees=brt.a5simp$gbm.call$best.trees, type="response") 
# plot(predbrt5,subset(binded, AGE == 5)[predset5,5])
# 
# summary(lm(subset(binded, AGE == 5)[predset5,5] ~ predbrt5))


### Age 6 ####
brt.a6 = gbm.step(data = subset(binded, AGE == 6), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a6) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a6, main = "Age 6")

#interactions
# brt.int6=gbm.interactions(brt.a6)
# brt.simp6 = gbm.simplify(brt.a6)
# 
# #predict
# ni6=nrow(subset(binded, AGE == 6))
# predset6=sample(1:ni6,100) 
# predbrt6=predict(brt.a6simp,subset(binded, AGE == 6)[predset6,xlist_simp6],n.trees=brt.a6simp$gbm.call$best.trees, type="response") 
# plot(predbrt6,subset(binded, AGE == 6)[predset6,5])
# 
# summary(lm(subset(binded, AGE == 6)[predset6,5] ~ predbrt6))


### Age 7 ####
brt.a7 = gbm.step(data = subset(binded, AGE == 7), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a7) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a7, main = "Age 7")

#interaction
# brt.int7=gbm.interactions(brt.a7)
# brt.simp7 = gbm.simplify(brt.a7)
# 
# #prediction
# ni7=nrow(subset(binded, AGE == 7))
# predset7=sample(1:ni7,100) 
# predbrt7=predict(brt.a7simp,subset(binded, AGE == 7)[predset7,xlist_simp7],n.trees=brt.a7simp$gbm.call$best.trees, type="response") 
# plot(predbrt7,subset(binded, AGE == 7)[predset7,5])
# 
# summary(lm(subset(binded, AGE == 7)[predset7,5] ~ predbrt7))


### Age 8 #### 
brt.a8 = gbm.step(data = subset(binded, AGE == 8), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a8) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a8, main = "Age 8")

#interaction
# brt.int8=gbm.interactions(brt.a8)
# brt.simp8 = gbm.simplify(brt.a8)
# 
# #prediction 
# ni8=nrow(subset(binded, AGE == 8))
# predset8=sample(1:ni8,100) 
# predbrt8=predict(brt.a8simp,subset(binded, AGE == 8)[predset8,xlist_simp8],n.trees=brt.a8simp$gbm.call$best.trees, type="response") 
# plot(predbrt8,subset(binded, AGE == 8)[predset8,5])
# 
# summary(lm(subset(binded, AGE == 8)[predset8,5] ~ predbrt8))


#### combine all ages plots #### 
######Format fitted functions##################
lepmac.response.matrix <- bind_rows(
  bind_rows(
    #age group 1
    gbm::plot.gbm(brt.a1, i.var = "DD_mean", return.grid = TRUE) %>% 
      rename(Predictor_Value = DD_mean) %>% 
      mutate(Predictor_Name = "DD_mean",
             y = scale(y)),
    gbm::plot.gbm(brt.a1, i.var = "mean_secchi_m", return.grid = TRUE) %>% 
      rename(Predictor_Value = mean_secchi_m) %>% 
      mutate(Predictor_Name = "mean_secchi_m",
             y = scale(y)),
    gbm::plot.gbm(brt.a1, i.var = "urban", return.grid = TRUE) %>% 
      rename(Predictor_Value = urban) %>% 
      mutate(Predictor_Name = "urban",
             y = scale(y)),
    gbm::plot.gbm(brt.a1, i.var = "agriculture", return.grid = TRUE) %>% 
      rename(Predictor_Value = agriculture) %>% 
      mutate(Predictor_Name = "agriculture",
             y = scale(y)),
    gbm::plot.gbm(brt.a1, i.var = "forests", return.grid = TRUE) %>% 
      rename(Predictor_Value = forests) %>% 
      mutate(Predictor_Name = "forests",
             y = scale(y)),
    gbm::plot.gbm(brt.a1, i.var = "wetlands", return.grid = TRUE) %>% 
      rename(Predictor_Value = wetlands) %>% 
      mutate(Predictor_Name = "wetlands",
             y = scale(y)),
    gbm::plot.gbm(brt.a1, i.var = "doy", return.grid = TRUE) %>% 
      rename(Predictor_Value = doy) %>% 
      mutate(Predictor_Name = "doy",
             y = scale(y)),
    gbm::plot.gbm(brt.a1, i.var = "logdepth", return.grid = TRUE) %>% 
      rename(Predictor_Value = logdepth) %>% 
      mutate(Predictor_Name = "logdepth",
             y = scale(y)),
    gbm::plot.gbm(brt.a1, i.var = "logarea", return.grid = TRUE) %>% 
      rename(Predictor_Value = logarea) %>% 
      mutate(Predictor_Name = "logarea",
             y = scale(y)),
    gbm::plot.gbm(brt.a1, i.var = "logcounty", return.grid = TRUE) %>% 
      rename(Predictor_Value = logcounty) %>% 
      mutate(Predictor_Name = "logcounty",
             y = scale(y)),
    gbm::plot.gbm(brt.a1,i.var = "cpue", return.grid = TRUE) %>% 
      rename(Predictor_Value = cpue) %>% 
      mutate(Predictor_Name = "cpue",
             y = scale(y)),
    gbm::plot.gbm(brt.a1,i.var = "walleye", return.grid = TRUE) %>% 
      rename(Predictor_Value = walleye) %>% 
      mutate(Predictor_Name = "walleye",
             y = scale(y)),
    gbm::plot.gbm(brt.a1,i.var = "pike", return.grid = TRUE) %>% 
      rename(Predictor_Value = pike) %>% 
      mutate(Predictor_Name = "pike",
             y = scale(y)),
    gbm::plot.gbm(brt.a1,i.var = "perch", return.grid = TRUE) %>% 
      rename(Predictor_Value = perch) %>% 
      mutate(Predictor_Name = "perch",
             y = scale(y)),
    gbm::plot.gbm(brt.a1,i.var = "lmb", return.grid = TRUE) %>% 
      rename(Predictor_Value = lmb) %>% 
      mutate(Predictor_Name = "lmb",
             y = scale(y)),
    gbm::plot.gbm(brt.a1,i.var = "surf_temp_year", return.grid = TRUE) %>% 
      rename(Predictor_Value = surf_temp_year) %>% 
      mutate(Predictor_Name = "surf_temp_year",
             y = scale(y))
  ) %>% mutate(Age_Group = 1),
  #add age group 2
  bind_rows(
    gbm::plot.gbm(brt.a2, i.var = "DD_mean", return.grid = TRUE) %>% 
      rename(Predictor_Value = DD_mean) %>% 
      mutate(Predictor_Name = "DD_mean",
             y = scale(y)),
    gbm::plot.gbm(brt.a2, i.var = "mean_secchi_m", return.grid = TRUE) %>% 
      rename(Predictor_Value = mean_secchi_m) %>% 
      mutate(Predictor_Name = "mean_secchi_m",
             y = scale(y)),
    gbm::plot.gbm(brt.a2, i.var = "urban", return.grid = TRUE) %>% 
      rename(Predictor_Value = urban) %>% 
      mutate(Predictor_Name = "urban",
             y = scale(y)),
    gbm::plot.gbm(brt.a2, i.var = "agriculture", return.grid = TRUE) %>% 
      rename(Predictor_Value = agriculture) %>% 
      mutate(Predictor_Name = "agriculture",
             y = scale(y)),
    gbm::plot.gbm(brt.a2, i.var = "forests", return.grid = TRUE) %>% 
      rename(Predictor_Value = forests) %>% 
      mutate(Predictor_Name = "forests",
             y = scale(y)),
    gbm::plot.gbm(brt.a2, i.var = "wetlands", return.grid = TRUE) %>% 
      rename(Predictor_Value = wetlands) %>% 
      mutate(Predictor_Name = "wetlands",
             y = scale(y)),
    gbm::plot.gbm(brt.a2, i.var = "doy", return.grid = TRUE) %>% 
      rename(Predictor_Value = doy) %>% 
      mutate(Predictor_Name = "doy",
             y = scale(y)),
    gbm::plot.gbm(brt.a2, i.var = "logdepth", return.grid = TRUE) %>% 
      rename(Predictor_Value = logdepth) %>% 
      mutate(Predictor_Name = "logdepth",
             y = scale(y)),
    gbm::plot.gbm(brt.a2, i.var = "logarea", return.grid = TRUE) %>% 
      rename(Predictor_Value = logarea) %>% 
      mutate(Predictor_Name = "logarea",
             y = scale(y)),
    gbm::plot.gbm(brt.a2, i.var = "logcounty", return.grid = TRUE) %>% 
      rename(Predictor_Value = logcounty) %>% 
      mutate(Predictor_Name = "logcounty",
             y = scale(y)),
    gbm::plot.gbm(brt.a2,i.var = "cpue", return.grid = TRUE) %>% 
      rename(Predictor_Value = cpue) %>% 
      mutate(Predictor_Name = "cpue",
             y = scale(y)),
    gbm::plot.gbm(brt.a2,i.var = "walleye", return.grid = TRUE) %>% 
      rename(Predictor_Value = walleye) %>% 
      mutate(Predictor_Name = "walleye",
             y = scale(y)),
    gbm::plot.gbm(brt.a2,i.var = "pike", return.grid = TRUE) %>% 
      rename(Predictor_Value = pike) %>% 
      mutate(Predictor_Name = "pike",
             y = scale(y)),
    gbm::plot.gbm(brt.a2,i.var = "perch", return.grid = TRUE) %>% 
      rename(Predictor_Value = perch) %>% 
      mutate(Predictor_Name = "perch",
             y = scale(y)),
    gbm::plot.gbm(brt.a2,i.var = "lmb", return.grid = TRUE) %>% 
      rename(Predictor_Value = lmb) %>% 
      mutate(Predictor_Name = "lmb",
             y = scale(y)),
    gbm::plot.gbm(brt.a2,i.var = "surf_temp_year", return.grid = TRUE) %>% 
      rename(Predictor_Value = surf_temp_year) %>% 
      mutate(Predictor_Name = "surf_temp_year",
             y = scale(y))
  ) %>% mutate(Age_Group = 2),
  bind_rows(
    #age group 3
    gbm::plot.gbm(brt.a3, i.var = "DD_mean", return.grid = TRUE) %>% 
      rename(Predictor_Value = DD_mean) %>% 
      mutate(Predictor_Name = "DD_mean",
             y = scale(y)),
    gbm::plot.gbm(brt.a3, i.var = "mean_secchi_m", return.grid = TRUE) %>% 
      rename(Predictor_Value = mean_secchi_m) %>% 
      mutate(Predictor_Name = "mean_secchi_m",
             y = scale(y)),
    gbm::plot.gbm(brt.a3, i.var = "urban", return.grid = TRUE) %>% 
      rename(Predictor_Value = urban) %>% 
      mutate(Predictor_Name = "urban",
             y = scale(y)),
    gbm::plot.gbm(brt.a3, i.var = "agriculture", return.grid = TRUE) %>% 
      rename(Predictor_Value = agriculture) %>% 
      mutate(Predictor_Name = "agriculture",
             y = scale(y)),
    gbm::plot.gbm(brt.a3, i.var = "forests", return.grid = TRUE) %>% 
      rename(Predictor_Value = forests) %>% 
      mutate(Predictor_Name = "forests",
             y = scale(y)),
    gbm::plot.gbm(brt.a3, i.var = "wetlands", return.grid = TRUE) %>% 
      rename(Predictor_Value = wetlands) %>% 
      mutate(Predictor_Name = "wetlands",
             y = scale(y)),
    gbm::plot.gbm(brt.a3, i.var = "doy", return.grid = TRUE) %>% 
      rename(Predictor_Value = doy) %>% 
      mutate(Predictor_Name = "doy",
             y = scale(y)),
    gbm::plot.gbm(brt.a3, i.var = "logdepth", return.grid = TRUE) %>% 
      rename(Predictor_Value = logdepth) %>% 
      mutate(Predictor_Name = "logdepth",
             y = scale(y)),
    gbm::plot.gbm(brt.a3, i.var = "logarea", return.grid = TRUE) %>% 
      rename(Predictor_Value = logarea) %>% 
      mutate(Predictor_Name = "logarea",
             y = scale(y)),
    gbm::plot.gbm(brt.a3, i.var = "logcounty", return.grid = TRUE) %>% 
      rename(Predictor_Value = logcounty) %>% 
      mutate(Predictor_Name = "logcounty",
             y = scale(y)),
    gbm::plot.gbm(brt.a3,i.var = "cpue", return.grid = TRUE) %>% 
      rename(Predictor_Value = cpue) %>% 
      mutate(Predictor_Name = "cpue",
             y = scale(y)),
    gbm::plot.gbm(brt.a3,i.var = "walleye", return.grid = TRUE) %>% 
      rename(Predictor_Value = walleye) %>% 
      mutate(Predictor_Name = "walleye",
             y = scale(y)),
    gbm::plot.gbm(brt.a3,i.var = "pike", return.grid = TRUE) %>% 
      rename(Predictor_Value = pike) %>% 
      mutate(Predictor_Name = "pike",
             y = scale(y)),
    gbm::plot.gbm(brt.a3,i.var = "perch", return.grid = TRUE) %>% 
      rename(Predictor_Value = perch) %>% 
      mutate(Predictor_Name = "perch",
             y = scale(y)),
    gbm::plot.gbm(brt.a3,i.var = "lmb", return.grid = TRUE) %>% 
      rename(Predictor_Value = lmb) %>% 
      mutate(Predictor_Name = "lmb",
             y = scale(y)),
    gbm::plot.gbm(brt.a3,i.var = "surf_temp_year", return.grid = TRUE) %>% 
      rename(Predictor_Value = surf_temp_year) %>% 
      mutate(Predictor_Name = "surf_temp_year",
             y = scale(y))
  ) %>% mutate(Age_Group = 3),
  bind_rows(
    #age group 4
    gbm::plot.gbm(brt.a4, i.var = "DD_mean", return.grid = TRUE) %>% 
      rename(Predictor_Value = DD_mean) %>% 
      mutate(Predictor_Name = "DD_mean",
             y = scale(y)),
    gbm::plot.gbm(brt.a4, i.var = "mean_secchi_m", return.grid = TRUE) %>% 
      rename(Predictor_Value = mean_secchi_m) %>% 
      mutate(Predictor_Name = "mean_secchi_m",
             y = scale(y)),
    gbm::plot.gbm(brt.a4, i.var = "urban", return.grid = TRUE) %>% 
      rename(Predictor_Value = urban) %>% 
      mutate(Predictor_Name = "urban",
             y = scale(y)),
    gbm::plot.gbm(brt.a4, i.var = "agriculture", return.grid = TRUE) %>% 
      rename(Predictor_Value = agriculture) %>% 
      mutate(Predictor_Name = "agriculture",
             y = scale(y)),
    gbm::plot.gbm(brt.a4, i.var = "forests", return.grid = TRUE) %>% 
      rename(Predictor_Value = forests) %>% 
      mutate(Predictor_Name = "forests",
             y = scale(y)),
    gbm::plot.gbm(brt.a4, i.var = "wetlands", return.grid = TRUE) %>% 
      rename(Predictor_Value = wetlands) %>% 
      mutate(Predictor_Name = "wetlands",
             y = scale(y)),
    gbm::plot.gbm(brt.a4, i.var = "doy", return.grid = TRUE) %>% 
      rename(Predictor_Value = doy) %>% 
      mutate(Predictor_Name = "doy",
             y = scale(y)),
    gbm::plot.gbm(brt.a4, i.var = "logdepth", return.grid = TRUE) %>% 
      rename(Predictor_Value = logdepth) %>% 
      mutate(Predictor_Name = "logdepth",
             y = scale(y)),
    gbm::plot.gbm(brt.a4, i.var = "logarea", return.grid = TRUE) %>% 
      rename(Predictor_Value = logarea) %>% 
      mutate(Predictor_Name = "logarea",
             y = scale(y)),
    gbm::plot.gbm(brt.a4, i.var = "logcounty", return.grid = TRUE) %>% 
      rename(Predictor_Value = logcounty) %>% 
      mutate(Predictor_Name = "logcounty",
             y = scale(y)),
    gbm::plot.gbm(brt.a4,i.var = "cpue", return.grid = TRUE) %>% 
      rename(Predictor_Value = cpue) %>% 
      mutate(Predictor_Name = "cpue",
             y = scale(y)),
    gbm::plot.gbm(brt.a4,i.var = "walleye", return.grid = TRUE) %>% 
      rename(Predictor_Value = walleye) %>% 
      mutate(Predictor_Name = "walleye",
             y = scale(y)),
    gbm::plot.gbm(brt.a4,i.var = "pike", return.grid = TRUE) %>% 
      rename(Predictor_Value = pike) %>% 
      mutate(Predictor_Name = "pike",
             y = scale(y)),
    gbm::plot.gbm(brt.a4,i.var = "perch", return.grid = TRUE) %>% 
      rename(Predictor_Value = perch) %>% 
      mutate(Predictor_Name = "perch",
             y = scale(y)),
    gbm::plot.gbm(brt.a4,i.var = "lmb", return.grid = TRUE) %>% 
      rename(Predictor_Value = lmb) %>% 
      mutate(Predictor_Name = "lmb",
             y = scale(y)),
    gbm::plot.gbm(brt.a4,i.var = "surf_temp_year", return.grid = TRUE) %>% 
      rename(Predictor_Value = surf_temp_year) %>% 
      mutate(Predictor_Name = "surf_temp_year",
             y = scale(y))
  ) %>% mutate(Age_Group = 4),
  bind_rows(
    #age group 5
    gbm::plot.gbm(brt.a5, i.var = "DD_mean", return.grid = TRUE) %>% 
      rename(Predictor_Value = DD_mean) %>% 
      mutate(Predictor_Name = "DD_mean",
             y = scale(y)),
    gbm::plot.gbm(brt.a5, i.var = "mean_secchi_m", return.grid = TRUE) %>% 
      rename(Predictor_Value = mean_secchi_m) %>% 
      mutate(Predictor_Name = "mean_secchi_m",
             y = scale(y)),
    gbm::plot.gbm(brt.a5, i.var = "urban", return.grid = TRUE) %>% 
      rename(Predictor_Value = urban) %>% 
      mutate(Predictor_Name = "urban",
             y = scale(y)),
    gbm::plot.gbm(brt.a5, i.var = "agriculture", return.grid = TRUE) %>% 
      rename(Predictor_Value = agriculture) %>% 
      mutate(Predictor_Name = "agriculture",
             y = scale(y)),
    gbm::plot.gbm(brt.a5, i.var = "forests", return.grid = TRUE) %>% 
      rename(Predictor_Value = forests) %>% 
      mutate(Predictor_Name = "forests",
             y = scale(y)),
    gbm::plot.gbm(brt.a5, i.var = "wetlands", return.grid = TRUE) %>% 
      rename(Predictor_Value = wetlands) %>% 
      mutate(Predictor_Name = "wetlands",
             y = scale(y)),
    gbm::plot.gbm(brt.a5, i.var = "doy", return.grid = TRUE) %>% 
      rename(Predictor_Value = doy) %>% 
      mutate(Predictor_Name = "doy",
             y = scale(y)),
    gbm::plot.gbm(brt.a5, i.var = "logdepth", return.grid = TRUE) %>% 
      rename(Predictor_Value = logdepth) %>% 
      mutate(Predictor_Name = "logdepth",
             y = scale(y)),
    gbm::plot.gbm(brt.a5, i.var = "logarea", return.grid = TRUE) %>% 
      rename(Predictor_Value = logarea) %>% 
      mutate(Predictor_Name = "logarea",
             y = scale(y)),
    gbm::plot.gbm(brt.a5, i.var = "logcounty", return.grid = TRUE) %>% 
      rename(Predictor_Value = logcounty) %>% 
      mutate(Predictor_Name = "logcounty",
             y = scale(y)),
    gbm::plot.gbm(brt.a5,i.var = "cpue", return.grid = TRUE) %>% 
      rename(Predictor_Value = cpue) %>% 
      mutate(Predictor_Name = "cpue",
             y = scale(y)),
    gbm::plot.gbm(brt.a5,i.var = "walleye", return.grid = TRUE) %>% 
      rename(Predictor_Value = walleye) %>% 
      mutate(Predictor_Name = "walleye",
             y = scale(y)),
    gbm::plot.gbm(brt.a5,i.var = "pike", return.grid = TRUE) %>% 
      rename(Predictor_Value = pike) %>% 
      mutate(Predictor_Name = "pike",
             y = scale(y)),
    gbm::plot.gbm(brt.a5,i.var = "perch", return.grid = TRUE) %>% 
      rename(Predictor_Value = perch) %>% 
      mutate(Predictor_Name = "perch",
             y = scale(y)),
    gbm::plot.gbm(brt.a5,i.var = "lmb", return.grid = TRUE) %>% 
      rename(Predictor_Value = lmb) %>% 
      mutate(Predictor_Name = "lmb",
             y = scale(y)),
    gbm::plot.gbm(brt.a5,i.var = "surf_temp_year", return.grid = TRUE) %>% 
      rename(Predictor_Value = surf_temp_year) %>% 
      mutate(Predictor_Name = "surf_temp_year",
             y = scale(y))
  ) %>% mutate(Age_Group = 5),
  bind_rows(
    #age group 6
    gbm::plot.gbm(brt.a6, i.var = "DD_mean", return.grid = TRUE) %>% 
      rename(Predictor_Value = DD_mean) %>% 
      mutate(Predictor_Name = "DD_mean",
             y = scale(y)),
    gbm::plot.gbm(brt.a6, i.var = "mean_secchi_m", return.grid = TRUE) %>% 
      rename(Predictor_Value = mean_secchi_m) %>% 
      mutate(Predictor_Name = "mean_secchi_m",
             y = scale(y)),
    gbm::plot.gbm(brt.a6, i.var = "urban", return.grid = TRUE) %>% 
      rename(Predictor_Value = urban) %>% 
      mutate(Predictor_Name = "urban",
             y = scale(y)),
    gbm::plot.gbm(brt.a6, i.var = "agriculture", return.grid = TRUE) %>% 
      rename(Predictor_Value = agriculture) %>% 
      mutate(Predictor_Name = "agriculture",
             y = scale(y)),
    gbm::plot.gbm(brt.a6, i.var = "forests", return.grid = TRUE) %>% 
      rename(Predictor_Value = forests) %>% 
      mutate(Predictor_Name = "forests",
             y = scale(y)),
    gbm::plot.gbm(brt.a6, i.var = "wetlands", return.grid = TRUE) %>% 
      rename(Predictor_Value = wetlands) %>% 
      mutate(Predictor_Name = "wetlands",
             y = scale(y)),
    gbm::plot.gbm(brt.a6, i.var = "doy", return.grid = TRUE) %>% 
      rename(Predictor_Value = doy) %>% 
      mutate(Predictor_Name = "doy",
             y = scale(y)),
    gbm::plot.gbm(brt.a6, i.var = "logdepth", return.grid = TRUE) %>% 
      rename(Predictor_Value = logdepth) %>% 
      mutate(Predictor_Name = "logdepth",
             y = scale(y)),
    gbm::plot.gbm(brt.a6, i.var = "logarea", return.grid = TRUE) %>% 
      rename(Predictor_Value = logarea) %>% 
      mutate(Predictor_Name = "logarea",
             y = scale(y)),
    gbm::plot.gbm(brt.a6, i.var = "logcounty", return.grid = TRUE) %>% 
      rename(Predictor_Value = logcounty) %>% 
      mutate(Predictor_Name = "logcounty",
             y = scale(y)),
    gbm::plot.gbm(brt.a6,i.var = "cpue", return.grid = TRUE) %>% 
      rename(Predictor_Value = cpue) %>% 
      mutate(Predictor_Name = "cpue",
             y = scale(y)),
    gbm::plot.gbm(brt.a6,i.var = "walleye", return.grid = TRUE) %>% 
      rename(Predictor_Value = walleye) %>% 
      mutate(Predictor_Name = "walleye",
             y = scale(y)),
    gbm::plot.gbm(brt.a6,i.var = "pike", return.grid = TRUE) %>% 
      rename(Predictor_Value = pike) %>% 
      mutate(Predictor_Name = "pike",
             y = scale(y)),
    gbm::plot.gbm(brt.a6,i.var = "perch", return.grid = TRUE) %>% 
      rename(Predictor_Value = perch) %>% 
      mutate(Predictor_Name = "perch",
             y = scale(y)),
    gbm::plot.gbm(brt.a6,i.var = "lmb", return.grid = TRUE) %>% 
      rename(Predictor_Value = lmb) %>% 
      mutate(Predictor_Name = "lmb",
             y = scale(y)),
    gbm::plot.gbm(brt.a6,i.var = "surf_temp_year", return.grid = TRUE) %>% 
      rename(Predictor_Value = surf_temp_year) %>% 
      mutate(Predictor_Name = "surf_temp_year",
             y = scale(y))
  ) %>% mutate(Age_Group = 6),
  bind_rows(
    #age group 7
    gbm::plot.gbm(brt.a7, i.var = "DD_mean", return.grid = TRUE) %>% 
      rename(Predictor_Value = DD_mean) %>% 
      mutate(Predictor_Name = "DD_mean",
             y = scale(y)),
    gbm::plot.gbm(brt.a7, i.var = "mean_secchi_m", return.grid = TRUE) %>% 
      rename(Predictor_Value = mean_secchi_m) %>% 
      mutate(Predictor_Name = "mean_secchi_m",
             y = scale(y)),
    gbm::plot.gbm(brt.a7, i.var = "urban", return.grid = TRUE) %>% 
      rename(Predictor_Value = urban) %>% 
      mutate(Predictor_Name = "urban",
             y = scale(y)),
    gbm::plot.gbm(brt.a7, i.var = "agriculture", return.grid = TRUE) %>% 
      rename(Predictor_Value = agriculture) %>% 
      mutate(Predictor_Name = "agriculture",
             y = scale(y)),
    gbm::plot.gbm(brt.a7, i.var = "forests", return.grid = TRUE) %>% 
      rename(Predictor_Value = forests) %>% 
      mutate(Predictor_Name = "forests",
             y = scale(y)),
    gbm::plot.gbm(brt.a7, i.var = "wetlands", return.grid = TRUE) %>% 
      rename(Predictor_Value = wetlands) %>% 
      mutate(Predictor_Name = "wetlands",
             y = scale(y)),
    gbm::plot.gbm(brt.a7, i.var = "doy", return.grid = TRUE) %>% 
      rename(Predictor_Value = doy) %>% 
      mutate(Predictor_Name = "doy",
             y = scale(y)),
    gbm::plot.gbm(brt.a7, i.var = "logdepth", return.grid = TRUE) %>% 
      rename(Predictor_Value = logdepth) %>% 
      mutate(Predictor_Name = "logdepth",
             y = scale(y)),
    gbm::plot.gbm(brt.a7, i.var = "logarea", return.grid = TRUE) %>% 
      rename(Predictor_Value = logarea) %>% 
      mutate(Predictor_Name = "logarea",
             y = scale(y)),
    gbm::plot.gbm(brt.a7, i.var = "logcounty", return.grid = TRUE) %>% 
      rename(Predictor_Value = logcounty) %>% 
      mutate(Predictor_Name = "logcounty",
             y = scale(y)),
    gbm::plot.gbm(brt.a7,i.var = "cpue", return.grid = TRUE) %>% 
      rename(Predictor_Value = cpue) %>% 
      mutate(Predictor_Name = "cpue",
             y = scale(y)),
    gbm::plot.gbm(brt.a7,i.var = "walleye", return.grid = TRUE) %>% 
      rename(Predictor_Value = walleye) %>% 
      mutate(Predictor_Name = "walleye",
             y = scale(y)),
    gbm::plot.gbm(brt.a7,i.var = "pike", return.grid = TRUE) %>% 
      rename(Predictor_Value = pike) %>% 
      mutate(Predictor_Name = "pike",
             y = scale(y)),
    gbm::plot.gbm(brt.a7,i.var = "perch", return.grid = TRUE) %>% 
      rename(Predictor_Value = perch) %>% 
      mutate(Predictor_Name = "perch",
             y = scale(y)),
    gbm::plot.gbm(brt.a7,i.var = "lmb", return.grid = TRUE) %>% 
      rename(Predictor_Value = lmb) %>% 
      mutate(Predictor_Name = "lmb",
             y = scale(y)),
    gbm::plot.gbm(brt.a7,i.var = "surf_temp_year", return.grid = TRUE) %>% 
      rename(Predictor_Value = surf_temp_year) %>% 
      mutate(Predictor_Name = "surf_temp_year",
             y = scale(y))
  ) %>% mutate(Age_Group = 7),
  bind_rows(
    #age group 8
    gbm::plot.gbm(brt.a8, i.var = "DD_mean", return.grid = TRUE) %>% 
      rename(Predictor_Value = DD_mean) %>% 
      mutate(Predictor_Name = "DD_mean",
             y = scale(y)),
    gbm::plot.gbm(brt.a8, i.var = "mean_secchi_m", return.grid = TRUE) %>% 
      rename(Predictor_Value = mean_secchi_m) %>% 
      mutate(Predictor_Name = "mean_secchi_m",
             y = scale(y)),
    gbm::plot.gbm(brt.a8, i.var = "urban", return.grid = TRUE) %>% 
      rename(Predictor_Value = urban) %>% 
      mutate(Predictor_Name = "urban",
             y = scale(y)),
    gbm::plot.gbm(brt.a8, i.var = "agriculture", return.grid = TRUE) %>% 
      rename(Predictor_Value = agriculture) %>% 
      mutate(Predictor_Name = "agriculture",
             y = scale(y)),
    gbm::plot.gbm(brt.a8, i.var = "forests", return.grid = TRUE) %>% 
      rename(Predictor_Value = forests) %>% 
      mutate(Predictor_Name = "forests",
             y = scale(y)),
    gbm::plot.gbm(brt.a8, i.var = "wetlands", return.grid = TRUE) %>% 
      rename(Predictor_Value = wetlands) %>% 
      mutate(Predictor_Name = "wetlands",
             y = scale(y)),
    gbm::plot.gbm(brt.a8, i.var = "doy", return.grid = TRUE) %>% 
      rename(Predictor_Value = doy) %>% 
      mutate(Predictor_Name = "doy",
             y = scale(y)),
    gbm::plot.gbm(brt.a8, i.var = "logdepth", return.grid = TRUE) %>% 
      rename(Predictor_Value = logdepth) %>% 
      mutate(Predictor_Name = "logdepth",
             y = scale(y)),
    gbm::plot.gbm(brt.a8, i.var = "logarea", return.grid = TRUE) %>% 
      rename(Predictor_Value = logarea) %>% 
      mutate(Predictor_Name = "logarea",
             y = scale(y)),
    gbm::plot.gbm(brt.a8, i.var = "logcounty", return.grid = TRUE) %>% 
      rename(Predictor_Value = logcounty) %>% 
      mutate(Predictor_Name = "logcounty",
             y = scale(y)),
    gbm::plot.gbm(brt.a8,i.var = "cpue", return.grid = TRUE) %>% 
      rename(Predictor_Value = cpue) %>% 
      mutate(Predictor_Name = "cpue",
             y = scale(y)),
    gbm::plot.gbm(brt.a8,i.var = "walleye", return.grid = TRUE) %>% 
      rename(Predictor_Value = walleye) %>% 
      mutate(Predictor_Name = "walleye",
             y = scale(y)),
    gbm::plot.gbm(brt.a8,i.var = "pike", return.grid = TRUE) %>% 
      rename(Predictor_Value = pike) %>% 
      mutate(Predictor_Name = "pike",
             y = scale(y)),
    gbm::plot.gbm(brt.a8,i.var = "perch", return.grid = TRUE) %>% 
      rename(Predictor_Value = perch) %>% 
      mutate(Predictor_Name = "perch",
             y = scale(y)),
    gbm::plot.gbm(brt.a8,i.var = "lmb", return.grid = TRUE) %>% 
      rename(Predictor_Value = lmb) %>% 
      mutate(Predictor_Name = "lmb",
             y = scale(y)),
    gbm::plot.gbm(brt.a8,i.var = "surf_temp_year", return.grid = TRUE) %>% 
      rename(Predictor_Value = surf_temp_year) %>% 
      mutate(Predictor_Name = "surf_temp_year",
             y = scale(y))
  ) %>% mutate(Age_Group = 8)
) %>% mutate(Age_Group = as.factor(Age_Group),
             Predictor_Name = case_when(
               Predictor_Name == "agriculture" ~ "Agriculture",
               Predictor_Name == "DD_mean" ~ "Degree Days",
               Predictor_Name == "doy" ~ "Day of Year",
               Predictor_Name == "forests" ~ "Forests",
               Predictor_Name == "logarea" ~ "Lake Area",
               Predictor_Name == "logcounty" ~ "County Population",
               Predictor_Name == "logdepth" ~ "Lake Depth",
               Predictor_Name == "mean_secchi_m" ~ "Secchi Depth",
               Predictor_Name == "month" ~ "Month",
               Predictor_Name == "urban" ~ "Urban",
               Predictor_Name == "wetlands" ~ "Wetlands",
               Predictor_Name == "cpue" ~ "Bluegill CPUE",
               Predictor_Name == "walleye" ~ "Walleye",
               Predictor_Name == "pike" ~ "Northern Pike",
               Predictor_Name == "perch" ~ "Yellow Perch",
               Predictor_Name == "lmb" ~ "Largemouth Bass",
               Predictor_Name == "surf_temp_year" ~ "Surface Temperature",
               T ~ Predictor_Name
             ))

#######Format relative influence#############################

lepmac.rel.inf <- bind_rows(
  brt.a1$contributions %>% mutate(Age_Group = 1),
  brt.a2$contributions %>% mutate(Age_Group = 2),
  brt.a3$contributions %>% mutate(Age_Group = 3),
  brt.a4$contributions %>% mutate(Age_Group = 4),
  brt.a5$contributions %>% mutate(Age_Group = 5),
  brt.a6$contributions %>% mutate(Age_Group = 6),
  brt.a7$contributions %>% mutate(Age_Group = 7),
  brt.a8$contributions %>% mutate(Age_Group = 8)
) %>% 
  mutate(Age_Group = as.factor(Age_Group)) %>% 
  mutate(Predictor_Name = case_when(
    var == "agriculture" ~ "Agriculture",
    var == "DD_mean" ~ "Degree Days",
    var == "doy" ~ "Day of Year",
    var == "forests" ~ "Forests",
    var == "logarea" ~ "Lake Area",
    var == "logcounty" ~ "County Population",
    var == "logdepth" ~ "Lake Depth",
    var == "mean_secchi_m" ~ "Secchi Depth",
    var == "month" ~ "Month",
    var == "urban" ~ "Urban",
    var == "wetlands" ~ "Wetlands",
    var == "cpue" ~ "Bluegill CPUE",
    var == "walleye" ~ "Walleye",
    var == "pike" ~ "Northern Pike",
    var == "perch" ~ "Yellow Perch",
    var == "lmb" ~ "Largemouth Bass",
    var == "surf_temp_year" ~ "Surface Temperature",
  )) %>% 
  select(-var) %>% 
  mutate(`Variable Type` = case_when(
    Predictor_Name %in% c("Agriculture", "Forests", "Urban", "Wetlands") ~ "Land Cover",
    Predictor_Name %in% c("Degree Days", "Surface Temperature") ~ "Climate",
    Predictor_Name %in% c("Day of Year") ~ "Temporal",
    Predictor_Name %in% c("Lake Area", "Lake Depth", "Secchi Depth", "County Population") ~ "Lake Attributes",
    Predictor_Name %in% c("Bluegill CPUE", "Walleye", "Northern Pike", "Yellow Perch", "Largemouth Bass") ~ "Biotic"
  )
  )

mean.rel.inf <- lepmac.rel.inf %>% 
  group_by(Predictor_Name) %>% 
  summarise(mean_rel_inf = mean(rel.inf)) %>% 
  arrange(desc(mean_rel_inf))

response.matrix.rel.inf <- lepmac.response.matrix %>% 
  left_join(mean.rel.inf) %>% 
  mutate(Predictor_Name = fct_reorder(Predictor_Name, desc(mean_rel_inf)))



#Plots##############################################
#-------Partial dependency plot-------------------

(partial.plot <- ggplot(response.matrix.rel.inf %>% 
                          rename(`Age Class` = Age_Group), 
                        aes(x = Predictor_Value, y = y, color = `Age Class`))+
    geom_line()+
    geom_rug(sides = "b", linewidth = 0.05)+
    facet_wrap(~Predictor_Name, scales = "free")+
    labs(y = "Fitted Function", x = "Predictor Values")+
    scale_color_viridis_d()+
    theme_bw()+
   theme(
     axis.text.x = element_text(size = 6),
     strip.text = element_text(size = 7)
   )
)

ggsave(filename = "~/GitHub/bluegill_growth/Figures/partial_dependency.tiff",
       partial.plot,
       dpi = 300,
       width = 200,
       height = 150,
       units = "mm")


###-------Relative influence plot-------------------------------
(rel.inf.plot <- ggplot(lepmac.rel.inf ,
                        aes(x = fct_reorder(Predictor_Name, rel.inf), 
                                             y = rel.inf, fill = `Variable Type`))+
    geom_bar(stat = "identity")+
    geom_hline(yintercept = (1/length(xx.list)*100))+
    facet_wrap(~Age_Group, scales = "free_x")+
    scale_fill_viridis_d(direction = -1)+
    labs(y = "Relative Influence", x = "Variable Name")+
    scale_y_continuous(expand = c(0,0))+
    coord_flip()+
    theme_bw()+
    theme(
      axis.text.y = element_text(size = 6)
    )
  
)

ggsave(filename = "~/GitHub/bluegill_growth/Figures/rel_inf.tiff",
       rel.inf.plot,
       dpi = 301,
       width = 200,
       height = 150,
       units = "mm")

#----Combined model summary stats-----------------------------------
brt.sum.stats <- bind_rows(
  #Age 1
  ggPerformance(brt.a1) %>% 
    mutate(Age = 1) %>% 
    rownames_to_column("Stat"),
  #Age 2
  ggPerformance(brt.a2) %>% 
    mutate(Age = 2) %>% 
    rownames_to_column("Stat"),
  #Age 3
  ggPerformance(brt.a3) %>% 
    mutate(Age = 3) %>% 
    rownames_to_column("Stat"),
  #Age 4
  ggPerformance(brt.a4) %>% 
    mutate(Age = 4) %>% 
    rownames_to_column("Stat"),
  #Age 5
  ggPerformance(brt.a5) %>% 
    mutate(Age = 5) %>% 
    rownames_to_column("Stat"),
  #Age 6
  ggPerformance(brt.a6) %>% 
    mutate(Age = 6) %>% 
    rownames_to_column("Stat"),
  #Age 7
  ggPerformance(brt.a7) %>% 
    mutate(Age = 7) %>% 
    rownames_to_column("Stat"),
  #Age 8
  ggPerformance(brt.a8) %>% 
    mutate(Age = 8) %>% 
    rownames_to_column("Stat")
) %>% 
  pivot_wider(names_from = "Stat", values_from = "Model 1") 

write.csv(brt.sum.stats, file = "Tables/brt_sum_stats.csv", row.names = F)


