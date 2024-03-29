
#Boosted Regression Tree Code 
# Written by Elise Grabda and modified by Katelyn King and Peter Flood 

#### load libraries ####
library(dismo) #for BRT modeling
library(vip) #for plotting 
#devtools::install_github("JBjouffray/ggBRT") # only need to install once
library(ggBRT) #BRT plotting and summary statistics
library(cowplot)

### read in data that was created from 01_data cleaning scripts # 
binded<-read.csv("bluegill_data/model_data.csv")

#### BRT models for individual ages #### 

#set list of variables (16 predictors)
##year, Secchi, perch, lmb, pike, walleye,  urban, ag, forest, wetlands, logdepth, logarea, logcounty, doy, DD_mean, cpue
xx.list <- c('year', 'mean_secchi_m', 'perch', 'lmb', 'pike', 'walleye',  'urban', 'agriculture', 'forests', 'wetlands', 'logdepth', 'logarea', 'logcounty', 'doy', 'DD_mean', 'cpue')

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
brt.int4=gbm.interactions(brt.a4)
brt.simp4 = gbm.simplify(brt.a4)

#predict
ni4=nrow(subset(binded, AGE == 4)) #number of rows in the data
predset4=sample(1:ni4,100) #random draw of 100 lakes
predbrt4=predict(brt.a4simp,subset(binded, AGE == 4)[predset4,xlist_simp4],n.trees=brt.a4simp$gbm.call$best.trees, type="response") #predictions from the model to the 100 lakes
plot(predbrt4,subset(binded, AGE == 4)[predset4,5])

summary(lm(subset(binded, AGE == 4)[predset4,5] ~ predbrt4))


### Age 5 ####
brt.a5 = gbm.step(data = subset(binded, AGE == 5), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a5) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a5, main = "Age 5")

#interaction
brt.int5=gbm.interactions(brt.a5)
brt.simp5 = gbm.simplify(brt.a5)

#predict
ni5=nrow(subset(binded, AGE == 5))
predset5=sample(1:ni5,100) 
predbrt5=predict(brt.a5simp,subset(binded, AGE == 5)[predset5,xlist_simp5],n.trees=brt.a5simp$gbm.call$best.trees, type="response") 
plot(predbrt5,subset(binded, AGE == 5)[predset5,5])

summary(lm(subset(binded, AGE == 5)[predset5,5] ~ predbrt5))


### Age 6 ####
brt.a6 = gbm.step(data = subset(binded, AGE == 6), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a6) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a6, main = "Age 6")

#interactions
brt.int6=gbm.interactions(brt.a6)
brt.simp6 = gbm.simplify(brt.a6)

#predict
ni6=nrow(subset(binded, AGE == 6))
predset6=sample(1:ni6,100) 
predbrt6=predict(brt.a6simp,subset(binded, AGE == 6)[predset6,xlist_simp6],n.trees=brt.a6simp$gbm.call$best.trees, type="response") 
plot(predbrt6,subset(binded, AGE == 6)[predset6,5])

summary(lm(subset(binded, AGE == 6)[predset6,5] ~ predbrt6))


### Age 7 ####
brt.a7 = gbm.step(data = subset(binded, AGE == 7), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a7) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a7, main = "Age 7")

#interaction
brt.int7=gbm.interactions(brt.a7)
brt.simp7 = gbm.simplify(brt.a7)

#prediction
ni7=nrow(subset(binded, AGE == 7))
predset7=sample(1:ni7,100) 
predbrt7=predict(brt.a7simp,subset(binded, AGE == 7)[predset7,xlist_simp7],n.trees=brt.a7simp$gbm.call$best.trees, type="response") 
plot(predbrt7,subset(binded, AGE == 7)[predset7,5])

summary(lm(subset(binded, AGE == 7)[predset7,5] ~ predbrt7))


### Age 8 #### 
brt.a8 = gbm.step(data = subset(binded, AGE == 8), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

#cvCorrelation is how well training data predictions correlate w/ validation data
ggPerformance(brt.a8) 

#relative influence plot with bars instead of points 
ggInfluence(brt.a8, main = "Age 8")

#interaction
brt.int8=gbm.interactions(brt.a8)
brt.simp8 = gbm.simplify(brt.a8)

#prediction 
ni8=nrow(subset(binded, AGE == 8))
predset8=sample(1:ni8,100) 
predbrt8=predict(brt.a8simp,subset(binded, AGE == 8)[predset8,xlist_simp8],n.trees=brt.a8simp$gbm.call$best.trees, type="response") 
plot(predbrt8,subset(binded, AGE == 8)[predset8,5])

summary(lm(subset(binded, AGE == 8)[predset8,5] ~ predbrt8))


#### combine all ages plots #### 




