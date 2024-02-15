
# R script for: Surveying Historical Growth Data: The Effects of Climate and Other Environmental Condition Factors on Bluegill Growth
#### Boosted Regression Tree Code 
# Written by Elise Grabda and modified by Katelyn King 

#### load libraries ####
library(dismo) #for BRT modeling
library(vip) #for plotting 

### read in data that was created from 01_data cleaning scripts # 
binded<-read.csv("bluegill_data/model_data.csv")

#### BRT models for individual ages #### 

#set list of variables (15 predictors)
##year, Secchi, perch, lmb, pike, walleye,  urban, ag, forest, wetlands, logdepth, logarea, logcounty, doy, DD_mean
xx.list <- c(2,9:13,18:21,28:30,32:33)

### AGE1 ####
brt.a1 = gbm.step(data = subset(binded, AGE == 1), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", 
                  tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)
summary(brt.a1)
gbm.plot(brt.a1) #partial dependence plots 
vi(brt.a1)
vip(brt.a1, 
    num_features = 15L,
    geom = c( "point")) + 
  ggtitle("Age 1") +  
  theme_light() + 
  geom_hline(yintercept=0.0667,linetype="dashed", lwd=0.5,colour="blue") #adding critical threshold 1/15 = 0.0667

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

summary(lm(subset(binded, AGE == 1)[predset1,5] ~ predbrt1))


#### Age 2 #### 
brt.a2 = gbm.step(data = subset(binded, AGE == 2), gbm.x = xx.list, gbm.y = 'length_mean_mm', family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int2=gbm.interactions(brt.a2)
brt.simp2 = gbm.simplify(brt.a2)

if (min(brt.simp2$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp2$deviance.summary$mean,index.return=TRUE) 
  xlist_simp2=brt.simp2$pred.list[[droplist$ix[1]]] 
} else {xlist_simp2=xx.list}

brt.a2simp = gbm.step(data = subset(binded, AGE == 2), gbm.x = xlist_simp2, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)


ni2=nrow(subset(binded, AGE == 2))
predset2=sample(1:ni2,100) 
predbrt2=predict(brt.a2simp,subset(binded, AGE == 2)[predset2,xlist_simp2],n.trees=brt.a2simp$gbm.call$best.trees, type="response") 
plot(predbrt2,subset(binded, AGE == 2)[predset2,5])

summary(lm(subset(binded, AGE == 2)[predset2,5] ~ predbrt2))
####


### Age3 ####
brt.a3 = gbm.step(data = subset(binded, AGE == 3), gbm.x = xx.list, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int3 = gbm.interactions(brt.a3)
brt.simp3 = gbm.simplify(brt.a3)

if (min(brt.simp3$deviance.summary$mean)<0) { 
  droplist = sort(brt.simp3$deviance.summary$mean,index.return=TRUE) 
  xlist_simp3 = brt.simp3$pred.list[[droplist$ix[1]]] 
} else {xlist_simp3=xx.list}

brt.a3simp = gbm.step(data = subset(binded, AGE == 3), gbm.x = xlist_simp3, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

summary(brt.a3simp)

ni3=nrow(subset(binded, AGE == 3))
predset3=sample(1:ni3,100) 
predbrt3=predict(brt.a3simp,subset(binded, AGE == 3)[predset3,xlist_simp3],n.trees=brt.a3simp$gbm.call$best.trees, type="response") 
plot(predbrt3,subset(binded, AGE == 3)[predset3,5])

summary(lm(subset(binded, AGE == 3)[predset3,5] ~ predbrt3))
###


### Age 4 #### 

brt.a4 = gbm.step(data = subset(binded, AGE == 4), gbm.x = xx.list, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int4=gbm.interactions(brt.a4)
brt.simp4 = gbm.simplify(brt.a4)

if (min(brt.simp4$deviance.summary$mean)<0) { #if dropping any predictor improves model fit
  droplist=sort(brt.simp4$deviance.summary$mean,index.return=TRUE) #list the number of drops (used to find the best number)
  xlist_simp4=brt.simp4$pred.list[[droplist$ix[1]]] #returns the list of predictors after dropping the least important, which
  #can be used to fit a new model
} else {xlist_simp4=xx.list}

brt.a4simp = gbm.step(data = subset(binded, AGE == 4), gbm.x = xlist_simp4, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)



ni4=nrow(subset(binded, AGE == 4)) #number of rows in the data
predset4=sample(1:ni4,100) #random draw of 100 lakes
predbrt4=predict(brt.a4simp,subset(binded, AGE == 4)[predset4,xlist_simp4],n.trees=brt.a4simp$gbm.call$best.trees, type="response") #predictions from the model to the 100 lakes
plot(predbrt4,subset(binded, AGE == 4)[predset4,5])

summary(lm(subset(binded, AGE == 4)[predset4,5] ~ predbrt4))



### Age 5 ####
brt.a5 = gbm.step(data = subset(binded, AGE == 5), gbm.x = xx.list, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int5=gbm.interactions(brt.a5)
brt.simp5 = gbm.simplify(brt.a5)

if (min(brt.simp5$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp5$deviance.summary$mean,index.return=TRUE) 
  xlist_simp5=brt.simp5$pred.list[[droplist$ix[1]]] 
} else {xlist_simp5=xx.list}

brt.a5simp = gbm.step(data = subset(binded, AGE == 5), gbm.x = xlist_simp5, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)




ni5=nrow(subset(binded, AGE == 5))
predset5=sample(1:ni5,100) 
predbrt5=predict(brt.a5simp,subset(binded, AGE == 5)[predset5,xlist_simp5],n.trees=brt.a5simp$gbm.call$best.trees, type="response") 
plot(predbrt5,subset(binded, AGE == 5)[predset5,5])

summary(lm(subset(binded, AGE == 5)[predset5,5] ~ predbrt5))
###

### Age 6 ####
brt.a6 = gbm.step(data = subset(binded, AGE == 6), gbm.x = xx.list, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int6=gbm.interactions(brt.a6)
brt.simp6 = gbm.simplify(brt.a6)

if (min(brt.simp6$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp6$deviance.summary$mean,index.return=TRUE) 
  xlist_simp6=brt.simp6$pred.list[[droplist$ix[1]]] 
} else {xlist_simp6=xx.list}

brt.a6simp = gbm.step(data = subset(binded, AGE == 6), gbm.x = xlist_simp6, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)



ni6=nrow(subset(binded, AGE == 6))
predset6=sample(1:ni6,100) 
predbrt6=predict(brt.a6simp,subset(binded, AGE == 6)[predset6,xlist_simp6],n.trees=brt.a6simp$gbm.call$best.trees, type="response") 
plot(predbrt6,subset(binded, AGE == 6)[predset6,5])

summary(lm(subset(binded, AGE == 6)[predset6,5] ~ predbrt6))
###

### Age 7 ####
brt.a7 = gbm.step(data = subset(binded, AGE == 7), gbm.x = xx.list, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int7=gbm.interactions(brt.a7)
brt.simp7 = gbm.simplify(brt.a7)

if (min(brt.simp7$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp7$deviance.summary$mean,index.return=TRUE) 
  xlist_simp7=brt.simp7$pred.list[[droplist$ix[1]]] 
} else {xlist_simp7=xx.list}

brt.a7simp = gbm.step(data = subset(binded, AGE == 7), gbm.x = xlist_simp7, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)



ni7=nrow(subset(binded, AGE == 7))
predset7=sample(1:ni7,100) 
predbrt7=predict(brt.a7simp,subset(binded, AGE == 7)[predset7,xlist_simp7],n.trees=brt.a7simp$gbm.call$best.trees, type="response") 
plot(predbrt7,subset(binded, AGE == 7)[predset7,5])

summary(lm(subset(binded, AGE == 7)[predset7,5] ~ predbrt7))
###

### Age 8 #### 
brt.a8 = gbm.step(data = subset(binded, AGE == 8), gbm.x = xx.list, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                  n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)

brt.int8=gbm.interactions(brt.a8)
brt.simp8 = gbm.simplify(brt.a8)

if (min(brt.simp1$deviance.summary$mean)<0) { 
  droplist=sort(brt.simp8$deviance.summary$mean,index.return=TRUE) 
  xlist_simp8=brt.simp8$pred.list[[droplist$ix[1]]] 
} else {xlist_simp8 = xx.list}

brt.a8simp = gbm.step(data = subset(binded, AGE == 8), gbm.x = xlist_simp8, gbm.y = 5, family = "gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5,
                      n.folds = 10, n.trees = 50, step.size = 100, max.trees = 1000000, tolerance = 0.001)



ni8=nrow(subset(binded, AGE == 8))
predset8=sample(1:ni8,100) 
predbrt8=predict(brt.a8simp,subset(binded, AGE == 8)[predset8,xlist_simp8],n.trees=brt.a8simp$gbm.call$best.trees, type="response") 
plot(predbrt8,subset(binded, AGE == 8)[predset8,5])

summary(lm(subset(binded, AGE == 8)[predset8,5] ~ predbrt8))