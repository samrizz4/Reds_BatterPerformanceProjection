#Sam Rizzuto
#Cincinnati Reds
#Technical Assessment: 2
#21 January 2022


#load libraries
library(ggplot2)
library(dplyr)
library(mgcv)
library(arm)
library(gridExtra)
library(randomForest)
library(e1071)
library(caret)
library(rminer)
library(scales)

#set working directory
setwd("~/Desktop/22-ds")

#load in datasets
trainDF <- read.csv("train.csv")
testDF <- read.csv("test.csv")

#Calc prob for 1b, 2b, 3b, hr
#doesn't account for shift

summary(trainDF)

###########Filtering Data and Removing Outliers through IQR
Q1_Angle <- quantile(trainDF$ANGLE, .25)
Q3_Angle <- quantile(trainDF$ANGLE, .75)
IQR_Angle <- IQR(trainDF$ANGLE)

Q1_EXIT_SPEED <- quantile(trainDF$EXIT_SPEED, .25)
Q3_EXIT_SPEED <- quantile(trainDF$EXIT_SPEED, .75)
IQR_EXIT_SPEED <- IQR(trainDF$EXIT_SPEED)

Q1_DIRECTION <- quantile(trainDF$DIRECTION, .25)
Q3_DIRECTION <- quantile(trainDF$DIRECTION, .75)
IQR_DIRECTION <- IQR(trainDF$DIRECTION)

trainDF <- subset(trainDF, trainDF$ANGLE > (Q1_Angle - 1.5*IQR_Angle) & trainDF$ANGLE < (Q3_Angle + 1.5*IQR_Angle))
trainDF <- subset(trainDF, trainDF$EXIT_SPEED > (Q1_EXIT_SPEED - 1.5*IQR_EXIT_SPEED) & trainDF$EXIT_SPEED < (Q3_EXIT_SPEED + 1.5*IQR_EXIT_SPEED))
trainDF <- subset(trainDF, trainDF$DIRECTION > (Q1_DIRECTION - 1.5*IQR_DIRECTION) & trainDF$DIRECTION < (Q3_DIRECTION + 1.5*IQR_DIRECTION))

#disregard strikeouts, hbp, walks
trainDF <- trainDF %>% filter(PITCH_RESULT_KEY == "InPlay")







###########add column of binary 1,0 whether the PA resulted in hit or not
trainDF <- trainDF %>% mutate(`Hit_YesNo` = if_else(X1B == 1, 1, if_else(X2B == 2, 1, 
                                                                      if_else(X3B == 1, 1,
                                                                              if_else(HR == 1, 1, 0)))))
#convert from numeric to factor
trainDF$`Hit_YesNo` <- as.factor(trainDF$`Hit_YesNo`)

#plot angle vs exit velo based on whether it is a hit or not
plot1 <- ggplot(trainDF, aes(EXIT_SPEED, ANGLE, color = `Hit_YesNo`)) + geom_point() +
  ggtitle("Launch Angle vs Exit Speed Based on Hit or Out")
plot1
ggsave("launchAngle_ExitSpeedBasedOnHitOrOut.png")



###########generalized additive model to include most important variables in training dataset
mod1 <- gam(`Hit_YesNo` ~ s(RELEASE_SPEED, PLATE_X, PLATE_Z, INDUCED_VERTICAL_BREAK, 
                         HORIZONTAL_BREAK, EXIT_SPEED, ANGLE), data = trainDF, family = binomial)
#direction, slash line, stadium,VERTICAL_APPROACH_ANGLE, HORIZONTAL_APPROACH_ANGLE, 

mod1
summary(mod1) #r^2 adj of 0.269

 
predLogVals <- predict(mod1, EXIT_SPEED = 110, ANGLE = 25)[1] #predicted logit value of hit with given exit velo=110 & launch angle of 25

#inverse log the coefficient
1-invlogit(predLogVals) # 84.5% prob of hit given the above parameters


###########a way to find predicted hit values for given parameters
###########ineffecient on large dataset, will take 220 hours to run through entire dataset
#for (i in 1:nrow(trainDF)) {
#  print(i)
#  predLogVals <- 1-invlogit(predict(mod1, EXIT_SPEED = trainDF$EXIT_SPEED[i], ANGLE = trainDF$ANGLE[i])[1])
#  
#}

#group exit velo based on range of every 10 mph
trainDF <- trainDF %>% mutate(exitSpeedR = if_else(EXIT_SPEED <= 65, 60,
                                                   if_else(EXIT_SPEED <= 75 & EXIT_SPEED > 65, 70,
                                                           if_else(EXIT_SPEED <= 85 & EXIT_SPEED > 75, 80,
                                                                   if_else(EXIT_SPEED <= 95 & EXIT_SPEED > 85, 90,
                                                                           if_else(EXIT_SPEED <= 105 & EXIT_SPEED > 95, 100,
                                                                                   if_else(EXIT_SPEED <= 115 & EXIT_SPEED > 105, 110,
                                                                                           if_else(EXIT_SPEED <= 125 & EXIT_SPEED > 115, 120,
                                                                                                   if_else(EXIT_SPEED <= 135 & EXIT_SPEED > 125, 130, 140)))))))))
#group launch angle based on range of every 10 degrees
trainDF <- trainDF %>% mutate(angleR = if_else(ANGLE <= -65, -70,
                                               if_else(ANGLE <= -55 & ANGLE > -65, -60,
                                                   if_else(ANGLE <= -45 & ANGLE > -55, -50,
                                                           if_else(ANGLE <= -35 & ANGLE > -45, -40,
                                                                   if_else(ANGLE <= -25 & ANGLE > -35, -30,
                                                                           if_else(ANGLE <= -15 & ANGLE > -25, -20,
                                                                                   if_else(ANGLE <= -5 & ANGLE > -15, -10,
                                                                                           if_else(ANGLE <= 5 & ANGLE > -5, 0,
                                                                                                   if_else(ANGLE <= 15 & ANGLE > 5, 10,
                                                                                                           if_else(ANGLE <= 25 & ANGLE > 15, 20,
                                                                                                                   if_else(ANGLE <= 35 & ANGLE > 25, 30, 
                                                                                                                           if_else(ANGLE <= 45 & ANGLE > 35, 40, 50)))))))))))))



summary(trainDF$ANGLE)

trainDF$HR <- as.factor(trainDF$HR)
ggplot(trainDF, aes(DIRECTION, angleR,color = HR)) + geom_point() +
  ggtitle("Graph Showing Most Likely Angles/Exit Speeds to Result in Home Run") + ylab("ANGLE")
#can be repeated for 1b, 2b, 3b

ggsave("mostLikelyAngles_ExitSpeedsResultHR.png")






#distance = ( (3.2804 * sin(2*(ANGLE*(pi/180))) * (EXIT_SPEED*0.44704)^2 ) / 9.8)
#meters to feet = 3.2804
#mph to m/s = 0.44704
#gravity = 9.8 m/s

###########calculate distance of ball traveled in feet
#not including air resistance (air resistance = 0 in scenario)
#exit speed mph
#convert from radians to degrees
#9.8 gravity convert from m/s to mph = 21.922
trainDF <- trainDF %>% mutate(distance = (sin(2*(ANGLE*(pi/180)))*(EXIT_SPEED^2))/21.922)


ggplot(trainDF, aes(DIRECTION,distance,color = HR)) + geom_point() + ylab("DISTANCE (ft)") + 
  ggtitle("Plot of Home Runs Based On Distance and Direction")

ggsave("homeRunsBasedOnDistanceDirection.png")

#creating an outcome of pa column
trainDF <- trainDF %>% mutate(Outcome = as.factor(if_else(X1B == 1, "Single",
                                                          if_else(X2B == 1, "Double",
                                                                  if_else(X3B == 1, "Triple",
                                                                          if_else(HR == 1, "Home Run", "Out"))))))

#refactoring levels to rearrange order in legend
trainDF$Outcome <- factor(trainDF$Outcome, levels = c("Out","Single","Double","Triple","Home Run"))
ggplot(trainDF, aes(DIRECTION, distance, color = Outcome)) + geom_point() + 
  coord_polar(theta = "x", start = pi, direction = 1, clip = "off") + xlim(-180,180) +
  ggtitle("Outcome of Each PA, Represented in Shape of Field") + ylab("DISTANCE (ft)")
#center of circle is (0, 0), distance is shifted

ggsave("paOutcomeShapeOfField.png")

######################GLM model
############prob of single
singleCalc <- glm(X1B ~ distance*ANGLE + EXIT_SPEED + DIRECTION, 
                   data = trainDF, 
                   family = binomial)
singleCalc
summary(singleCalc)
singleCalc$coefficients #coefficients used to determine probability of hits


############Prob of double
doubleCalc <- glm(X2B ~ distance*ANGLE + EXIT_SPEED + DIRECTION, 
                  data = trainDF, 
                  family = binomial)
doubleCalc
summary(doubleCalc)
doubleCalc$coefficients 

############prob of triple
tripleCalc <- glm(X3B ~ distance*ANGLE + EXIT_SPEED + DIRECTION, 
                  data = trainDF, 
                  family = binomial)
tripleCalc
summary(tripleCalc)
tripleCalc$coefficients


############prob of hr
hrCalc <- glm(HR ~ distance*ANGLE + EXIT_SPEED + DIRECTION, 
                  data = trainDF, 
                  family = binomial)
hrCalc
summary(hrCalc)
hrCalc$coefficients


#log
trainDF <- trainDF %>% 
  mutate(probabiltySingle_GLM = round(exp(-0.7376997503 + (0.0008686946*distance) + 
                                            (0.0169380748*ANGLE) + (0.0059603225*EXIT_SPEED) + 
                                            (0.0019138319*DIRECTION) - 0.0003038377) / (1 + exp(-0.7376997503 + (0.0008686946*distance) + 
                                                                                                           (0.0169380748*ANGLE) + (0.0059603225*EXIT_SPEED) + 
                                                                                                           (0.0019138319*DIRECTION) - 0.0003038377)),3))

trainDF <- trainDF %>% 
  mutate(probabiltyDouble_GLM = round(exp(-5.7519999323 + (0.0109285819*distance) +
                                          (-0.0059862407*ANGLE) + (0.0312272614*EXIT_SPEED) +
                                          (-0.0103714825*DIRECTION) - 0.0002850389) / (1 + exp(-5.7519999323 + (0.0109285819*distance) +
                                                                                                          (-0.0059862407*ANGLE) + (0.0312272614*EXIT_SPEED) +
                                                                                                          (-0.0103714825*DIRECTION) - 0.0002850389)),3))
                                
trainDF <- trainDF %>% 
  mutate(probabiltyTriple_GLM = round(exp(-7.8669984929 + (0.0138750297*distance) +
                                          (-0.0369037772*ANGLE) + (0.0211161497*EXIT_SPEED) +
                                          (0.0338612093*DIRECTION) - 0.0002127065) / (1 + exp(-7.8669984929 + (0.0138750297*distance) +
                                                                                                         (-0.0369037772*ANGLE) + (0.0211161497*EXIT_SPEED) +
                                                                                                         (0.0338612093*DIRECTION) - 0.0002127065)),3))

trainDF <- trainDF %>% 
  mutate(probabiltyHR_GLM = round(exp(-25.345192516 + (0.106382973*distance) + (0.346910890*ANGLE) +
                                      (-0.053218299*EXIT_SPEED) + (-0.018464555*DIRECTION) - 0.001803663) / (1 + exp(-25.345192516 + (0.106382973*distance) + (0.346910890*ANGLE) +
                                                                                                                       (-0.053218299*EXIT_SPEED) + (-0.018464555*DIRECTION) - 0.001803663)),3))

# mean(singleCalc$residuals^2)
# mean(doubleCalc$residuals^2)
# mean(tripleCalc$residuals^2)
# mean(hrCalc$residuals^2)









#############Random Forest model
#copying training dataset into new df
trainReset <- read.csv("train.csv")
###########Filtering Data and Removing Outliers through IQR
Q1_Angle <- quantile(trainReset$ANGLE, .25)
Q3_Angle <- quantile(trainReset$ANGLE, .75)
IQR_Angle <- IQR(trainReset$ANGLE)
Q1_EXIT_SPEED <- quantile(trainReset$EXIT_SPEED, .25)
Q3_EXIT_SPEED <- quantile(trainReset$EXIT_SPEED, .75)
IQR_EXIT_SPEED <- IQR(trainReset$EXIT_SPEED)
Q1_DIRECTION <- quantile(trainReset$DIRECTION, .25)
Q3_DIRECTION <- quantile(trainReset$DIRECTION, .75)
IQR_DIRECTION <- IQR(trainReset$DIRECTION)
trainReset <- subset(trainReset, trainReset$ANGLE > (Q1_Angle - 1.5*IQR_Angle) & trainReset$ANGLE < (Q3_Angle + 1.5*IQR_Angle))
trainReset <- subset(trainReset, trainReset$EXIT_SPEED > (Q1_EXIT_SPEED - 1.5*IQR_EXIT_SPEED) & trainReset$EXIT_SPEED < (Q3_EXIT_SPEED + 1.5*IQR_EXIT_SPEED))
trainReset <- subset(trainReset, trainReset$DIRECTION > (Q1_DIRECTION - 1.5*IQR_DIRECTION) & trainReset$DIRECTION < (Q3_DIRECTION + 1.5*IQR_DIRECTION))
#disregard strikeouts, hbp, walks
trainReset <- trainReset %>% filter(PITCH_RESULT_KEY == "InPlay")
#removing categorical variables
trainReset <- trainReset[-c(22,23)]
#running random forest on four hit prob types
rfALLtrain1B <- randomForest(X1B ~ ., data = trainReset)
rfALLtrain2B <- randomForest(X2B ~ ., data = trainReset)
rfALLtrain3B <- randomForest(X3B ~ ., data = trainReset)
rfALLtrainHR <- randomForest(HR ~ ., data = trainReset)
#viewing importance plots of each hit type to determine most significant vars in model
varImpPlot(rfALLtrain1B)
varImpPlot(rfALLtrain2B)
varImpPlot(rfALLtrain3B)
varImpPlot(rfALLtrainHR)

#choose first grouping vars
rfUpdated1B <- randomForest(X1B ~ ANGLE + 
                               EXIT_SPEED +
                               DIRECTION +
                               PLATE_X +
                               PLATE_Z +
                               INDUCED_VERTICAL_BREAK + 
                               HORIZONTAL_APPROACH_ANGLE + 
                               RELEASE_SPEED +
                               HORIZONTAL_BREAK + 
                               VERTICAL_APPROACH_ANGLE, data = trainReset)
varImpPlot(rfUpdated1B)
rfUpdated1B$terms
importance(rfUpdated1B) #importance vars of rf singles model
plot(rfUpdated1B) #error of rf singles model


rfUpdated2B <- randomForest(X2B ~ ANGLE + 
                              DIRECTION + 
                              EXIT_SPEED + 
                              PLATE_X + 
                              PLATE_Z + 
                              HORIZONTAL_APPROACH_ANGLE + 
                              INDUCED_VERTICAL_BREAK + 
                              HORIZONTAL_BREAK + 
                              RELEASE_SPEED + 
                              VERTICAL_APPROACH_ANGLE, data = trainReset)

rfUpdated3B <- randomForest(X3B ~ DIRECTION + 
                              EXIT_SPEED + 
                              ANGLE + 
                              HORIZONTAL_APPROACH_ANGLE + 
                              PLATE_Z + 
                              RELEASE_SPEED + 
                              PLATE_X + 
                              HORIZONTAL_BREAK + 
                              INDUCED_VERTICAL_BREAK, data = trainReset)


rfUpdatedHR <- randomForest(HR ~ ANGLE + 
                              EXIT_SPEED + 
                              DIRECTION, data = trainReset)



#plot randomForest results back in trainDF
trainDF <- trainDF %>% 
  mutate(predictedSingle_RF = round(predict(rfUpdated1B, newdata = .),3))
trainDF <- trainDF %>% 
  mutate(predictedDouble_RF = round(predict(rfUpdated2B, newdata = .),3))
trainDF <- trainDF %>% 
  mutate(predictedTriple_RF = round(predict(rfUpdated3B, newdata = .),3))
trainDF <- trainDF %>% 
  mutate(predictedHR_RF = round(predict(rfUpdatedHR, newdata = .),3))



#############confusion matrix
#singles accuracy table
table1B <- table(round(rfUpdated1B$predicted), trainDF$X1B)
table1B
sum(diag(table1B))/sum(table1B) #accuracy of singles prediction = 85.04%

#doubles accuracy table
table2B <- table(round(rfUpdated2B$predicted), trainDF$X2B)
table2B
sum(diag(table2B))/sum(table2B) #accuracy of doubles prediction = 93.09%

#triples accuracy table
table3B <- table(round(rfUpdated3B$predicted), trainDF$X3B)
table3B
sum(diag(table3B))/sum(table3B) #accuracy of triples prediction = 99.13%

#HRs accuracy table
tableHR <- table(round(rfUpdatedHR$predicted), trainDF$HR)
tableHR
sum(diag(tableHR))/sum(tableHR) #accuracy of HRs prediction = 97.18%



#Running random forest model on whether PA was hit or not
trainReset <- trainReset %>% mutate(`Hit_YesNo` = if_else(X1B == 1, 1, if_else(X2B == 2, 1, 
                                                                         if_else(X3B == 1, 1,
                                                                                 if_else(HR == 1, 1, 0)))))
rfALLtrainHit <- randomForest(as.numeric(`Hit_YesNo`) ~ ., data = trainReset)
varImpPlot(rfALLtrainHit)

#trimming down to importance vars
rfUpdatedHit <- randomForest(as.numeric(`Hit_YesNo`) ~ ANGLE + 
                               EXIT_SPEED +
                               DIRECTION, data = trainReset)

#hits accuracy table
tableHits <- table(round(rfUpdatedHit$predicted), trainDF$`Hit_YesNo`)
tableHits
sum(diag(tableHits))/sum(tableHits) #accuracy of hits prediction = 81.38%

trainDF <- trainDF %>% 
  mutate(predictedHit_RF = round(predict(rfUpdatedHit, newdata = .),3))




trainDF <- trainDF %>% rename(`Exit Speed` = exitSpeedR,
                              `Predicted Hit - RF` = predictedHit_RF)
trainDF$`Exit Speed` <- as.factor(trainDF$`Exit Speed`)
ggplot(trainDF, aes(DIRECTION, `Predicted Hit - RF`, color = `Exit Speed`)) + geom_point() + ylab("Hit Probability") +
  xlab("Direction") + ggtitle("Hit Probability vs Direction By Exit Speed of Ball")

ggsave("hitProb_DirectionByExitSpeed.png")


#making new column concatenating by rounded angle value and the string `Angle:`
trainDF <- trainDF %>% mutate(angleString = paste("Angle:", angleR))


ggplot(trainDF, aes(DIRECTION, `Predicted Hit - RF`, color = `Exit Speed`, group = `Exit Speed`)) + 
  geom_point() + facet_wrap(~ as.factor(angleString)) + ylab("Hit Probability") +
  xlab("Direction") + ggtitle("Probability of Hit Comparing Direction of Ball Off Bat and \n Launch Angle of Swing Grouped by Exit Velo")

ggsave("probHitComparingDirection_AngleGroupedByExitVelo.png")






#############SVM model
svmFullSingle <- svm(X1B ~ ANGLE + 
                       EXIT_SPEED +
                       DIRECTION +
                       PLATE_X +
                       PLATE_Z +
                       INDUCED_VERTICAL_BREAK + 
                       HORIZONTAL_APPROACH_ANGLE + 
                       HORIZONTAL_BREAK + 
                       RELEASE_SPEED + 
                       VERTICAL_APPROACH_ANGLE, data = trainDF, cost = 100, gamma = 1)
svmFullSingle

#removing predictor variable of single
svmFullSingle_Pred <- predict(svmFullSingle, trainDF[,-25])
#creating confusion matrix
confMatSingle <- confusionMatrix(as.factor(round(svmFullSingle_Pred)), as.factor(trainDF$X1B), 
                                 dnn = c("Prediction", "Reference"))
#display conf matrix
confMatSingle$table
#add svm prob of single into training df
trainDF <- trainDF %>% mutate(`Prob Single: SVM` = round(rescale(svmFullSingle_Pred, to = c(0, 1)),3))


#running svm on doubles
svmFullDouble <- svm(X2B ~ ANGLE + 
                       EXIT_SPEED +
                       DIRECTION +
                       PLATE_X +
                       PLATE_Z +
                       INDUCED_VERTICAL_BREAK + 
                       HORIZONTAL_APPROACH_ANGLE + 
                       HORIZONTAL_BREAK + 
                       RELEASE_SPEED + 
                       VERTICAL_APPROACH_ANGLE, data = trainDF, cost = 100, gamma = 1)
svmFullDouble
#removing predictor variable of double
svmFullDouble_Pred <- predict(svmFullDouble, trainDF[,-26])
#creating conf matrix
confMatDouble <- confusionMatrix(as.factor(round(svmFullDouble_Pred)), as.factor(trainDF$X2B), 
                                 dnn = c("Prediction", "Reference"))
confMatDouble$table
#adding prob of double into training df
trainDF <- trainDF %>% mutate(`Prob Double: SVM` = round(rescale(svmFullDouble_Pred, to = c(0, 1)),3))



#running svm model on triples
svmFullTriple <- svm(X3B ~ ANGLE + 
                       EXIT_SPEED +
                       DIRECTION +
                       PLATE_X +
                       PLATE_Z +
                       INDUCED_VERTICAL_BREAK + 
                       HORIZONTAL_APPROACH_ANGLE + 
                       HORIZONTAL_BREAK + 
                       RELEASE_SPEED + 
                       VERTICAL_APPROACH_ANGLE, data = trainDF, cost = 100, gamma = 1)

svmFullTriple
#removing predictor variable of triple
svmFullTriple_Pred <- predict(svmFullTriple, trainDF[,-27])
#creating conf matrix
confMatTriple <- confusionMatrix(as.factor(round(svmFullTriple_Pred)), as.factor(trainDF$X3B), 
                                 dnn = c("Prediction", "Reference"))
confMatTriple$table
#adding prob of triple into training df
trainDF <- trainDF %>% mutate(`Prob Triple: SVM` = round(rescale(svmFullTriple_Pred, to = c(0, 1)),3))



#checker if hr is in 1/2 levels not 0/1
trainDF$HR <- if_else(trainDF$HR == 1, 0, 1)

#running svm on hr
svmFullHR <- svm(HR ~ ANGLE + 
                       EXIT_SPEED +
                       DIRECTION +
                       PLATE_X +
                       PLATE_Z +
                       INDUCED_VERTICAL_BREAK + 
                       HORIZONTAL_APPROACH_ANGLE + 
                       HORIZONTAL_BREAK + 
                       RELEASE_SPEED + 
                       VERTICAL_APPROACH_ANGLE, data = trainDF, cost = 100, gamma = 1)

svmFullHR
#removing predictor variable of home run
svmFullHR_Pred <- predict(svmFullHR, trainDF[,-28])
#creating conf matrix
confMatHR <- confusionMatrix(as.factor(round(svmFullHR_Pred)), as.factor(trainDF$HR), 
                                 dnn = c("Prediction", "Reference"))
confMatHR$table
#adding prob of hr into training df
trainDF <- trainDF %>% mutate(`Prob HR: SVM` = round(rescale(svmFullHR_Pred, to = c(0, 1)),3))






##################Testing Data
summary(trainDF)
#remake glm model on first, double, triple, hr for vars in test df
probHitGLM_Train1B <- glm(X1B ~ RELEASE_SPEED + PLATE_X + PLATE_Z + INDUCED_VERTICAL_BREAK +
                          HORIZONTAL_BREAK + VERTICAL_APPROACH_ANGLE + HORIZONTAL_APPROACH_ANGLE +
                        OUTS + BALLS + STRIKES + PITCH_NUMBER, 
                  data = trainDF, 
                  family = binomial)
probHitGLM_Train2B <- glm(X2B ~ RELEASE_SPEED + PLATE_X + PLATE_Z + INDUCED_VERTICAL_BREAK +
                            HORIZONTAL_BREAK + VERTICAL_APPROACH_ANGLE + HORIZONTAL_APPROACH_ANGLE +
                            OUTS + BALLS + STRIKES + PITCH_NUMBER, 
                          data = trainDF, 
                          family = binomial)
probHitGLM_Train3B <- glm(X3B ~ RELEASE_SPEED + PLATE_X + PLATE_Z + INDUCED_VERTICAL_BREAK +
                            HORIZONTAL_BREAK + VERTICAL_APPROACH_ANGLE + HORIZONTAL_APPROACH_ANGLE +
                            OUTS + BALLS + STRIKES + PITCH_NUMBER, 
                          data = trainDF, 
                          family = binomial)
probHitGLM_TrainHR <- glm(HR ~ RELEASE_SPEED + PLATE_X + PLATE_Z + INDUCED_VERTICAL_BREAK +
                            HORIZONTAL_BREAK + VERTICAL_APPROACH_ANGLE + HORIZONTAL_APPROACH_ANGLE +
                            OUTS + BALLS + STRIKES + PITCH_NUMBER, 
                          data = trainDF, 
                          family = binomial)

#find coefficients for prob of each type of hit
round(probHitGLM_Train1B$coefficients,5)
round(probHitGLM_Train2B$coefficients,5)
round(probHitGLM_Train3B$coefficients,5)
round(probHitGLM_TrainHR$coefficients,5)

#calculate prob of each hit through glm in each newly created column 
testDF <- testDF %>% mutate(`Prob of Single` = round(exp(-3.58791 + (0.02893 * RELEASE_SPEED) + (0.04570 * PLATE_X) + 
                              (-0.10962 * PLATE_Z) + (-0.01308 * INDUCED_VERTICAL_BREAK) + (-0.00027 * HORIZONTAL_BREAK) + 
                              (-0.02931 * VERTICAL_APPROACH_ANGLE) + (-0.00809 * HORIZONTAL_APPROACH_ANGLE) + (-0.02993 * OUTS) + 
                              (0.01702 * BALLS) + (0.10875 * STRIKES) + (-0.03950 * PITCH_NUMBER) ) / (1 + exp(-3.58791 + (0.02893 * RELEASE_SPEED) + (0.04570 * PLATE_X) + 
                                                                                                                 (-0.10962 * PLATE_Z) + (-0.01308 * INDUCED_VERTICAL_BREAK) + (-0.00027 * HORIZONTAL_BREAK) + 
                                                                                                                 (-0.02931 * VERTICAL_APPROACH_ANGLE) + (-0.00809 * HORIZONTAL_APPROACH_ANGLE) + (-0.02993 * OUTS) + 
                                                                                                                 (0.01702 * BALLS) + (0.10875 * STRIKES) + (-0.03950 * PITCH_NUMBER) )),3))

testDF <- testDF %>% mutate(`Prob of Double` = round(exp(-1.70162 + (-0.01024 * RELEASE_SPEED) + (-0.19487 * PLATE_X) + 
                                                         (-0.00974 * PLATE_Z) + (0.01121 * INDUCED_VERTICAL_BREAK) + (-0.00193 * HORIZONTAL_BREAK) + 
                                                         (-0.03076 * VERTICAL_APPROACH_ANGLE) + (0.01238 * HORIZONTAL_APPROACH_ANGLE) + (-0.02813 * OUTS) + 
                                                         (0.07493 * BALLS) + (0.04723 * STRIKES) + (-0.06484 * PITCH_NUMBER)) / (1 + exp(-1.70162 + (-0.01024 * RELEASE_SPEED) + (-0.19487 * PLATE_X) + 
                                                                                                                                           (-0.00974 * PLATE_Z) + (0.01121 * INDUCED_VERTICAL_BREAK) + (-0.00193 * HORIZONTAL_BREAK) + 
                                                                                                                                           (-0.03076 * VERTICAL_APPROACH_ANGLE) + (0.01238 * HORIZONTAL_APPROACH_ANGLE) + (-0.02813 * OUTS) + 
                                                                                                                                           (0.07493 * BALLS) + (0.04723 * STRIKES) + (-0.06484 * PITCH_NUMBER))),3))

testDF <- testDF %>% mutate(`Prob of Triple` = round(exp(-2.89301 + (-0.01751 * RELEASE_SPEED) + (0.37832 * PLATE_X) + 
                                                         (0.04209 * PLATE_Z) + (.00476 * INDUCED_VERTICAL_BREAK) + (-0.00270 * HORIZONTAL_BREAK) + 
                                                         (0.08278 * VERTICAL_APPROACH_ANGLE) + (-0.00138 * HORIZONTAL_APPROACH_ANGLE) + (0.08373 * OUTS) + 
                                                         (0.15181 * BALLS) + (0.25904 * STRIKES) + (-0.14086 * PITCH_NUMBER)) / (1 + exp(-2.89301 + (-0.01751 * RELEASE_SPEED) + (0.37832 * PLATE_X) + 
                                                                                                                                           (0.04209 * PLATE_Z) + (.00476 * INDUCED_VERTICAL_BREAK) + (-0.00270 * HORIZONTAL_BREAK) + 
                                                                                                                                           (0.08278 * VERTICAL_APPROACH_ANGLE) + (-0.00138 * HORIZONTAL_APPROACH_ANGLE) + (0.08373 * OUTS) + 
                                                                                                                                           (0.15181 * BALLS) + (0.25904 * STRIKES) + (-0.14086 * PITCH_NUMBER))),3))


testDF <- testDF %>% mutate(`Prob of Home Run` = round(exp(0.60130 + (-0.04438 * RELEASE_SPEED) + (-0.10141 * PLATE_X) + 
                                                       (0.32469 * PLATE_Z) + (0.01772 * INDUCED_VERTICAL_BREAK) + (0.00589 * HORIZONTAL_BREAK) + 
                                                       (0.05973 * VERTICAL_APPROACH_ANGLE) + (0.01628 * HORIZONTAL_APPROACH_ANGLE) + (0.06094 * OUTS) + 
                                                       (0.12887 * BALLS) + (-0.38385 * STRIKES) + (0.03728 * PITCH_NUMBER)) / (1 + exp(0.60130 + (-0.04438 * RELEASE_SPEED) + (-0.10141 * PLATE_X) + 
                                                                                                                                         (0.32469 * PLATE_Z) + (0.01772 * INDUCED_VERTICAL_BREAK) + (0.00589 * HORIZONTAL_BREAK) + 
                                                                                                                                         (0.05973 * VERTICAL_APPROACH_ANGLE) + (0.01628 * HORIZONTAL_APPROACH_ANGLE) + (0.06094 * OUTS) + 
                                                                                                                                         (0.12887 * BALLS) + (-0.38385 * STRIKES) + (0.03728 * PITCH_NUMBER))),3))
############in the future, I would not have hard coded these coefficients,
############instead, would have looked like: probHitGLM_TrainHR$coefficients[1] + (probHitGLM_TrainHR$coefficients[2] * RELEASE_SPEED) * ...



#reordering columns to fit my format
trainDF <- trainDF[c(1:33,43,34:42,44:47)]
#renaming columns to fit my format
trainDF <- trainDF %>% rename(probabilitySingle_RF = predictedSingle_RF,
                              probabilityDouble_RF = predictedDouble_RF,
                              probabilityTriple_RF = predictedTriple_RF,
                              probabilityHR_RF = predictedHR_RF,
                              probabilityHit_RF = `Predicted Hit - RF`,
                              probabilitySingle_SVM = `Prob Single: SVM`,
                              probabilityDouble_SVM = `Prob Double: SVM`,
                              probabilityTriple_SVM = `Prob Triple: SVM`,
                              probabilityHR_SVM = `Prob HR: SVM`)

write.csv(trainDF, "myTrainDF.csv")
write.csv(testDF, "myTestDF.csv")


