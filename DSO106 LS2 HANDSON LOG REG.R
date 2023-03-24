###### DSO106 LS2 HANDSON LOGISTIC REGRESSION ######

#Import libraries
library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")

#### Read in data ###

read.csv('/Users/dorothycouch/Documents/minerals.csv')

# Antimony level = IV 
# Gold presence yes/no = DV

''''Hypothesis is that higher antimony levels mean higher possibility of gold being present nearby
Assess probability of the gold being present using these data. '''

#data is already coded for 0/1.

#### Test Assumptions ###
#Sample sz
mylogit <- glm(Gold ~ Antimony, data=minerals, family="binomial")

probabilities <- predict(mylogit, type = "response")

minerals$Predicted <- ifelse(probabilities > .5, "pos", "neg")

######RECODE new variable to numeric
minerals$PredictedR <- NA
minerals$PredictedR[minerals$Predicted=='pos'] <- 1
minerals$PredictedR[minerals$Predicted=='neg'] <- 0

######Convert Variables to Factors
minerals$PredictedR <- as.factor(minerals$PredictedR)
minerals$Gold <- as.factor(minerals$Gold)

###Create a Confusion Matrix
conf_mat <- caret::confusionMatrix(minerals$PredictedR, minerals$Gold)
conf_mat

#####OUTPUT
#Confusion Matrix and Statistics

#            Reference
#Prediction  0  1
#          0 34  8
#          1  2 20

#Accuracy : 0.8438          
#95% CI : (0.7314, 0.9224)
#No Information Rate : 0.5625          
#P-Value [Acc > NIR] : 1.615e-06       

#Kappa : 0.6748          

#Mcnemar's Test P-Value : 0.1138          
                                          
#            Sensitivity : 0.9444          
#            Specificity : 0.7143          
#         Pos Pred Value : 0.8095          
#         Neg Pred Value : 0.9091          
#             Prevalence : 0.5625          
#         Detection Rate : 0.5312          
#   Detection Prevalence : 0.6562          
#      Balanced Accuracy : 0.8294          
                                          
#       'Positive' Class : 0               

#select numeric columns
minerals1 <- minerals %>% 
  dplyr::select_if(is.numeric)

#Rename column names
predictors <- colnames(minerals1)

##Run logit
minerals1 <- minerals1 %>%
mutate(logit=log(probabilities/(1-probabilities))) %>%
gather(key= "predictors", value="predictor.value", -logit)

#Graph to test linearity
ggplot(minerals1, aes(logit, predictor.value))+
geom_point(size=.5, alpha=.5)+
geom_smooth(method= "loess")+
theme_bw()+
facet_wrap(~predictors, scales="free_y")

#met assumption
#Multicollinearity...not needed w/only 1 IV

###Independent Errors
#graph erros
plot(mylogit$residuals)

#Assumptions of independence of errors met

#Screen for outliers
infl <- influence.measures(mylogit)
summary(infl)

#OUTPUT
#Potentially influential observations of
#glm(formula = Gold ~ Antimony, family = "binomial", data = minerals) :
  
#    dfb.1_ dfb.Antm dffit   cov.r   cook.d  hat  
#3  -0.08   0.20     0.22    1.11_*  0.01    0.08
#9   0.35  -0.55    -0.57_*  0.63_*  1.46_*  0.02
#25  0.26  -0.62    -0.72_*  0.95    0.23    0.08
#34  0.38  -0.22     0.39    0.88_*  0.08    0.03
#46 -0.08   0.17     0.19    1.11_*  0.01    0.08
#57 -0.08   0.18     0.20    1.11_*  0.01    0.08
#63 -0.08   0.22     0.26    1.10_*  0.01    0.08

#If dfb.1_ or dffit values are greater than 1, 
#or if hat is greater than .3 or so, probably
#have an outlier than should be examined and possibly removed.

#Outliers assumption met

summary(mylogit)
###OUPTUT

#Call:
#  glm(formula = Gold ~ Antimony, family = "binomial", data = minerals)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.1918  -0.5462  -0.4471   0.2447   1.9748  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -2.5184     0.5958  -4.227 2.37e-05 ***
#  Antimony      1.7606     0.4883   3.606 0.000311 ***
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 87.720  on 63  degrees of freedom
#Residual deviance: 45.332  on 62  degrees of freedom
#AIC: 49.332

#Number of Fisher Scoring iterations: 7

#Looking in the Coefficients table under Antimony, you see that the p value is significant at p < .0003. 
#This means that the presence of antimony is a significant predictor of the presence of gold. 

#For every one unit increase in Antimony, the log odds of gold being present (versus absent) 
#are increased by 1.76.

#### NOTE #####
#The presence of Antimony is a significant indicater of the presence of Gold nearby (as we had guessed).
#The higher the measured level of Antimony typically has shown the higher likelyhood Gold is nearby.


