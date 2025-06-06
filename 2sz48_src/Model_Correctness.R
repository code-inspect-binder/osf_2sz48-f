## ***********************
## ** CORRECTNESS MODEL **
## ***********************
##
## Fisher, Haegeli and Mair: 
## Impact of information presentation on interpretability of spatial
## hazard information: Lessons from a study in avalanche safety
## April 14, 2021


## ---- 1) PREPARATION -----

## Necessary packages
if (!require("glmmTMB")) {install.packages("glmmTMB"); require("glmmTMB")}              ## for model estimation
if (!require("car")) {install.packages("car"); require("car")}                          ## for Anova call
if (!require("effects")) {install.packages("effects"); require("effects")}              ## for overview effects plots
if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}              ## for estimating marginal means
if (!require("performance")) {install.packages("performance"); require("performance")}  ## for ICC computation
if (!require("MuMIn")) {install.packages("MuMIn"); require("MuMIn")}                    ## for R2 computation 
if (!require("DHARMa")) {install.packages("DHARMa"); require("DHARMa")}                 ## for residual checks for GLMM  

## Setting contrasts to dummy coding for all types of factors 
options(contrasts=c('contr.treatment','contr.treatment'))

## Information for output file and open sink
WriteToFile <- T
OutputFile <- "Model_Correctness_Output.txt"
if (WriteToFile) sink(OutputFile, split = T)

## Load data
load("ProcessedSurveyData.RData")

## Overview
str(Rank)
nrow(Rank)
length(unique(Rank$Id))

## Basic statistics of response variable ResultCat
table(Rank$ResultCat)
round(100*prop.table(table(Rank$ResultCat)), 1)
plot(Rank$ResultCat, main = "Distribution of target variable: ResultCat")


## ---- 2) MODEL ESTIMATION ----

## Model estimation with training and interactions
CorResult <- glmmTMB(ResultCat ~ FormatInfo*BackgrAvTraining + FormatFeedback*BackgrAvTraining + 
                     RouteType + Set + MapTest + BackgrActivity_1 + UsedPhone + (1|Id) + (1|ScenID), 
                     family = binomial, data = Rank)

## Model overview
Anova(CorResult)
# -> Interactions not significant -> eliminate


## Model estimation without interactions
CorResult <- glmmTMB(ResultCat ~ FormatInfo + FormatFeedback + BackgrAvTraining + 
                       RouteType + Set + MapTest + BackgrActivity_1 + UsedPhone + (1|Id) + (1|ScenID), 
                     family = binomial, data = Rank)

## Model overview
Anova(CorResult)
# -> Feedback not significant


## Model estimation without feedback
CorResult <- glmmTMB(ResultCat ~ FormatInfo + BackgrAvTraining + 
                       RouteType + Set + MapTest + BackgrActivity_1 + UsedPhone + (1|Id) + (1|ScenID), 
                     family = binomial, data = Rank)


## Model overview
Anova(CorResult)
summary(CorResult)
plot(allEffects(CorResult), type = "response")

## Interclass-correlation coefficient
performance::icc(CorResult)   

## R2 (amout of explained variance in response)
r.squaredGLMM(CorResult)

## Residual check using DHARMa package
set.seed(123)
simres_gamma <- simulateResiduals(CorResult)
plot(simres_gamma)          
## -> Left panel: Q-Q-plot (uniform distribution)
## -> Right panel: Residuals against predicted values; shaded (due to sample size) with extreme residuals colored red, and 


## ---- 3) MAIN EFFECTS OF KEY VARIABLES ----

## FormatInfo
emmip(CorResult, ~FormatInfo, type = "response", CIs = TRUE)
(emm <- emmeans(CorResult, specs = ~FormatInfo, type = "response"))
pairs(emm)
EffPlotData_CorrFormat <- summary(emm)  ## Data for Fig. 3

## BackgrAvTraining
emmip(CorResult, ~BackgrAvTraining, type = "response", CIs = TRUE)
(emm <- emmeans(CorResult, specs = ~BackgrAvTraining, type = "response"))
contrast(emm, method = "consec")
EffPlotData_CorrTrain <- summary(emm)  ## Data for Fig. 3

## Save data for Fig. 3
save(list = c("EffPlotData_CorrFormat", "EffPlotData_CorrTrain"), file = "EffPlotData_Corr.RData")


## ---- 4) MAIN EFFECTS OF SECONDARY VARIABLES ----

## RouteType
emmip(CorResult, ~RouteType, type = "response", CIs = TRUE)
(emm <- emmeans(CorResult, specs = ~RouteType, type = "response"))
pairs(emm)

## Set
emmip(CorResult, ~Set, type = "response", CIs = TRUE)
(emm <- emmeans(CorResult, specs = ~Set, type = "response"))
pairs(emm)

## Activity
emmip(CorResult, ~BackgrActivity_1, type = "response", CIs = TRUE)
(emm <- emmeans(CorResult, specs = ~BackgrActivity_1, type = "response"))
pairs(emm)

## MapTest
emmip(CorResult, ~MapTest, type = "response", CIs = TRUE)
(emm <- emmeans(CorResult, specs = ~MapTest, type = "response"))
pairs(emm)

## Phone use
emmip(CorResult, ~UsedPhone, type = "response", CIs = TRUE)
(emm <- emmeans(CorResult, specs = ~UsedPhone, type = "response"))
pairs(emm)


## ---- 5) FINISH -----

## Close sink
if(WriteToFile) sink()
