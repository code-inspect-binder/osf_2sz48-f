## ********************
## ** DURATION MODEL **
## ********************
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
OutputFile <- "Model_Duration_Output.txt"
if (WriteToFile) sink(OutputFile, split = T)

## Load data
load("ProcessedSurveyData.RData")

## Overview
str(Rank)
nrow(Rank)
length(unique(Rank$Id))

## Basic statistics of response variable DurSec
summary(Rank$DurSec)
hist(Rank$DurSec, xlab = "Duration in Sec", main = "Histogram of response variable", breaks = 120, xlim = c(0, 600), col = "gold")


## ---- 2) MODEL ESTIMATION ----

## Model estimation with training and interactions
DurSec <- glmmTMB(DurSec ~ FormatInfo*BackgrAvTraining + FormatFeedback*BackgrAvTraining + 
                            RouteType + Set + MapTest + DemogrAge + (1|Id) + (1|ScenID), 
                   family = Gamma(link = "log"), data = Rank)

## Model overview
Anova(DurSec)
## -> Interactions not significant!


## Model estimation without training interactions
DurSec <- glmmTMB(DurSec ~ FormatInfo + FormatFeedback + BackgrAvTraining + 
                    RouteType + Set + MapTest + DemogrAge + (1|Id) + (1|ScenID), 
                  family = Gamma(link = "log"), data = Rank)

## Model overview
Anova(DurSec)
summary(DurSec)

## Overview of effects plots
plot(allEffects(DurSec), type = "response")
## -> Linear trend in DemogrAge

## Convert DemogrAge into numerical value for estimating linear trend
Rank$DemogrAgeLin <- as.numeric(Rank$DemogrAge)

## Model estimation without training interactions
DurSec <- glmmTMB(DurSec ~ FormatInfo + FormatFeedback + BackgrAvTraining + 
                    RouteType + Set + MapTest + DemogrAgeLin + (1|Id) + (1|ScenID), 
                  family = Gamma(link = "log"), data = Rank)

## Model overview
Anova(DurSec)
summary(DurSec)
plot(allEffects(DurSec), type = "response")

## Interclass-correlation coefficient
performance::icc(DurSec)   

## R2 (amout of explained variance in response)
r.squaredGLMM(DurSec)

## Residual check using DHARMa package
set.seed(123)
simres_gamma <- simulateResiduals(DurSec)
plot(simres_gamma)
## -> Left panel: Q-Q-plot (uniform distribution)
## -> Right panel: Residuals against predicted values; shaded (due to sample size) with extreme residuals colored red, and 


## ---- 3) MAIN EFFECTS OF KEY VARIABLES ----

## FormatInfo
emmip(DurSec, ~FormatInfo, type = "response", CIs = TRUE) 
(emm <- emmeans(DurSec, specs = ~FormatInfo, type = "response"))
pairs(emm)
EffPlotData_DurFormat <- summary(emm) ## Data for Fig. 3 

## FormatFeedback
emmip(DurSec, ~FormatFeedback, type = "response", CIs = TRUE)
(emm <- emmeans(DurSec, specs = ~FormatFeedback, type = "response"))
contrast(emm, method = "trt.vs.ctrl")

## BackgrAvTraining
emmip(DurSec, ~BackgrAvTraining, type = "response", CIs = TRUE)
(emm <- emmeans(DurSec, specs = ~BackgrAvTraining, type = "response"))
contrast(emm, method = "consec")
EffPlotData_DurTrain <- summary(emm) ## Data for Fig. 3
## This shows all pairwise comparisons.
          
## Save data for Fig. 3
save(list = c("EffPlotData_DurFormat", "EffPlotData_DurTrain"), file = "EffPlotData_Dur.RData")


## ---- 4) MAIN EFFECTS OF SECONDARY VARIABLES ----

## RouteType
emmip(DurSec, ~RouteType, type = "response", CIs = TRUE)
(emm <- emmeans(DurSec, specs = ~RouteType, type = "response"))
pairs(emm)

## Set
emmip(DurSec, ~Set, type = "response", CIs = TRUE)
(emm <- emmeans(DurSec, specs = ~Set, type = "response"))
pairs(emm)

## MapTest
emmip(DurSec, ~MapTest, type = "response", CIs = TRUE)
(emm <- emmeans(DurSec, specs = ~MapTest, type = "response"))
pairs(emm)

## DemogrAgeLin
## Since this is a numeric variable, the emmip plot is not meaningful
(emm <- emmeans(DurSec, specs = ~DemogrAgeLin, type = "response"))


## ---- 5) FINISH -----

## Close sink
if(WriteToFile) sink()

