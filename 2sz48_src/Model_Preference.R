## **********************
## ** PREFERENCE MODEL **
## **********************
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
OutputFile <- "Model_Preferences_Output.txt"
if (WriteToFile) sink(OutputFile, split = T)

## Load data
load("ProcessedSurveyData.RData")

## Overview
str(Pref)
nrow(Pref)
length(unique(Pref$Id))

## Basic statistics of response variable EffectRating
summary(Pref$EffectRating)
hist(Pref$EffectRating, xlab = "Preference rating", col = 'gold', breaks = 50, main = "Distribution of Preference Ratings")


## ---- 2) MODEL ESTIMATION ----

## Model estimation with training and interactions
PrefRate <- glmmTMB(EffectRating ~ AspElevForm*BullCountry + AspElevForm*BackgrAvTraining + AspElevForm*UsedinTask + 
                      AspElevForm*RankTaskPerform + UsedPhone + (1 | Id), family=beta_family(link="logit"), data=Pref)

Anova(PrefRate)
## -> Main effect just significant, but interaction is highly significant.
summary(PrefRate)

## Effects plots
eff <- allEffects(PrefRate)
plot(eff, type = "response")
plot(eff, "AspElevForm:BackgrAvTraining", type = "response")
plot(eff, "AspElevForm:UsedinTask", type = "response")
plot(eff, "AspElevForm:RankTaskPerform", type = "response")

## Interclass-correlation coefficient
performance::icc(PrefRate)   

## Residual check using DHARMa package
set.seed(123)
simres_gamma <- simulateResiduals(PrefRate)
plot(simres_gamma)
## -> Left panel: Q-Q-plot (uniform distribution)
## -> Right panel: Residuals against predicted values; shaded (due to sample size) with extreme residuals colored red, and 


## ---- 3) MAIN EFFECTS OF KEY VARIABLES ----

## AspElevForm
emmip(PrefRate, ~AspElevForm, type = "response", CIs = TRUE)
(emm <- emmeans(PrefRate, specs = ~AspElevForm, type = "response"))
pairs(emm)

## BullCountry
emmip(PrefRate, ~BullCountry, type = "response", CIs = TRUE)
(emm <- emmeans(PrefRate, specs = ~BullCountry, type = "response"))
pairs(emm)

## UsedinTask
emmip(PrefRate, ~UsedinTask, type = "response", CIs = TRUE)
(emm <- emmeans(PrefRate, specs = ~BullCountry, type = "response"))
pairs(emm)


## ---- 4) INTERACTION EFFECTS OF KEY VARIABLES ----

## AspElevForm and BullCountry
emmip(PrefRate, BullCountry ~ AspElevForm, type = "response", CIs = TRUE)
(em_int <- emmeans(PrefRate, specs = ~AspElevForm*BullCountry, type = "response"))
pairs(em_int, by = "BullCountry") 
EffPlotData_Rate_IAFormatCountry <- summary(em_int)  ## Data for Fig. 4

## AspElevForm and AvTraining
emmip(PrefRate, BackgrAvTraining ~ AspElevForm, type = "response", CIs = TRUE)
(em_int <- emmeans(PrefRate, specs = ~AspElevForm*BackgrAvTraining, type = "response"))
pairs(em_int, by = "BackgrAvTraining")  
EffPlotData_Rate_IAFormatTrain <- summary(em_int)  ## Data for Fig. 4       

## AspElevForm and UsedinTask
emmip(PrefRate, UsedinTask ~ AspElevForm, type = "response", CIs = TRUE)
(em_int <- emmeans(PrefRate, specs = ~AspElevForm*UsedinTask, type = "response"))
pairs(em_int, by = "AspElevForm")  
EffPlotData_Rate_IAFormatUsed <- summary(em_int)  ## Data for Fig. 4

## AspElevForm and RankTaskPerform
emmip(PrefRate, AspElevForm ~ RankTaskPerform, type = "response", CIs = F)
(em_int <- emmeans(PrefRate, specs = ~AspElevForm*RankTaskPerform, type = "response"))
EffPlotData_Rate_IAFormatPerform <- em_int   ## Data for Fig. 4

## Export data for Figure 4
save(list = c("EffPlotData_Rate_IAFormatCountry", "EffPlotData_Rate_IAFormatTrain", "EffPlotData_Rate_IAFormatUsed", "EffPlotData_Rate_IAFormatPerform"), file = "EffPlotData_Rate.RData")


## ---- 5) MAIN EFFECTS OF OTHER VARIABLES ----

## UsedPhone
emmip(PrefRate, ~UsedPhone, type = "response", CIs = TRUE) 
(emm <- emmeans(PrefRate, specs = ~UsedPhone, type = "response"))
pairs(emm)


## ---- 6) FINISH -----

## Close sink
if(WriteToFile) sink()
