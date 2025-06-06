## *******************************
## ** OVERVIEW OF SURVEY SAMPLE **
## *******************************
##
## Fisher, Haegeli and Mair: 
## Impact of information presentation on interpretability of spatial
## hazard information: Lessons from a study in avalanche safety
## April 14, 2021


## ---- 1) PREPARATION -----

## Load data
load("ProcessedSurveyData.RData")

## Overview
str(Tbl)
nrow(Tbl)

## ---- 2) SUMMARY STATISTICS -----

## Gender
table(Tbl$DemogrGender)
round(100*prop.table(table(Tbl$DemogrGender)),1)

## Eductions
table(Tbl$DemogrEduc)
round(100*prop.table(table(Tbl$DemogrEduc)),1)

## Country of residence
table(Tbl$DemogrCountry)
round(100*prop.table(table(Tbl$DemogrCountry)),1)

## Avalanche awareness training
table(Tbl$BackgrAvTraining)
round(100*prop.table(table(Tbl$BackgrAvTraining)),1)

## Primariy backcountry activity
table(Tbl$BackgrActivity_1)
round(100*prop.table(table(Tbl$BackgrActivity_1)),1)

## Bulletin user type
table(Tbl$BullUseType)
round(100*prop.table(table(Tbl$BullUseType)),1)

## Years of experience
table(Tbl$BackgrYrsOfExp)
round(100*prop.table(table(Tbl$BackgrYrsofExp)),1)

## Age categories
table(Tbl$DemogrAge)
round(100*prop.table(table(Tbl$DemogrAge)),1)