## **************************
## ** FIG. 4: DEMOGRAPHICS **
## **************************
##
## Fisher, Haegeli and Mair: 
## Impact of information presentation on interpretability of spatial
## hazard information: Lessons from a study in avalanche safety
## Oct. 1, 2021


## Load data
load("ProcessedSurveyData.RData")

str(Tbl)
colnames(Tbl)

## Simplify some of the variables
Tbl$BackgrActivity_1 <- factor(Tbl$BackgrActivity_1, labels = c("SS", "IC", "OB", "BC", "BC", "SM"))
Tbl$BackgrAvTraining <- ordered(Tbl$BackgrAvTraining, labels = c("None", "None", "None", "Introductory", "Advanced", "Professional"))

## Summary stats
table(Tbl$DemogrGender)
table(Tbl$DemogrEduc)
table(Tbl$DemogrCountry)
table(Tbl$BackgrAvTraining)
table(Tbl$BackgrActivity_1)
table(Tbl$BullUseType)
table(Tbl$BackgrYrsOfExp)
table(Tbl$DemogrAge)

round(100*prop.table(table(Tbl$DemogrGender)),1)
round(100*prop.table(table(Tbl$DemogrEduc)),1)
round(100*prop.table(table(Tbl$DemogrCountry)),1)
round(100*prop.table(table(Tbl$BackgrAvTraining)),1)
round(100*prop.table(table(Tbl$BackgrActivity_1)),1)
round(100*prop.table(table(Tbl$BullUseType)),1)
round(100*prop.table(table(Tbl$BackgrYrsOfExp)),1)
round(100*prop.table(table(Tbl$DemogrAge)),1)

## Plotting
png(filename = "Fig04_Demographics.png", width = 15, height = 10, units = "cm", res = 300, pointsize = 6)
par(mfrow=c(2,3))

## Age
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl$DemogrAge))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl$DemogrAge, main = "a) Age Categories", las = 1, ylab = "Number participants", ylim = c(0, 1.15*max(counts)), col = "#a6cee3")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Education
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl$DemogrEduc))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl$DemogrEduc, main = "b) Highest Level of Education Completed", xaxt = "n", las = 1, ylab = "", ylim = c(0, 1.15*max(counts)), col = "#b2df8a")
axis(1, at = xaxis, labels = c("< HS", "HS", "PostSec", "Trades", "UGrad", "Grad"), tick = F)
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Primariy BC activity
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl$BackgrActivity_1))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl$BackgrActivity_1, main = "c) Primary Backcountry Activity", las = 1, ylab = "", ylim = c(0, 1.15*max(counts)), col = "#fb9a99")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Avalanche Training
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl$BackgrAvTraining))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl$BackgrAvTraining, main = "d) Avalanche Awareness Training", las = 1, ylab = "Number participants", ylim = c(0, 1.15*max(counts)), col = "#fdbf6f")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Years of Experience
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl$BackgrYrsOfExp))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl$BackgrYrsOfExp, main = "e) Years of Backcountry Experience", las = 1, ylab = "", ylim = c(0, 1.15*max(counts)), col = "#cab2d6")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Bulletin User Type
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl$BullUseType))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl$BullUseType, main = "f) Bulletin User Type", las = 1, ylab = "", ylim = c(0, 1.15*max(counts)), col = "#ffff99")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

dev.off()

## Reset
par(mar = c(5.1, 4.1, 4.1, 2.1))
