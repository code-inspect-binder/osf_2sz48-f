## *********************************************
## ** FIG. 5: CORRECTNESS AND COMPLETION TIME **
## *********************************************
##
## Fisher, Haegeli and Mair: 
## Impact of information presentation on interpretability of spatial
## hazard information: Lessons from a study in avalanche safety
## Oct. 1, 2021


## Get data
load("EffPlotData_Corr.RData")
load("EffPlotData_Dur.RData")

## Opening pn file
# png(filename = "Fig05.png", width = 800, height = 800)
png(filename = "Fig05_CorrectnessCompletionTime.png", width = 10, height = 10, units = "cm", res = 300, pointsize = 6)

par(mfrow=c(2,2))

## General settings
pts.cex <- 2
lbl.cex <- 1.1
ci.lwd <- 1.5
ci.length <- 0.02

## Color
ColFormat1 <- "#e31a1c"
ColFormat2 <- "#fb9a99"
ColTrain1 <- "#1f78b4"
ColTrain2 <- "#a6cee3"

## Top-left panel
plot(EffPlotData_CorrFormat$prob, ylim = c(0.4, 0.9), xlim = c(0.5, 3.5), xlab = "Presentation format", xaxt='n', 
     col = ColFormat1, type = "b", ylab = "Probability of ranking correctly", las = 1, lty = 2)
axis(1, at = c(1:3), labels= c("Separate", "Asp-Elev Rose", "Combined"))
grid(nx = NA, ny = NULL)
box()
arrows(c(1:3), EffPlotData_CorrFormat$lower.CL, c(1:3), EffPlotData_CorrFormat$upper.CL, angle = 90, code = 3, 
       lwd = ci.lwd, col = ColFormat2, length = ci.length)
title(main = "a) Correctness: Information Format")
points(c(1:3), EffPlotData_CorrFormat$prob, pch = 21, cex = pts.cex, bg = ColFormat1)
text(c(1:3), EffPlotData_CorrFormat$prob, labels = format(round(EffPlotData_CorrFormat$prob, 3), nsmall = 3), 
     adj = 0.5, pos = 3, offset = 1, cex = lbl.cex, font = 2) 


## Top right panel
plot(EffPlotData_CorrTrain$prob, ylim = c(0.4, 0.9), xlim = c(0.5, 4.5), xlab = "Training level", xaxt='n', 
     col = ColTrain1, type = "b", ylab = "Probability of ranking correctly", las = 1, lty = 2)
axis(1, at = c(1:4), labels= c("None", "Intro.", "Adv.", "Prof."))
grid(nx = NA, ny = NULL)
box()
arrows(c(1:4), EffPlotData_CorrTrain$lower.CL, c(1:4), EffPlotData_CorrTrain$upper.CL, angle = 90, code = 3, 
       lwd = ci.lwd, col = ColTrain2, length = ci.length)
title(main = "b) Correctness: Aval. Awareness Training")
points(c(1:4), EffPlotData_CorrTrain$prob, pch = 21, cex = pts.cex, bg = ColTrain1)
text(c(1:4), EffPlotData_CorrTrain$prob, labels = format(round(EffPlotData_CorrTrain$prob, 3), nsmall = 3), 
     adj = 0.5, pos = 3, offset = 1, cex = lbl.cex, font = 2) 



## Bottom left panel
plot(EffPlotData_DurFormat$response, ylim = c(75, 125), xlim = c(0.5, 3.5), xlab = "Information Presentation Format", xaxt='n', 
     col = ColFormat1, type = "b", ylab = "Completion time (sec)", las = 1, lty = 2)
axis(1, at = c(1:3), labels= c("Separate", "Asp-Elev Rose", "Combined"))
grid(nx = NA, ny = NULL)
box()
arrows(c(1:3), EffPlotData_DurFormat$lower.CL, c(1:3), EffPlotData_DurFormat$upper.CL, angle = 90, code = 3, 
       lwd = ci.lwd, col = ColFormat2, length = ci.length)
title(main = "c) Completion Time: Information Format")
points(c(1:3), EffPlotData_DurFormat$response, pch = 21, cex = pts.cex, bg = ColFormat1)
text(c(1:3), EffPlotData_DurFormat$response, labels = format(round(EffPlotData_DurFormat$response, 1), nsmall = 1), 
     adj = 0.5, pos = 3, offset = 1, cex = lbl.cex, font = 2) 


## Bottom right panel 
plot(EffPlotData_DurTrain$response, ylim = c(75, 125), xlim = c(0.5, 4.5), xlab = "Training level", xaxt='n', 
     col = ColTrain1, type = "b", ylab = "Completion time (sec)", las = 1, lty = 2)
axis(1, at = c(1:4), labels= c("None", "Intro.", "Adv.", "Prof,"))
grid(nx = NA, ny = NULL)
box()
arrows(c(1:4), EffPlotData_DurTrain$lower.CL, c(1:4), EffPlotData_DurTrain$upper.CL, angle = 90, code = 3, 
       lwd = ci.lwd, col = ColTrain2, length = ci.length)
title(main = "d) Completion Time: Aval. Awareness Training")
points(c(1:4), EffPlotData_DurTrain$response, pch = 21, cex = pts.cex, bg = ColTrain1)
text(c(1:4), EffPlotData_DurTrain$response, labels = format(round(EffPlotData_DurTrain$response, 1), nsmall = 1), 
     adj = 0.5, pos = 3, offset = 1, cex = lbl.cex, font = 2) 

## Closing PNG file
dev.off()
