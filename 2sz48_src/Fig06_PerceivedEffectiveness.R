## *************************************
## ** FIG. 6: PERCEIVED EFFECTIVENESS **
## *************************************
##
## Fisher, Haegeli and Mair: 
## Impact of information presentation on interpretability of spatial
## hazard information: Lessons from a study in avalanche safety
## Oct. 1, 2021


## Get data
load("EffPlotData_Rate.RData")

## Opening pn file
##png(filename = "Fig06.png", width = 1200, height = 400)
png(filename = "Fig06_PerceivedEffectiveness.png", width = 15, height = 5, units = "cm", res = 300, pointsize = 6)
par(mfrow=c(1,3))

## General settings
ylim <- c(60, 85)
pts.cex <- 2
lbl.cex <- 1.1
ci.lwd <- 1.5
ci.length <- 0.02

## Left panel: IA with country
## ***************************
(PlotData <- EffPlotData_Rate_IAFormatCountry)
PlotData$response <- 100*PlotData$response
PlotData$lower.CL <- 100*PlotData$lower.CL
PlotData$upper.CL <- 100*PlotData$upper.CL

offset1 <- -0.1
offset2 <- 0.1

ColCan1 <- "#e31a1c"
ColCan2 <- "#fb9a99"
ColUSA1 <- "#1f78b4"
ColUSA2 <- "#a6cee3"

## Setup
plot(x = c(1:3)+offset1, y=PlotData$response[1:3], ylim = ylim, xlim = c(0.5, 3.5), xlab = "Presentation format", xaxt='n', 
     col = ColCan1, type = "b", ylab = "Perceived effectiveness rating", las = 1, lty = 2)
axis(1, at = c(1:3), labels= c("Separate", "Asp-Elev Rose", "Combined"))
grid(nx = NA, ny = NULL)
box()
title(main = "a) Country of Residence")
points(x = c(1:3)+offset2, y=PlotData$response[4:6], col = ColUSA1, type = "b", lty = 2)

## Canada: Overplotting
arrows(c(1:3)+offset1, PlotData$lower.CL[1:3], c(1:3)+offset1, PlotData$upper.CL[1:3], angle = 90, code = 3, lwd = ci.lwd, 
       col = ColCan2, length = ci.length)
points(c(1:3)+offset1, PlotData$response[1:3], pch = 21, cex = pts.cex, bg = ColCan1)
text(c(1:3)+offset1, PlotData$response[1:3], labels = format(round(PlotData$response[1:3], 1), nsmall = 1), adj = 0.5, 
     pos = c(3, 1, 1), offset = 1, cex = lbl.cex, font = 2) 

## USA: Overplotting
arrows(c(1:3)+offset2, PlotData$lower.CL[4:6], c(1:3)+offset2, PlotData$upper.CL[4:6], angle = 90, code = 3, lwd = ci.lwd, 
       col = ColUSA2, length = ci.length)
points(c(1:3)+offset2, PlotData$response[4:6], pch = 22, cex = pts.cex, bg = ColUSA1)
text(c(1:3)+offset2, PlotData$response[4:6], labels = format(round(PlotData$response[4:6], 1), nsmall = 1), adj = 0.5, 
     pos = c(3, 3, 3), offset = 1, cex = lbl.cex, font = 2) 

## Legend
legend("bottom", c("Canada   ", "USA"), pch = c(21, 22), pt.bg = c(ColCan1, ColUSA1), pt.cex = pts.cex, cex = lbl.cex, bty = "n", horiz = T,
       x.intersp = 1.5, inset = 0.02)


## Middel panel: IA with Training
## ******************************
(PlotData <- EffPlotData_Rate_IAFormatTrain)
PlotData$response <- 100*PlotData$response
PlotData$lower.CL <- 100*PlotData$lower.CL
PlotData$upper.CL <- 100*PlotData$upper.CL

offset1 <- -0.35
offset2 <- -0.125
offset3 <- 0.125
offset4 <- 0.35

ColNone1 <- "#1f78b4"
ColNone2 <- "#a6cee3"
ColIntro1 <- "#33a02c"
ColIntro2 <- "#b2df8a"
ColAdv1  <- "#6a3d9a"
ColAdv2  <- "#cab2d6"
ColProf1 <- "#ff7f00"
ColProf2 <- "#fdbf6f"


## Setup
plot(x = c(1:3)+offset1, y=PlotData$response[1:3], ylim = ylim, xlim = c(0.5, 3.5), xlab = "Presentation format", xaxt='n', 
     col = ColNone1, type = "b", ylab = "Perceived effectiveness rating", las = 1, lty = 2)
axis(1, at = c(1:3), labels= c("Separate", "Asp-Elev Rose", "Combined"))
grid(nx = NA, ny = NULL)
box()
title(main = "b) Aval. Awareness Training")
points(x = c(1:3)+offset2, y=PlotData$response[4:6], col = ColIntro1, type = "b", lty = 2)
points(x = c(1:3)+offset3, y=PlotData$response[7:9], col = ColAdv1, type = "b", lty = 2)
points(x = c(1:3)+offset4, y=PlotData$response[10:12], col = ColProf1, type = "b", lty = 2)

## None: Overplotting
arrows(c(1:3)+offset1, PlotData$lower.CL[1:3], c(1:3)+offset1, PlotData$upper.CL[1:3], angle = 90, code = 3, lwd = ci.lwd, 
       col = ColNone2, length = ci.length)
points(c(1:3)+offset1, PlotData$response[1:3], pch = 21, cex = pts.cex, bg = ColNone1)
text(c(1:3)+offset1, PlotData$response[1:3], labels = format(round(PlotData$response[1:3], 1), nsmall = 1), adj = 0.5, 
     pos = c(3, 3, 3), offset = 1, cex = lbl.cex, font = 2) 

## Intro: Overplotting
arrows(c(1:3)+offset2, PlotData$lower.CL[4:6], c(1:3)+offset2, PlotData$upper.CL[4:6], angle = 90, code = 3, lwd = ci.lwd, 
       col = ColIntro2, length = ci.length)
points(c(1:3)+offset2, PlotData$response[4:6], pch = 22, cex = pts.cex, bg = ColIntro1)
text(c(1:3)+offset2, PlotData$response[4:6], labels = format(round(PlotData$response[4:6], 1), nsmall = 1), adj = 0.5, 
     pos = c(1, 1, 1), offset = 1, cex = lbl.cex, font = 2) 

## Advanced: Overplotting
arrows(c(1:3)+offset3, PlotData$lower.CL[7:9], c(1:3)+offset3, PlotData$upper.CL[7:9], angle = 90, code = 3, lwd = ci.lwd, 
       col = ColAdv2, length = ci.length)
points(c(1:3)+offset3, PlotData$response[7:9], pch = 23, cex = pts.cex, bg = ColAdv1)
text(c(1:3)+offset3, PlotData$response[7:9], labels = format(round(PlotData$response[7:9], 1), nsmall = 1), adj = 0.5, 
     pos = c(3, 3, 3), offset = 1, cex = lbl.cex, font = 2) 

## Prof: Overplotting
arrows(c(1:3)+offset4, PlotData$lower.CL[10:12], c(1:3)+offset4, PlotData$upper.CL[10:12], angle = 90, code = 3, lwd = ci.lwd, 
       col = ColProf2, length = ci.length)
points(c(1:3)+offset4, PlotData$response[10:12], pch = 24, cex = pts.cex, bg = ColProf1)
text(c(1:3)+offset4, PlotData$response[10:12], labels = format(round(PlotData$response[10:12], 1), nsmall = 1), adj = 0.5, 
     pos = c(1, 1, 3), offset = 1, cex = lbl.cex, font = 2) 

## Legend
legend("bottom", c("None  ", "Intro ", "Advan. ", "Prof.  "), pch = c(21, 22, 23, 24), 
       pt.bg = c(ColNone1, ColIntro1, ColAdv1, ColProf1), pt.cex = pts.cex, cex = lbl.cex,
       bty = "n", horiz = T, x.intersp = 1.5, inset = 0.02)


## Left panel: IA with used
## ************************
(PlotData <- EffPlotData_Rate_IAFormatUsed)
PlotData$response <- 100*PlotData$response
PlotData$lower.CL <- 100*PlotData$lower.CL
PlotData$upper.CL <- 100*PlotData$upper.CL

offset1 <- -0.1
offset2 <- 0.1

ColNot1 <- "#6a3d9a"
ColNot2 <- "#cab2d6"
ColUse1 <- "#ff7f00"
ColUse2 <- "#fdbf6f"


## Setup
plot(x = c(1:3)+offset1, y=PlotData$response[1:3], ylim = ylim, xlim = c(0.5, 3.5), xlab = "Presentation format", xaxt='n', 
     col = ColNot1, type = "b", ylab = "Perceived effectiveness rating", las = 1, lty = 2)
axis(1, at = c(1:3), labels= c("Separate", "Asp-Elev Rose", "Combined"))
grid(nx = NA, ny = NULL)
box()
title(main = "c) Used in Experiment")
points(x = c(1:3)+offset2, y=PlotData$response[4:6], col = ColUse1, type = "b", lty = 2)

## Not used: Overplotting
arrows(c(1:3)+offset1, PlotData$lower.CL[1:3], c(1:3)+offset1, PlotData$upper.CL[1:3], angle = 90, code = 3, lwd = ci.lwd, 
       col = ColNot2, length = ci.length)
points(c(1:3)+offset1, PlotData$response[1:3], pch = 21, cex = pts.cex, bg = ColNot1)
text(c(1:3)+offset1, PlotData$response[1:3], labels = format(round(PlotData$response[1:3], 1), nsmall = 1), adj = 0.5, 
     pos = 3, offset = 1, cex = lbl.cex, font = 2) 

## Used: Overplotting
arrows(c(1:3)+offset2, PlotData$lower.CL[4:6], c(1:3)+offset2, PlotData$upper.CL[4:6], angle = 90, code = 3, lwd = ci.lwd, 
       col = ColUse2, length = ci.length)
points(c(1:3)+offset2, PlotData$response[4:6], pch = 22, cex = pts.cex, bg = ColUse1)
text(c(1:3)+offset2, PlotData$response[4:6], labels = format(round(PlotData$response[4:6], 1), nsmall = 1), adj = 0.5, 
     pos = 3, offset = 1, cex = lbl.cex, font = 2) 

## Legend
legend("bottom", c("Not used   ", "Used   "), pch = c(21, 22), pt.bg = c(ColNot1, ColUse1), pt.cex = pts.cex, cex = lbl.cex,
       bty = "n", horiz = T, x.intersp = 1.5, inset = 0.02)



## Closing PNG file
dev.off()
