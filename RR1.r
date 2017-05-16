###########################################################
## JHU Reproducibe Research course Assignment 1
## USA medical expenditure analysis
##
###########################################################
library(RColorBrewer)

## Read in the data
payments <- read.csv("payments.csv")

## Plot 1: what is the relationship between 
##         mean covered charges (Average.Covered.Charges) and 
##         mean total payments (Average.Total.Payments) in New York?

par(mfrow = c(1,1))
nypayments <- subset(payments, Provider.State=="NY")

with(nypayments, plot(Average.Covered.Charges, Average.Total.Payments, 
                    xlab = "Mean covered charges ($)", ylab = "Mean total payments ($)", 
                    pch=20, col = rgb(1,0,0.5,0.15))) 
abline(lm(Average.Total.Payments ~ Average.Covered.Charges, nypayments), lwd=2, col = rgb(1,0,0.5))
title("Relationship between mean covered charges and 
mean total payments in New York healthcare")

dev.copy2pdf(file = "plot1.pdf")
dev.off()

## Plot 2: how does the relationship between mean covered charges (Average.Covered.Charges) 
##         and mean total payments (Average.Total.Payments) 
##         vary by medical condition (DRG.Definition) 
##         and the state in which care was received (Provider.State)?

## A log-log plot seems to present the data better

States <- unique(payments$Provider.State)
DRG <- unique(payments$DRG.Definition)
                   
## Set up the display for 2 x 3 graphs with a space at the bottom for the key
m <- matrix(c(1,2,3,4,5,6,7,7,7),nrow = 3,ncol = 3,byrow = TRUE)
layout(mat = m,heights = c(0.37,0.37,0.26))
par(mar = c(4,4,2,1), oma=c(0,0,5,0))
cols <- brewer.pal(6,"Accent")


## use the same range for all plots
xrange <- log(range(payments$Average.Covered.Charges))
yrange <- log(range(payments$Average.Total.Payments))


## Cycle through states and conditions. Plot 1 graph per state
## Differentiate conditions by colour and fit a line for each condition
for(st in States) {
    for(c in 1:length(DRG)) {
        n <- subset(payments, Provider.State == st & DRG.Definition == DRG[c])
        if (nrow(n) == 0) break
        if(c == 1) {
            with(n, plot(log(Average.Covered.Charges), log(Average.Total.Payments), col = cols[c], 
                    pch=21, ylab = "Log Mean total payments", xlab = "Log Mean covered charges", main = st,
                    xlim = xrange, ylim = yrange))
            abline(lm(log(Average.Total.Payments) ~ log(Average.Covered.Charges), n), lwd=2, col = cols[c])
            
        }
        else {
            with(n, points(log(Average.Covered.Charges), log(Average.Total.Payments), col = cols[c], 
                      pch=21))
            abline(lm(log(Average.Total.Payments) ~ log(Average.Covered.Charges), n), lwd=2, col = cols[c])
            
        }
    }
}

## Add the legend at the bottom (plot a blank graph and add the legend to it)
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend("center", legend = DRG, col = cols[as.integer(DRG)], pch=19, cex = 1.2, title = "Key: Medical Condition")
mtext("US Medical Payments:
Mean covered charges vs total payments by State and Condition", outer= TRUE, cex = 1.8)

## Copy the plot to a PDFlw
dev.copy2pdf(file = "plot2.pdf", width = 12)
dev.off()

par(mfrow=c(1,1))
