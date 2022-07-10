rm(list=ls())
library(dplyr)

## Load csv
inv <- read.csv("inventory_2022.csv", header=TRUE,
                stringsAsFactors=FALSE)

## Subset for all color graphic
inv_full <- inv[,c(1:2)]

## Subset for liquid graphic
invWet <- inv %>%
    filter(Wet.=="Y") %>%
    select(name, hex)


#############################################################################
#############################################################################
## Full graphic
graphics.off()
png(filename="colors_2022.png", width=5, height=8, units="in",
    res=800)
##dev.new(height=8, width=5)
nRows <- if(nrow(inv_full)%%2==0) {
             nrow(inv_full)/2
         } else {
             ceiling(nrow(inv_full)/2)
         }           
par(mai=c(1,1.75,1,1.75))
plot(x=c(rep(1, nRows), rep(2, nRows)),
     y=rep(seq(1,nRows),2),
     pch=15,
     cex=4,
     col=inv_full$hex,
     xlab=NA,
     ylab=NA,
     axes=FALSE,
     xlim=c(0.5,2.5)
     )
box()
axis(side=2, at=seq(1,nRows), labels=NA)
axis(side=4, at=seq(1,nRows), labels=NA)
par(xpd=TRUE)
text(x=0.25,
     y=seq(1,nRows),
     labels=inv_full$name[1:nRows],
     pos=2)
text(x=2.75,
     y=seq(1,nRows),
     labels=inv_full$name[nRows+1:(nRows*2)],
     pos=4)
graphics.off()


#############################################################################
#############################################################################
## Wet graphic

graphics.off()
png(filename="colors_wet_2022.png", width=5, height=8, units="in",
    res=800)
nRows <- if(nrow(invWet)%%2==0) {
             nrow(invWet)/2
         } else {
             ceiling(nrow(invWet)/2)
         }
par(mai=c(1,1.75,1,1.75))
plot(x=c(rep(1, nRows), rep(2, nRows)),
     y=rep(seq(1,nRows),2),
     pch=15,
     cex=7,
     col=invWet$hex,
     xlab=NA,
     ylab=NA,
     axes=FALSE,
     xlim=c(0.5,2.5),
     ylim=c(.5,nRows+.5)
     )
box()
axis(side=2, at=seq(1,nRows), labels=NA)
axis(side=4, at=seq(1,nRows), labels=NA)
par(xpd=TRUE)
text(x=0.25,
     y=seq(1,nRows),
     labels=invWet$name[1:nRows],
     pos=2)
text(x=2.75,
     y=seq(1,nRows),
     labels=invWet$name[nRows+1:(nRows*2)],
     pos=4)
graphics.off()



