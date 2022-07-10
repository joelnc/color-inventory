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

## Subjective color panels NEEDS UPDATE 
blues <- inv_full %>%
    filter(name %in% c("Strong Navy","Navy Blue", "Rhythm and Blue",
                       "Baby Blue", "Turquoise", "Saphire Blue"))

greens <- inv_full %>%
    filter(name %in% c("Kelly Green", "Avocado", "Chartruesse",
                       "Muir Green", "Mermaids Dream", "Sage Green",
                       "Sea Foam", "Jade Green"))

reds <- inv_full %>%
    filter(name %in% c("Cardinal Red","Pagoda Red","Pomegranate",
                       "Watermelon","Black Cherry","Chinese Red"))

springs <- inv_full %>%
    filter(name %in% c("Sea Foam","Plum Blossom","Citrus Yellow",
                       "Lavender","Chartruesse","Ice Blue","Wisteria",
                       "Watermelon","Jade Green"))

falls <- inv_full %>%
    filter(name %in% c("Golden Yellow","Golden Brown","Deep Yellow",
                       "Dark Brown","Pagoda Red","Orange Crush",
                       "Cardinal Red","Muir Green","Avocado"))

fills <- inv_full %>%
    filter(name %in% c("Mist Gray","Brushed Steel","Lavender",
                       "Baby Pink","Baby Blue","Citrus Yellow","Sea Foam",
                       "Ivory","Wisteria","Ice Blue"))


#############################################################################
## How about a function??????

## First up single pallet single page
colorFun <- function(ds, dsName) {

    ## Determine nrows for 2 column setup
    nRows <- if(nrow(ds)%%2==0) {
                 nrow(ds)/2
             } else {
                 ceiling(nrow(ds)/2)
             }

    ## Init png
    png(filename=paste0(dsName,".png"), width=5,
        height=4+(0.3*nRows),
        units="in", res=800)

    
    ## Init plot
    par(mai=c(1,1.75,1,1.75))
    plot(x=c(rep(1, nRows), rep(2, nRows)),
         y=rep(seq(1,nRows),2),
         pch=15,
         cex=4,
         col=ds$hex,
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
         labels=ds$name[1:nRows],
         pos=2)
    text(x=2.75,
         y=seq(1,nRows),
         labels=ds$name[nRows+1:(nRows*2)],
         pos=4)
    graphics.off()
}

colorFun(ds=inv_full, dsName="all_colors")
colorFun(ds=invWet, dsName="wet_colors")
colorFun(ds=blues, dsName="blues")
colorFun(ds=reds, dsName="reds")
colorFun(ds=greens, dsName="greens")
colorFun(ds=falls, dsName="falls")
colorFun(ds=springs, dsName="springs")
colorFun(ds=fills, dsName="fills")



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
par(mai=c(.3,1.75,.3,1.75))
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



#############################################################################
#############################################################################
## Blues graphic

