# THIS FILE SAVES THE EVALUATION OF THE DATA

# Loading the single file directly from .csv.bz2 (to have a 50MB file instead of 500MB):
d0 <- read.csv("./repdata_data_StormData.csv.bz2", header=TRUE, sep=",")

# Basic infos:
dim(d0)
names(d0)
eventtypes <- unique(d0$EVTYPE)
head(eventtypes,n=10)
length(eventtypes)
summary(d0$FATALITIES)
summary(d0$INJURIES)
sum(is.na(d0))
sum(is.na(d0[,c("FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]))

# Restriction of the dataframe:
d <- d0[c("BGN_DATE","BGN_TIME","TIME_ZONE","LATITUDE","LONGITUDE","STATE","EVTYPE","MAG","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
rm(d0)

# Time zones:
summary(d$TIME_ZONE)

# Changing time columns to a single date type one:
d$BGN_DATE <- as.Date(as.character(d$BGN_DATE),format="%m/%d/%Y")
d <- d[c("BGN_DATE","LATITUDE","LONGITUDE","STATE","EVTYPE","MAG","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

# Renaming colums
names(d) <- tolower(names(d))
names(d)[1] <- "date"
names(d)[2] <- "lat"
names(d)[3] <- "long"
names(d)[7] <- "fatal"
names(d)[8] <- "injur"

# Showing the ready table:
head(d)


# Libraries
library(dplyr)
library(ggplot2)
library(lattice)

# Showing the most fatal events:
d1 <- d %>% group_by(evtype) %>% summarise(TotFatal=sum(fatal),TotInjur=sum(injur))
# d1 <- d1[with(d1, order(TotFatal,TotInjur, decreasing = TRUE)),]

d1Fatal <- d1[c("evtype","TotFatal")]
d1Injur <- d1[c("evtype","TotInjur")]

d1Fatal <- d1Fatal[order(d1Fatal$TotFatal, decreasing = TRUE),]
d1Injur <- d1Injur[order(d1Injur$TotInjur, decreasing = TRUE),]

d1Fatal$evtype <- factor(d1Fatal$evtype, levels=d1Fatal$evtype[order(d1Fatal$TotFatal)])
d1Injur$evtype <- factor(d1Injur$evtype, levels=d1Injur$evtype[order(d1Injur$TotInjur)])

png("./Plot1-Fatalities_Injuries.png", width=480, height=480, units="px")
require(gridExtra)
p1 <- ggplot(d1Fatal[1:15,], aes(x=evtype, y=TotFatal)) + geom_col(width=0.7) + coord_flip() + theme(axis.text.x = element_text(angle = -90, hjust = 1)) + xlab("Event Type") + ylab("Total Fatalities")
p2 <- ggplot(d1Injur[1:15,], aes(x=evtype, y=TotInjur)) + geom_col(width=0.7) + coord_flip() + theme(axis.text.x = element_text(angle = -90, hjust = 1)) + xlab("") + ylab("Total Injuries")
grid.arrange(p1,p2,ncol=2)
dev.off()


# Economy
d2 <- d[,c("evtype","propdmg","propdmgexp","cropdmg","cropdmgexp")]
d2$propdmg[d2$propdmgexp=="K"] <- d2$propdmg[d2$propdmgexp=="K"] * 1E3
d2$propdmg[d2$propdmgexp=="M"] <- d2$propdmg[d2$propdmgexp=="M"] * 1E6
d2$propdmg[d2$propdmgexp=="B"] <- d2$propdmg[d2$propdmgexp=="B"] * 1E9
d2$cropdmg[d2$cropdmgexp=="K"] <- d2$cropdmg[d2$cropdmgexp=="K"] * 1E3
d2$cropdmg[d2$cropdmgexp=="M"] <- d2$cropdmg[d2$cropdmgexp=="M"] * 1E6
d2$cropdmg[d2$cropdmgexp=="B"] <- d2$cropdmg[d2$cropdmgexp=="B"] * 1E9

d2 <- d2 %>% group_by(evtype) %>% summarise(TotPropDmg=sum(propdmg),TotCropDmg=sum(cropdmg))
d2Prop <- d2[c("evtype","TotPropDmg")]
d2Crop <- d2[c("evtype","TotCropDmg")]
d2Prop <- d2Prop[order(d2Prop$TotPropDmg, decreasing = TRUE),]
d2Crop <- d2Crop[order(d2Crop$TotCropDmg, decreasing = TRUE),]

d2Prop$evtype <- factor(d2Prop$evtype, levels=d2Prop$evtype[order(d2Prop$TotPropDmg)])
d2Crop$evtype <- factor(d2Crop$evtype, levels=d2Crop$evtype[order(d2Crop$TotCropDmg)])

png("./Plot2-PropDmg_CropDmg.png", width=480, height=480, units="px")
require(gridExtra)
p3 <- ggplot(d2Prop[1:15,], aes(x=evtype, y=TotPropDmg)) + geom_col(width=0.7) + coord_flip() + theme(axis.text.x = element_text(angle = -90, hjust = 1)) + xlab("Event Type") + ylab("Total Property Damage")
p4 <- ggplot(d2Crop[1:15,], aes(x=evtype, y=TotCropDmg)) + geom_col(width=0.7) + coord_flip() + theme(axis.text.x = element_text(angle = -90, hjust = 1)) + xlab("") + ylab("Total Crops Damage")
grid.arrange(p3,p4,ncol=2)
dev.off()


# Bonus: Evolution over time:
# of fatalities/injuries by tornado
d3 <- d[c("date","evtype","fatal","injur")]
d3 <- d3[d3$evtype=="TORNADO",]
# of property damage by flood
d4 <- d[c("date","evtype","propdmg","propdmgexp")]
d4$propdmg[d4$propdmgexp=="K"] <- d4$propdmg[d4$propdmgexp=="K"] * 1E3
d4$propdmg[d4$propdmgexp=="M"] <- d4$propdmg[d4$propdmgexp=="M"] * 1E6
d4$propdmg[d4$propdmgexp=="B"] <- d4$propdmg[d4$propdmgexp=="B"] * 1E9
d4 <- d4[d4$evtype=="FLOOD",]
# of crop damage by drought
d5 <- d[c("date","evtype","cropdmg","cropdmgexp")]
d5$cropdmg[d5$cropdmgexp=="K"] <- d5$cropdmg[d5$cropdmgexp=="K"] * 1E3
d5$cropdmg[d5$cropdmgexp=="M"] <- d5$cropdmg[d5$cropdmgexp=="M"] * 1E6
d5$cropdmg[d5$cropdmgexp=="B"] <- d5$cropdmg[d5$cropdmgexp=="B"] * 1E9
d5 <- d5[d5$evtype=="DROUGHT",]

png("./Plot3-Time_Evolution.png", width=480, height=480, units="px")
require(gridExtra)
p5 <- ggplot() + geom_point(data=d3, aes(x=date, y=log(1+fatal)), color="red", alpha=0.5) + geom_point(data=d3, aes(x=date, y=log(1+injur)), color="blue", alpha=0.1) + ylab("log(1+X)") + ggtitle("Fatalities (red) & Injuries (blue) from Tornados over time")
p6 <- ggplot() + geom_point(data=d4, aes(x=date, y=log(1+propdmg)), color="green", alpha=0.1) + geom_point(data=d5, aes(x=date, y=log(1+cropdmg)), color="orange", alpha=0.5) + ylab("log(1+X)") + ggtitle("Floods Property-damages (green) & Drought Crop-damages (orange) \nover time")
grid.arrange(p5,p6,nrow=2)
dev.off()


