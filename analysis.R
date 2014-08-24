library(R.utils)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)

bunzip2("repdata-data-StormData.csv.bz2")

# read in data
alldata <- read.csv("repdata-data-StormData.csv")
alldata <- tbl_df(as.data.table(alldata))
rm(list=setdiff(ls(), "alldata"))


# mean injuries and fatalities per event
healthdmg <- alldata %>% 
                select(c(EVTYPE, INJURIES, FATALITIES)) %>%
                group_by(EVTYPE) %>% 
                summarise(
                        inj <- sum(INJURIES),
                        fat <- sum(FATALITIES))

names(healthdmg) <- c("event", "inj", "fat")

inj <- arrange(healthdmg, desc(inj))
fat <- arrange(healthdmg, desc(fat))

inj$event <- factor(inj$event, levels = inj$event, ordered = T)
fat$event <- factor(fat$event, levels = fat$event, ordered = T)

# plot
ggplot(inj[1:15, ], aes(x = event, y = inj, fill = event)) +
        geom_bar(stat = "identity") +
        scale_fill_hue(l=30) +
        coord_flip() +
        xlab("Event") +
        ylab("Number of injuries") +
        theme_minimal(base_size = 18) +
        guides(fill=FALSE) +
        ggtitle("Natural events with highest number of injuries in the US")

ggplot(fat[1:15, ], aes(x = event, y = fat, fill = event)) +
        geom_bar(stat = "identity") +
        scale_fill_hue(l=30) +
        coord_flip() +
        xlab("Event") +
        ylab("Number of fatalities") +
        theme_minimal(base_size = 18) +
        guides(fill=FALSE) +
        ggtitle("Natural events with highest death rates in the US")

# economic damage---------

# check 
table(alldata$PROPDMGEXP)
table(alldata$CROPDMGEXP)

# make all lowercase
alldata$PROPDMGEXP <- tolower(alldata$PROPDMGEXP)
alldata$CROPDMGEXP <- tolower(alldata$CROPDMGEXP)

# mapping h=100, k=1000, m=million, b=billion
# just take the mappings where I am sure of the meaning
objectdmg <- alldata %>%
        filter((PROPDMGEXP %in% c("h", "k", "m", "b") &
                CROPDMGEXP %in% c("h", "k", "m", "b"))) %>%
        select(c(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))

get_mult <- function(x) {
        if (x == "h") x <- 100
        if (x == "k") x <- 1000
        if (x == "m") x <- 1000000
        if (x == "b") x <- 1000000000
        x
}

objectdmg$PROPDMGEXP <- sapply(objectdmg$PROPDMGEXP, get_mult)
objectdmg$CROPDMGEXP <- sapply(objectdmg$CROPDMGEXP, get_mult)

options(scipen = 2)

dmg <- objectdmg %>%
        mutate(DAMAGE = PROPDMG * PROPDMGEXP + CROPDMG * CROPDMGEXP) %>%
        group_by(EVTYPE) %>% 
        summarise(dmg = (sum(DAMAGE)/1000000)) %>%
        arrange(desc(dmg))

names(dmg) <- c("event", "dmg")
dmg$event <- factor(dmg$event, levels = dmg$event, ordered = T)

ggplot(dmg[1:15, ], aes(x = event, y = dmg, fill = event)) +
        geom_bar(stat = "identity") +
        scale_fill_hue(l=30) +
        coord_flip() +
        xlab("Event") +
        ylab("Damage in Million Dollars") +
        theme_minimal(base_size = 18) +
        guides(fill=FALSE) +
        ggtitle("Natural events with highest negative economic impact in the US")

   