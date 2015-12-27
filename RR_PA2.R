suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

calc.exp <- function(col.exp) {
    col.exp = ifelse(col.exp == "H", "2", col.exp)
    col.exp = ifelse(col.exp == "K", "3", col.exp)
    col.exp = ifelse(col.exp == "M", "6", col.exp)
    col.exp = ifelse(col.exp == "B", "9", col.exp)
    col.exp = ifelse(col.exp == "",  "0", col.exp)
    return(as.integer(col.exp))
}

calc.dmg <- function(col.mag, col.exp) {
    return(col.mag * (10 ^ calc.exp(col.exp)))
}

str.trim <- function(col) {
    return(trimws(as.character(col)))
}

data.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
data.bz2 <- "StormData.csv.bz2"
data.csv <- "StormData.csv"
evnt.csv <- "events.csv"

if (!file.exists(data.bz2)) {
    download.file(data.url, data.bz2)
}

if (!file.exists(data.csv)) {
    bunzip2(data.bz2)
}

if (!exists("raw.df")) {
    raw.df <- read.csv(data.csv)
}

#if (!exists("evt.df")) {
    evt.df <- read.csv(evnt.csv, stringsAsFactors = FALSE)
#}

wrk.df <-
    raw.df %>%
    filter(PROPDMG != 0 | CROPDMG != 0 | FATALITIES != 0 | INJURIES != 0) %>%
    filter(PROPDMGEXP != "+" & PROPDMGEXP != "-" & PROPDMGEXP != "?") %>%
    filter(CROPDMGEXP != "+" & CROPDMGEXP != "-" & CROPDMGEXP != "?") %>%
    select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
    mutate(XEVTYPE = toupper(trimws(gsub( " +", " ", as.character(EVTYPE) )))) %>%
    mutate(XPROPDMGEXP = ifelse(str.trim(PROPDMGEXP) == "", "0", toupper(str.trim(PROPDMGEXP)))) %>%
    mutate(XCROPDMGEXP = ifelse(str.trim(CROPDMGEXP) == "", "0", toupper(str.trim(CROPDMGEXP))))

for (i in 1:nrow(evt.df)) {
    wrk.df$XEVTYPE[grep(evt.df[i,]$regex, wrk.df$XEVTYPE)] <- evt.df[i,]$class
}
wrk.df$XEVTYPE[grep("[a-z]", wrk.df$XEVTYPE, invert = TRUE)] <- "Other"

cln.df <- 
    with(wrk.df, {
        wrk.df %>%
        transmute(EVTYPE = as.factor(XEVTYPE)) %>%
        transform(CASUALTIES = FATALITIES + INJURIES) %>%
        transform(DAMAGE = calc.dmg(PROPDMG, XPROPDMGEXP) +  calc.dmg(CROPDMG, XCROPDMGEXP))
    })

top.health.df <- 
    cln.df %>%
    group_by(EVTYPE) %>%
    summarize(TOTAL_CASUALTIES = sum(CASUALTIES)) %>%
    arrange(desc(TOTAL_CASUALTIES))

top.health.df$EVTYPE <- 
    factor(
        top.health.df$EVTYPE, 
        levels = top.health.df$EVTYPE[order(top.health.df$TOTAL_CASUALTIES, decreasing = TRUE)]
    )

health.plot <-
    ggplot(data = top.health.df[1:5,], aes(x = EVTYPE, y = TOTAL_CASUALTIES)) +
    geom_bar(stat = "identity") +
    xlab("Event Class") +
    ylab("Health Damage (Fatalities + Injuries)") +
    ggtitle("Top Five Severe Weather Events Contributing to Human Losses in the U.S., 1950-2011")

print(health.plot)

top.damage.df <- 
    cln.df %>%
    group_by(EVTYPE) %>%
    summarize(TOTAL_DAMAGE = sum(DAMAGE)) %>%
    arrange(desc(TOTAL_DAMAGE))

top.damage.df$EVTYPE <- 
    factor(
        top.damage.df$EVTYPE, 
        levels = top.damage.df$EVTYPE[order(top.damage.df$TOTAL_DAMAGE,  decreasing = TRUE)]
    )

damage.plot <-
    ggplot(data = top.damage.df[1:5,], aes(x = EVTYPE, y = TOTAL_DAMAGE / 1000000)) +
    geom_bar(stat = "identity") +
    xlab("Event Class") +
    ylab("Economic Damage (in million $)") + 
    ggtitle("Top Five Severe Weather Events Contributing to Economic Losses in the U.S., 1950-2011")

print(damage.plot)