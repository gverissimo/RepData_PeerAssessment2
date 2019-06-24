# clear workspace
ls()
rm(list=ls())

## Load  libraries
library(R.utils)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
##library(purrr)

## *** TITLE ***
## Your document should have a title that briefly summarizes your data analysis

## *** SYNOPSIS ***
## Immediately after the title, there should be a synopsis which describes and 
## summarizes your analysis in at most 10 complete sentences.

## *** DATA PROCESSING  ***
## There should be a section titled Data Processing which describes (in words and code) 
## how the data were loaded into R and processed for analysis. 
## In particular, your analysis must start from the raw CSV file containing the data. 
## You cannot do any preprocessing outside the document. 
## If preprocessing is time-consuming you may consider using the cache=TRUE option 
## for certain code chunks.

## download raw data
if (!file.exists("./data/repdata-data-StormData.csv.bz2")) {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                      destfile = "./data/repdata-data-StormData.csv.bz2",
                      method = "curl"
                      )
        bunzip2("./data/repdata-data-StormData.csv.bz2", "./data/repdata-data-StormData.csv", 
                remove = FALSE, 
                skip = TRUE
                )
}


if (!file.exists("./data/NWS_WFO_ID.txt")) {
        download.file("https://www.spc.noaa.gov/misc/NWS_WFO_ID.txt", 
                      destfile = "./data/NWS_WFO_ID.txt",
                      method = "curl"
        )
}


## import NWS Weather Forecast (WFO) data
NwsInfoId_raw <- readLines("./data/NWS_WFO_ID.txt")    ## load WFO ID text file
NwsInfoId_raw <- NwsInfoId_raw[-c(1:2)]                ## trim off first two title lines

## clean NWS WFO info
NwsInfoId_list <- NwsInfoId_raw %>%
        gsub(pattern = "\t+", replace = ",") %>%
        gsub(pattern = ", +", replace = ",") %>%
        strsplit(",")
## ...and convert to dataframe
NwsInfoId <- as.data.frame( 
        matrix(unlist(NwsInfoId_list), ncol=3, byrow=TRUE )
        )
names(NwsInfoId) = c("City", "State", "WFO")
print(NwsInfoId)  ## debug

## import storm incident dataset
stormData_raw <- read.csv(file = "./data/repdata-data-StormData.csv", 
                          sep = ",", 
                          header=TRUE, 
                          stringsAsFactors = FALSE
                          )

## first subset and tidy up the data
stormData <- stormData_raw %>% 
        ## subset the variables we'll be using for the analysis
        select(WFO, EVTYPE, BGN_DATE, BGN_TIME, 
               PROPDMG, PROPDMGEXP, 
               CROPDMG, CROPDMGEXP, 
               INJURIES, FATALITIES
               ) %>%
        
        ## replace missing WFO with NA
        mutate(
                WFO = ifelse(WFO=="", NA_character_, WFO)
        ) %>%

        ## uppercase event types and rename the variable
        mutate(EVTYPE = toupper(EVTYPE)) %>%
        rename(EventType = EVTYPE) %>%
        
        ## clean up event start date and combine date & time into a single variable
        separate(BGN_DATE, c("BGN_DATE", NA), sep="[ ]") %>%  ## discard dangling "0:00:00"
        mutate(
                ## combine date/time
                EventStart = paste(BGN_DATE, " ", substr(BGN_TIME,1,2), ":", substr(BGN_TIME,3,4),":00", sep=""),
                EventStart = mdy_hms(EventStart)
        ) %>%
        ## ...and drop the old variables
        select(-c(BGN_DATE, BGN_TIME)) %>%
        
        ## combine (PROPDMG+PROPDMGEXP and CROPDMG+CROPDMGEXP) into numbers
        mutate(
                PropertyDamage = PROPDMG*recode(toupper(PROPDMGEXP), 
                                                "H"=10^2, "K"=10^3, "M"=10^6, "B"=10^9, 
                                                "0"=1, "1"=10, "2"=10^2, "3"=10^3, "4"=10^4, "5"=10^5, 
                                                "6"=10^6, "7"=10^7, "8"=10^8, 
                                                "+"=1, "-"=1, .default=NA_real_),
                CropDamage = CROPDMG*recode(toupper(CROPDMGEXP), 
                                            "H"=10^2, "K"=10^3, "M"=10^6, "B"=10^9, 
                                            "0"=1, "1"=10, "2"=10^2, "3"=10^3, "4"=10^4, "5"=10^5, 
                                            "6"=10^6, "7"=10^7, "8"=10^8, 
                                            "+"=1, "-"=1, .default=NA_real_),
                ## for later ordering, sum propertyDamage + cropDamage
                PropertyAndCropDamage = ifelse(!is.na(PropertyDamage), PropertyDamage, 0) + 
                        ifelse(!is.na(CropDamage), CropDamage, 0),
                
        ) %>%
        ## ...and drop the old variables
        select(-c(PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)) %>%
        
        ## similarly combine fatalities and inturies into fatalitiesAndInjuries
        ## and rename injuries & fatalities ...just cuz I dislike allcaps
        mutate(
                FatalitiesAndInjuries = ifelse(!is.na(FATALITIES), FATALITIES, 0) + 
                        ifelse(!is.na(INJURIES), INJURIES, 0)
        ) %>%
        rename(Injuries = INJURIES) %>%
        rename(Fatalities = FATALITIES)

summary(stormData$PropertyAndCropDamage)  ## debug
summary(stormData$FatalitiesAndInjuries)  ## debug

## Consolidate and clean storm event descriptions
stormData <- stormData %>% 
        mutate(EventType = case_when(
                grepl("COLD|WINDCHILL|COOL|WINTER WEATHER", EventType) ~ "Cold",
                grepl("HEAT|WARM|HOT|DRY", EventType) ~ "Heat",
                grepl("DROUGHT|DRY|LOW RAINFALL", EventType) ~ "Drought",
                grepl("DUST", EventType) ~ "Dust",
                grepl("FIRE|SMOKE", EventType) ~ "Fire / Smoke",
                grepl("FLOOD|FLD|FLDG|FLOOOD|SURGE|HIGH WATER|SEICHE", EventType) ~ "Water / Flood",
                grepl("HURRICANE|CYCLONE|TYPHOON|TROPICAL STORM|TROPICAL DEPRESSION", EventType) ~  "Hurricane / Tropical Storm",
                grepl("TSTM|THUNDERSTORM|LIGHTNING|LIGHTING|LIGNTNING", EventType) ~ "Thunder / Lightning",
                grepl("TORNADO|TORNDAO|FUNNEL|GUSTNADO|WATERSPOUT", EventType) ~ "Tornado / Waterspout",
                grepl("RAIN|SHOWER|WET|DOWNBURST|PRECIPITATION|HEAVY MIX", EventType) ~ "Rain",
                grepl("SNOW|BLIZZARD|AVALANCHE|AVALANCE|ICE|ICY|FROST|FREEZE|FREEZING|GLAZE|HAIL|SLEET|FREEZING RAIN|WINTER STORM", EventType) ~ "Snow / Ice",
                grepl("SURF|CURRENT|TIDE|TIDAL FLOOD|COASTAL|FLOOD", EventType) ~ "Surf / Tide",
                grepl("EARETHQUAKE|TSUNAMI", EventType) ~ "Earthquake / Tsunami",
                grepl("AVALANCHE|AVALANCE", EventType) ~ "Avalanche",
                grepl("WIND|MICROBURST", EventType) ~ "High Winds",
                ## lower priority classifications
                grepl("EROSION|EROSIN|SLIDE|SLUMP", EventType) ~ "Erosion / Landslide",
                grepl("FOG", EventType) ~ "Fog",
                grepl("DAM", EventType) ~ "Dam Failure",
                grepl("VOLCANO|VOLCANIC", EventType) ~ "Volcano",
                is.na(EventType) ~ NA_character_,
                TRUE ~ "<other>"
                )
        )

## assess which years to utilize for the analysis
stormDates <- data.frame(EventStart=stormData$EventStart)
stormDates <-stormDates %>% 
        group_by(year=floor_date(EventStart, "year")) %>%
        summarize(counts=n())
plot(stormDates)

## looking at the plot, e see what appears to be an exponential curve but with significant
## jump above 30k events per year starting in the late 1990's -- specifically: 1996
## specifically:
stormDates %>% 
        filter(counts>=30000) %>%
        head(n=1)
##  <dttm>               <int>
##  1 1996-01-01 00:00:00  32270

## and filter stormData
targetStormData <- stormData %>% filter(EventStart >= as_date("1996-01-01 00:00:00"))

## summarize health impact
healthImpact <- targetStormData %>% 
        select(EventType, Injuries, Fatalities, FatalitiesAndInjuries) %>%
        group_by(EventType) %>% 
        summarise(
                TotalInjuries = sum(Injuries, na.rm = TRUE), 
                TotalFatalities = sum(Fatalities, na.rm = TRUE),
                TotalFatalitiesAndInjuries = TotalInjuries + TotalFatalities
        ) %>%
        arrange(desc(TotalFatalitiesAndInjuries)) %>%
        head(10) ## %>% print()

healthImpact %>%
        ## for sorting the bars, convert eventType to factors
        mutate(EventType = factor(EventType, levels = EventType[order(-TotalFatalitiesAndInjuries)])) %>%
        ## reshape data to long form for plotting
        gather(Type, HealthImpact, TotalInjuries:TotalFatalities) %>%
        ## and plot
        ggplot(aes(fill=Type, y=HealthImpact, x=EventType)) + 
        geom_bar( stat="identity") + 
        scale_fill_brewer(palette="Set1", 0.3) + 
        labs(y = "Health Impact (injuries & fatalities)") +
        theme(axis.text.x=element_text(angle=90, hjust=0.95, vjust = 0.2),
              axis.title.x = element_blank(),
        )

## summarize economic impact
economicImpact <- targetStormData %>% 
        select(EventType, PropertyDamage, CropDamage, PropertyAndCropDamage) %>%
        group_by(EventType) %>% 
        summarise(
                TotalPropertyDamage = sum(PropertyDamage, na.rm = TRUE)/10^9, 
                TotalCropDamage = sum(CropDamage, na.rm = TRUE)/10^9,
                TotalPropertyAndCropDamage = TotalPropertyDamage + TotalCropDamage
                ) %>%
        arrange(desc(TotalPropertyAndCropDamage)) %>%
        head(10) ## %>% print()

economicImpact %>%
        ## for sorting the bars, convert eventType to factors
        mutate(EventType = factor(EventType, levels = EventType[order(-TotalPropertyAndCropDamage)])) %>%
        ## reshape data to long form for plotting
        gather(Type, Damage, TotalPropertyDamage:TotalCropDamage) %>%
        ## and plot
        ggplot(aes(fill=Type, y=Damage, x=EventType)) + 
        geom_bar( stat="identity") + 
        scale_fill_brewer(palette="Set1") + 
        labs(y = "Economic Impact (in billions)") +
        theme(axis.text.x=element_text(angle=90, hjust=0.95, vjust = 0.2),
              axis.title.x = element_blank(),
              )



## ****************************************************************
## SCRATCHSPACE
## ****************************************************************
stormData <- stormData %>% 
        rename(LONGITUDE_E = LONGITUDE_) %>%
        ## delete the dangling  "0:00:00" from beginning date 
        separate(BGN_DATE, c("BGN_DATE", NA), sep="[ ]") %>%
        ## random cleanup
        
        mutate(
                ## convert various parameters to upper case (and remap exponent codes)
                TIME_ZONE = toupper(TIME_ZONE),
                EVTYPE = toupper(EVTYPE),
                PROPDMGEXP = toupper(PROPDMGEXP),
                CROPDMGEXP = toupper(CROPDMGEXP),
                
                ## combine lattitude/longitude
                LatLong = if_else( 
                        is.na(LATITUDE) | is.na(LONGITUDE), 
                        NA_character_,
                        paste(LATITUDE, LONGITUDE, sep = ",")
                ),
                
                ## replace missing & unknown data with NA
                LATITUDE = na_if(LATITUDE, 0),
                LONGITUDE = na_if(LONGITUDE, 0),
                LATITUDE_E = na_if(LATITUDE_E, 0),
                LONGITUDE_E = na_if(LONGITUDE_E, 0),
                WFO = replace(WFO, WFO=="", NA),
                TIME_ZONE = replace(TIME_ZONE, TIME_ZONE == "UNK", NA),
                
                ## fix apparent typos in time zones
                TIME_ZONE = replace(TIME_ZONE, TIME_ZONE == "UTC", "GMT"),
                TIME_ZONE = replace(TIME_ZONE, TIME_ZONE == "ESY", "EST"),
                TIME_ZONE = replace(TIME_ZONE, TIME_ZONE == "CSC", "CST"),
                TIME_ZONE = replace(TIME_ZONE, TIME_ZONE == "SCT", "CST")
        )

## not in TZ list
## 
oddSTATE <- c("AM", "AN", "DC", "GM", "LC", "LE", "LH", "LM", "LO", "LS", 
              "MH", "PH", "PK", "PM", "PR", "PZ", "SL", "ST", "XX")
stormData %>% 
        select(STATE, TIME_ZONE, WFO) %>%
        filter(STATE %in% oddSTATE) %>%
        group_by(STATE) %>%
        summarise(
                count = n()
        )
## AM    AN    DC    GM    LC    LE    LH    LM    LO    LS    MH    PH    PK    PM    PR    PZ    SL    ST    XX
## 1879  3250  437   5337  274   1526  654   1347  70    262   1     28    23    1     3015  96    7     1     2

oddStateTZ <- stormData %>% 
        select(STATE, TIME_ZONE, WFO) %>%
        filter(STATE %in% oddSTATE) %>%
        group_by(STATE, TIME_ZONE) %>%
        summarise(
                count = n()
        )

ollStateTZ <- stormData %>% 
        select(STATE, TIME_ZONE, WFO) %>%
        group_by(STATE, TIME_ZONE, WFO) %>%
        summarise(
                count = n()
        )

ollStateTZ <- stormData %>% 
        select(STATE, TIME_ZONE, WFO) %>%
        group_by(STATE, WFO, TIME_ZONE) %>%
        summarise(
                count = n()
        )

##  STATE TIME_ZONE count
##  1	AM	AST	149
##  2	AM	EST	1730
##  3	AN	EST	3250
##  4	DC	CST	37
##  5	DC	EST	391
##  6	DC	PST	9
##  7	GM	CST	3134
##  8	GM	EST	2203
##  9	LC	EST	274
##  10	LE	EST	1526
##  11	LH	EST	654
##  12	LM	CST	748
##  13	LM	EST	599
##  14	LO	EST	70
##  15	LS	CST	41
##  16	LS	EST	221
##  17	MH	GMT	1
##  18	PH	HST	28
##  19	PK	AKS	13
##  20	PK	AST	10
##  21	PM	SST	1
##  22	PR	AST	3014
##  23	PR	CST	1
##  24	PZ	PST	96
##  25	SL	EST	7
##  26	ST	CST	1
##  27	XX	EST	2

stormData %>% 
        select(TIME_ZONE) %>%
        group_by(TIME_ZONE) %>%
        count(n=n())

eventTypesAll <- stormData %>% 
        select(EVTYPE) %>%
        group_by(EVTYPE) %>%
        count(n=n())

## Timezones file compiled from: 
## list of timezones in the US:
##     - https://en.wikipedia.org/wiki/Time_in_the_United_States
## list of NWS timezone indicators: 
##     - https://www.nws.noaa.gov/directives/sym/pd01017001curr.pdf
## list of NWS offices:  
##     - https://www.spc.noaa.gov/misc/NWS_WFO_ID.txt
## list of TZ data f(compiled from (IANA time zone database):
##     = https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
## other:
##     - http://www.timezoneconverter.com/cgi-bin/zonehelp?cc=US&ccdesc=United%20States
##
## load US time zones csv file
UsTimeZones <- read.csv("./data/US_Timezones.csv", skip = 2, na.strings = "UNK") 
names(UsTimeZones) = c("STATNAME", "STATE", "StdTimeZone", "TZ")
print(UsTimeZones)

##****************************************************************
## ~~~~~~ THIS WORKS!!  ~~~~~~
## solution from: Stackoverflow
## user: spren9er
## answered Jul 31 '18 at 10:47
## edited Aug 1 '18 at 18:37
## https://stackoverflow.com/a/51611574/6833187

timestamp_utc <- c("2018-03-10 23:30:00", "2018-03-10 23:30:00", "2018-03-10 23:30:00")
local_tz <- c("America/Los_Angeles", "America/Chicago", "America/Los_Angeles")
data <- data.frame(timestamp_utc, local_tz, stringsAsFactors = FALSE)
data


with_tz_utc <- function(ts, tz) force_tz(with_tz(ts, tz), 'UTC')
as_datetime_with_tz_utc <- compose(as_datetime, Vectorize(with_tz_utc))

##Now use mutate as usual

data %>%
        mutate(
                timestamp_utc = as_datetime(timestamp_utc),
                timestamp_local = as_datetime_with_tz_utc(timestamp_utc, local_tz)
        )

## this works!!!!
##         timestamp_utc            local_tz     timestamp_local
##  1 2018-03-10 23:30:00 America/Los_Angeles 2018-03-10 15:30:00
##  2 2018-03-10 23:30:00     America/Chicago 2018-03-10 17:30:00
##  3 2018-03-10 23:30:00 America/Los_Angeles 2018-03-10 15:30:00


##****************************************************************
## ~~~~~~ THIS DOESN"T SEEM TO WORK  ~~~~~~
## solution from: Stackoverflow
## user: josiekre
## answered Nov 23 '15 at 16:05
## edited Nov 24 '15 at 16:57
## https://stackoverflow.com/a/33875411/6833187

timestamp_utc <- c("2018-03-10 23:30:00", "2018-03-10 23:30:00", "2018-03-10 23:30:00")
local_tz <- c("America/Los_Angeles", "America/Chicago", "America/Los_Angeles")
data <- data.frame(timestamp_utc, local_tz, stringsAsFactors = FALSE)
data

get_local_time <- function(timestamp_utc, local_tz) {
        l <- lapply(seq(length(timestamp_utc)), 
                    function(x) {with_tz(timestamp_utc[x], local_tz[x])})
        combine(l)
}

data$timestamp_utc <- as.POSIXct(data$timestamp_utc, tz = "UTC")

data %>%
        group_by(local_tz) %>%
        mutate(timestamp_local = get_local_time(timestamp_utc, tz = local_tz))

## This doesn't seem to work:
## Error in get_local_time(timestamp_utc, tz = local_tz) : 
##     unused argument (tz = local_tz)
##


## NA, ADT, AKS, AST, CDT, CST, EDT, EST, GST, HST, MDT, MST, PDT, PST, SST, GMT, UTC

## check storm data for any remaining time zone  inconsistencies
stormData %>% 
        select(TIME_ZONE) %>%
        group_by(TIME_ZONE) %>%
        count(n=n())

stormData %>% 
        select(TIME_ZONE, STATE, WFO) %>%
        filter(TIME_ZONE %in% c("UTC")) %>%
        group_by(TIME_ZONE, STATE, WFO) %>%
        summarise(
                count = n()
        )
## 
## 4/18/1950 0130

summary(unique(stormData$EVTYPE))


## *** RESULTS ***
## There should be a section titled Results in which your results are presented.

### *** NOTES ***
### 1) The analysis document must have at least one figure containing a plot.
### 2) AND the analysis must have no more than three figures. 
###    - Figures may have multiple plots in them (i.e. panel plots), 
###    - but there cannot be more than three figures total.
### 3) You must show all your code for the work in your analysis document. 
###    - This may make the document a bit verbose, but that is okay. 
###    - In general, you should ensure that echo = TRUE for every code chunk 
###      (this is the default setting in knitr).

### *** REVIEW CRITERIA ***
### Review criterialess 
###  1) Has either a (1) valid RPubs URL pointing to a data analysis document for this 
###     assignment been submitted; or (2) a complete PDF file presenting the data 
###     analysis been uploaded?
###  2) Is the document written in English?
###  3) Does the analysis include description and justification for any data 
###     transformations?
###  4) Does the document have a title that briefly summarizes the data analysis?
###  5) Does the document have a synopsis that describes and summarizes the data  
###     analysis in less than 10 sentences?
###  6) Is there a section titled "Data Processing" that describes how the data were 
###     loaded into R and processed for analysis?
###  7) Is there a section titled "Results" where the main results are presented?
###  8) Is there at least one figure in the document that contains a plot?
###  9) Are there at most 3 figures in this document?
### 10) Does the analysis start from the raw data file (i.e. the original .csv.bz2 file)?
### 11) Does the analysis address the question of which types of events are most harmful 
###     to population health?
### 12) Does the analysis address the question of which types of events have the 
###     greatest economic consequences?
### 13) Do all the results of the analysis (i.e. figures, tables, numerical summaries) 
###     appear to be reproducible?
### 14) Do the figure(s) have descriptive captions (i.e. there is a description near the 
###     figure of what is happening in the figure)?
### 15) As far as you can determine, does it appear that the work submitted for this 
###     project is the work of the student who submitted it?

## test push
