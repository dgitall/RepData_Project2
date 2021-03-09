library(data.table)
library(R.utils)
library(stringr)

## Download and unzip the zip file from the course website
#     NOTE: checks to see if this was already done and skips if yes
destFolder = "./data"
if(!file.exists(destFolder)) {dir.create(destFolder)}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destFile = "./data/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists(destFile)) {
  download.file(fileUrl, destfile = destFile, method = "curl")
}
dataFile <-   bunzip2(filename=destFile,skip = TRUE, remove=FALSE)

##  Load the data file
rawData <- data.table(read.csv(dataFile, header = TRUE))


## Data Cleaning
# Make the column titles into valid names

Data <- data.table()
Data$EVTYPE <- rawData$EVTYPE
Data$FATALITIES <- rawData$FATALITIES
Data$INJURIES <- rawData$INJURIES
Data$PROPDMG <- rawData$PROPDMG
Data$PROPDMGEXP <- rawData$PROPDMGEXP
Data$CROPDMG <- rawData$CROPDMG
Data$CROPDMGEXP <- rawData$CROPDMGEXP
## The docs say there are 48 event types but there are far more unique types
## In the data. Let's work on the misspellings and other errors to get to a
## reasonable subset. We'll also merge some of the categories where it makes sense.
print(uniqueN(Data$EVTYPE))
# First, change all to upper case and copy to a new variable to maintain the
# original for checking later.
Data$NEW_EVTYPE <- toupper(Data$EVTYPE)
print(uniqueN(Data$NEW_EVTYPE))
# Set the Event type to NA for those that aren't recording an event
Data[grepl("SUMMARY", NEW_EVTYPE), NEW_EVTYPE:= NA]
Data[grepl("NORMAL", NEW_EVTYPE), NEW_EVTYPE:= NA]
Data[grepl("MONTHLY", NEW_EVTYPE), NEW_EVTYPE:= NA]
Data[grepl("NO SEVERE", NEW_EVTYPE), NEW_EVTYPE:= NA]
Data[grepl("NONE", NEW_EVTYPE), NEW_EVTYPE:= NA]
Data[grepl("NORTHERN LIGHTS", NEW_EVTYPE), NEW_EVTYPE:= NA]
print(uniqueN(Data$NEW_EVTYPE))
# Merge anything with 'Winter Weather' in it to just that. Also, include
# variations on 'Wintery Mix'
# setDT(Data)[, EVTYPE := str_replace(EVTYPE, c("WINTER WEATHER", "WINTERY", "WINTRY"), "WINTER WEATHER")]
Data[grepl("WINTER WEATHER", NEW_EVTYPE), NEW_EVTYPE:= "WINTER WEATHER"]
Data[grepl("WINTERY", NEW_EVTYPE), NEW_EVTYPE:= "WINTER WEATHER"]
Data[grepl("WINTRY", NEW_EVTYPE), NEW_EVTYPE:= "WINTER WEATHER"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("WINTER STORM", NEW_EVTYPE), NEW_EVTYPE:= "WINTER STORM"]
Data[grepl("WINTER MIX", NEW_EVTYPE), NEW_EVTYPE:= "WINTER STORM"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("FIRE", NEW_EVTYPE), NEW_EVTYPE:= "WILDFIRE"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("WATERSPOUT", NEW_EVTYPE), NEW_EVTYPE:= "WATERSPOUT"]
Data[grepl("WATER SPOUT", NEW_EVTYPE), NEW_EVTYPE:= "WATERSPOUT"]
Data[grepl("WAYTERSPOUT", NEW_EVTYPE), NEW_EVTYPE:= "WATERSPOUT"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("VOLCAN", NEW_EVTYPE), NEW_EVTYPE:= "VOLCANIC ASH"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("TROPICAL STORM", NEW_EVTYPE), NEW_EVTYPE:= "TROPICAL STORM"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("TORNADO", NEW_EVTYPE), NEW_EVTYPE:= "TORNADO"]
Data[grepl("TORNADOE", NEW_EVTYPE), NEW_EVTYPE:= "TORNADO"]
Data[grepl("TORNDAO", NEW_EVTYPE), NEW_EVTYPE:= "TORNADO"]
print(uniqueN(Data$NEW_EVTYPE))
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("BLIZZARD", NEW_EVTYPE), NEW_EVTYPE:= "BLIZZARD"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("WIND CH", NEW_EVTYPE), NEW_EVTYPE:= "EXTREME COLD/WIND CHILL"]
Data[grepl("WINDCHILL", NEW_EVTYPE), NEW_EVTYPE:= "EXTREME COLD/WIND CHILL"]
Data[grepl("COLD", NEW_EVTYPE), NEW_EVTYPE:= "EXTREME COLD/WIND CHILL"]
Data[grepl("COOL", NEW_EVTYPE), NEW_EVTYPE:= "EXTREME COLD/WIND CHILL"]
Data[grepl("HYPOTHERMIA", NEW_EVTYPE), NEW_EVTYPE:= "EXTREME COLD/WIND CHILL"]
Data[grepl("HYPERTHERMIA", NEW_EVTYPE), NEW_EVTYPE:= "EXTREME COLD/WIND CHILL"]
Data[grepl("LOW TEMPERATURE", NEW_EVTYPE), NEW_EVTYPE:= "EXTREME COLD/WIND CHILL"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("FUNNEL", NEW_EVTYPE), NEW_EVTYPE:= "FUNNEL CLOUD"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("SURF", NEW_EVTYPE), NEW_EVTYPE:= "HIGH SURF"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("SURGE", NEW_EVTYPE), NEW_EVTYPE:= "STORM SURGE/TIDE"]
Data[grepl("SWELL", NEW_EVTYPE), NEW_EVTYPE:= "STORM SURGE/TIDE"]
Data[grepl("WAVE", NEW_EVTYPE), NEW_EVTYPE:= "STORM SURGE/TIDE"]
Data[grepl("SEA", NEW_EVTYPE), NEW_EVTYPE:= "STORM SURGE/TIDE"]
Data[grepl("BLOW-OUT TIDE", NEW_EVTYPE), NEW_EVTYPE:= "STORM SURGE/TIDE"]
Data[grepl("HIGH TIDE", NEW_EVTYPE), NEW_EVTYPE:= "STORM SURGE/TIDE"]
Data[grepl("COASTAL STORM", NEW_EVTYPE), NEW_EVTYPE:= "STORM SURGE/TIDE"]
Data[grepl("COASTALSTORM", NEW_EVTYPE), NEW_EVTYPE:= "STORM SURGE/TIDE"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("HURRICANE", NEW_EVTYPE), NEW_EVTYPE:= "HURRICANE/TYPHOON"]
Data[grepl("TYPHOON", NEW_EVTYPE), NEW_EVTYPE:= "HURRICANE/TYPHOON"]
Data[grepl("REMNANTS OF FLOYD", NEW_EVTYPE), NEW_EVTYPE:= "HURRICANE/TYPHOON"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("FROST", NEW_EVTYPE), NEW_EVTYPE:= "FROST/FREEZE"]
Data[grepl("FREEZE", NEW_EVTYPE), NEW_EVTYPE:= "FROST/FREEZE"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("RIP CURRENT", NEW_EVTYPE), NEW_EVTYPE:= "RIP CURRENT"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("LANDSL", NEW_EVTYPE), NEW_EVTYPE:= "DEBRIS FLOW"]
Data[grepl("DAM BREAK", NEW_EVTYPE), NEW_EVTYPE:= "DEBRIS FLOW"]
Data[grepl("DAM FAILURE", NEW_EVTYPE), NEW_EVTYPE:= "DEBRIS FLOW"]
Data[grepl("MUD", NEW_EVTYPE), NEW_EVTYPE:= "DEBRIS FLOW"]
Data[grepl("ROCK", NEW_EVTYPE), NEW_EVTYPE:= "DEBRIS FLOW"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("AVAL", NEW_EVTYPE), NEW_EVTYPE:= "AVALANCHE"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("DRY", NEW_EVTYPE), NEW_EVTYPE:= "DROUGHT"]
Data[grepl("DROUGHT", NEW_EVTYPE), NEW_EVTYPE:= "DROUGHT"]
Data[grepl("BELOW NORMAL PRECIPITATION", NEW_EVTYPE), NEW_EVTYPE:= "DROUGHT"]
Data[grepl("RECORD LOW RAINFALL", NEW_EVTYPE), NEW_EVTYPE:= "DROUGHT"]
Data[grepl("DRIEST MONTH", NEW_EVTYPE), NEW_EVTYPE:= "DROUGHT"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("LIGHTNING", NEW_EVTYPE), NEW_EVTYPE:= "LIGHTNING"]
Data[grepl("LIGHTING", NEW_EVTYPE), NEW_EVTYPE:= "LIGHTNING"]
Data[grepl("LIGNTNING", NEW_EVTYPE), NEW_EVTYPE:= "LIGHTNING"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("DENSE FOG", NEW_EVTYPE), NEW_EVTYPE:= "FOG"]
Data[grepl("VOG", NEW_EVTYPE), NEW_EVTYPE:= "FOG"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("ICE", NEW_EVTYPE), NEW_EVTYPE:= "ICE STORM"]
Data[grepl("ICY", NEW_EVTYPE), NEW_EVTYPE:= "ICE STORM"]
Data[grepl("GLAZE", NEW_EVTYPE), NEW_EVTYPE:= "ICE STORM"]
Data[grepl("FREEZING RAIN", NEW_EVTYPE), NEW_EVTYPE:= "ICE STORM"]
Data[grepl("FREEZING DRIZZLE", NEW_EVTYPE), NEW_EVTYPE:= "ICE STORM"]
Data[grepl("FREEZING SPRAY", NEW_EVTYPE), NEW_EVTYPE:= "ICE STORM"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("LAKE EFFECT", NEW_EVTYPE), NEW_EVTYPE:= "LAKE-EFFECT"]
Data[grepl("LAKE-EFFECT", NEW_EVTYPE), NEW_EVTYPE:= "LAKE-EFFECT"]
Data[grepl("LAKE SNOW", NEW_EVTYPE), NEW_EVTYPE:= "LAKE-EFFECT"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("SNOW", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY SNOW"]
print(uniqueN(Data$NEW_EVTYPE))
## Combine together all types of Hail into one
Data[grepl("HAIL", NEW_EVTYPE), NEW_EVTYPE:= "HAIL"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("HEAT", NEW_EVTYPE), NEW_EVTYPE:= "EXCESSIVE HEAT"]
Data[grepl("HOT", NEW_EVTYPE), NEW_EVTYPE:= "EXCESSIVE HEAT"]
Data[grepl("WARM", NEW_EVTYPE), NEW_EVTYPE:= "EXCESSIVE HEAT"]
Data[grepl("HIGH TEMP", NEW_EVTYPE), NEW_EVTYPE:= "EXCESSIVE HEAT"]
Data[grepl("RECORD TEMP", NEW_EVTYPE), NEW_EVTYPE:= "EXCESSIVE HEAT"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("RAIN", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY RAIN"]
Data[grepl("HEAVY SHOWER", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY RAIN"]
Data[grepl("HEAVY PRECIP", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY RAIN"]
Data[grepl("MIXED PRECIP", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY RAIN"]
Data[grepl("HEAVY MIX", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY RAIN"]
Data[grepl("BURST", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY RAIN"]
Data[grepl("LANDSPOUT", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY RAIN"]
Data[grepl("RECORD PRECIPITATION", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY RAIN"]
Data[grepl("EXCESSIVE PRECIPITATION", NEW_EVTYPE), NEW_EVTYPE:= "HEAVY RAIN"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("SMOKE", NEW_EVTYPE), NEW_EVTYPE:= "DENSE SMOKE"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("SLEET", NEW_EVTYPE), NEW_EVTYPE:= "SLEET"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("WIND", NEW_EVTYPE), NEW_EVTYPE:= "STRONG WIND"]
Data[grepl("GUST", NEW_EVTYPE), NEW_EVTYPE:= "STRONG WIND"]
Data[grepl("WND", NEW_EVTYPE), NEW_EVTYPE:= "STRONG WIND"]
Data[grepl("SEVERE TURBULENCE", NEW_EVTYPE), NEW_EVTYPE:= "STRONG WIND"]
print(uniqueN(Data$NEW_EVTYPE))
Data[grepl("THUNDERSTORM", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("TSTM", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("TUNDERSTORM", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("THUNERSTORM", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("THUNDERTSORM", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("THUNDERTORM", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("THUNDESTORM", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("THUNDERSTORMW", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("THUNDERESTORM", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("THUNDEERSTORM", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("THUDERSTORM WIND", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
Data[grepl("WALL CLOUD", NEW_EVTYPE), NEW_EVTYPE:= "THUNDERSTORM"]
print(uniqueN(Data$NEW_EVTYPE))
## Combine together all types of flooding into one
Data[grepl("FLOO", NEW_EVTYPE), NEW_EVTYPE:= "FLOOD"]
Data[grepl("WET", NEW_EVTYPE), NEW_EVTYPE:= "FLOOD"]
Data[grepl("EROS", NEW_EVTYPE), NEW_EVTYPE:= "FLOOD"]
Data[grepl("HIGH WATER", NEW_EVTYPE), NEW_EVTYPE:= "FLOOD"]
Data[grepl("FLD", NEW_EVTYPE), NEW_EVTYPE:= "FLOOD"]
Data[grepl("STREAM", NEW_EVTYPE), NEW_EVTYPE:= "FLOOD"]
Data[grepl("URBAN", NEW_EVTYPE), NEW_EVTYPE:= "FLOOD"]
Data[grepl("RAPIDLY RISING WATER", NEW_EVTYPE), NEW_EVTYPE:= "FLOOD"]
Data[grepl("METRO STORM", NEW_EVTYPE), NEW_EVTYPE:= "FLOOD"]
print(uniqueN(Data$NEW_EVTYPE))
# Combine all dust events into dust storms
Data[grepl("DUST", NEW_EVTYPE), NEW_EVTYPE:= "DUST STORM"]
print(uniqueN(Data$NEW_EVTYPE))
# Those that don't fit a category or are missing enough info are assigned to OTHER
Data[grepl("\\?", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("APACHE COUNTY", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("HIGH", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("MARINE ACCIDENT", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("MARINE MISHAP", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("MILD PATTERN", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("SOUTHEAST", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("TEMPERATURE RECORD", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("DROWNING", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("TEMPERATURE RECORD", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("RED FLAG CRITERIA", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("RECORD LOW", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
Data[grepl("EXCESSIVE$", NEW_EVTYPE), NEW_EVTYPE:= "OTHER"]
print(uniqueN(Data$NEW_EVTYPE))

EVTable <- data.table(table(Data$NEW_EVTYPE))

Data$PROPERTY_DMG <- Data$PROPDMG
setDT(Data)[PROPDMGEXP %in% c('B','b'), PROPERTY_DMG:= PROPERTY_DMG*10^9]
setDT(Data)[PROPDMGEXP %in% c('M','m'), PROPERTY_DMG:= PROPERTY_DMG*10^6]
setDT(Data)[PROPDMGEXP %in% c('K','k'), PROPERTY_DMG:= PROPERTY_DMG*10^3]
setDT(Data)[PROPDMGEXP %in% c('H','h'), PROPERTY_DMG:= PROPERTY_DMG*10^2]
setDT(Data)[PROPDMGEXP %in% c('0','1','2','3','4','5','6','7','8','9'),
            PROPERTY_DMG:= PROPERTY_DMG*10]

Data$CROP_DMG <- Data$CROPDMG
setDT(Data)[CROPDMGEXP %in% c('B','b'), CROP_DMG:= CROP_DMG*10^9]
setDT(Data)[CROPDMGEXP %in% c('M','m'), CROP_DMG:= CROP_DMG*10^6]
setDT(Data)[CROPDMGEXP %in% c('K','k'), CROP_DMG:= CROP_DMG*10^3]
setDT(Data)[CROPDMGEXP %in% c('H','h'), CROP_DMG:= CROP_DMG*10^2]
setDT(Data)[CROPDMGEXP %in% c('0','1','2','3','4','5','6','7','8','9'),
            CROP_DMG:= CROP_DMG*10]

PropDmg <- Data[,list(PROPERTY_DMG = sum(PROPERTY_DMG, na.rm = T)),by='NEW_EVTYPE']
CropDmg <- Data[,list(CROP_DMG = sum(CROP_DMG, na.rm = T)),by='NEW_EVTYPE']
Damage <- merge(PropDmg, CropDmg, by='NEW_EVTYPE')
Damage$TOTAL_DMG <- Damage$CROP_DMG + Damage$PROPERTY_DMG
Damage <- Damage[order(-TOTAL_DMG)]
Damage$Graph <- "Full"
PartDamage <- Damage[1:4,]
PartDamage$Graph <- "Partial"
Damage <- rbind(Damage, PartDamage)

g <- ggplot(Damage, aes(x=NEW_EVTYPE, y=TOTAL_DMG/10^9), group=Graph ) + 
  geom_bar(stat="identity") + 
  scale_x_discrete(limits=Damage$NEW_EVTYPE, labels=rep("",length(Damage$NEW_EVTYPE))) +
  labs(title = "Total Damage from U.S. Storm Events (1950-2020)",
       x="", y="Total Damage (Billion Dollars)") +
  facet_grid(Graph~., scales = "free_x") + 
  geom_col(fill="green") + 

  labs(title = "Total Damage from U.S. Storm Events (1950-2020)",
       x="Storm Event", y="Total Damage (Billion Dollars)")

print(g)
  
