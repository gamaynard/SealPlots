## ---------------------------
##
## Script name: DeterrentCheck.R
##
## Purpose of script:
##
## Author: George A. Maynard
##
## Date Created: 2020-06-18
##
## Copyright (c) George Alphonse Maynard, 2020
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory

## ---------------------------

## Set options
options(scipen = 6, digits = 4) # eliminate scientific notation
## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(RMySQL)
library(lubridate)
library(vioplot)
## ---------------------------

## load up our functions into memory

## ---------------------------
## Read in and join all data from the database
db=dbConnect(
  MySQL(), 
  dbname="HaulDB",
  host='localhost',
  port=3306,
  user=user,
  password=password
)
species=dbReadTable(
  conn=db,
  name="species"
)
trap=dbReadTable(
  conn=db,
  name="trap"
)
tend=dbReadTable(
  conn=db,
  name="tend"
)
hauls=dbReadTable(
  conn=db,
  name="haul"
)
hauls$SpeciesID=as.character(hauls$Species_SpeciesID)
hauls$Species_SpeciesID=NULL
hauls=merge(hauls,species)
rm(species)
hauls$TendID=as.character(hauls$Tend_TendID)
hauls$Tend_TendID=NULL
hauls=merge(hauls,tend)
hauls$TrapID=as.character(hauls$Trap_TrapID)
hauls$Trap_TrapID=NULL
hauls=merge(hauls,trap)
rm(trap)

## Create a list of dates when the deterrent was operational
op=round_date(ymd_hms(subset(tend,tend$DeterrentOperational==1)$Date),unit="day")
## Assign each trap day a "mode"
## Separate out deterrent operation into single tone, multi-tone, and off
tend$mode="NONE"
tend$Date=round_date(ymd_hms(tend$Date),unit="day")
multi=tend$Date[which(grepl("Tones",tend$DeterrentNotes))]
tend$mode=ifelse(
  tend$Date>=min(multi),
  "MULTI",
  "SINGLE"
)
tend$mode=ifelse(
  round_date(tend$Date,unit="day")%in%op==FALSE,
  "NONE",
  tend$mode
)
tend$mode=factor(
  tend$mode,
  levels=c("NONE","SINGLE","MULTI"),
  ordered=TRUE
)
## Subset out data from the trap of interest
data=tend
#trap="OFFSHORE"
trap="INSHORE"
#trap="BOTH"
if(trap=="OFFSHORE"){
  data=subset(data,data$Trap_TrapID==2)
  data$DeterrentOperational=ifelse(data$Date%in%op,1,0)
} else {
  if(trap=="INSHORE"){
    data=subset(data,data$Trap_TrapID==1)
  } else {
    data=data
    data$DeterrentOperational=ifelse(data$Date%in%op,1,0)
  }
}
## Plot the number of seals based on deterrent operation
boxplot(
  data$NumSeals~data$DeterrentOperational,
  xlab="Deterrent Operational",
  ylab="Seals Observed at / near Trap",
  main=trap
  )
## Add a watermark because the data is preliminary
text(x = grconvertX(0.5, from = "npc"),  # align to center of plot X axis
     y = grconvertY(0.5, from = "npc"), # align to center of plot Y axis
     labels = "PRELIMINARY", # our watermark
     cex = 3, font = 2, # large, bold font - hard to miss
     col = rgb(1, 0, 0, .5), # translucent (0.2 = 20%) red color
     srt = 45) # srt = angle of text: 45 degree angle to X axis
# Add another watermark in lower (side = 1) right (adj = 1) corner
watermark=paste(
  "DATA CURRENT TO: ", 
  max(round_date((data$Date),unit="day"))
  )
mtext(watermark, side = 1, line = -1, adj = 1, col = rgb(1, 0, 0, .5), cex = 1.2)
wilcox.test(data$NumSeals~data$DeterrentOperational)
## Plot the number of seals based on deterrent mode
boxplot(
  data$NumSeals~data$mode,
  xlab="Deterrent Mode",
  ylab="Seals Observed at / near Trap",
  main=trap
)
## Add a watermark because the data is preliminary
text(x = grconvertX(0.5, from = "npc"),  # align to center of plot X axis
     y = grconvertY(0.5, from = "npc"), # align to center of plot Y axis
     labels = "PRELIMINARY", # our watermark
     cex = 3, font = 2, # large, bold font - hard to miss
     col = rgb(1, 0, 0, .5), # translucent (0.2 = 20%) red color
     srt = 45) # srt = angle of text: 45 degree angle to X axis
# Add another watermark in lower (side = 1) right (adj = 1) corner
watermark=paste(
  "DATA CURRENT TO: ", 
  max(data$Date)
)
mtext(watermark, side = 1, line = -1, adj = 1, col = rgb(1, 0, 0, .5), cex = 1.2)
x=kruskal.test(data$NumSeals~data$mode)
x
## Test for differences in weight of a species of interest hauled and weight 
## partially consumed when the deterrent was running or not
## Species of interest (soi)
soi="ATLANTIC MENHADEN"
data=subset(
  hauls,
  hauls$TrapID==1&hauls$CommonName==soi
  )
lumf=subset(
  data,
  data$Action=="PARTIALLY CONSUMED"
)
land=subset(
  data,
  data$Action=="HAULED"
)
boxplot(
  land$Quantity~land$DeterrentOperational,
  xlab="Deterrent Operational",
  ylab="Landed (lbs)",
  main=paste(soi," Landings",sep="")
)
wilcox.test(land$Quantity~land$DeterrentOperational)
boxplot(
  lumf$Quantity~lumf$DeterrentOperational,
  xlab="Deterrent Operational",
  ylab="LUMF (lbs)",
  main=paste(soi," Legal and Unmarketable",sep="")
)
wilcox.test(lumf$Quantity~lumf$DeterrentOperational)

