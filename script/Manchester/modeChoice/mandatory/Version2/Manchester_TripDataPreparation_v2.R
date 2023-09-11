### Clear memory
rm(list = ls())

suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
suppressPackageStartupMessages(library(fitdistrplus)) # for log normal distributions
suppressPackageStartupMessages(library(ggplot2)) # for plotting data
suppressPackageStartupMessages(library(purrr)) # for nested dataframes
suppressPackageStartupMessages(library(stringr))# for editing columns 
suppressPackageStartupMessages(library(tidyverse))# for manipulating data
suppressPackageStartupMessages(library(expss))# for manipulating data

#reading files
trads <- read_rds("data/manchester/TRADS_safe.rds")
list2env(trads, globalenv())
trips <- trips
route_short <- read.csv("data/manchester/AllShort92.csv", header = T)
route_fast <- read.csv("data/manchester/AllFast92.csv", header = T)
updatedpt <- read.csv("data/manchester/ptIndicators.csv", header = T)

## renaming columns
names(route_short)[names(route_short) == "IDNumber"] <- "hh.id"
names(route_short)[names(route_short) == "PersonNumber"] <- "p.id"
names(route_short)[names(route_short) == "TripNumber"] <- "t.id"
names(route_fast)[names(route_fast) == "IDNumber"] <- "hh.id"
names(route_fast)[names(route_fast) == "PersonNumber"] <- "p.id"
names(route_fast)[names(route_fast) == "TripNumber"] <- "t.id"


# #creating unique id for individuals
# indiv <- indiv %>%
#   unite("indiv.id", c('hh.id', 'p.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
#   relocate(indiv.id, .after = hh.id)
# route_short <- route_short %>%
#   unite("indiv.id", c('hh.id', 'p.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
#   relocate(indiv.id, .after = hh.id)
# route_fast <- route_fast %>%
#   unite("indiv.id", c('hh.id', 'p.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
#   relocate(indiv.id, .after = hh.id)
# 
# #creating unique id for trips
# # trips$trip.id <- paste(trips$hh.id, oldtrips$p.id, oldtrips$t.id, sep ='')
# # trips <- trips %>% relocate(trip.id, .after = t.id)
# route_short <- route_short %>%
#   unite("trip.id", c('indiv.id', 't.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
#   relocate(trip.id, .after = indiv.id)
# route_fast <- route_fast %>%
#   unite("trip.id", c('indiv.id', 't.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
#   relocate(trip.id, .after = indiv.id)


## adding short and fast to the columns name
start_col <- 4
end_col <- 24
colnames(route_short)[start_col:end_col] <- paste("short_", colnames(route_short)[start_col:end_col], sep = "")
colnames(route_fast)[start_col:end_col] <- paste("fast_", colnames(route_fast)[start_col:end_col], sep = "")

## merging files
route_all <- left_join(route_short, route_fast)
trips <- left_join(route_all, trips)
trips <- left_join(trips, updatedpt)
trips_hh <- left_join(trips, households)
trips_hh_p <- left_join(trips_hh, indiv)

# deleting the old BE variables
names(trips)[names(trips) == "t.route.pt_ptTravelTime"] <- "oldpt_ptTravelTime"
names(trips)[names(trips) == "t.route.pt_totalTravelTime"] <- "oldpt_totalTravelTime"
names(trips)[names(trips) == "t.route.pt_walkTravelTime"] <- "oldpt_walkTravelTime"
names(trips)[names(trips) == "t.route.car_time"] <- "troutecar_time"
names(trips)[names(trips) == "t.route.walk_short_time"] <- "troutewalk_short_time"
names(trips)[names(trips) == "t.route.bike_short_time"] <- "troutebike_short_time"
cols_to_delete <- grep("t.route", names(trips), value = TRUE)
trips <- trips[, !(names(trips) %in% cols_to_delete)]

#recoding sex var
trips_hh_p$sex[trips_hh_p$p.female=="TRUE"] = 1
trips_hh_p$sex[trips_hh_p$p.female=="FALSE"] = 0
trips_hh_p <- trips_hh_p[!(is.na(trips_hh_p$sex)),]

#trips_hh_p$sex <- factor(trips_hh_p$sex, levels = c(1,0),labels = c("female", "male"))

#recoding age var
trips_hh_p$agegroup[trips_hh_p$p.age_group=="5-9"|trips_hh_p$p.age_group=="10-14"] = 1
trips_hh_p$agegroup[trips_hh_p$p.age_group=="15-19"|trips_hh_p$p.age_group=="20-24"] = 2
trips_hh_p$agegroup[trips_hh_p$p.age_group=="25-29"|trips_hh_p$p.age_group=="30-34"] = 3
trips_hh_p$agegroup[trips_hh_p$p.age_group=="35-39"|trips_hh_p$p.age_group=="40-44"] = 4
trips_hh_p$agegroup[trips_hh_p$p.age_group=="45-49"|trips_hh_p$p.age_group=="50-54"] = 5
trips_hh_p$agegroup[trips_hh_p$p.age_group=="55-59"|trips_hh_p$p.age_group=="60-64"] = 6
trips_hh_p$agegroup[trips_hh_p$p.age_group=="65-69"|trips_hh_p$p.age_group=="70-74"|trips_hh_p$p.age_group=="75-79"|trips_hh_p$p.age_group=="80-44"|
trips_hh_p$p.age_group=="85+"] = 7
trips_hh_p <- trips_hh_p[!(trips_hh_p$agegroup==""),]

#trips_hh_p$agegroup <- factor(trips_hh_p$agegroup,
#levels = c(1,2,3,4,5,6,7),labels = c("5-14", "15-24", "25-34","35-44","45-54","55-64","65+"))

#recoding work type
trips_hh_p$worktype[trips_hh_p$p.ws_workOver30h=="TRUE"] = 1 #working full time
trips_hh_p$worktype[trips_hh_p$p.ws_work16to30h=="TRUE"|trips_hh_p$p.ws_workUnder16h=="TRUE"|trips_hh_p$p.ws_unpaid=="TRUE"] = 2 #working part time/casual/volunteer
trips_hh_p$worktype[trips_hh_p$p.ws_retired=="TRUE"] = 3
trips_hh_p$worktype[trips_hh_p$p.ws_studyFullTime=="TRUE"|trips_hh_p$p.ws_studyPartTime=="TRUE"] = 4 #studying full/part time
trips_hh_p$worktype[trips_hh_p$p.ws_homeMaker=="TRUE"|trips_hh_p$p.ws_unemployed=="TRUE"|trips_hh_p$p.ws_other=="TRUE"|trips_hh_p$p.ws_longTermDisabled=="TRUE"] = 5 #not in work force/other

#trips_hh_p$worktype <- factor(trips_hh_p$worktype,
#levels = c(1,2,3,4,5),labels = c("full time", "part time/casual/volunteer", "retired","studying full/part time","not in work force/other"))

# recoding No of cars in HH
trips_hh_p$carsno[trips_hh_p$hh.cars == 0] = 0
trips_hh_p$carsno[trips_hh_p$hh.cars == 1] = 1
trips_hh_p$carsno[trips_hh_p$hh.cars== 2] = 2
trips_hh_p$carsno[trips_hh_p$hh.cars >=3] = 3 #more than 3 cars

# recoding No of bikes in HH
trips_hh_p$bikesno[trips_hh_p$hh.bikes == 0] = 0
trips_hh_p$bikesno[trips_hh_p$hh.bikes == 1] = 1
trips_hh_p$bikesno[trips_hh_p$hh.bikes== 2] = 2
trips_hh_p$bikesno[trips_hh_p$hh.bikes ==3] = 3
trips_hh_p$bikesno[trips_hh_p$hh.bikes >=4] = 4 #more than 4 bikes

#recoding household income var
trips_hh_p$hhincome[trips_hh_p$hh.income=="less than £5000"|trips_hh_p$hh.income=="£5000 to £9999"|trips_hh_p$hh.income=="£10000 to £14999"] = 1 #hh income less than £14999 
trips_hh_p$hhincome[trips_hh_p$hh.income=="£15000 to £19999"|trips_hh_p$hh.income=="£20000 to £24999"] = 2  
trips_hh_p$hhincome[trips_hh_p$hh.income=="£25000 to £34999"] = 3 
trips_hh_p$hhincome[trips_hh_p$hh.income=="£35000 to £49999"] = 4  
trips_hh_p$hhincome[trips_hh_p$hh.income=="£50000 to £74999"|trips_hh_p$hh.income=="£75000"] = 5 #hh income more than £50000 
trips_hh_p$hhincome[trips_hh_p$hh.income=="unknown"] = 6 # missing/refused to respond

#trips_hh_p$hhincome <- factor(trips_hh_p$hhincome,
#levels = c(1,2,3,4,5,6),labels = c("less than £14999", "£15000 to £24999", "£25000 to £34999","£35000 to £49999",
#        "more than £50000","missing/refused to respond"))

#recoding household structure var
trips_hh_p$hhstructure[trips_hh_p$hh.structure2=="2 adults, 1 child"|trips_hh_p$hh.structure2=="2 adults, 2 children"|trips_hh_p$hh.structure2=="2 adults, 3+ children"|
trips_hh_p$hh.structure2=="3+ adults, 1+ children"|trips_hh_p$hh.structure2=="Single parent family"] = 1 # hh with children
trips_hh_p$hhstructure[trips_hh_p$hh.structure2=="Single Adult 16 to 64"|trips_hh_p$hh.structure2=="Single Adult 65+"|trips_hh_p$hh.structure2=="Three of more Adults" |
trips_hh_p$hh.structure2=="Two Adults Hoh or HRP 16 to 64"|trips_hh_p$hh.structure2=="Two Adults Hoh or HRP 65+"] = 0 # hh without children

#trips_hh_p$hhstructure <- factor(trips_hh_p$hhstructure, levels = c(1,0),labels = c("households with children", "households without children"))


## modifying route-based attributes (divided by distance to get the raw values) 
## row #105 to #206 were deleted from the old file due to not being applicable to the updated data

# creating dummy variable for vgvi and light based on the day and night
trips_hh_p <- trips_hh_p %>%
       mutate(hours = t.departureTime %/% 3600)
trips_hh_p$vgvi_day = 0
trips_hh_p$vgvi_day[6<=trips_hh_p$hours & trips_hh_p$hours<=20] = 1
trips_hh_p$light_night = 1
trips_hh_p$light_night[6<=trips_hh_p$hours & trips_hh_p$hours<=20] = 0

#generating mainmode 
trips_hh_p$mainmode[trips_hh_p$t.m_carDriver=="TRUE"] = 1 
trips_hh_p$mainmode[trips_hh_p$t.m_carPassenger=="TRUE"|trips_hh_p$t.m_taxi=="TRUE"] = 2
trips_hh_p$mainmode[trips_hh_p$t.m_walk=="TRUE"] = 3
trips_hh_p$mainmode[trips_hh_p$t.m_cycle=="TRUE"] = 4
trips_hh_p$mainmode[trips_hh_p$t.m_train=="TRUE"|trips_hh_p$t.m_bus=="TRUE"|trips_hh_p$t.m_metrolink=="TRUE"] = 5 
trips_hh_p <- trips_hh_p[!(trips_hh_p$t.m_main=="Other"),]
trips_hh_p <- trips_hh_p[complete.cases(trips_hh_p$mainmode), ]

#replacing NAs in time and cost variables with 0
trips_hh_p$short_car_time[is.na(trips_hh_p$short_car_time)] = 0
trips_hh_p$fast_car_time[is.na(trips_hh_p$fast_car_time)] = 0
trips_hh_p$short_walk_dist[is.na(trips_hh_p$short_walk_dist)] = 0
trips_hh_p$fast_walk_time[is.na(trips_hh_p$fast_walk_time)] = 0
trips_hh_p$short_bike_dist[is.na(trips_hh_p$short_bike_dist)] = 0
trips_hh_p$fast_bike_time[is.na(trips_hh_p$fast_bike_time)] = 0
trips_hh_p$pt_totalTravelTime[is.na(trips_hh_p$pt_totalTravelTime)] = 0

#trips$mainmode <- factor(trips$mainmode, levels = c(1,2,3,4,5),labels = c("card", "carp", "walk","bike","ptwalk"))

#generating availability of modes
trips_hh_p$availcard <- 1
trips_hh_p$availcard[trips_hh_p$carsno == 0] = 0
trips_hh_p$availcard[trips_hh_p$t.route.car_time == 0] = 0
trips_hh_p$availcard[trips_hh_p$mainmode == 1] = 1
trips_hh_p$availcarp <- 1
trips_hh_p$availcarp[trips_hh_p$t.route.car_time == 0] = 0
trips_hh_p$availcarp[trips_hh_p$mainmode == 2] = 1
trips_hh_p$availwalk <- 1
trips_hh_p$availwalk[trips_hh_p$troutewalk_short_time == 0] = 0
trips_hh_p$availwalk[trips_hh_p$mainmode == 3] = 1
trips_hh_p$availbike <- 1
trips_hh_p$availbike[trips_hh_p$bikesno == 0] = 0
trips_hh_p$availbike[trips_hh_p$troutebike_short_time == 0] = 0
trips_hh_p$availbike[trips_hh_p$mainmode == 4] = 1
trips_hh_p$availpt <- 1
trips_hh_p$availpt[trips_hh_p$otptransit_time == 0] = 0
trips_hh_p$availpt[trips_hh_p$pt_totalTravelTime_time == 0] = 0
trips_hh_p$availpt[trips_hh_p$mainmode == 5] = 1

### creating binary variables for walking and cycling 
trips_hh_p$walking <- 0
trips_hh_p$walking[trips_hh_p$mainmode == 3] = 1
trips_hh_p$bicycle <- 0
trips_hh_p$bicycle[trips_hh_p$mainmode == 4] = 1

# log transformation of distance variable 
# trips_hh_p<-trips_hh_p[trips_hh_p$short_walk_dist > 0 & trips_hh_p$short_bike_dist > 0 & trips_hh_p$fast_car_time>0 & trips_hh_p$pt_totalTravelTime >0,]
# trips_hh_p$logwalkdist <- log(trips_hh_p$short_walk_dist)
# trips_hh_p$logbikedist <- log(trips_hh_p$short_bike_dist)
trips_hh_p$logwalkdist <- ifelse(trips_hh_p$short_walk_dist > 0, log(trips_hh_p$short_walk_dist), 0)
trips_hh_p$logbikedist <- ifelse(trips_hh_p$short_bike_dist > 0, log(trips_hh_p$short_bike_dist), 0)

#save to csv
write.csv(trips_hh_p,file = "data/manchester/All_trips92.csv",row.names=FALSE)

