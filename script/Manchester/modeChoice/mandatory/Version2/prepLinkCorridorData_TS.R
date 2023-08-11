### Clear memory
rm(list = ls())
library(tidyverse)
# setwd("~/Documents/manchester")

## MODE CHOICE JOINING / AGGREGATION CODE ##

# ESTIMATE DECAY PARAMETER
PERCENTILE = 0.9
DETOUR = 0.25

# estimate (from exponential distribution CDF)
-log(1-PERCENTILE)/DETOUR

ALPHA = 9.2

# combining walk commute trips with intrazonal and discretionary trips
commuteWalkFast <- read_csv("data/manchester/commuteWalkFast.csv")
intrazonalWalk <- read_csv("data/manchester/intrazonalWalk.csv")
discretionaryWalkFast <- read_csv("data/manchester/discretionaryWalkFast.csv")
AllWalkFast<-rbind(commuteWalkFast,intrazonalWalk,discretionaryWalkFast)
commuteWalkShort <- read_csv("data/manchester/commuteWalkShort.csv")
intrazonalWalk <- read_csv("data/manchester/intrazonalWalk.csv")
discretionaryWalkShort <- read_csv("data/manchester/discretionaryWalkDist.csv")
AllWalkShort<-rbind(commuteWalkShort,intrazonalWalk,discretionaryWalkShort)
# # Save to csv
# write_csv(AllWalkFast,"data/manchester/AllWalkFast.csv")
# write_csv(AllWalkShort,"data/manchester/AllWalkShort.csv")

# combining bike commute trips with intrazonal and discretionary trips
commuteBikeFast <- read_csv("data/manchester/commuteBikeFast.csv")
intrazonalBike <- read_csv("data/manchester/intrazonalBike.csv")
discretionaryBikeFast <- read_csv("data/manchester/discretionaryBikeFast.csv")
AllBikeFast<-rbind(commuteBikeFast,intrazonalBike,discretionaryWalkShort)
commuteBikeShort <- read_csv("data/manchester/commuteBikeShort.csv")
intrazonalBike <- read_csv("data/manchester/intrazonalBike.csv")
discretionaryBikeShort <- read_csv("data/manchester/discretionaryBikeDist.csv")
AllBikeShort<-rbind(commuteBikeShort,intrazonalBike,discretionaryBikeShort)
# # Save to csv
# write_csv(AllBikeFast,"data/manchester/AllBikeFast.csv")
# write_csv(AllBikeShort,"data/manchester/AllBikeShort.csv")

########### GET SHORTEST/FASTEST PATH INFO ###########
routesDist <- read_csv("data/manchester/routesShort.csv")
routesTime <- read_csv("data/manchester/routesFast.csv")

########### COMPUTE AND AGGREGATE LINK DATA BASED ON DETOUR ###########

# BRING IN ADDITIONAL LINK DATA FROM NETWORK GPKG
networkBike <- read_csv("data/manchester/networkBike.csv")
networkWalk <- read_csv("data/manchester/networkWalk.csv")

networkWalk <- networkWalk %>%
  mutate(streetLightsDensity = pmin(1/15,streetLights / length))

networkBike <- networkBike %>%
  mutate(streetLightsDensity = pmin(1/15,streetLights / length))

# READ IN LINK DATA
# bikeLinksShort <- readr::read_csv("data/manchester/AllBikeShort.csv") %>% left_join(networkBike)
# bikeLinksFast <- readr::read_csv("data/manchester/AllBikeFast.csv") %>% left_join(networkBike)
# walkLinksShort <- readr::read_csv("data/manchester/AllWalkShort.csv") %>% left_join(networkWalk)
# walkLinksFast <- readr::read_csv("data/manchester/AllWalkFast.csv") %>% left_join(networkWalk)

bikeLinksShort <- left_join(networkBike,AllBikeShort)
bikeLinksFast <- left_join(networkBike,AllBikeFast)
walkLinksShort <- left_join(networkWalk,AllWalkShort)
walkLinksFast <- left_join(networkWalk,AllWalkFast)

# GROUP AND AGGREGATE
aggregateBE <- function(linkData,costVar,modeName) {
  linkData %>%
    mutate(df = exp(-1 * ALPHA * detour),
           wt = df * !!enquo(costVar)) %>%
    group_by(IDNumber,PersonNumber,TripNumber) %>%
    summarise(sumWt = sum(wt),
              stressLink = sum(stressLink * wt) / sumWt,
              stressJct = sum(stressJct * df) / sumWt,
              vgvi = sum(vgvi * wt) / sumWt,
              shannon = sum(shannon * wt) / sumWt,
              POIs = sum(POIs * df) / sumWt,
              negPOIs = sum(negPOIs * df) / sumWt,
              crime = sum(crime * df) / sumWt,
              lights = sum(streetLights * df) / sumWt,
              lights2 = sum(streetLightsDensity * wt) / sumWt) %>%
    ungroup() %>%
    dplyr::select(-sumWt) %>%
    rename_with(~paste(modeName,.x,sep="_"),.cols = c(stressLink,stressJct,vgvi,shannon,POIs,negPOIs,crime,lights,lights2))
}

bikeAggShort = bikeLinksShort %>% aggregateBE(length,"bike")
walkAggShort = walkLinksShort %>% aggregateBE(length,"walk")
bikeAggFast = bikeLinksFast %>% aggregateBE(time,"bike")
walkAggFast = walkLinksFast %>% aggregateBE(time,"walk")

resultShort <- routesDist %>% right_join(bikeAggShort) %>% right_join(walkAggShort)
resultFast <- routesTime %>% right_join(bikeAggFast) %>% right_join(walkAggFast)

# Save to csv
write_csv(resultShort,"data/manchester/AllShort92.csv")
write_csv(resultFast,"data/manchester/AllFast92.csv")