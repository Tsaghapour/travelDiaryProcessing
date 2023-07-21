## fitting binomial model for walking (using short route linked based attributes)

### Clear memory
rm(list = ls())

# Libraries 
library(tidyverse)
library(glmmML)
library(lme4)
library(lattice)

## reading data
mandatory_trips<-read.csv("data/manchester/mandatory_trips92.csv")

## correlation tests between variables
cor(mandatory_trips[,c('short_walk_lights2','short_walk_POIs','short_walk_negPOIs','short_walk_vgvi','short_walk_crime','short_walk_shannon',
             'short_walk_stressJct','short_walk_stressLink')])

cor(mandatory_trips[,c('short_bike_lights2','short_bike_POIs','short_bike_negPOIs','short_bike_vgvi','short_bike_crime','short_bike_shannon',
                       'short_bike_stressJct','short_bike_stressLink')])

## mandatory trips binomial regression model for walking
# walking
glm_w1 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w1)
glm_w2 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_vgvi, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w2)
glm_w3 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_shannon, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w3)
glm_w4 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_crime, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w4)
glm_w5 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_stressLink, data=mandatory_trips,family = binomial, model=TRUE) 
summary(glm_w5)
glm_w6 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_stressJct, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w6)
glm_w7 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_POIs, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w7)
glm_w8 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_negPOIs, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w8)
glm_w9 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct,
             data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w9)
glm_w10 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink,
              data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w10)
glm_w11 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct + short_walk_POIs,
              data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w11)
glm_w12 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink + short_walk_POIs,
               data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w12)
glm_w13 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct + short_walk_shannon,
               data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w13)
glm_w14 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink + short_walk_shannon,
               data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w14)
glm_w15 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct + short_walk_crime +
                short_walk_shannon, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w15)
glm_w16 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink + short_walk_stressJct + 
                short_walk_crime + short_walk_shannon, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w16)
glm_w17 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink + short_walk_stressJct + 
                 short_walk_crime + short_walk_shannon, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_w17)

# cycling
glm_b1 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b1)
glm_b2 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_vgvi, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b2)
glm_b3 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_shannon, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b3)
glm_b4 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_crime, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b4)
glm_b5 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_stressLink, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b5)
glm_b6 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_stressJct, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b6)
glm_b7 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_POIs, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b7)
glm_b8 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_negPOIs, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b8)
glm_b9 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct,
                data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b9)
glm_b10 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink,
              data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b10)
glm_b11 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct + short_bike_POIs,
              data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b11)
glm_b12 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink + short_bike_POIs,
               data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b12)
glm_b13 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct + short_bike_shannon,
               data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b13)
glm_b14 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink + short_bike_shannon,
               data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b14)
glm_b15 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct + short_bike_crime +
                short_bike_shannon, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b15)
glm_b16 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink + short_bike_stressJct + short_bike_crime +
                short_bike_shannon, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b16)
glm_b17 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink + short_bike_stressJct + short_bike_crime +
                 short_bike_POIs, data=mandatory_trips, family = binomial, model=TRUE)
summary(glm_b17)

# multilevel binomial regression model with nested random effects
# two level model (trips nested within individuals)
ML_w1 <- glmer(walking ~ as.factor(agegroup) + p.female + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct + 
                 short_walk_shannon + (1 | indiv.id), family = binomial(), data = mandatory_trips)
summary(ML_w1)
# three level model (trips nested within individuals, individuals nested within households)
ML_w2 <- glmer(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct + 
                     short_walk_POIs + (1 | hh.id.x) + (1 | indiv.id), family = binomial(), data = mandatory_trips)
summary(ML_w2)

# Print the summary of the model
summary(model_manchester)