## fitting binomial model for walking (using short route linked based attributes)

### Clear memory
rm(list = ls())

# Libraries 
library(tidyverse)
library(glmmML)
library(lme4)
library(lattice)

## reading data
mandatory_trips<-read.csv("C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/LinkedBasedAttributes/LinkBased/TEST/mandatory_trips92.csv")

## correlation tests between variables
cor(mandatory_trips[,c('short_walk_lights2','short_walk_POIs','short_walk_negPOIs','short_walk_vgvi','short_walk_crime','short_walk_shannon',
             'short_walk_stressJct','short_walk_stressLink')])

cor(mandatory_trips[,c('short_bike_lights2','short_bike_POIs','short_bike_negPOIs','short_bike_vgvi','short_bike_crime','short_bike_shannon',
                       'short_bike_stressJct','short_bike_stressLink')])

## mandatory trips binomial regression model for walking
# walking
glm_w1 <- glm(walking ~ short_walk_lights2, data=mandatory_trips, family = binomial, model=TRUE)
glm_w2 <- glm(walking ~ short_walk_vgvi, data=mandatory_trips, family = binomial, model=TRUE)
glm_w3 <- glm(walking ~ short_walk_lights2 + short_walk_vgvi, data=mandatory_trips, family = binomial, model=TRUE)
glm_w4 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi, data=mandatory_trips, family = binomial, model=TRUE)
glm_w5 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_POIs +short_walk_negPOIs, data=mandatory_trips, 
              family = binomial, model=TRUE)
glm_w6 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_shannon + short_walk_stressJct, data=mandatory_trips, 
              family = binomial, model=TRUE)
glm_w7 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct + short_walk_crime +
             short_walk_shannon, data=mandatory_trips, family = binomial, model=TRUE)

summary(glm_w7)

# cycling
glm_b1 <- glm(bicycle ~ short_bike_lights2, data=mandatory_trips, family = binomial, model=TRUE)
glm_b2 <- glm(bicycle ~ short_bike_vgvi, data=mandatory_trips, family = binomial, model=TRUE)
glm_b3 <- glm(bicycle ~ short_bike_lights2 + short_bike_vgvi, data=mandatory_trips, family = binomial, model=TRUE)
glm_b4 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi, data=mandatory_trips, family = binomial, model=TRUE)
glm_b5 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_POIs +short_bike_negPOIs, data=mandatory_trips, 
              family = binomial, model=TRUE)
glm_b6 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_shannon + short_bike_stressJct, data=mandatory_trips, 
              family = binomial, model=TRUE)
glm_b7 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct + short_bike_crime +
                short_bike_shannon, data=mandatory_trips, family = binomial, model=TRUE)

summary(glm_b7)

# Create your multilevel binomial regression model with nested random effects
model_manchester <- glmer(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct + short_bike_crime +
                     short_bike_shannon + (1 | hh.id.x) + (1 | indiv.id), family = binomial(), data = mandatory_trips)

# Print the summary of the model
summary(model_manchester)