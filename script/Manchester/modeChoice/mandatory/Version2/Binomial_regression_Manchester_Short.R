## fitting binomial model for walking (using short route linked based attributes)

### Clear memory
rm(list = ls())

# Libraries 
library(tidyverse)
library(glmmML)
library(lme4)
library(lattice)
library(margins)

## reading data
mandatory_trips<-read.csv("data/manchester/mandatory_trips92.csv")

## correlation tests between variables
cor(mandatory_trips[,c('short_walk_lights2','short_walk_POIs','short_walk_negPOIs','short_walk_vgvi','short_walk_crime','short_walk_shannon',
             'short_walk_stressJct','short_walk_stressLink')])

cor(mandatory_trips[,c('short_bike_lights2','short_bike_POIs','short_bike_negPOIs','short_bike_vgvi','short_bike_crime','short_bike_shannon',
                       'short_bike_stressJct','short_bike_stressLink')])

## mandatory trips binomial regression model for walking
# walking
# model1
glm_w1 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w1)
mar_w1 <- margins(glm_w1); summary(mar_w1)
# model2
glm_w2 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_vgvi, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w2)
mar_w2 <- margins(glm_w2);summary(mar_w2)
# model3
glm_w3 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_shannon, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w3)
mar_w3 <- margins(glm_w3);summary(mar_w3)
# model4
glm_w4 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_crime, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w4)
mar_w4 <- margins(glm_w4);summary(mar_w4)
# model5
glm_w5 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_stressLink, data=mandatory_trips,family = binomial, model=TRUE);summary(glm_w5) 
mar_w5 <- margins(glm_w5);summary(mar_w5)
# model6
glm_w6 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_stressJct, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w6)
mar_w6 <- margins(glm_w6);summary(mar_w6)
# model7
glm_w7 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_POIs, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w7)
mar_w7 <- margins(glm_w7);summary(mar_w7)
# model8
glm_w8 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_negPOIs, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w8)
mar_w8 <- margins(glm_w8);summary(mar_w8)
# model9
glm_w9 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct,
             data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w9)
mar_w9 <- margins(glm_w9);summary(mar_w9)
# model10
glm_w10 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink,
              data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w10)
mar_w10 <- margins(glm_w10);summary(mar_w10)
# model11
glm_w11 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct + short_walk_POIs,
              data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w11)
mar_w11 <- margins(glm_w11);summary(mar_w11)

# model12
glm_w12 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink + short_walk_POIs,
               data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w12)
mar_w12 <- margins(glm_w12);summary(mar_w12)
# model13
glm_w13 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct + short_walk_shannon,
               data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w13)
mar_w13 <- margins(glm_w13);summary(mar_w13)
# model14
glm_w14 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink + short_walk_shannon,
               data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w14)
mar_w14 <- margins(glm_w14);summary(mar_w14)
# model15
glm_w15 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressJct + short_walk_crime +
                short_walk_shannon, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w15)
mar_w15 <- margins(glm_w15);summary(mar_w15)
# model16
glm_w16 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink + short_walk_stressJct + 
                short_walk_crime + short_walk_shannon, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w16)
mar_w16 <- margins(glm_w16);summary(mar_w16)
# model17
glm_w17 <- glm(walking ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_walk_lights2 + short_walk_vgvi + short_walk_stressLink + short_walk_stressJct + 
                 short_walk_crime + short_walk_POIs, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_w17)
mar_w17 <- margins(glm_w17);summary(mar_w17)


# cycling
# model1
glm_b1 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b1)
mar_b1 <- margins(glm_b1); summary(mar_b1)
# model2
glm_b2 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_vgvi, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b2)
mar_b2 <- margins(glm_b2); summary(mar_b2)
# model3
glm_b3 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_shannon, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b3)
mar_b3 <- margins(glm_b3); summary(mar_b3)
# model4
glm_b4 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_crime, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b4)
mar_b4 <- margins(glm_b4); summary(mar_b4)
# model5
glm_b5 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_stressLink, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b5)
mar_b5 <- margins(glm_b5); summary(mar_b5)
# model6
glm_b6 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_stressJct, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b6)
mar_b6 <- margins(glm_b6); summary(mar_b6)
# model7
glm_b7 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_POIs, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b7)
mar_b7 <- margins(glm_b7); summary(mar_b7)
# model8
glm_b8 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_negPOIs, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b8)
mar_b8 <- margins(glm_b8); summary(mar_b8)
# model9
glm_b9 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct,
                data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b9)
mar_b9 <- margins(glm_b9); summary(mar_b9)
# model10
glm_b10 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink,
              data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b10)
mar_b10 <- margins(glm_b10); summary(mar_b10)
# model11
glm_b11 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct + short_bike_POIs,
              data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b11)
mar_b11 <- margins(glm_b11); summary(mar_b11)
# model12
glm_b12 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink + short_bike_POIs,
               data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b12)
mar_b12 <- margins(glm_b12); summary(mar_b12)
# model13
glm_b13 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct + short_bike_shannon,
               data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b13)
mar_b13 <- margins(glm_b13); summary(mar_b13)
# model14
glm_b14 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink + short_bike_shannon,
               data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b14)
mar_b14 <- margins(glm_b14); summary(mar_b14)
# model15
glm_b15 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressJct + short_bike_crime +
                short_bike_shannon, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b15)
mar_b15 <- margins(glm_b15); summary(mar_b15)
# model16
glm_b16 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink + short_bike_stressJct + short_bike_crime +
                short_bike_shannon, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b16)
mar_b16 <- margins(glm_b16); summary(mar_b16)
# model17
glm_b17 <- glm(bicycle ~ as.factor(agegroup) + p.female + hh.income + as.factor(worktype) + carsno + bikesno + short_bike_lights2 + short_bike_vgvi + short_bike_stressLink + short_bike_stressJct + short_bike_crime +
                 short_bike_POIs, data=mandatory_trips, family = binomial, model=TRUE);summary(glm_b17)
mar_b17 <- margins(glm_b17); summary(mar_b17)

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