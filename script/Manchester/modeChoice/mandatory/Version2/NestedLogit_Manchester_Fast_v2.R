# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
### Clear memory
rm(list = ls())
#rm(list= ls()[!(ls() %in% c('modelmnl','modelfst','modelfst2'))])

### Load Apollo library
library(apollo)
suppressPackageStartupMessages(library(dplyr)) # for manipulating data

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #
All_trips <- read.csv("data/manchester/All_trips92.csv")
# for commute trips
database <- subset(All_trips, t.purpose %in% c("work","education"))
# for non-commute trips
database <- subset(All_trips, t.purpose %in% c("shop","recreation","rrt","nhb work","nhb other","escort","other"))
# database <- subset(All_trips, t.purpose %in% c("shop"))
# database <- subset(All_trips, t.purpose %in% c("recreation"))
# database <- subset(All_trips, t.purpose %in% c("rrt","nhb work","nhb other","escort","other"))

# database <- database[!is.na(database$pt_totalTravelTime), ]

#generating unique tripid
database <- mutate(database, tripID = row_number())

#relocating columns 
database <- database %>%
  unite("indiv.id", c('hh.id', 'p.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
  relocate(indiv.id, .after = hh.id) %>%
  relocate(tripID, .before = indiv.id)

# correlation test
# cor(database[,c('fast_walk_vgvi','fast_walk_POIs','fast_walk_negPOIs','fast_walk_shannon','fast_walk_jctStress','fast_walk_linkStress','fast_walk_crime',
#                'fast_walk_lights')], use = "complete.obs")

### Initialize code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "modelfst",
  modelDescr = "Nested logit with socio-demographics and fast route-based measures on mode choice",
  indivID    = "tripID",
  panelData  = FALSE,
  nCores     = 6
)

outputDir <- "JIBE/DATA Analysis/R/Manchester/"

#### DEFINE MODEL PARAMETERS
### Vector of parameters, including any that are kept fixed in estimation
apollo_beta =c(cons_carp = 0, cons_walk = 0, cons_bike = 0, cons_ptwalk = 0,
               #f_age1_carp = 0, f_age1_walk = 0, f_age1_bike = 0, f_age1_ptwalk = 0,
               f_age2_carp = 0, f_age2_walk = 0, f_age2_bike = 0, f_age2_ptwalk = 0,
               f_age4_carp = 0, f_age4_walk = 0, f_age4_bike = 0, f_age4_ptwalk = 0,
               f_age5_carp = 0, f_age5_walk = 0, f_age5_bike = 0, f_age5_ptwalk = 0,
               f_age6_carp = 0, f_age6_walk = 0, f_age6_bike = 0, f_age6_ptwalk = 0,
               # f_age7_carp = 0, f_age7_walk = 0, f_age7_bike = 0, f_age7_ptwalk = 0,
               f_female_carp = 0, f_female_walk = 0, f_female_bike = 0, f_female_ptwalk = 0,
               #f_hhstructure_carp = 0, f_hhstructure_walk = 0, f_hhstructure_bike = 0, f_hhstructure_ptwalk = 0, 
               f_inc1_carp = 0, f_inc1_walk = 0, f_inc1_bike = 0, f_inc1_ptwalk = 0, 
               f_inc2_carp = 0, f_inc2_walk = 0, f_inc2_bike = 0, f_inc2_ptwalk = 0,
               f_inc3_carp = 0, f_inc3_walk = 0, f_inc3_bike = 0, f_inc3_ptwalk = 0, 
               f_inc4_carp = 0, f_inc4_walk = 0, f_inc4_bike = 0, f_inc4_ptwalk = 0, 
               f_inc6_carp = 0, f_inc6_walk = 0, f_inc6_bike = 0, f_inc6_ptwalk = 0,
               f_cars_carp = 0, f_cars_walk = 0, f_cars_bike = 0, f_cars_ptwalk = 0,
               f_bikes_carp = 0, f_bikes_walk = 0, f_bikes_bike = 0, f_bikes_ptwalk = 0,
               f_work2_carp = 0, f_work2_walk = 0, f_work2_bike = 0, f_work2_ptwalk = 0,
               #f_work3_carp = 0, f_work3_walk = 0, f_work3_bike = 0, f_work3_ptwalk = 0, # low number of obs in worktype 3 and 5
               f_work4_carp = 0, f_work4_walk = 0, f_work4_bike = 0, f_work4_ptwalk = 0, 
               #f_work5_carp = 0, f_work5_walk = 0, f_work5_bike = 0, f_work5_ptwalk = 0, 
               f_vgvi_walk = 0, f_vgvi_bike = 0,
               f_light_walk = 0, f_light_bike = 0,
               f_shannon_walk = 0, f_shannon_bike = 0,
               f_crime_walk = 0, f_crime_bike = 0,
               f_streslnk_bike = 0, # f_streslnk_walk = 0, 
               f_stresjct_walk = 0, # f_stresjct_bike = 0, 
               #f_poi_walk = 0, f_poi_bike = 0, 
               #f_negpoi_walk = 0, f_negpoi_bike = 0, 
               f_timecarp = 0, f_timewalk = 0, f_timebike = 0, f_timept = 0,
               #f_distwalk = 0, f_distbike = 0,
               lambda_car = 1)

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

### Read in starting values for cons only model output file
#apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Apollo/MNL_Model",overwriteFixed=FALSE)

#### GROUP AND VALIDATE INPUTS
apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Create alternative specific constants and coefficients 
  ###age
  #age1_carp = f_age1_carp * (agegroup == 1); age1_walk = f_age1_walk * (agegroup == 1);age1_bike = f_age1_bike * (agegroup == 1);age1_ptwalk = f_age1_ptwalk * (agegroup == 1)
  age2_carp = f_age2_carp * (agegroup == 2); age2_walk = f_age2_walk * (agegroup == 2);age2_bike = f_age2_bike * (agegroup == 2);age2_ptwalk = f_age2_ptwalk * (agegroup == 2)
  age4_carp = f_age4_carp * (agegroup == 4); age4_walk = f_age4_walk * (agegroup == 4);age4_bike = f_age4_bike * (agegroup == 4);age4_ptwalk = f_age4_ptwalk * (agegroup == 4)
  age5_carp = f_age5_carp * (agegroup == 5); age5_walk = f_age5_walk * (agegroup == 5);age5_bike = f_age5_bike * (agegroup == 5);age5_ptwalk = f_age5_ptwalk * (agegroup == 5)
  age6_carp = f_age6_carp * (agegroup == 6); age6_walk = f_age6_walk * (agegroup == 6);age6_bike = f_age6_bike * (agegroup == 6);age6_ptwalk = f_age6_ptwalk * (agegroup == 6)
  # age7_carp = f_age7_carp * (agegroup == 7); age7_walk = f_age7_walk * (agegroup == 7);age7_bike = f_age7_bike * (agegroup == 7);age7_ptwalk = f_age7_ptwalk * (agegroup == 7)
  ###sex
  female_carp = f_female_carp * (sex ==1); female_walk = f_female_walk * (sex==1); female_bike = f_female_bike * (sex==1); female_ptwalk = f_female_ptwalk * (sex==1)
  
  ###hhstructure
  #hhstructure_carp = f_hhstructure_carp * (hhstructure ==1); hhstructure_walk = f_hhstructure_walk * (hhstructure ==1); hhstructure_bike = f_hhstructure_bike * (hhstructure==1); hhstructure_ptwalk = f_hhstructure_ptwalk * (hhstructure==1)
  
  ###hhincome
  inc1_carp = f_inc1_carp * (hhincome == 1); inc1_walk = f_inc1_walk * (hhincome == 1); inc1_bike = f_inc1_bike * (hhincome == 1); inc1_ptwalk = f_inc1_ptwalk * (hhincome == 1)
  inc2_carp = f_inc2_carp * (hhincome == 2); inc2_walk = f_inc2_walk * (hhincome == 2); inc2_bike = f_inc2_bike * (hhincome == 2); inc2_ptwalk = f_inc2_ptwalk * (hhincome == 2)
  inc3_carp = f_inc3_carp * (hhincome == 3); inc3_walk = f_inc3_walk * (hhincome == 3); inc3_bike = f_inc3_bike * (hhincome == 3); inc3_ptwalk = f_inc3_ptwalk * (hhincome == 3)
  inc4_carp = f_inc4_carp * (hhincome == 4); inc4_walk = f_inc4_walk * (hhincome == 4); inc4_bike = f_inc4_bike * (hhincome == 4); inc4_ptwalk = f_inc4_ptwalk * (hhincome == 4)
  inc6_carp = f_inc6_carp * (hhincome == 6); inc6_walk = f_inc6_walk * (hhincome == 6); inc6_bike = f_inc6_bike * (hhincome == 6); inc6_ptwalk = f_inc6_ptwalk * (hhincome == 6)
  
  ###carno
  carsno_carp = f_cars_carp * carsno ; carsno_walk = f_cars_walk * carsno; carsno_bike = f_cars_bike * carsno; carsno_ptwalk = f_cars_ptwalk * carsno
  
  ###bikeno
  bikesno_carp = f_bikes_carp * bikesno; bikesno_walk = f_bikes_walk * bikesno; bikesno_bike = f_bikes_bike * bikesno; bikesno_ptwalk = f_bikes_ptwalk * bikesno
  
  ###worktype
  worktype2_carp = f_work2_carp * (worktype == 2); worktype2_walk = f_work2_walk * (worktype == 2); worktype2_bike = f_work2_bike * (worktype == 2); worktype2_ptwalk = f_work2_ptwalk * (worktype == 2)
  #worktype3_carp = f_work3_carp * (worktype == 3); worktype3_walk = f_work3_walk * (worktype == 3); worktype3_bike = f_work3_bike * (worktype == 3); worktype3_ptwalk = f_work3_ptwalk * (worktype == 3)
  worktype4_carp = f_work4_carp * (worktype == 4); worktype4_walk = f_work4_walk * (worktype == 4); worktype4_bike = f_work4_bike * (worktype == 4); worktype4_ptwalk = f_work4_ptwalk * (worktype == 4)
  #worktype5_carp = f_work5_carp * (worktype == 5); worktype5_walk = f_work5_walk * (worktype == 5); worktype5_bike = f_work5_bike * (worktype == 5); worktype5_ptwalk = f_work5_ptwalk * (worktype == 5)
  
  ####Linked-based measures
  ###################################################################################################
  vgvi_walk = f_vgvi_walk * fast_walk_vgvi*vgvi_day; vgvi_bike = f_vgvi_bike * fast_bike_vgvi * vgvi_day
  light_walk = f_light_walk * fast_walk_lights2*light_night; light_bike = f_light_bike * fast_bike_lights2*light_night 
  shannon_walk = f_shannon_walk *fast_walk_shannon; shannon_bike = f_shannon_bike *fast_bike_shannon 
  crime_walk = f_crime_walk *fast_walk_crime; crime_bike = f_crime_bike *short_bike_crime 
  # streslnk_walk = f_streslnk_walk *fast_walk_stressLink; 
  streslnk_bike = f_streslnk_bike *fast_bike_stressLink 
  stresjct_walk = f_stresjct_walk *fast_walk_stressJct; #stresjct_bike = f_stresjct_bike *fast_bike_stressJct
  # poi_walk = f_poi_walk *fast_walk_POIs; poi_bike = f_poi_bike *fast_bike_POIs;
  # negpoi_walk = f_negpoi_walk *fast_walk_negPOIs; negpoi_bike = f_negpoi_bike *fast_bike_negPOIs
  
  ###traveltime
  # carptime = f_timecarp * car_time; walkdist = f_distwalk *logwalkdist; bikedist = f_distbike * logbikedist; ptwalktime = f_timept * otptotalpt_time
  carptime = f_timecarp * fast_car_time; walktime = f_timewalk *fast_walk_time; biketime = f_timebike * fast_bike_time; ptwalktime = f_timept * pt_totalTravelTime
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[['card']]  = 0  
  V[['carp']]  = cons_carp + age2_carp + age4_carp + age5_carp + age6_carp + female_carp + inc1_carp + inc2_carp + inc3_carp + inc4_carp + inc6_carp+
    carsno_carp + bikesno_carp + worktype2_carp + worktype4_carp + carptime 
  V[['walk']]  = cons_walk + age2_walk + age4_walk + age5_walk + age6_walk + female_walk +inc1_walk + inc2_walk + inc3_walk + inc4_walk + inc6_walk +
    carsno_walk + bikesno_walk + worktype2_walk + worktype4_walk + stresjct_walk + light_walk + vgvi_walk + shannon_walk + crime_walk + walktime 
  V[['bike']] = cons_bike + age2_bike + age4_bike + age5_bike + age6_bike + female_bike + inc1_bike + inc2_bike + inc3_bike + inc4_bike +inc6_bike +
    carsno_bike + bikesno_bike + worktype2_bike + worktype4_bike + streslnk_bike + light_bike + vgvi_bike + shannon_bike + crime_bike + biketime 
  V[['ptwalk']] = cons_ptwalk + age2_ptwalk + age4_ptwalk + age5_ptwalk + age6_ptwalk + female_ptwalk + inc1_ptwalk + inc2_ptwalk + inc3_ptwalk + 
    inc4_ptwalk + inc6_ptwalk + carsno_ptwalk + bikesno_ptwalk + worktype2_ptwalk + worktype4_ptwalk + ptwalktime 
  
  ### Specify nests for NL model
  nlNests      = list(root=1, car = lambda_car)
  #, active=lambda_active)
  
  ### Specify tree structure for NL model
  nlStructure = list()
  nlStructure[["root"]]   = c("car","walk","bike","ptwalk")
  nlStructure[["car"]] = c("card","carp")
  #nlStructure[["active"]] = c("walk","bike")
  
  ### Define settings for NL model component
  nl_settings = list(
    alternatives  = c(card= 1, carp= 2, walk = 3, bike = 4, ptwalk = 5),
    choiceVar     = mainmode,
    avail         = list(card=availcard, carp=availcarp, walk=availwalk, bike=availbike, ptwalk=availpt),
    V             = V,
    nlNests       = nlNests,
    nlStructure   = nlStructure
  )
  
  ### Compute probabilities using NL model
  P[['model']] = apollo_nl(nl_settings, functionality)
  
  ### Prepare and return outputs of function
  # P = apollo_weighting(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

modelfst = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

#, estimate_settings=list(constraints=c(
#                                       "lambda_car < 1 + 1e-10", "lambda_car > -1e-10",
#                                       "lambda_active < 1 + 1e-10", "lambda_PT > -1e-10")))

# ################################################################# #
#### MODEL PEREDICTION                                           ####
# ################################################################# #
forecast = apollo_prediction(modelfst,
                                     apollo_probabilities,
                                     apollo_inputs)
                                      
write.csv(forecast,file ="result/Manchester/mandatory/Version2/forcast_modelfst.csv")

#### Likelihood ratio tests against MNL model
# ################################################################# #
#lrTest = apollo_lrTest(modelfst,modelmnl)                                                                                                                                                                                                                                                                                                                           

#### MODEL OUTPUTS                                               ####
# ################################################################# #
#apollo_modelOutput(modelfst,modelOutput_settings = list(printT1=1))
# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILES)                               ----
# ----------------------------------------------------------------- #
apollo_saveOutput(modelfst)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #
apollo_modelOutput(modelfst)

# Print estimates
modelfst_estimates <- modelfst$estimate
modelfst_tTest <- modelfst$estimate / modelfst$se
modelfst_pValue <- 2 * (1 - stats::pnorm(abs(modelfst$estimate/modelfst$se)))
modelfst_sig <- case_when(modelfst_pValue <= 0.001 ~ "***",
                         modelfst_pValue <= 0.01 ~ "**",
                         modelfst_pValue <= 0.05 ~ "*",
                         modelfst_pValue <= 0.1 ~ ".",
                         TRUE ~ "")
estimateValues <- paste(round(modelfst_estimates,3),"[",round(modelfst_tTest,3),modelfst_sig,"]")
estimateValues[modelfst_estimates == 0] <- NA
names(estimateValues) = names(apollo_beta)
estimate_lambdas <- estimateValues[startsWith(names(apollo_beta),"lambda")]
write.csv(estimateValues,file = "result/Manchester/mandatory/Version2/estimatevalues_modelfst_f2.csv")

