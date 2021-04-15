library(readr)
library(dplyr)
library(tidyr)
library(stringr)
hcrimes <- read_csv("hate_crime.csv")

hcrimes$Year <- hcrimes$DATA_YEAR
hcrimes$DATA_YEAR <- NULL

hcrimes$Date <- hcrimes$INCIDENT_DATE
hcrimes$INCIDENT_DATE <- NULL

hcrimes$OrgID <- hcrimes$ORI
hcrimes$ORI <- NULL

hcrimes$Agency <- hcrimes$PUB_AGENCY_NAME
hcrimes$PUB_AGENCY_NAME <- NULL

hcrimes$Unit <- hcrimes$PUB_AGENCY_UNIT
hcrimes$PUB_AGENCY_UNIT <- NULL


hcrimes$Agency_Type <- hcrimes$AGENCY_TYPE_NAME
hcrimes$AGENCY_TYPE_NAME <- NULL

hcrimes$St <- hcrimes$STATE_ABBR
hcrimes$STATE_ABBR <- NULL

hcrimes$State <- hcrimes$STATE_NAME
hcrimes$STATE_NAME <- NULL

hcrimes$Division <- hcrimes$DIVISION_NAME
hcrimes$DIVISION_NAME <- NULL

hcrimes$Region <- hcrimes$REGION_NAME
hcrimes$REGION_NAME <- NULL

hcrimes$Group_Code <- hcrimes$POPULATION_GROUP_CODE
hcrimes$POPULATION_GROUP_CODE <- NULL

hcrimes$Population <- hcrimes$POPULATION_GROUP_DESC
hcrimes$POPULATION_GROUP_DESC <- NULL

hcrimes$nAdultV <- hcrimes$ADULT_VICTIM_COUNT
hcrimes$ADULT_VICTIM_COUNT <- NULL

hcrimes$nJuvV <- hcrimes$JUVENILE_VICTIM_COUNT
hcrimes$JUVENILE_VICTIM_COUNT <- NULL

hcrimes$nOff <- hcrimes$TOTAL_OFFENDER_COUNT 
hcrimes$TOTAL_OFFENDER_COUNT <- NULL

hcrimes$nAdultO <- hcrimes$ADULT_OFFENDER_COUNT 
hcrimes$ADULT_OFFENDER_COUNT <- NULL

hcrimes$nJuvO <- hcrimes$JUVENILE_OFFENDER_COUNT
hcrimes$JUVENILE_OFFENDER_COUNT <- NULL

hcrimes$ORace <- hcrimes$OFFENDER_RACE
hcrimes$OFFENDER_RACE <- NULL

hcrimes$OEth <- hcrimes$OFFENDER_ETHNICITY
hcrimes$OFFENDER_ETHNICITY <- NULL

hcrimes$nVictims <- hcrimes$VICTIM_COUNT
hcrimes$VICTIM_COUNT <- NULL

hcrimes$niVictims <- hcrimes$TOTAL_INDIVIDUAL_VICTIMS
hcrimes$TOTAL_INDIVIDUAL_VICTIMS <- NULL

hcrimes$Offense <- hcrimes$OFFENSE_NAME
hcrimes$OFFENSE_NAME <- NULL

hcrimes$Location <- hcrimes$LOCATION_NAME
hcrimes$LOCATION_NAME <- NULL

hcrimes$Bias <- hcrimes$BIAS_DESC
hcrimes$BIAS_DESC <- NULL
hcrimes$Bias <- as.factor(hcrimes$Bias)

hcrimes$VicType <- hcrimes$VICTIM_TYPES
hcrimes$VICTIM_TYPES <- NULL

hcrimes$mOff <- hcrimes$MULTIPLE_OFFENSE
hcrimes$MULTIPLE_OFFENSE <- NULL

hcrimes$mBias <- hcrimes$MULTIPLE_BIAS
hcrimes$MULTIPLE_BIAS <- NULL



#summary(hcrimes)
hcrimes$VicType <- as.factor(hcrimes$VicType)

RaceBias <- c(rep(0,length(hcrimes$Bias)))
ReligionBias <- c(rep(0,length(hcrimes$Bias)))
SexBias <- c(rep(0,length(hcrimes$Bias)))
GenderBias <- c(rep(0,length(hcrimes$Bias)))
DisBias <- c(rep(0,length(hcrimes$Bias)))

race <- c("American Indian","Alaska Native","Asian","Black","Hispanic",
          "Native Hawaiian","Arab","Race","Races","White")
religion <- c("Islamic","Jewish","Religions","Atheism","Hindu","Sikh","Religion",
              "Mormon","Christian","Protestant","Buddhist","Catholic","Orthodox",
              "Jehovah's Witness")
sexuality <- c("Gay","Lesbian","Bisexual","Heterosexual")
gender <- c("Male","Female","Transgender","Gender")
disability <- c("Disability")
for (i in 1:length(hcrimes$Bias)) {

  for (r in 1:length(race)) {
    if(str_detect(hcrimes$Bias[i],race[r])) {
      RaceBias[i] <- 1
      break
    }
  }
  for (l in 1:length(religion)) {
    if(str_detect(hcrimes$Bias[i],religion[l])) {
      ReligionBias[i] <- 1
      break
    }
  }
  for (s in 1:length(sexuality)) {
    if(str_detect(hcrimes$Bias[i],sexuality[s])) {
      SexBias[i] <- 1
      break
    }
  }
  for (g in 1:length(gender)) {
    if(str_detect(hcrimes$Bias[i],gender[g])) {
      GenderBias[i] <- 1
      break
    }
  }
  for (d in 1:length(disability)) {
    if(str_detect(hcrimes$Bias[i],disability[d])) {
      DisBias[i] <- 1
      break
    }
  }
}

hcrimes$RaceBias <- RaceBias
hcrimes$ReligionBias <- ReligionBias
hcrimes$SexBias <- SexBias
hcrimes$GenderBias <- GenderBias
hcrimes$DisBias <- DisBias

write_csv(hcrimes, "HateCrimes.csv")
