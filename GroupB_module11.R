## NR 995 Module 11
## Correlation and ANOVA 
## Group B, Erica Holme and Katie Moran
## Last modified: 11/07/17

## Set wd and read in ants_duke.csv
getwd() #using GitHub to complete assignment

ants <- read.table("ants_duke.csv", sep=",", header=T)
head(ants)
str(ants) # 54 observations of 4 variables
summary(ants)
length(unique(ants$Binom)) # 5 ant species in dataset
length(unique(ants$Delta)) # 12 unique temp treatments (Delta)
unique(is.na(ants)) # no NAs in data set

## Do the values and descriptive statistics of numerical
## variables make sense with their definitions above? 
## KM to EH: I'm unsure on answering this questions. Delta has a vague definition--why are Delta values much less
## than the CTmax? Is Delta measuring an increase in temperature above the 
## ant's resting temperature? 

## Categorize values into low, moderate, and high Delta classes

ants$DeltaClass <- NA
ants$DeltaClass[ants$Delta <= 4] <- "low"
ants$DeltaClass[ants$Delta >8] <- "high"
ants$DeltaClass[ants$Delta >4 & ants$Delta <= 8] <- "moderate"
ants # yay it worked


