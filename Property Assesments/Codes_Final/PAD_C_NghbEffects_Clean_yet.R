#__________________________________#
#       Running an MLM in R        #
#__________________________________#
#### STEP 0 #######
library(nlme)
library(tidyverse)
library(lme4)
library(rcompanion)
library(outliers)

prop.acs <- read.csv("~/Desktop/Academic/BARI/Scripts/Property Assesments/Outputs/PADCross.Record.csv")

# is age 2018-year of construction? or remodel
names(prop.acs)


#### Model for CT_ID_20 ####
model <- prop.acs %>%
  dplyr::select(PID,CT_ID_20,
                TOTAL_VALUE,
                LAND_SF,SIMPLIFIED_LU,GROSS_AREA,
                LIVING_AREA,RES_FLOOR,
                BLDG_AGE # used to be called age
                # ,PopDen,VacantUnitPer # not clear where these three variables come from
  )
## factoring the categorical variables
model$simplu <- as.factor(model$SIMPLIFIED_LU)
colnames(model)

## just keeping the full observations
model <- model[complete.cases(model),] #75233 #146879 in 2023

## data screening and fixing variables
model$propid <- as.numeric(model$PID)
model$AV_TOTAL <- as.numeric(model$TOTAL_VALUE)
model$GROSS_AREA <- as.numeric(model$GROSS_AREA)
model$LAND_SF <- as.numeric(model$LAND_SF)
model$LIVING_AREA <- as.numeric(model$LIVING_AREA)

##### way 1 of dealing with outliers
names(model) #

nrow(model) #75233 # 71525 in 2023 #146879 in sep23 # 136462 in Nov 23 #137566 in 2024


model <- model[model$AV_TOTAL < quantile(model$AV_TOTAL, 0.96, na.rm = T),] #72223 #68664 in 2023 # 141003 in Sep 23 #131003 in Nov 23 #132062 in 2024
#model <- model[model$AV_BLDG_PER_SF < quantile(model$AV_BLDG_PER_SF, 0.999, na.rm = T),] #168600
#model <- model[model$AV_LAND_PER_SF < quantile(model$AV_LAND_PER_SF, 0.996, na.rm = T),] #168309
#model <- model[model$LAND_SF < quantile(model$LAND_SF, 0.98, na.rm = T),] #166860
#model <- model[model$GROSS_AREA < quantile(model$GROSS_AREA, 0.98, na.rm = T),] #165444
#model <- model[model$RES_FLOOR < quantile(model$RES_FLOOR, 0.999, na.rm = T),] #165177
#model <- model[model$BLDG_AGE < quantile(model$BLDG_AGE, 0.999, na.rm = T),] #165102

# model$occ <- 1-model$VacantUnitPer # need to add this var in
#model$total_sqft <- ((model$AV_BLDG_PER_SF) + (model$AV_LAND_PER_SF))
#model$total_sqft <- rowSums(model[,c("AV_BLDG_PER_SF", "AV_LAND_PER_SF")], na.rm=TRUE) # creates 0s instead of NAs - fixing that:
#model$total_sqft <- ifelse(model$total_sqft == 0, NA, model$total_sqft) # done
#model$logtotalsq <-(log(model$total_sqft))
model$simplu <- relevel(model$simplu, ref = "RESIDENTIAL")

#
colnames(model)
model.mlm <- subset(model, simplu != "CONDO_UNIT") %>%
  dplyr::select(propid, CT_ID_20,simplu, 
                AV_TOTAL,LAND_SF,GROSS_AREA,
                LIVING_AREA, RES_FLOOR, 
                BLDG_AGE
                # occ # these two still not in here
  )

names(model.mlm)

# we cannot do HML on groups w less than 7 obs 
outliers <- model.mlm %>%
  group_by(CT_ID_20) %>%
  tally %>%
  arrange(n) %>%
  filter( n < 7)

outlier_list<-as.list(outliers$CT_ID_20)
#Count number of neighborhoods that need to be deleted
length(outlier_list) # dropping 10 CTS for not having enough observations #9 in 2023 #10 in 2024 with CT_ID_20

###Subset listings data to remove outliers
nrow(model.mlm) #73464 in 2023 #63253 in Nov 23 #63037 in 2024
model.mlm<-model.mlm[!(model.mlm$CT_ID_20 %in% outlier_list) ,]
nrow(model.mlm) #72108 #3742 in 2023 #73444 in Sep 23 #63228 in Nov 23 #63007 in 2024

noout <- model.mlm

noout$RES_FLOOR[which(noout$RES_FLOOR >50)]
noout$BLDG_AGE[which(noout$BLDG_AGE >500)]
noout$LAND_SF[which(noout$LAND_SF >300000)]

noout$CT_ID_20 <- as.factor(noout$CT_ID_20)

names(noout)

#I got an error here, which saying that  NA/NaN/Inf in 'y' and 
#Some predictor variables are on very different scales: consider rescaling

# Identify any NA/NaN/Inf in the relevant columns
problematic_indices <- apply(noout[, c("AV_TOTAL", "LAND_SF", "GROSS_AREA", "LIVING_AREA", "RES_FLOOR", "BLDG_AGE")], 1, function(x) any(is.na(x) | is.nan(x) | is.infinite(x)))

# Extract rows without NA/NaN/Inf values 
#clean_data <- noout[!problematic_indices, ] # no problematic rows.
clean_data <- noout 
# Check for non-positive values in 'AV_TOTAL'
#non_positive <- which(clean_data$AV_TOTAL <= 0) # no negative

# If there are non-positive values, you'll need to decide how to handle them. 
# You might remove them or adjust them if they're due to some known data issue.
#clean_data <- clean_data[-non_positive, ]


# Standardize predictors
predictors_to_scale <- c("LAND_SF", "GROSS_AREA", "LIVING_AREA", "RES_FLOOR", "BLDG_AGE")
clean_data[predictors_to_scale] <- scale(clean_data[predictors_to_scale])



(fullmodel <- lmer(log(AV_TOTAL) ~ LAND_SF+GROSS_AREA+LIVING_AREA+
                     RES_FLOOR+BLDG_AGE+(1|CT_ID_20),
                   data = clean_data,
                   REML = F,
                   na.action = "na.omit")) #simplu+
summary(fullmodel)

tract.unique <- fixef(fullmodel)[1] + ranef(fullmodel)$CT_ID_20
tract.unique$random <- ranef(fullmodel)$CT_ID_20
tract.unique$CT_ID_20 <- rownames(tract.unique)
length(tract.unique$CT_ID_20) #155 #166 in 2024
tract.unique$CensusTract <- c(1:166) #Error in `$<-.data.frame`(`*tmp*`, CensusTract, value = 1:166) : 
#replacement has 166 rows, data has 155
names(tract.unique)[1] <- "Intercept"
names(tract.unique)[2] <- "random"
tract.unique <- tract.unique[,c(3,4,1,2)] #what is rationale to do this?
tract.unique <- tract.unique[,c(1,3,4)]
tract.unique <- as.data.frame(tract.unique, row.names = NULL)
#rownames(tract.unique) <-NULL

tract.unique$random <- tract.unique$random$`(Intercept)`
tract.unique$random <- exp(tract.unique$random)

write.csv(tract.unique, "proptracts_effects_2024_20.csv", row.names = F)





####  Model for CT_ID_10 #####


model <- prop.acs %>%
  dplyr::select(PID,CT_ID_10,
                TOTAL_VALUE,
                LAND_SF,SIMPLIFIED_LU,GROSS_AREA,
                LIVING_AREA,RES_FLOOR,
                BLDG_AGE # used to be called age
                # ,PopDen,VacantUnitPer # not clear where these three variables come from
  )
## factoring the categorical variables
model$simplu <- as.factor(model$SIMPLIFIED_LU)
colnames(model)

## just keeping the full observations
model <- model[complete.cases(model),] #75233 #146879 in 2023

## data screening and fixing variables
model$propid <- as.numeric(model$PID)
model$AV_TOTAL <- as.numeric(model$TOTAL_VALUE)
model$GROSS_AREA <- as.numeric(model$GROSS_AREA)
model$LAND_SF <- as.numeric(model$LAND_SF)
model$LIVING_AREA <- as.numeric(model$LIVING_AREA)

##### way 1 of dealing with outliers
names(model) #

nrow(model) #75233 # 71525 in 2023 #146879 in sep23 # 136462 in Nov 23 #137566 in 2024


model <- model[model$AV_TOTAL < quantile(model$AV_TOTAL, 0.96, na.rm = T),] #72223 #68664 in 2023 # 141003 in Sep 23 #131003 in Nov 23 #132062 in 2024
#model <- model[model$AV_BLDG_PER_SF < quantile(model$AV_BLDG_PER_SF, 0.999, na.rm = T),] #168600
#model <- model[model$AV_LAND_PER_SF < quantile(model$AV_LAND_PER_SF, 0.996, na.rm = T),] #168309
#model <- model[model$LAND_SF < quantile(model$LAND_SF, 0.98, na.rm = T),] #166860
#model <- model[model$GROSS_AREA < quantile(model$GROSS_AREA, 0.98, na.rm = T),] #165444
#model <- model[model$RES_FLOOR < quantile(model$RES_FLOOR, 0.999, na.rm = T),] #165177
#model <- model[model$BLDG_AGE < quantile(model$BLDG_AGE, 0.999, na.rm = T),] #165102

# model$occ <- 1-model$VacantUnitPer # need to add this var in
#model$total_sqft <- ((model$AV_BLDG_PER_SF) + (model$AV_LAND_PER_SF))
#model$total_sqft <- rowSums(model[,c("AV_BLDG_PER_SF", "AV_LAND_PER_SF")], na.rm=TRUE) # creates 0s instead of NAs - fixing that:
#model$total_sqft <- ifelse(model$total_sqft == 0, NA, model$total_sqft) # done
#model$logtotalsq <-(log(model$total_sqft))
model$simplu <- relevel(model$simplu, ref = "RESIDENTIAL")

#
colnames(model)
model.mlm <- subset(model, simplu != "CONDO_UNIT") %>%
  dplyr::select(propid, CT_ID_10,simplu, 
                AV_TOTAL,LAND_SF,GROSS_AREA,
                LIVING_AREA, RES_FLOOR, 
                BLDG_AGE
                # occ # these two still not in here
  )

names(model.mlm)

# we cannot do HML on groups w less than 7 obs 
outliers <- model.mlm %>%
  group_by(CT_ID_10) %>%
  tally %>%
  arrange(n) %>%
  filter( n < 7)

outlier_list<-as.list(outliers$CT_ID_10)
#Count number of neighborhoods that need to be deleted
length(outlier_list) # dropping 10 CTS for not having enough observations #9 in 2023 #10 in 2024 with CT_ID_10

###Subset listings data to remove outliers
nrow(model.mlm) #73464 in 2023 #63253 in Nov 23 #63037 in 2024
model.mlm<-model.mlm[!(model.mlm$CT_ID_10 %in% outlier_list) ,]
nrow(model.mlm) #72108 #3742 in 2023 #73444 in Sep 23 #63228 in Nov 23 #63007 in 2024

noout <- model.mlm

noout$RES_FLOOR[which(noout$RES_FLOOR >50)]
noout$BLDG_AGE[which(noout$BLDG_AGE >500)]
noout$LAND_SF[which(noout$LAND_SF >300000)]

noout$CT_ID_10 <- as.factor(noout$CT_ID_10)

names(noout)

#I got an error here, which saying that  NA/NaN/Inf in 'y' and 
#Some predictor variables are on very different scales: consider rescaling

# Identify any NA/NaN/Inf in the relevant columns
problematic_indices <- apply(noout[, c("AV_TOTAL", "LAND_SF", "GROSS_AREA", "LIVING_AREA", "RES_FLOOR", "BLDG_AGE")], 1, function(x) any(is.na(x) | is.nan(x) | is.infinite(x)))

# Extract rows without NA/NaN/Inf values 
#clean_data <- noout[!problematic_indices, ] # no problematic rows.
clean_data <- noout 
# Check for non-positive values in 'AV_TOTAL'
#non_positive <- which(clean_data$AV_TOTAL <= 0) # no negative

# If there are non-positive values, you'll need to decide how to handle them. 
# You might remove them or adjust them if they're due to some known data issue.
#clean_data <- clean_data[-non_positive, ]


# Standardize predictors
predictors_to_scale <- c("LAND_SF", "GROSS_AREA", "LIVING_AREA", "RES_FLOOR", "BLDG_AGE")
clean_data[predictors_to_scale] <- scale(clean_data[predictors_to_scale])



(fullmodel <- lmer(log(AV_TOTAL) ~ LAND_SF+GROSS_AREA+LIVING_AREA+
                     RES_FLOOR+BLDG_AGE+(1|CT_ID_10),
                   data = clean_data,
                   REML = F,
                   na.action = "na.omit")) #simplu+
summary(fullmodel)

tract.unique <- fixef(fullmodel)[1] + ranef(fullmodel)$CT_ID_10
tract.unique$random <- ranef(fullmodel)$CT_ID_10
tract.unique$CT_ID_10 <- rownames(tract.unique)
length(tract.unique$CT_ID_10) #155 #166 in 2024
tract.unique$CensusTract <- c(1:166) #Error in `$<-.data.frame`(`*tmp*`, CensusTract, value = 1:166) : 
#replacement has 166 rows, data has 155
names(tract.unique)[1] <- "Intercept"
names(tract.unique)[2] <- "random"
tract.unique <- tract.unique[,c(3,4,1,2)] #what is rationale to do this?
tract.unique <- tract.unique[,c(1,3,4)]
tract.unique <- as.data.frame(tract.unique, row.names = NULL)
#rownames(tract.unique) <-NULL

tract.unique$random <- tract.unique$random$`(Intercept)`
tract.unique$random <- exp(tract.unique$random)

write.csv(tract.unique, "proptracts_effects_2024_10.csv", row.names = F)












#### I DO NOT KNOW WHY THERE ARE TONS OF MODELS #######################


summary(tract.unique$random) # -1M to 4.2M


# -537969 to 1837270 
# -537398 to 1808626 
# -537795 to 1809199
# -538362 to 1837834 
# -612298 to 2779547
# -611891 to 2779476 # 97
# -493045 to 1481647 # 95
# -537969 to 1837270 # 96
nrow(model)
model %>%  
  summarise(across(everything(), ~ sum(is.na(.))))



mean(clean_data$AV_TOTAL) #838046.6 #820718.2 in 2023



## Model for manipulation
model <- prop.acs %>%
  dplyr::select(PID,CT_ID_20,GIS_ID,
                TOTAL_VALUE,AV_BLDG_PER_SF,AV_LAND_PER_SF,
                LAND_SF,SIMPLIFIED_LU,GROSS_AREA,
                LIVING_AREA,RES_FLOOR,
                BLDG_AGE # used to be called age
                # ,PopDen,VacantUnitPer # not clear where these three variables come from
  ) #177091
## factoring the categorical variables
model$simplu <- as.factor(model$SIMPLIFIED_LU)
colnames(model)

## data screening and fixing variables
model$propid <- as.numeric(model$PID)
model$AV_TOTAL <- as.numeric(model$TOTAL_VALUE)
model$GROSS_AREA <- as.numeric(model$GROSS_AREA)
model$LAND_SF <- as.numeric(model$LAND_SF)
model$LIVING_AREA <- as.numeric(model$LIVING_AREA)

##### way 2 of dealing with outliers
names(model) #177091


A <- boxplot(model$AV_TOTAL)$out
model <- model[!(model$AV_TOTAL %in% A),] #161688
A <- boxplot(model$LAND_SF)$out
model <- model[!(model$LAND_SF %in% A),] #156712
A <- boxplot(model$GROSS_AREA)$out
model <- model[!(model$GROSS_AREA %in% A),] #154726
A <- boxplot(model$AV_BLDG_PER_SF)$out
model <- model[!(model$AV_BLDG_PER_SF %in% A),] #147250
A <- boxplot(model$AV_LAND_PER_SF)$out
model <- model[!(model$AV_LAND_PER_SF %in% A),] #139683
A <- boxplot(model$RES_FLOOR)$out
model <- model[!(model$RES_FLOOR %in% A),] #135781
A <- boxplot(model$BLDG_AGE)$out
model <- model[!(model$BLDG_AGE %in% A),] #135294
names(model)
summary(model)

# model$occ <- 1-model$VacantUnitPer # need to add this var in
model$total_sqft <- ((model$AV_BLDG_PER_SF) + (model$AV_LAND_PER_SF))
#model$total_sqft <- rowSums(model[,c("AV_BLDG_PER_SF", "AV_LAND_PER_SF")], na.rm=TRUE) # creates 0s instead of NAs - fixing that:
#model$total_sqft <- ifelse(model$total_sqft == 0, NA, model$total_sqft) # done
model$logtotalsq <-(log(model$total_sqft))
model$simplu <- relevel(model$simplu, ref = "RESIDENTIAL")


colnames(model)

model.mlm <- subset(model, simplu != "CONDO_UNIT") %>%
  dplyr::select(propid, CT_ID_20, GIS_ID,simplu, 
                total_sqft,AV_TOTAL,LAND_SF,GROSS_AREA,
                LIVING_AREA, RES_FLOOR, 
                BLDG_AGE
                # occ # these two still not in here
  )

names(model.mlm)

# we cannot do HML on groups w less than 7 obs 
outliers <- model.mlm %>%
  group_by(CT_ID_20) %>%
  tally %>%
  arrange(n) %>%
  filter( n < 7)

outlier_list<-as.list(outliers$CT_ID_20)
#Count number of neighborhoods that need to be deleted
length(outlier_list) # dropping 11 CTS for not having enough observations

###Subset listings data to remove outliers
nrow(model.mlm)
model.mlm<-model.mlm[!(model.mlm$CT_ID_20 %in% outlier_list) ,]
nrow(model.mlm) #73661

noout <- model.mlm

names(noout)

(fullmodel <- lmer(AV_TOTAL ~ LAND_SF+GROSS_AREA+LIVING_AREA+
                     RES_FLOOR+BLDG_AGE+(1|CT_ID_20),
                   data = noout,
                   REML = F,
                   na.action = "na.omit")) #simplu+
summary(fullmodel)

tract.unique <- fixef(fullmodel)[1] + ranef(fullmodel)$CT_ID_20
tract.unique$random <- ranef(fullmodel)$CT_ID_20
tract.unique$CT_ID_20 <- rownames(tract.unique)
tract.unique$CensusTract <- c(1:155)
names(tract.unique)[1] <- "Intercept"
names(tract.unique)[2] <- "random"
tract.unique <- tract.unique[,c(3,4,1,2)]

tract.unique <- tract.unique[,c(1,3,4)]
tract.unique <- as.data.frame(tract.unique)

tract.unique$random <- tract.unique$random$`(Intercept)`

write.csv(tract.unique, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/proptracts_effects_06202022.csv")

summary(tract.unique$random) # -368k to 467K





## Model for manipulation
model <- prop.acs %>%
  dplyr::select(PID,CT_ID_20,GIS_ID,
                TOTAL_VALUE,AV_BLDG_PER_SF,AV_LAND_PER_SF,
                LAND_SF,SIMPLIFIED_LU,GROSS_AREA,
                LIVING_AREA,RES_FLOOR,
                BLDG_AGE # used to be called age
                # ,PopDen,VacantUnitPer # not clear where these three variables come from
  ) #177091
## factoring the categorical variables
model$simplu <- as.factor(model$SIMPLIFIED_LU)
colnames(model)

## data screening and fixing variables
model$propid <- as.numeric(model$PID)
model$AV_TOTAL <- as.numeric(model$TOTAL_VALUE)
model$GROSS_AREA <- as.numeric(model$GROSS_AREA)
model$LAND_SF <- as.numeric(model$LAND_SF)
model$LIVING_AREA <- as.numeric(model$LIVING_AREA)

##### way 3 of dealing with outliers
names(model) 
nrow(model) ##177091
# model$occ <- 1-model$VacantUnitPer # need to add this var in
model$total_sqft <- ((model$AV_BLDG_PER_SF) + (model$AV_LAND_PER_SF))
#model$total_sqft <- rowSums(model[,c("AV_BLDG_PER_SF", "AV_LAND_PER_SF")], na.rm=TRUE) # creates 0s instead of NAs - fixing that:
#model$total_sqft <- ifelse(model$total_sqft == 0, NA, model$total_sqft) # done
model$logtotalsq <-(log(model$total_sqft))
model$simplu <- relevel(model$simplu, ref = "RESIDENTIAL")

colnames(model)

model.mlm <- subset(model, simplu != "CONDO_UNIT") %>%
  dplyr::select(propid, CT_ID_20, GIS_ID,simplu, 
                total_sqft,AV_TOTAL,LAND_SF,GROSS_AREA,
                LIVING_AREA, RES_FLOOR, 
                BLDG_AGE
                # occ # these two still not in here
  )

names(model.mlm)

model.mlm$mahal <- mahalanobis(model.mlm[,c(6:11)],
                               colMeans(model.mlm[,c(6:11)], na.rm = T),
                               cov(model.mlm[,c(6:11)], 
                                   use = "pairwise.complete.obs"))

model.mlm$p <- pchisq(model.mlm$mahal, df=ncol(model.mlm[,c(5:11)])-1, lower.tail=FALSE)

#sum(model.mlm$p < 0.001)
noout <- subset(model.mlm, model.mlm$p > 0.001) #74432
summary(noout$AV_TOTAL)


# we cannot do HML on groups w less than 7 obs 
outliers <- model.mlm %>%
  group_by(CT_ID_20) %>%
  tally %>%
  arrange(n) %>%
  filter( n < 7)

outlier_list<-as.list(outliers$CT_ID_20)
#Count number of neighborhoods that need to be deleted
length(outlier_list) # dropping 4 CTS for not having enough observations

###Subset listings data to remove outliers
nrow(model.mlm)
model.mlm<-model.mlm[!(model.mlm$CT_ID_20 %in% outlier_list) ,]
nrow(model.mlm) #109683

noout <- model.mlm

names(noout)

(fullmodel <- lmer(AV_TOTAL ~ simplu+LAND_SF+GROSS_AREA+LIVING_AREA+
                     RES_FLOOR+BLDG_AGE+(1|CT_ID_20),
                   data = noout,
                   REML = F,
                   na.action = "na.omit")) 
summary(fullmodel)

tract.unique <- fixef(fullmodel)[1] + ranef(fullmodel)$CT_ID_20
tract.unique$random <- ranef(fullmodel)$CT_ID_20
tract.unique$CT_ID_20 <- rownames(tract.unique)
tract.unique$CensusTract <- c(1:176)
names(tract.unique)[1] <- "Intercept"
names(tract.unique)[2] <- "random"
tract.unique <- tract.unique[,c(3,4,1,2)]

tract.unique <- tract.unique[,c(1,3,4)]
tract.unique <- as.data.frame(tract.unique)

tract.unique$random <- tract.unique$random$`(Intercept)`

write.csv(tract.unique, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/proptracts_effects_06202022.csv")

summary(tract.unique$random) # -29M to 161M




## Model for manipulation
model <- prop.acs %>%
  dplyr::select(PID,CT_ID_20,GIS_ID,
                TOTAL_VALUE,AV_BLDG_PER_SF,AV_LAND_PER_SF,
                LAND_SF,SIMPLIFIED_LU,GROSS_AREA,
                LIVING_AREA,RES_FLOOR,
                BLDG_AGE # used to be called age
                # ,PopDen,VacantUnitPer # not clear where these three variables come from
  )
## factoring the categorical variables
model$simplu <- as.factor(model$SIMPLIFIED_LU)
colnames(model)

## data screening and fixing variables
model$propid <- as.numeric(model$PID)
model$AV_TOTAL <- as.numeric(model$TOTAL_VALUE)
model$GROSS_AREA <- as.numeric(model$GROSS_AREA)
model$LAND_SF <- as.numeric(model$LAND_SF)
model$LIVING_AREA <- as.numeric(model$LIVING_AREA)

##### way 1 of dealing with outliers
names(model) #177091
nrow(model)
model <- model[model$AV_TOTAL < quantile(model$AV_TOTAL, 0.9, na.rm = T),] 
model <- model[model$AV_BLDG_PER_SF < quantile(model$AV_BLDG_PER_SF, 0.9, na.rm = T),] 
model <- model[model$AV_LAND_PER_SF < quantile(model$AV_LAND_PER_SF, 0.9, na.rm = T),] 
model <- model[model$LAND_SF < quantile(model$LAND_SF, 0.9, na.rm = T),] 
model <- model[model$GROSS_AREA < quantile(model$GROSS_AREA, 0.9, na.rm = T),] 
model <- model[model$RES_FLOOR < quantile(model$RES_FLOOR, 0.9, na.rm = T),] 
model <- model[model$BLDG_AGE < quantile(model$BLDG_AGE, 0.9, na.rm = T),] 
#115827
# model$occ <- 1-model$VacantUnitPer # need to add this var in
model$total_sqft <- ((model$AV_BLDG_PER_SF) + (model$AV_LAND_PER_SF))
#model$total_sqft <- rowSums(model[,c("AV_BLDG_PER_SF", "AV_LAND_PER_SF")], na.rm=TRUE) # creates 0s instead of NAs - fixing that:
#model$total_sqft <- ifelse(model$total_sqft == 0, NA, model$total_sqft) # done
model$logtotalsq <-(log(model$total_sqft))
model$simplu <- relevel(model$simplu, ref = "RESIDENTIAL")


colnames(model)
model.mlm <- subset(model, simplu != "CONDO_UNIT") %>%
  dplyr::select(propid, CT_ID_20, GIS_ID,simplu, 
                AV_TOTAL,LAND_SF,GROSS_AREA,
                LIVING_AREA, RES_FLOOR, 
                BLDG_AGE
                # occ # these two still not in here
  )

names(model.mlm)

# we cannot do HML on groups w less than 7 obs 
outliers <- model.mlm %>%
  group_by(CT_ID_20) %>%
  tally %>%
  arrange(n) %>%
  filter( n < 7)

outlier_list<-as.list(outliers$CT_ID_20)
#Count number of neighborhoods that need to be deleted
length(outlier_list) # dropping 26 CTS for not having enough observations

###Subset listings data to remove outliers
nrow(model.mlm)
model.mlm<-model.mlm[!(model.mlm$CT_ID_20 %in% outlier_list) ,]
nrow(model.mlm) #34613

noout <- model.mlm

names(noout)

(fullmodel <- lmer(AV_TOTAL ~ simplu+LAND_SF+GROSS_AREA+LIVING_AREA+
                     RES_FLOOR+BLDG_AGE+(1|CT_ID_20),
                   data = noout,
                   REML = F,
                   na.action = "na.omit")) 
summary(fullmodel)

tract.unique <- fixef(fullmodel)[1] + ranef(fullmodel)$CT_ID_20
tract.unique$random <- ranef(fullmodel)$CT_ID_20
tract.unique$CT_ID_20 <- rownames(tract.unique)
tract.unique$CensusTract <- c(1:120)
names(tract.unique)[1] <- "Intercept"
names(tract.unique)[2] <- "random"
tract.unique <- tract.unique[,c(3,4,1,2)]

tract.unique <- tract.unique[,c(1,3,4)]
tract.unique <- as.data.frame(tract.unique)

tract.unique$random <- tract.unique$random$`(Intercept)`

write.csv(tract.unique, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/proptracts_effects_06202022.csv")

summary(tract.unique$random) # -227k to 462K






model %>%  
  summarise_all(funs(sum(is.na())))

model %>% 
  group_by(simplu) %>% 
  summarise(sumNA = sum(is.na(CT_ID_20)))


A <- boxplot(model$AV_TOTAL)$out
A <- model[!(model$AV_TOTAL %in% A),] #161688
# Take out CONDO/CONDO_UNIT since it has the most NAs - which ever has the most NAs

#### Linearity Assumptions ###
## accuracy
summary(model.mlm)

## outliers using Tundey
# JdBK notes: need to know what decisions were made about these outliers. removed or not?
# outlierKD(model.mlm, AV_TOTAL)
# skim(model.mlm$AV_TOTAL)
# outlierKD(model.mlm, total_sqft)
# skim(model.mlm$total_sqft)
# outlierKD(model.mlm, LAND_SF)
# skim(model.mlm$LAND_SF)
# outlierKD(model.mlm, GROSS_AREA)
# skim(model.mlm$GROSS_AREA)
# outlierKD(model.mlm, LIVING_AREA)
colnames(model.mlm)
model.mlm$mahal <- mahalanobis(model.mlm[,c(6:11)],
                               colMeans(model.mlm[,c(6:11)], na.rm = T),
                               cov(model.mlm[,c(6:11)], 
                                   use = "pairwise.complete.obs"))

model.mlm$p <- pchisq(model.mlm$mahal, df=ncol(model.mlm[,c(5:11)])-1, lower.tail=FALSE)

#sum(model.mlm$p < 0.001)
noout <- subset(model.mlm, model.mlm$p > 0.001) #74384
#cutoff <- qchisq(1-.001, ncol(model.mlm[,c(5:11)]))
#summary(mahal < cutoff) #False = outliers
#noout <- subset(model.mlm, mahal<cutoff)


model.mlm_backup <- model.mlm #109683
model.mlm <- model.mlm_backup
model.mlm <- model.mlm[model.mlm$propid %in% PA$PID,] #85975


names(model.mlm)
model.mlm <- model.mlm[model.mlm$AV_TOTAL < quantile(model.mlm$AV_TOTAL, 0.9, na.rm = T),] #99737
model.mlm <- model.mlm[model.mlm$LAND_SF < quantile(model.mlm$LAND_SF, 0.9, na.rm = T),] #91438
model.mlm <- model.mlm[model.mlm$GROSS_AREA < quantile(model.mlm$GROSS_AREA, 0.99, na.rm = T),] #90792
model.mlm <- model.mlm[model.mlm$total_sqft < quantile(model.mlm$total_sqft, 0.94, na.rm = T),] #86960
model.mlm <- model.mlm[model.mlm$RES_FLOOR < quantile(model.mlm$RES_FLOOR, 0.999, na.rm = T),] #86891
model.mlm <- model.mlm[model.mlm$BLDG_AGE < quantile(model.mlm$BLDG_AGE, 0.996, na.rm = T),] #86626

A <- boxplot(model.mlm$AV_TOTAL)$out
model.mlm <- model.mlm[!(model.mlm$AV_TOTAL %in% A),] #100159
A <- boxplot(model.mlm$LAND_SF)$out
model.mlm <- model.mlm[!(model.mlm$LAND_SF %in% A),] #95073
A <- boxplot(model.mlm$GROSS_AREA)$out
model.mlm <- model.mlm[!(model.mlm$GROSS_AREA %in% A),] #93758
A <- boxplot(model.mlm$total_sqft)$out
model.mlm <- model.mlm[!(model.mlm$total_sqft %in% A),] #83047
summary(model.mlm)
A <- boxplot(model.mlm$RES_FLOOR)$out

# we cannot do HML on groups w less than 7 obs 
outliers <- model.mlm %>%
  group_by(CT_ID_20) %>%
  tally %>%
  arrange(n) %>%
  filter( n < 7)

outlier_list<-as.list(outliers$CT_ID_20)
#Count number of neighborhoods that need to be deleted
length(outlier_list)

###Subset listings data to remove outliers
nrow(model.mlm)
model.mlm<-model.mlm[!(model.mlm$CT_ID_20 %in% outlier_list) ,]
nrow(model.mlm) #73684


noout <- model.mlm
## multicollinearity
summary(noout)
colnames(noout)
cor(noout[,c(6:11)], use = "pairwise.complete.obs")





## running "fake" model
random = rchisq(nrow(noout), 7) #random variable to get random errors
fake = lm(random ~ ., data = noout[,c(6:11)])
fitted = scale(fake$fitted.values)
standardize = rstudent(fake)

## linearity
qqnorm(standardize)
abline(0,1)

## normality
hist(standardize)
plotNormalHistogram(model.mlm$AV_TOTAL)
skim(model.mlm)

## homogeneity
plot(fitted, standardize)
abline(0,0)
abline(v=0) #heteroskedastic

### Sampling the data for visual inspection of differences in value by
#   landuse and Census Tracts
library(lattice)
library(MASS)
library(car)
library(ggplot2)
library(lmom)
library(fitdistrplus)

noout$AV_TOTAL.t <- noout$AV_TOTAL +1

## Normal
qqp(noout$AV_TOTAL.t, "norm")
## Log Normal
qqp(noout$AV_TOTAL.t, "lnorm")
## Negative Binomial
nbinom <- fitdistr(10*complete.cases(noout$AV_TOTAL.t), "Negative Binomial")
qqp(noout$AV_TOTAL.t, "nbinom", size= nbinom$estimate[[1]], mu= nbinom$estimate[[2]])
## Poisson
poisson <- fitdistr(10*complete.cases(noout$AV_TOTAL.t), "Poisson")
qqp(noout$AV_TOTAL.t, "pois", poisson$estimate)
## gamma
gamma<- fitdist(10*complete.cases(noout$AV_TOTAL.t), distr = "gamma", method = "mge")
qqp(noout$AV_TOTAL.t, "gamma", shape = gamma$estimate[[1]], rate=gamma$estimate[[2]])


#---------------------------------------------------------------#
#   Running Intercept, Fixed-Only, Random-Only, and Full Model  #
#---------------------------------------------------------------#
library(lmerTest)
## Just Intercept
intermodel <- lm(AV_TOTAL ~ 1,
                 data = noout,
                 na.action = "na.omit")
summary(intermodel)

## Just Random Effects
randomodel <- lmer(AV_TOTAL ~ (1|CT_ID_20),
                   data = noout,
                   REML = F,
                   na.action = "na.omit")
summary(randomodel)
anova(intermodel, randomodel)

## Just Fixed Effects
fixedmodel <- lm(AV_TOTAL ~ simplu+LAND_SF+GROSS_AREA+LIVING_AREA+
                   RES_FLOOR+BLDG_AGE,
                 data = noout,
                 na.action = "na.omit")
summary(fixedmodel)
anova(randomodel,fixedmodel)






## Full MLM Model 
names(noout)

(fullmodel <- lmer(AV_TOTAL ~ LAND_SF+GROSS_AREA+LIVING_AREA+
                     RES_FLOOR+BLDG_AGE+(1|CT_ID_20),
                   data = noout,
                   REML = F,
                   na.action = "na.omit")) #exclude simplu as it includes only one unique value.
summary(fullmodel)

## Models "Fit"
anova(randomodel,fixedmodel)
anova(randomodel,fullmodel)

## Fixed Effects or the slopes (i.e. Means) of all IVs
fixed.coefs <- coef(summary(fullmodel))[, "Estimate"]
fixed.coefs

### Random Effects (variance components for each Census Tract. Correspond
# to the differences in relation to the fixed effects)
rando.coefs <- ranef(fullmodel)$CT_ID_20
head(rando.coefs)
colMeans(rando.coefs)
skim(rando.coefs)

## The Coefficients for the Full Model
coefs <- coef(fullmodel)$CT_ID_20
head(coefs)

tract.unique <- fixef(fullmodel)[1] + ranef(fullmodel)$CT_ID_20
tract.unique$random <- ranef(fullmodel)$CT_ID_20
tract.unique$CT_ID_20 <- rownames(tract.unique)
tract.unique$CensusTract <- c(1:153)
names(tract.unique)[1] <- "Intercept"
names(tract.unique)[2] <- "random"
tract.unique <- tract.unique[,c(3,4,1,2)]

tract.unique <- tract.unique[,c(1,3,4)]
tract.unique <- as.data.frame(tract.unique)

tract.unique$random <- tract.unique$random$`(Intercept)`

write.csv(tract.unique, "/Users/Shared/Files From d.localized/School/Semester 6/BARI PAD update/PAD/proptracts_effects_06202022.csv")

test <- tract.unique$Intercept - ranef(fullmodel)$CT_ID_20

tract.unique$CT_ID_20 <- as.numeric(tract.unique$CT_ID_20)
# proptracts_effects <- left_join(proptracts, tract.unique, by= "CT_ID_20")
proptracts_effects <- merge(proptracts, tract.unique, by= "CT_ID_20", all=F)
row.names(proptracts_effects) <- c(1:178)
proptracts_effects <- as.data.frame(proptracts_effects)
proptracts_effects$random <- tract.unique$Intercept - 249757.33034
write.csv(proptracts_effects, "D:\\Google Drive\\Final presentation\\proptracts_effects_0820.csv", row.names = F)


hen <- read.csv("/Users/saina/Downloads/PADCross.CT.06122022.csv")
names(hen)
summary(hen$nbhdval)
sum(is.na(hen$nbhdval))


