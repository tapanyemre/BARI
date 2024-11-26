
### ************      THIS SECTION NEEDS TO BE EDITED BEFORE RUNNING THE CODE       ************* #######

# Load required package
if (!require("tidyverse", character.only = TRUE)) install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)

# Set global options
options(scipen = 999)
`%notin%` <- Negate(`%in%`)

# Define custom negation operator
`%notin%` <- Negate(`%in%`)

input_dir <- "~/BARI/Property Assesments/Inputs"
output_dir <- "~/BARI/Property Assesments/Outputs"

pa_path <- file.path(input_dir, "fy2024-property-assessment-data_1_5_2024.csv")
SAM_path <- file.path(input_dir, "Live_Street_Address_Management_SAM_Addresses.csv")

outlier_path <- file.path(output_dir, "PA_outliers_2.csv")
padunit_path <- file.path(output_dir, "PAD.Record.wUnit_2.csv")

# Load input data
property_dt <- read.csv(pa_path, header = TRUE)
SAM_dt <- read.csv(SAM_path, header = TRUE)


############## preparing data ##########

# defining unit_N by LU
property_dt <- property_dt %>%
  mutate(unit_N_org = case_when(
    LU == "R1" ~ 1,
    LU == "R2" ~ 2,
    LU == "R3" ~ 3,
    LU %in% c("CL", "RL - RL", "CP") ~ 0,
    LU %in% c("CM", "AH") ~ 1,
    LU == "CD" & UNIT_NUM != "" ~ 1,
    TRUE ~ NA_real_
  ))

SAM_dt <- SAM_dt %>%
  filter(!is.na(PARCEL)) %>%
  rename(GIS_ID = PARCEL) %>%
  group_by(BUILDING_ID, GIS_ID) %>%
  summarise(unit_count = n(), .groups = 'drop')



# Remove duplicates based on GIS_ID
SAM_dt <- distinct(SAM_dt, GIS_ID, unit_count)


property_dt$unit_count <- SAM_dt$unit_count[match(property_dt$GIS_ID, SAM_dt$GIS_ID)]


# what we did here is, we counted the number of units in each property in SAM, 
#and then merge this unit_count into property_id by parcel number. 
#now the new problem is that the nrow has increase 216700 - 174668 = 42032. 
#same GIS_id, same PID, different unit_count in property_id. This records have ferent property ids in SAM


grouping_vars <- names(property_dt)[!grepl("unit_count", names(property_dt))]

data_all <- property_dt %>%
  group_by(across(all_of(grouping_vars))) %>%
  mutate(
    New_unit = sum(unit_count, na.rm = TRUE),
    unit_N = ifelse(is.na(unit_N_org), New_unit, unit_N_org),
    unit_N_orig = ifelse(UNIT_NUM == "", NA, unit_N)
  ) %>%
  ungroup()


data_all$GROSS_AREA_org <- data_all$GROSS_AREA
data_all$LIVING_AREA_org <- data_all$LIVING_AREA


apply(data_all, 2, function(x) sum(is.na(x)))# this dataset has 14126 NA values of units 
# NA rows: unit_N   5553,      GROSS_AREA_org    34633,   LIVING_AREA_org  34923 


#######################
###Start of the code

# Calculate the number of NAs in `unit_N` by `LU` type
na_count_by_LU <- data_all %>%
  group_by(LU) %>%
  summarise(num_NA = sum(is.na(unit_N)), .groups = 'drop')

print(na_count_by_LU)
#num_NA
#R3           0
#R2           0
#CM           0
#CD           0
#R1           0
#R4          32
#C          637
#RC         231
#RL - RL      0
#E         4171
#A          248
#CL           0
#I           92
#CC          51
#EA          91
#CP           0
#AH           0

#Identify LU types with NA values:
lu_with_na <- unique(data_all$LU[is.na(data_all$unit_N)])
lu_with_na
#only "I"  "E"  "C"  "A"  "EA" "RC" "R4" "CC" have NA values.
#Identify LU types without NA values:
lu_without_na <- setdiff(unique(data_all$LU), lu_with_na)
lu_without_na
#[1] "R3"      "R2"      "CM"      "CD"      "R1"      "RL - RL" "CL"      "CP" "AH"     
#Segregating the LU types which do not have NA values for the unit_N column and processing the subset with NA values separately
#For the subset with NA values:
data_with_na <- data_all[data_all$LU %in% lu_with_na, ]
#For the subset without NA values:
data_without_na <- data_all[!data_all$LU %in% lu_with_na, ]
data_original <- data_all
#data_all_other <- data_original[!(data_original$LU != "RL - RL" & data_original$LU != "AH" &  data_original$LU != "CM" & 
#                                    data_original$LU != "CL" & data_original$LU != "R1" & data_original$LU != "R2" & data_original$LU != "R3"),]
data_all_other <- data_without_na
#data_all <- data_all[(data_all$LU != "RL - RL" & data_all$LU != "AH" &  data_all$LU != "CM" & 
#                        data_all$LU != "CL" & data_all$LU != "R1" & data_all$LU != "R2" & data_all$LU != "R3"),]
data_all <- data_with_na
# Formatting the tax values by removing the $ symbols
data_all$BLDG_VALUE = as.numeric(gsub("[\\$,]", "", data_all$BLDG_VALUE))
data_all$LAND_VALUE = as.numeric(gsub("[\\$,]", "", data_all$LAND_VALUE))
data_all$TOTAL_VALUE = as.numeric(gsub("[\\$,]", "", data_all$TOTAL_VALUE))
data_all$GROSS_TAX = as.numeric(gsub("[\\$,]", "", data_all$GROSS_TAX))
#Missing value count for all the columns
apply(data_all,2,function(x) sum(is.na(x)))
# unit_N 5553  GROSS_AREA_org  8830  LIVING_AREA_org  8841 


#Impute missing data for GROSS_AREA and LIVING_AREA
quantile(data_all[!is.na(data_all$GROSS_AREA),]$GROSS_AREA ,na.rm = TRUE,probs = c(0.05,0.95))
# 5%  546   95%  7792 
quantile(data_all[!is.na(data_all$LIVING_AREA),]$LIVING_AREA ,na.rm = TRUE,probs = c(0.05,0.95))
# 5%  545.00   95% 5764.85 

data_all$GROSS_AREA <- ifelse(is.na(data_all$GROSS_AREA), 0, data_all$GROSS_AREA)
data_all$LIVING_AREA <- ifelse(is.na(data_all$LIVING_AREA), 0, data_all$LIVING_AREA)

###### start guessing to be able to guess! ### I modified these codes as  our dataset has zeros instead of NA values (Yet, 23)

######### 1st Way ############
# # Helper function for GLM imputation
# impute_glm <- function(formula, target_var, filter_condition) {
#   model <- glm(formula, data = data_all[filter_condition, ])
#   predictions <- predict(model, newdata = data_all[!filter_condition, ], type = "response")
#   data_all[!filter_condition, target_var] <- as.integer(predictions)
# }
# 
# # Impute missing numeric values using GLM
# impute_glm(GROSS_TAX ~ LIVING_AREA + GROSS_AREA, "GROSS_TAX", !is.na(data_all$GROSS_TAX))
# impute_glm(TOTAL_VALUE ~ GROSS_TAX + GROSS_AREA, "TOTAL_VALUE", data_all$TOTAL_VALUE != 0)
# impute_glm(BLDG_VALUE ~ TOTAL_VALUE + GROSS_TAX, "BLDG_VALUE", data_all$BLDG_VALUE != 0)
# impute_glm(LAND_VALUE ~ BLDG_VALUE + GROSS_TAX, "LAND_VALUE", data_all$LAND_VALUE != 0)
# impute_glm(GROSS_AREA ~ BLDG_VALUE + LAND_VALUE, "GROSS_AREA", data_all$GROSS_AREA != 0)
# impute_glm(LIVING_AREA ~ BLDG_VALUE + LAND_VALUE + GROSS_AREA, "LIVING_AREA", data_all$LIVING_AREA != 0)
# 
# # Helper function for imputing unit_N by LU type
# impute_unit_N <- function(LU_type, formula) {
#   model <- glm(formula, family = poisson(link = "log"), data = data_all[!is.na(data_all$unit_N) & data_all$LU == LU_type, ])
#   predictions <- predict(model, newdata = data_all[is.na(data_all$unit_N) & data_all$LU == LU_type, ], type = "response")
#   data_all[is.na(data_all$unit_N) & data_all$LU == LU_type, "unit_N"] <- as.integer(predictions)
# }
# 
# # Apply GLM imputation for unit_N by LU type
# impute_unit_N("R4", unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE)
# impute_unit_N("C", unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE)
# impute_unit_N("RC", unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE)
# impute_unit_N("E", unit_N ~ LAND_VALUE + GROSS_AREA + LIVING_AREA + BLDG_VALUE)
# impute_unit_N("A", unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE)
# impute_unit_N("I", unit_N ~ GROSS_AREA + LAND_VALUE)
# impute_unit_N("CC", unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE)
# impute_unit_N("EA", unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE)
# 
# # Handle outliers manually
# data_all[data_all$PID %in% data_all[is.na(data_all$unit_N), "PID"], "unit_N"] <- 1000
# data_all[is.na(data_all$unit_N), "unit_N"] <- 0
# data_all[data_all$PID == 304102000, "unit_N"] <- 849
# 
# # Save outliers and the final dataset
# k <- data_all[is.na(data_all$unit_N), ]
# write.csv(k, outlier_path, row.names = FALSE)
# write.csv(data_all, padunit_path, row.names = FALSE)



################## Second Way ##############

lm_md_gross_area =glm(GROSS_TAX ~ LIVING_AREA + GROSS_AREA, data=data_all[!is.na(data_all$GROSS_TAX),])
glm_pred_gross_area <- predict.glm(lm_md_gross_area,  newdata = data_all[is.na(data_all$GROSS_TAX),], type = "response")

data_all[is.na(data_all$GROSS_TAX),'GROSS_TAX'] <- as.integer(glm_pred_gross_area)

#summary(data_all)

lm_md_gross_area =glm(TOTAL_VALUE ~ GROSS_TAX + GROSS_AREA, data=data_all[data_all$TOTAL_VALUE != 0,])
glm_pred_gross_area <- predict.glm(lm_md_gross_area,  newdata = data_all[data_all$TOTAL_VALUE == 0,], type = "response")

data_all[data_all$TOTAL_VALUE == 0,'TOTAL_VALUE'] <- as.integer(glm_pred_gross_area)


lm_md_gross_area =glm(BLDG_VALUE ~ TOTAL_VALUE + GROSS_TAX, data=data_all[data_all$BLDG_VALUE != 0,])
glm_pred_gross_area <- predict.glm(lm_md_gross_area,  newdata = data_all[data_all$BLDG_VALUE == 0,],  type = "response")

# hist(glm_pred_gross_area)
# summary(glm_pred_gross_area)
data_all[data_all$BLDG_VALUE == 0,'BLDG_VALUE'] <- as.integer(glm_pred_gross_area)


lm_md_gross_area =glm(LAND_VALUE ~ BLDG_VALUE + GROSS_TAX, data=data_all[data_all$LAND_VALUE != 0,])
glm_pred_gross_area <- predict.glm(lm_md_gross_area,  newdata = data_all[data_all$LAND_VALUE == 0,], type = "response")

# hist(glm_pred_gross_area)
# summary(glm_pred_gross_area)
data_all[data_all$LAND_VALUE == 0,'LAND_VALUE'] <- as.integer(glm_pred_gross_area)


#AV_TOTAL causes the coeffs to be NA (rank deficiency)
lm_md_gross_area =glm(GROSS_AREA ~ BLDG_VALUE + LAND_VALUE,    data=data_all[data_all$GROSS_AREA != 0,])
glm_pred_gross_area <- predict.glm(lm_md_gross_area,  newdata = data_all[data_all$GROSS_AREA == 0,], type = "response")
# hist(glm_pred_gross_area)
# summary(glm_pred_gross_area)
data_all[data_all$GROSS_AREA == 0,'GROSS_AREA'] <- as.integer(glm_pred_gross_area)

#Impute missing data for LIVING_AREA
quantile(data_all[!is.na(data_all$LIVING_AREA),]$LIVING_AREA ,na.rm = TRUE,probs = c(0.05,0.95))
lm_md_living_area = glm(LIVING_AREA ~ BLDG_VALUE + LAND_VALUE + GROSS_AREA, data=data_all[data_all$LIVING_AREA != 0,])
glm_pred_living_area <- predict.glm(lm_md_living_area,  newdata = data_all[data_all$LIVING_AREA == 0,],  type = "response")
# hist(glm_pred_living_area)
# summary(glm_pred_living_area)
data_all[data_all$LIVING_AREA == 0,'LIVING_AREA'] <- as.integer(glm_pred_living_area)

######## now ready to guess unit N

apply(data_all,2,function(x) sum(is.na(x)))

# LUvec <- unique(data_all$LU)
# LUvec
#"R4" "C"  "RC" "E"  "CD" "A"  "I"  "CC" "EA" "CP"

###Impute unit_N for different LU
#LUvec = "R4"
md_r4 = glm(unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE , 
            family = poisson(link = "log"), 
            data = data_all[(!is.na(data_all$unit_N) & (data_all$LU=='R4')),])
glm_pred_r4 <- predict.glm(md_r4, 
                           newdata = data_all[(is.na(data_all$unit_N) & (data_all$LU=='R4')),], 
                           type = "response")
# hist(glm_pred_r4)
# summary(glm_pred_r4)
data_all[(is.na(data_all$unit_N) & (data_all$LU=='R4')),"LUC"] <- as.integer(glm_pred_r4)
#names(data_all)
#LUvec = "C"
md_c = glm(unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE , 
           family = poisson(link = "log"), 
           data = data_all[(!is.na(data_all$unit_N) & (data_all$LU=='C')),])
glm_pred_c <- predict.glm(md_c, 
                          newdata = data_all[(data_all$unit_N=="1000" & (data_all$LU=='C')),], 
                          type = "response")


# hist(glm_pred_c)
# summary(glm_pred_c)
data_all[(is.na(data_all$unit_N) & (data_all$LU=='C')),"LUC"] <- as.integer(glm_pred_c)

#LUvec = "RC"
md_rc = glm(unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE , 
            family = poisson(link = "log"), 
            data = data_all[(!is.na(data_all$unit_N) & (data_all$LU=='RC')),])
glm_pred_rc <- predict.glm(md_rc, 
                           newdata = data_all[(is.na(data_all$unit_N) & (data_all$LU=='RC')),], 
                           type = "response")
# hist(glm_pred_rc)
# summary(glm_pred_rc)
data_all[(is.na(data_all$unit_N) & (data_all$LU=='RC')),"LUC"] <- as.integer(glm_pred_rc)


#LUvec = "E"
md_e = glm(unit_N ~ LAND_VALUE + GROSS_AREA + LIVING_AREA + BLDG_VALUE, 
           family = poisson(link = "log"), 
           data = data_all[(!is.na(data_all$unit_N) & (data_all$LU=='E')),])
glm_pred_e <- predict.glm(md_e, newdata = data_all[(is.na(data_all$unit_N) & (data_all$LU=='E')),], type = "response")
#   + LAND_VALUE
# hist(glm_pred_e)
summary(glm_pred_e)
summary(md_e)
data_all[(is.na(data_all$unit_N) & (data_all$LU=='E')),"LUC"] <- as.integer(glm_pred_e)

#LUvec = "CD"
# dim(data_all[(!is.na(data_all$unit_N) & (data_all$LU=='CD')),])
#[1]  0 82

########It seems we have all the units for CD category probably from previous SAM data
# + LAND_VALUE
#md_cd = glm(unit_N ~  BLDG_VALUE + GROSS_AREA ,
#            family = poisson(link = "log"),
#             data = data_all[(!is.na(data_all$unit_N) & (data_all$LU=='CD')),])
# glm_pred_cd <- predict.glm(md_cd,
#                            newdata = data_all[(is.na(data_all$unit_N) & (data_all$LU=='CD')),],
#                            type = "response")
# # hist(glm_pred_cd)
#  summary(glm_pred_cd)
# data_all[(is.na(data_all$unit_N) & (data_all$LU=='CD')),"LUC"] <- as.integer(glm_pred_cd)


#LUvec = "A"
md_a = glm(unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE , 
           family = poisson(link = "log"), 
           data = data_all[(!is.na(data_all$unit_N) & (data_all$LU=='A')),])
glm_pred_a <- predict.glm(md_a, 
                          newdata = data_all[(is.na(data_all$unit_N) & (data_all$LU=='A')),], 
                          type = "response")
# hist(glm_pred_a)
# summary(glm_pred_a)
data_all[(is.na(data_all$unit_N) & (data_all$LU=='A')),"LUC"] <- as.integer(glm_pred_a)


#LUvec = "I"
md_i = glm(unit_N ~ GROSS_AREA + LAND_VALUE , 
           family = poisson(link = "log"), 
           data = data_all[(!is.na(data_all$unit_N) & (data_all$LU=='I')),])
glm_pred_i <- predict.glm(md_i, 
                          newdata = data_all[(is.na(data_all$unit_N) & (data_all$LU=='I')),], 
                          type = "response")
# hist(glm_pred_i)
# summary(glm_pred_i)
data_all[(is.na(data_all$unit_N) & (data_all$LU=='I')),"LUC"] <- as.integer(glm_pred_i)

#LUvec = "CC"
########It seems we have all the units for CD category probably from previous SAM data
md_cc = glm(unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE ,
            family = poisson(link = "log"), 
            data = data_all[(!is.na(data_all$unit_N) & (data_all$LU=='CC')),])
glm_pred_cc <- predict.glm(md_cc, 
                           newdata = data_all[(is.na(data_all$unit_N) & (data_all$LU=='CC')),], 
                           type = "response")
# hist(glm_pred_cc)
# summary(glm_pred_cc)
data_all[(is.na(data_all$unit_N) & (data_all$LU=='CC')),"LUC"] <- as.integer(glm_pred_cc)


#LUvec = "EA"

md_ea = glm(unit_N ~ GROSS_AREA + BLDG_VALUE + LAND_VALUE ,
            family = poisson(link = "log"), 
            data = data_all[(!is.na(data_all$unit_N) & (data_all$LU=='EA')),])
glm_pred_ea <- predict.glm(md_ea, 
                           newdata = data_all[(is.na(data_all$unit_N) & (data_all$LU=='EA')),], 
                           type = "response")
# hist(glm_pred_ea)
# summary(glm_pred_ea)
data_all[(is.na(data_all$unit_N) & (data_all$LU=='EA')),"LUC"] <- as.integer(glm_pred_ea)


############################# Imputing the unit_N for the rows with missing values ##########################

k <- data_all[(is.na(data_all$unit_N)),]
# Handling the outliers of the dataset by imputing the unit_N as 1000 instead of NA
data_all[data_all$PID %in% c(k$PID), "unit_N"] <- 1000
data_all[(is.na(data_all$unit_N)),"unit_N"] <- 0
data_all[data_all$PID==304102000,"unit_N"] <- 849 #Based on internet research. Subject to inaccuracy
k$unit_N <- 1000
write.csv(k, outlier_path, row.names= FALSE)

#############################################################################################################


#Impute all the missing
data_all[(data_all$unit_N=="1000" & (data_all$LU=='R4')),'unit_N'] <- as.integer(ceiling(glm_pred_r4))
data_all[(data_all$unit_N=="1000" & (data_all$LU=='C')),'unit_N'] <- as.integer(ceiling(glm_pred_c))
data_all[(data_all$unit_N=="1000" & (data_all$LU=='RC')),'unit_N'] <- as.integer(ceiling(glm_pred_rc))
data_all[(data_all$unit_N=="1000" & (data_all$LU=='E')),'unit_N'] <- as.integer(ceiling(glm_pred_e))
#data_all[(is.na(data_all$unit_N) & (data_all$LU=='CD')),"unit_N"] <- as.integer(ceiling(glm_pred_cd))
data_all[(data_all$unit_N=="1000" & (data_all$LU=='A')),'unit_N'] <- as.integer(ceiling(glm_pred_a))
data_all[(data_all$unit_N=="1000" & (data_all$LU=='I')),'unit_N'] <- as.integer(ceiling(glm_pred_i))
data_all[(data_all$unit_N=="1000" & (data_all$LU=='CC')),"unit_N"] <- as.integer(ceiling(glm_pred_cc))
data_all[(data_all$unit_N=="1000" & (data_all$LU=='EA')),'unit_N'] <- as.integer(ceiling(glm_pred_ea))
#data_all[(is.na(data_all$unit_N) & (data_all$LU=='CP')),"unit_N"] <- as.integer(ceiling(glm_pred_cp))

apply(data_all,2,function(x) sum(is.na(x)))


### WHAT IS THE REASON BEHIND THIS???
# data_all[data_all$LU == "R1",]$unit_N <- 1
# data_all[data_all$LU == "R2",]$unit_N <- 2
# data_all[data_all$LU == "R3",]$unit_N <- 3
# data_all[data_all$LU == "CL",]$unit_N <- 0
# data_all[data_all$LU == "RL",]$unit_N <- 0
# data_all[data_all$LU == "CP",]$unit_N <- 0
# data_all[data_all$LU == "CM",]$unit_N <- 1
# data_all[data_all$LU == "AH",]$unit_N <- 1
# 
# apply(data_all,2,function(x) sum(is.na(x)))


# data_all %>% 
#   filter(is.na(unit_N)) %>% 
#   group_by(LU) %>% 
#   summarise(count = n())

# Total percentage of NAs in Unit_N column in the dataset

(sum(is.na(data_all$unit_N))/nrow(data_all))*100 #0

# write.csv(data_all, "D:/School/BARI/Semester4/GI/data_unit_fnl.csv", row.names=F)


#summary of unit_N
# summary(data_all$unit_N)
# summary(data_original$unit_N)
# quantile(data_all$unit_N,probs = c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
# quantile(data_original$unit_N[!is.na(data_original$unit_N)],probs = c(0.01,0.05,0.25,0.5,0.75,0.95,0.99),rm.na = TRUE)
# 
table(data_all$LU,is.na(data_all$unit_N))
#FALSE  TRUE
#A   2858     0
#C   4443     0
#CC     0  1794
#CD     0 62647
#CP     0  5479
#E   8745     0
#EA   610     0
#I    496     0
#R4  2576     0
#RC  2950     0

#YET (Aug 23)
#FALSE
# A   2935
# C   4674
# CC  1536
# E   7605
# EA   263
# I    444
# R4  2514
# RC  2930


# merging with the property assessment dataset
# 
# table(data_original$LU,is.na(data_all$unit_N))
# nrow(data_all)
# 
# dtt <- fread("D:/School/Semester 4/BARI/dt1.csv")
# nrow(dtt)
# gc()
# 
# data_join <- dtt %>%
#   left_join(data_all, by = "PID")
# 
# 
# memory.limit()
# memory.limit(size=600000)


data_all %>%
  filter(unit_N > 1000)


data_all %>%
  filter(LU == "C")
# 
dt <- data_all
data_all <- dt

a <- data_all[data_all$unit_N > 300 & is.na(data_all$unit_N_orig),]
a %>%
  dplyr:: select("LU", "MAIL_ADDRESS", "MAIL_ADDRESSEE", "GROSS_AREA", "LIVING_AREA", "unit_N", "unit_N_org", ) %>%
  filter(unit_N > 200)

#Getting the array indices for rows satisfying the filter condition inside the which command and imputing the unit_N for those rows with 1000 in the dataframe
arr_ind <- c(which(data_all$unit_N > 20000 & is.na(data_all$unit_N_orig), arr.ind = TRUE))
data_all[arr_ind,"GROSS_TAX"] = 1000

#Getting the array indices for rows satisfying the filter condition inside the which command and imputing the unit_N for those rows with 1000 in the dataframe
arr_ind1 <- c(which(data_all$unit_N > 2000 & is.na(data_all$unit_N_orig)), arr.ind = TRUE)
data_all[which(data_all$unit_N > 2000 & is.na(data_all$unit_N_orig)),"unit_N"] = 1000

#Getting the array indices for rows satisfying the filter condition inside the which command and imputing the unit_N for those rows with 500 in the dataframe
arr_ind2 <- c(which(data_all$unit_N > 700 & is.na(data_all$unit_N_orig) & data_all$unit_N < 950), arr.ind = TRUE)
data_all[arr_ind2, "unit_N"] = 500
# 2020
#Getting the array indices for rows satisfying the filter condition inside the which command and imputing the unit_N for those rows with 500 in the dataframe
arr_ind3 <- c(which(data_all$unit_N > 700 & is.na(data_all$unit_N_orig)), arr.ind = TRUE)
data_all[which(data_all$unit_N > 700 & is.na(data_all$unit_N_orig)),"unit_N"] = 500

#PArecord_final_path = "Property Assessment Data/Data/PAD.Record.wUnit.05082020.csv"
#write.csv(data_all, paste0(BARI, PArecord_final_path), row.names=F)

############ Formatting the data properly #### 
data_all$LAND_VALUE <- as.character(data_all$LAND_VALUE)
data_all$BLDG_VALUE <- as.character(data_all$BLDG_VALUE)
data_all$TOTAL_VALUE <- as.character(data_all$TOTAL_VALUE)
data_all$GROSS_TAX <- as.character(data_all$GROSS_TAX)
data_all_other$LAND_VALUE <- as.character(data_all_other$LAND_VALUE)
data_all_other$BLDG_VALUE <- as.character(data_all_other$BLDG_VALUE)
data_all_other$TOTAL_VALUE <- as.character(data_all_other$TOTAL_VALUE)
data_all_other$GROSS_TAX <- as.character(data_all_other$GROSS_TAX)
####  combining with the data_all_other part #############
data_all_final <- rbind(data_all, data_all_other)


### WRITE CSV ###
write.csv(data_all_final, padunit_path, row.names=F)
# > 100267/175004
# [1] 0.5729412







#### add TLIDs

TLID <- read.csv("C:/Users/ux305/Google Drive/BARI Research Team Data Library/Geographical Infrastructure/Boston Geographical Infrastructure 2019/Data/parcels_fullupdatedTLID_190713.csv")


data_all1$TLID <- TLD$TLID[match(data_all_final$PID,TLD$PID)]






