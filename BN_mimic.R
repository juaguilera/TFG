library(readr)
library(arules)
library(bnlearn)

dataset_1_row_per_pacient <- read_csv("MatCAD/MatCAD-4/TFG/Dades/mimic-iii-clinical-database-1.4/dataset_1_row_per_pacient.csv")

colnames(dataset_1_row_per_pacient)

df <- as.data.frame(subset(dataset_1_row_per_pacient, select=-c(...1, subject_id, DRG_type)))

df <- df[complete.cases(df), ]

#DICRETIZE VARIABLES
df$weight <- arules::discretize(df$weight, method="interval", breaks=4, labels=c("underweight", "healthy", "overweight", "obese"))

df$LOS_x <- df$LOS_x / 24 # Passem de hores a dies.
df$LOS_x <- arules::discretize(df$LOS_x, method="interval", breaks=5, labels=c("0-1 day", "1-2 days", "2-3 days", "3-4 days", "4+ days"))

df$LOS_y <- df$LOS_y / (24 * 7) # Passem de hores a setmanes.
df$LOS_y <- arules::discretize(df$LOS_y, method="interval", breaks=5, labels=c("0-1 week", "1-2 weeks", "2-3 weeks", "3-4 weekss", "1+ month"))

breaks = c(15, 32, 49, 66, Inf)
df$age <- arules::discretize(df$age, method="fixed", breaks=breaks, labels=c("young", "young adult", "adult", "senior"))

#FACTORIZE VARIABLES
fac_cols <- c("gender", "admission_type", "admit_location", "DRG_code", "diagnosis", "diagnosis_severity",
              "diagnosis_mortality", "respiratory_services", "current_careunit", "LOS_x", "LOS_y", "discharge_location",
              "is_dead_hosp")
df[,fac_cols] <- lapply(df[,fac_cols], as.factor)

############################
# M2 - BINARY RELEVANCE
############################

outputs <- c("respiratory_services", "current_careunit", "LOS_x", "LOS_y", "discharge_location", "is_dead_hosp")
inputs <- c("gender", "weight", "age", "admission_type", "admit_location", "DRG_code", "diagnosis", "diagnosis_severity", "diagnosis_mortality")
df2_whitelist <- data.frame("from" = rep(outputs, each=length(inputs)),
                           "to" = rep(inputs, time=length(outputs)))

df2_blacklist <- data.frame("from" = rep(outputs, each=length(outputs)),
                           "to" = rep(outputs, time=length(outputs)))
                           
xarxa2 <- hc(head(df,n=nrow(df)/2), score="bic",whitelist = df2_whitelist, blacklist = df2_blacklist)
