if(!is.null(dev.list())) dev.off()
cat("\014") 
rm(list=ls())

setwd("~/Downloads/MMA 2020/Datathon 2020 data")

beneficiary <- read.csv("beneficiary.csv", header = TRUE)
inpatients <- read.csv("inpatients.csv", header = TRUE)
outpatients <- read.csv("outpatients.csv", header = TRUE)
providers <- read.csv("providers.csv", header = TRUE)

# Data pre-processing

###################
### beneficiary ###
###################

library(lubridate)

# I.calculate each beneficiary age
beneficiary$DOB <- ymd(beneficiary$DOB)
beneficiary$DOD <- ymd(beneficiary$DOD)
beneficiary$Age <- as.integer(round((beneficiary$DOD - beneficiary$DOB)/365,0))

# As the last DOD is '2009-12-01', which means beneficiary details is of year 2009
# so we calculate age of other beneficiaries as of year 2009
beneficiaryDate <- as.Date("2009-12-01","%Y-%m-%d")
beneficiary$Age <- ifelse(!is.na(beneficiary$Age),beneficiary$Age,
                          round((beneficiaryDate - beneficiary$DOB)/365,0))
# II.RenalDisease
beneficiary$RenalDisease <- as.integer(ifelse(beneficiary$RenalDisease == '0', '0','1'))

# III.Chronic_X
Chronic <- grep("^Chronic_", names(beneficiary), value = TRUE)
beneficiary <- ifelse(beneficiary[,Chronic] == 2,1,0)

# IV.Death Flag
beneficiary$DeadFlag <- as.integer(ifelse(!is.na(beneficiary$DOD),1,0))

# We can remove DOD and DOB for further analysis since we had use them to compute beneficiary's age and dead-flag

##################
### inpatients ###
##################

str(inpatients)

# I.calculate date period for claim
inpatients$ClaimDays <- as.integer(ymd(inpatients$EndDt) - ymd(inpatients$StartDt))

# II.calculate how many days each patient has been stayed in the hospital
inpatients$AdmissionDt <- ymd(inpatients$AdmissionDt)
inpatients$DischargeDt <- ymd(inpatients$DischargeDt)
inpatients$Admitdays <- as.integer(inpatients$DischargeDt - inpatients$AdmissionDt + 1)
head(inpatients)

colSums(is.na(inpatients))
# Count number of Diagnosis and Procedure
# Since DiagnosisCode_X and ProcedureCode_X are codes, it's hard to fill-in with NA.
# However, we can count number of Diagnosis and Procedure each inpatient beneficiary has
library(dplyr)

# III.Number of Diagnosis

Diagnosis <- grep("^DiagnosisCode_", names(inpatients), value = TRUE)
inpatientsNumberOfDiagnosis1or0 <- function(df){
  df[,Diagnosis] <- lapply(df[,Diagnosis], function(x) as.integer(ifelse(is.na(x),0,1)))
  return(df)
}
inpatients_Diagnosis <- inpatientsNumberOfDiagnosis1or0(inpatients)
inpatients$NumberOfDiagnosis <- inpatients_Diagnosis %>% select(Diagnosis) %>% rowSums
inpatients_Diagnosis <- NULL

# IV.Number of Procedures
Procedure <- grep("^ProcedureCode_", names(inpatients), value = TRUE)
inpatientsNumberofProcedure1or0 <- function(df){
  df[,Procedure] <- lapply(df[,Procedure], function(x) as.integer(ifelse(is.na(x),0,1)))
  return(df)
}
inpatients_Procedure <- inpatientsNumberofProcedure1or0(inpatients)
inpatients$NumberOfProcedures <- inpatients_Procedure %>% select(Procedure) %>% rowSums
inpatients_Procedure <- NULL

# V.Number of Physicians
Physicians <- c("AttendingPhysician","OperatingPhysician","OtherPhysician")
NumberOfPhysicians1or0 <- function(df){
  df[,Physicians] <- lapply(df[,Physicians], function(x) as.integer(ifelse(is.na(x),0,1)))
  return(df)
}
inpatients_Physicians <- NumberOfPhysicians1or0(inpatients)
inpatients$NumberOfPhysicians <- inpatients_Physicians %>% select(Physicians) %>% rowSums
inpatients_Physicians <- NULL

# VI. Number of BID and CID under each PID
inpatients_PIDBID <- inpatients %>% select(BID,PID) %>% group_by(PID) %>% summarise(NumberOfBID = n())
inpatients <- merge(inpatients,inpatients_PIDBID,by = "PID")
inpatients_PIDBID <- NULL
inpatients_PIDCID <- inpatients %>% select(CID,PID) %>% group_by(PID) %>% summarise(NumberOfCID = n())
inpatients <- merge(inpatients,inpatients_PIDCID, by = "PID")
inpatients_PIDCID <- NULL

# VII. Number of distinct BID and CID under each PID and (BID - distinct BID) & (CID - distinct CID)
inpatients_distinctPIDBID <- inpatients %>% select(BID,PID) %>% group_by(PID) %>% summarise(NumberOfDistinctBID = n_distinct(BID))
inpatients <- merge(inpatients,inpatients_distinctPIDBID,by = "PID")
inpatients_distinctPIDBID <- NULL
inpatients_distinctPIDCID <- inpatients %>% select(CID,PID) %>% group_by(PID) %>% summarise(NumberOfDistinctCID = n_distinct(CID))
inpatients <- merge(inpatients,inpatients_distinctPIDCID,by = "PID")
inpatients_distinctPIDCID <- NULL

inpatients$`Number of BID - Number of DisitnctBID` <- inpatients$NumberOfBID - inpatients$NumberOfDistinctBID
inpatients$`Number of CID - Number of DisitnctCID` <- inpatients$NumberOfCID - inpatients$NumberOfDistinctCID

# check inpatient data
head(inpatients)

###################
### outpatients ###
###################

# I.calculate date period for claim
outpatients$ClaimDays <- as.integer(ymd(outpatients$EndDt) - ymd(outpatients$StartDt))

# II. assign number of 0 to admit days to outpatients
outpatients$Admitdays <- 0

# III.Diagnosis
Diagnosis2 <- grep("^DiagnosisCode_", names(outpatients), value = TRUE)
outpatientsNumberOfDiagnosis1or0 <- function(df){
  df[,Diagnosis2] <- lapply(df[,Diagnosis2], function(x) as.integer(ifelse(is.na(x),0,1)))
  return(df)
}
outpatients_Diagnosis <- outpatientsNumberOfDiagnosis1or0(outpatients)
outpatients$NumberOfDiagnosis <- outpatients_Diagnosis %>% select(Diagnosis2) %>% rowSums
outpatients_Diagnosis <- NULL

# IV.Procedure
Procedure2 <- grep("^ProcedureCode_", names(outpatients), value = TRUE)
outpatientsNumberofProcedure1or0 <- function(df){
  df[,Procedure2] <- lapply(df[,Procedure2], function(x) as.integer(ifelse(is.na(x),0,1)))
  return(df)
}
outpatients_Procedure <- outpatientsNumberofProcedure1or0(outpatients)
outpatients$NumberOfProcedures <- outpatients_Procedure %>% select(Procedure2) %>% rowSums
outpatients_Procedure <- NULL

# V.Number of Physicians
outpatients_Physicians <- NumberOfPhysicians1or0(outpatients)
outpatients$NumberOfPhysicians <- outpatients_Physicians %>% select(Physicians) %>% rowSums
outpatients_Physicians <- NULL

# VI. Number of BID and CID under each PID
outpatients_PIDBID <- outpatients %>% select(BID,PID) %>% group_by(PID) %>% summarise(NumberOfBID = n())
outpatients <- merge(outpatients, outpatients_PIDBID,by = "PID")
outpatients_PIDBID <- NULL
outpatients_PIDCID <- outpatients %>% select(CID,PID) %>% group_by(PID) %>% summarise(NumberOfCID = n())
outpatients <- merge(outpatients, outpatients_PIDCID,by = "PID")
outpatients_PIDCID <- NULL

# VII. Number of distinct BID and CID under each PID and (BID - distinct BID) & (CID - distinct CID)
outpatients_distinctPIDBID <- outpatients %>% select(BID,PID) %>% group_by(PID) %>% summarise(NumberOfDistinctBID = n_distinct(BID))
outpatients <- merge(outpatients,outpatients_distinctPIDBID,by = "PID")
outpatients_distinctPIDBID <- NULL
outpatients_distinctPIDCID <- outpatients %>% select(CID,PID) %>% group_by(PID) %>% summarise(NumberOfDistinctCID = n_distinct(CID))
outpatients <- merge(outpatients,outpatients_distinctPIDCID,by = "PID")
outpatients_distinctPIDCID <- NULL

outpatients$`Number of BID - Number of DisitnctBID` <- outpatients$NumberOfBID - outpatients$NumberOfDistinctBID
outpatients$`Number of CID - Number of DisitnctCID` <- outpatients$NumberOfCID - outpatients$NumberOfDistinctCID

# VIII. DiagnosisGroupCode
outpatients$DiagnosisGroupCode = as.factor(NA)

# check outpatient data
head(outpatients)

###############################
### Check Data Before Merge ###
###############################

print(paste("dimensionality of inpatients dataset: " ,dim(inpatients)[1], "observations", dim(inpatients)[2], "features"))
print(paste("dimensionality of outpatients dataset: " ,dim(outpatients)[1], "observations", dim(outpatients)[2], "features"))

# Inpatient dataset (surplus): AdmissionDt, DischargeDt, DiagnosisGroupCode 
setdiff(inpatients,outpatients)

# Inpatient Admit days range from 1 to 36, outpatient equals 0
print(paste("Minimum days of admit: ", min(inpatients$Admitdays)));print(paste("Maximum days of admit: ", max(inpatients$Admitdays)))

# final conversion and dropout before output
inpatients$AdmissionDt <- NULL 
inpatients$DischargeDt <- NULL
setdiff(inpatients, outpatients)
inpatients$ProcedureCode_6 <- as.integer(inpatients$ProcedureCode_6)
outpatients$ProcedureCode_5 <- as.integer(outpatients$ProcedureCode_5)
outpatients$ProcedureCode_6 <- as.integer(outpatients$ProcedureCode_6)

# write.csv(inpatients,file="/Users/oliverdong/Downloads/MMA 2020/cleaned data/inpatients.csv")
# write.csv(outpatients,file="/Users/oliverdong/Downloads/MMA 2020/cleaned data/outpatients.csv")
# write.csv(beneficiary,file="/Users/oliverdong/Downloads/MMA 2020/cleaned data/beneficiary.csv")

#######################
### Join/Merge Data ###
#######################

# Union inpatient and outpatient data
if(!is.null(dev.list())) dev.off()
cat("\014") 
rm(list=ls())
setwd("~/Downloads/MMA 2020/cleaned data")
beneficiary <- read.csv("beneficiary.csv", header = TRUE)
inpatients <- read.csv("inpatients.csv", header = TRUE)
outpatients <- read.csv("outpatients.csv", header = TRUE)
providers <- read.csv("providers.csv", header = TRUE)

inpatients$X <- NULL
outpatients$X <- NULL
beneficiary$X <- NULL

# join full patient data from inpatient, outpatient and provider table
fullPatientData <- rbind(inpatients, outpatients)
fullPatientData <- inner_join(fullPatientData, providers, by = 'PID')
# join beneficiary table with BID
fullPatientData2 <- inner_join(fullPatientData,beneficiary, by = "BID")


## average coverage for both part A and B under each distinct PID
meanMonths_PartACov <- fullPatientData2 %>% select(PID, NumOfMonths_PartACov) %>% group_by(PID) %>% summarise(meanMonths_PartACov = mean(NumOfMonths_PartACov))
meanMonths_PartBCov <- fullPatientData2 %>% select(PID, NumOfMonths_PartBCov) %>% group_by(PID) %>% summarise(meanMonths_PartBCov = mean(NumOfMonths_PartBCov))
fullPatientData2 <- merge(fullPatientData2, meanMonths_PartACov, by ="PID")
fullPatientData2 <- merge(fullPatientData2, meanMonths_PartBCov, by ="PID")
meanMonths_PartACov <- NULL
meanMonths_PartBCov <- NULL

# save full data
# write.csv(fullPatientData2,file="/Users/oliverdong/Downloads/MMA 2020/cleaned data/fullPatientData.csv")


###################################
### Feature Engineer for Model  ###
###################################

if(!is.null(dev.list())) dev.off()
cat("\014") 
rm(list=ls())
setwd("~/Downloads/MMA 2020/cleaned data")
fullPatientData <- read.csv("fullPatientData.csv", header = TRUE)

# 1. duplicate fullpatientdata to fullpatientdata2
fullPatientData2 <- fullPatientData
fullPatientData2$DiagnosisGroupCode <- ifelse(is.na(fullPatientData2$DiagnosisGroupCode),0,1)

# 2.convert all numeric features to avergae of PID level in patients data
library(dplyr)
data1 <- fullPatientData2 %>% select(PID, Fraud, Number.of.BID...Number.of.DisitnctBID, Number.of.CID...Number.of.DisitnctCID
                                    ,ClaimDays, Admitdays
                                    ,NumberOfDiagnosis, NumberOfProcedures, NumberOfPhysicians
                                    ,AmtReimbursed, DeductibleAmt, DiagnosisGroupCode) %>% group_by(PID,Fraud) %>% 
  summarise(# AvgNumofBID = mean(NumberOfBID),
            # AvgNumofCID = mean(NumberOfCID),
            `AvgNumofCID - AvgNumofDistinctCID` = mean(Number.of.CID...Number.of.DisitnctCID),
            `AvgNumofBID - AvgNumofDistinctBID` = mean(Number.of.BID...Number.of.DisitnctBID),
            AvgClaimDays = mean(ClaimDays),
            AvgAdmitDays = mean(Admitdays),
            AvgNumofDiagnosis = mean(NumberOfDiagnosis),
            AvgNumofProcedures = mean(NumberOfProcedures),
            AvgNumofPhysicians = mean(NumberOfPhysicians),
            AvgAmtReimbursed = mean(AmtReimbursed),
            AvgAmtDeductible = mean(DeductibleAmt)
            # AvgDiagnosisGroupCode = mean(DiagnosisGroupCode)
            )
data1 <- as.data.frame(data1)

# 3.convert all numeric features to avergae of PID level in beneficiary data
data2 <- fullPatientData2 %>% select(PID, Fraud, Age, NumOfMonths_PartACov, NumOfMonths_PartBCov
                                     ,InpatientAnnualReimbursementAmt
                                     ,InpatientAnnualDeductibleAmt
                                     ,OutpatientAnnualReimbursementAmt
                                     ,OutpatientAnnualDeductibleAmt) %>% group_by(PID,Fraud) %>%
  summarise(AvgAge = mean(Age),
            AvgNumOfMonths_PartACov = mean(NumOfMonths_PartACov),
            AvgNumOfMonths_PartBCov = mean(NumOfMonths_PartBCov),
            AvgInpatientAnnualReimbursementAmt = mean(InpatientAnnualReimbursementAmt),
            AvgInpatientAnnualDeductibleAmt = mean(InpatientAnnualDeductibleAmt),
            AvgOutpatientAnnualReimbursementAmt = mean(OutpatientAnnualReimbursementAmt),
            AvgOutpatientAnnualDeductibleAmt = mean(OutpatientAnnualDeductibleAmt))
data2 <- as.data.frame(data2)

# 4. convert all the following features to sum for each distinct PID
# Renaldisease (46), Chronic_X (51:61), deadflag (67)
selectcolumns <- c(2,46,51:61,67)
data3 <- fullPatientData2[,selectcolumns]
data3 <- data3 %>% mutate_if(is.integer, as.integer) %>% 
  mutate(NumOfChronic = select(.,contains("Chronic_")) %>% rowSums()) %>%
  select(PID, RenalDisease, DeadFlag, NumOfChronic) %>% 
  group_by(PID) %>%
  summarise(SumRenalDisease = sum(RenalDisease),
            SumDeadFlag = sum(DeadFlag),
            SumNumOfChronic = sum(NumOfChronic))
data3$SumNumOfChronic <- data3$SumNumOfChronic/2

# These ordinal vaiable contains high dimensionlity will be analysis further in EDA:
# DiagnosisGroupCode, Gender, Race, State, County

# 5. merge table (data1:data3)

# merge feature engineered patient data and beneficiary data
model_data <- merge(data1,data2,by="PID")
# remove duplicate fraud column and adjust column name
model_data$Fraud.x <- NULL
names(model_data)[11] <- "Fraud"

# merge patient data with other categorical features
model_data <- merge(model_data, data3, by = "PID")

# remove "PID"
model_data$PID <- NULL
# model_data$`AvgNumofCID - AvgNumofDistinctCID` <- NULL

### rename columns
names(model_data)[1] <- "AvgNumofCID_minus_AvgNumofDistinctCID"
names(model_data)[2] <- "AvgNumofBID_minus_AvgNumofDistinctBID"

# write.csv(model_data,file="/Users/oliverdong/Downloads/MMA 2020/cleaned data/model_data.csv")

# correlation matrix plot
model_data2 <- model_data
model_data2$Fraud <- as.numeric(ifelse(model_data$Fraud == "No",0,1))
model_data2$AvgNumofCID_minus_AvgNumofDistinctCID <- NULL
corr <- cor(model_data2,use = "complete.obs")

library(corrplot)
corrplot(corr, 
         # adjust visualization method, display spot and character ordering
         method="color", type = "upper", order = "hclust",
         # add correlation coefficient and adjust text size
         addCoef.col = "black",number.cex = .6,
         # adjust color of corr.coef text
         tl.col = "black",
         diag = FALSE)


# random forest model
split_70percentage <- (.7)
trainingRowIndex_70pct <- sample(1:nrow(model_data), (split_70percentage)*nrow(model_data))
rf_train_70pct <- model_data[trainingRowIndex_70pct, ]
rf_test_30pct <- model_data[-trainingRowIndex_70pct, ]

library(randomForest)
rf_fit <- randomForest(Fraud~., data = rf_train_70pct, ntree = 100, importance = TRUE,na.action = na.exclude)

varImpPlot(rf_fit)

importance(rf_fit)

y_pred = predict(rf_fit, newdata = rf_test_30pct, type = "response")
library(caret)
caret::confusionMatrix(y_pred,rf_test_30pct$Fraud)
