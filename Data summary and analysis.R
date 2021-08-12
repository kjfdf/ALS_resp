# NA값 많은것=FVC_predicted, FEV1_predicted, MIP_sup, MEP_sup,SNIP_sup,PEF_sup,FEV1_sit,FVC_sit,FEV1/FVC_sit,FEV1_sup,FVC_sup,FEV1/FVC_sup
# "성별.x"              "진단명.x"            "서식명"              "성별.y"              "생년월일.y"          "진단명.y"            "검사명"              "Study_ID"            "age."               
# [10] "onset_age"           "ALSFRS_date"         "resp_date"           "patient_ID"          "init_ALSFRS.r"       "duration"            "init_prog_rate"      "ALSFRS.r"            "ALSFRS.r.bulbar"    
# [19] "ALSFRS_10"           "ALSFRS_11"           "ALSFRS_12"           "MIP_sit"             "MEP_sit"             "SNIP_sit"            "PEF_sit"             "MIP_MEP_ratio"       "FVC_predicted"      
# [28] "FEV1_predicted"      "heart"               "lung"                "Onset"               "Dx"                  "age"                 "height"              "weight"              "BMI"                
# [37] "pneumonia"           "Onset.region"        "MIP_sup"             "MEP_sup"             "SNIP_sup"            "PEF_sup"             "FEV1_sit"            "FVC_sit"             "FEV1.FVC_sit"       
# [46] "FEV1_sup"            "FVC_sup"             "FEV1.FVC_sup"        "survival_time"       "restrictive_pattern" "delta_MIP"           "delta_MEP"           "delta_SNIP"          "delta_PEF"          
# [55] "delta_FEV1"          "delta_FVC"           "delta_FEV1.FVC"      "DaysFromDx"          "DaysFromOnset"       "DxDelay"             "CoughAssist"
install.packages("dplyr")
install.packages("Epi")
install.packages("reportROC")
install.packages("pROC")
install.packages("generalhoslem")
install.packages("lawstat")
install.packages("verification")
install.packages("ROCR")
install.packages("readxl")
install.packages("writexl")
install.packages("ggplot2")
install.packages("moonBook")
library(Epi)
library(reportROC)
library(pROC)
library(generalhoslem)
library(lawstat)
library(verification)
library(ROCR)
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(moonBook)
save.image("20210809.RData")
load("20210809.RData")
dat <- read_xlsx("ALS_ALSFRS&호흡기검사-변환_init_ALSFRS.xlsx")
dim(dat) #210*61 variables
names(dat)
# onset age계산=Onset-생년월일
dat <- dat %>% mutate(onset_age=as.numeric(Onset-생년월일.y)%/%365.25)
table(dat$onset_age)
# DxDelay=Dx-Onset (in days)
dat <- dat %>% mutate(DxDelay=as.numeric(Dx-Onset)%/%86400)
table(dat$DxDelay)
dat$DxDelay
dat$DxDelay_month
dat <- dat %>% mutate(DxDelay_month=as.numeric(Dx-Onset)%/%2592000)
# init_ALSFRS는 ID당 첫 ALSFRS로
dat <- dat %>% group_by(Study_ID) %>% mutate(init_ALSFRS=min(ALSFRS_r))
table(dat$init_ALSFRS)
dat$init_ALSFRS
write_xlsx(dat,"ALS_ALSFRS&호흡기검사-변환_add.xlsx")
# duration은 ALSFRS_date-Onset으로 (in days)
dat <- dat %>% mutate(duration=as.numeric(ALSFRS_date-Onset))
dat$duration
dat <- dat %>% mutate(duration_month=duration%/%30)
# init_prog_rate=init_ALSFRS/duration
dat <- dat %>% mutate(init_prog_rate=init_ALSFRS/duration)
dat$init_prog_rate
dat <- dat %>% mutate(init_prog_rate_month=init_prog_rate * 30)
dat$init_prog_rate_month
# DaysFromDx=resp_date-Dx (in days)
dat <- dat %>% mutate(DaysFromDx=as.numeric(resp_date-Dx))
# DaysFromOnset=resp_date-Onset (in days)
dat <- dat %>% mutate(DaysFromOnset=as.numeric(resp_date-Onset))
# dat %>% summarise(IQR(ALSFRS_r))
# dat %>% summarise(first(resp_date))
# dat %>% sample_n(1:5,6)
# dat %>% sample_frac(1:5,0.2)
# dat %>% summarise_each(funs(mean,sd),age,duration)
names(dat)
mytable(ALSFRS_10~age_+onset_age+Onset_region+height+weight+BMI+ALSFRS_r+init_prog_rate_month+
          MIP_sit+SNIP_sit+MEP_sit+FVC_sit+FVC_predicted,dat,digits=2)
