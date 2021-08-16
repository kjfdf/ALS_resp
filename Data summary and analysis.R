# NA값 많은것=FVC_predicted, FEV1_predicted, MIP_sup, MEP_sup,SNIP_sup,PEF_sup,FEV1_sit,FVC_sit,FEV1/FVC_sit,FEV1_sup,FVC_sup,FEV1/FVC_sup
# "성별.x"              "진단명.x"            "서식명"              "성별.y"              "생년월일.y"          "진단명.y"            "검사명"              "Study_ID"            "age."               
# [10] "onset_age"           "ALSFRS_date"         "resp_date"           "patient_ID"          "init_ALSFRS.r"       "duration"            "init_prog_rate"      "ALSFRS.r"            "ALSFRS.r.bulbar"    
# [19] "ALSFRS_10"           "ALSFRS_11"           "ALSFRS_12"           "MIP_sit"             "MEP_sit"             "SNIP_sit"            "PEF_sit"             "MIP_MEP_ratio"       "FVC_predicted"      
# [28] "FEV1_predicted"      "heart"               "lung"                "Onset"               "Dx"                  "age"                 "height"              "weight"              "BMI"                
# [37] "pneumonia"           "Onset.region"        "MIP_sup"             "MEP_sup"             "SNIP_sup"            "PEF_sup"             "FEV1_sit"            "FVC_sit"             "FEV1.FVC_sit"       
# [46] "FEV1_sup"            "FVC_sup"             "FEV1.FVC_sup"        "survival_time"       "restrictive_pattern" "delta_MIP"           "delta_MEP"           "delta_SNIP"          "delta_PEF"          
# [55] "delta_FEV1"          "delta_FVC"           "delta_FEV1.FVC"      "DaysFromDx"          "DaysFromOnset"       "DxDelay"             "CoughAssist"
# install.packages("dplyr")
# install.packages("Epi")
# install.packages("reportROC")
# install.packages("pROC")
# install.packages("generalhoslem")
# install.packages("lawstat")
# install.packages("verification")
# install.packages("ROCR")
# install.packages("readxl")
# install.packages("writexl")
# install.packages("ggplot2")
# install.packages("moonBook")
# install.packages("nnet")
# install.packages("naniar")
library(nnet)
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
library(naniar) #naniar 패키지를 불러옵니다.
complete.cases(dat)
miss_case_summary(dat) # case : 행 기준
miss_var_summary(dat) # variable : 변수 기준
vis_miss(dat)
gg_miss_var(dat)
gg_miss_upset(dat)
dat[complete.cases(dat$PEF_sup, dat$MIP_sup), ]
dat %>% mutate(pregnant=ifelse(is.na(dat),
                               mean(dat,na.rm=T),
                               pregnant))
save.image("20210809.RData")
load("20210809.RData")
dat <- read_xlsx("ALS_ALSFRS&호흡기검사-변환_init_ALSFRS_complete.xlsx")
dat <- read.csv("ALS_ALSFRS&호흡기검사-변환_init_ALSFRS_complete_addProg.csv")
write.csv(dat,"ALS_ALSFRS&호흡기검사-변환_init_ALSFRS_complete_addProg.csv")
dat[,c("sex","heart","lung","pneumonia","Onset_region","ALSFRS_12_prog1","ALSFRS_12_prog2","ALSFRS_12_prog3","ALSFRS_10_prog","ALSFRS_11_prog")] <- lapply(dat[,c("sex","heart","lung","pneumonia","Onset_region","ALSFRS_12_prog1","ALSFRS_12_prog2","ALSFRS_12_prog3","ALSFRS_10_prog","ALSFRS_11_prog")],as.factor)
dat[,c("birth_date","ALSFRS_date","resp_date","Onset","Dx")] <- lapply(dat[,c("birth_date","ALSFRS_date","resp_date","Onset","Dx")],as.Date)
dat[,c("height","weight","BMI")] <- lapply(dat[,c("height","weight","BMI")],as.numeric)
dat$ALSFRS_10 <- factor(dat$ALSFRS_10,levels=c("4","3","2","1","0"))
dat$ALSFRS_11 <- factor(dat$ALSFRS_11,levels=c("4","3","2","1","0"))
dat$ALSFRS_12 <- factor(dat$ALSFRS_12,levels=c("4","3","2","1","0"))
dat[,c("ALSFRS_10","ALSFRS_11","ALSFRS_12","Onset_region")] <- lapply(dat[,c("ALSFRS_10","ALSFRS_11","ALSFRS_12","Onset_region")],as.factor)
dim(dat) #210*61 variables
names(dat)
str(dat) 
# onset age계산=Onset-생년월일
dat <- dat %>% mutate(onset_age=as.numeric(Onset-birth_date)%/%365.25)
table(dat$onset_age)
# DxDelay=Dx-Onset (in days)
dat <- dat %>% mutate(DxDelay=as.numeric(Dx-Onset)%/%86400)
table(dat$DxDelay)
dat$DxDelay
dat <- dat %>% mutate(DxDelay_day=as.numeric(Dx-Onset))
# init_ALSFRS는 ID당 첫 ALSFRS로
dat <- dat %>% group_by(Study_ID) %>% mutate(init_ALSFRS=min(ALSFRS_r))
table(dat$init_ALSFRS)
dat$init_ALSFRS
# duration은 ALSFRS_date-Onset으로 (in days)
str(dat)
dat <- dat %>% mutate(duration=ALSFRS_date-Onset)
dat$duration
dat$duration_month
dat <- dat %>% mutate(duration_month=as.numeric(duration)%/%30)
# init_prog_rate=init_ALSFRS/duration
dat <- dat %>% mutate(init_prog_rate=init_ALSFRS/as.numeric(duration))
dat$init_prog_rate
dat <- dat %>% mutate(init_prog_rate_month=init_ALSFRS/as.numeric(duration_month))
dat$init_prog_rate_month
# date of respiratory function test DaysFromDx=resp_date-Dx (in days)
dat <- dat %>% mutate(DaysFromDx=as.numeric(resp_date-Dx))
dat$DaysFromDx
# DaysFromOnset=resp_date-Onset (in days)
dat <- dat %>% mutate(DaysFromOnset=as.numeric(resp_date-Onset))
dat$DaysFromOnset
# dat %>% summarise(IQR(ALSFRS_r))
# dat %>% summarise(first(resp_date))
# dat %>% sample_n(1:5,6)
# dat %>% sample_frac(1:5,0.2)
# dat %>% summarise_each(funs(mean,sd),age,duration)
names(dat)
mytable(ALSFRS_10~age_+onset_age+Onset_region+height+weight+BMI+ALSFRS_r+init_prog_rate_month+
          MIP_sit+SNIP_sit+MEP_sit+FVC_sit+FVC_predicted,dat,digits=2)
mytable(ALSFRS_11~age_+onset_age+Onset_region+height+weight+BMI+ALSFRS_r+init_prog_rate_month+
          MIP_sit+SNIP_sit+MEP_sit+FVC_sit+FVC_predicted,dat,digits=2)
mytable(ALSFRS_12~age_+onset_age+Onset_region+height+weight+BMI+ALSFRS_r+init_prog_rate_month+
          MIP_sit+SNIP_sit+MEP_sit+FVC_sit+FVC_predicted,dat,digits=2)
dat %>% group_by(ALSFRS_10) %>% summarise(mean_h=mean(height))
table(dat$height)
ALSFRS_10_0 <- dat %>% filter(ALSFRS_10==0)
ALSFRS_10_1 <- dat %>% filter(ALSFRS_10==1)
ALSFRS_10_2 <- dat %>% filter(ALSFRS_10==2)
ALSFRS_10_3 <- dat %>% filter(ALSFRS_10==3)
ALSFRS_10_4 <- dat %>% filter(ALSFRS_10==4)
ALSFRS_11_0 <- dat %>% filter(ALSFRS_11==0)
ALSFRS_11_1 <- dat %>% filter(ALSFRS_11==1)
ALSFRS_11_2 <- dat %>% filter(ALSFRS_11==2)
ALSFRS_11_3 <- dat %>% filter(ALSFRS_11==3)
ALSFRS_11_4 <- dat %>% filter(ALSFRS_11==4)
quantile(ALSFRS_10_0$ALSFRS_r)
quantile(ALSFRS_10_1$ALSFRS_r)
quantile(ALSFRS_10_2$ALSFRS_r)
quantile(ALSFRS_10_3$ALSFRS_r)
quantile(ALSFRS_10_4$ALSFRS_r)
quantile(ALSFRS_11_0$ALSFRS_r)
quantile(ALSFRS_11_1$ALSFRS_r)
quantile(ALSFRS_11_2$ALSFRS_r)
quantile(ALSFRS_11_3$ALSFRS_r)
quantile(ALSFRS_11_4$ALSFRS_r)
# MIP, MEP, SNIP, FVC%pred사이의 correlation analysis
cor.test(dat$MIP_sit,dat$MEP_sit,na.rm=T) #P<.001, R=0.808719 
cor.test(dat$MIP_sit,dat$SNIP_sit,na.rm=T) #P<.001, R=0.8109927  
cor.test(dat$MEP_sit,dat$SNIP_sit,na.rm=T) #P<.001, R=0.5668431   
cor.test(dat$MIP_sit,dat$FVC_predicted,na.rm=T) #P<.001, R=0.6721775    
cor.test(dat$MEP_sit,dat$FVC_predicted,na.rm=T) #P<.001, R=0.5641132    
cor.test(dat$SNIP_sit,dat$FVC_predicted,na.rm=T) #P<.001, R=0.5232711     
ggplot(dat,aes(SNIP_sit),na.rm=T)+geom_smooth(aes(y=MIP_sit),color="blue",fill="blue")+
  geom_smooth(aes(y=MEP_sit),color="red",fill="red")+
  # geom_smooth(aes(y=FVC_predicted),color="green",fill="green")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("SNIP during sitting")+ylab("other respiratory function test values")+
  theme(plot.title=element_text(hjust=0.5,size="20",color="dark blue"), axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),legend.title = element_text(size=10,face="bold"))
# ALSFRS_10(2~4/0~1), indepedent variables: age_,onset_age,Onset_region, weight,BMI, ALSFRS_r,init_prog_rate, MIP_sit, SNIP_sit, MEP_sit, FVC_sit, FVC_predicted
levels(dat$ALSFRS_10)
class(dat$MIP_sit)
dat$init_prog_rate <- as.numeric(dat$init_prog_rate)
test <- multinom(ALSFRS_10 ~ age_+onset_age+Onset_region+weight+BMI+init_prog_rate_month+MIP_sit+SNIP_sit+MEP_sit, data = dat)
test <- multinom(ALSFRS_10 ~ MEP_sit, data = dat)
summary(test)
result <- step(test)
exp(cbind(OR = coef(test), confint(test)))
summary(result)
z <- summary(result)$coefficients/summary(result)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
exp(cbind(OR = coef(result), confint(result)))
exp(coef(result))
exp(confint(result))
exp(coef(test))
exp(confint(test))
MASS::dropterm(test,trace=F,test="Chisq")
MASS::dropterm(result,trace=F,test="Chisq")
dat <- dat %>% mutate(ALSFRS_10_prog=ifelse(
  as.numeric(ALSFRS_10)<2,"1","0"))
class(dat$ALSFRS_10_prog)
dat$ALSFRS_10_prog <- as.factor(dat$ALSFRS_10_prog)
result <- glm(ALSFRS_10_prog ~ age_+onset_age+Onset_region+weight+BMI+init_prog_rate_month+MIP_sit+SNIP_sit+MEP_sit, data = dat,family=binomial)
result <- glm(ALSFRS_10_prog ~ SNIP_sit, data = dat,family=binomial)
result1 <- step(result)
extractOR(result1,digits=2)
summary(result)
extractOR(result)
exp(cbind(OR = coef(result1), confint(result1)))
summary(result1)
# ALSFRS_11(2이상/1이하), indepedent variables: weight,BMI, ALSFRS-R,initial progression rate, MIP, SNIP, MEP, FVC, FVC_predicted
test <- multinom(ALSFRS_11 ~ age_+onset_age+Onset_region+weight+BMI+init_prog_rate_month+MIP_sit+SNIP_sit+MEP_sit, data = dat)
test <- multinom(ALSFRS_11 ~ MEP_sit, data = dat)
result <- step(test)
exp(cbind(OR = coef(test), confint(test)))
exp(cbind(OR = coef(result), confint(result)))
exp(coef(result))
exp(confint(result))
MASS::dropterm(result,trace=F,test="Chisq") 
exp(coef(test))
exp(confint(test))
MASS::dropterm(test,trace=F,test="Chisq")
dat <- dat %>% mutate(ALSFRS_11_prog=ifelse(
  as.numeric(ALSFRS_11)<2,"1","0"))
class(dat$ALSFRS_11_prog)
dat$ALSFRS_11_prog <- as.factor(dat$ALSFRS_11_prog)
result <- glm(ALSFRS_11_prog ~ age_+onset_age+Onset_region+weight+BMI+init_prog_rate_month+MIP_sit+SNIP_sit+MEP_sit, data = dat,family=binomial)
result <- glm(ALSFRS_11_prog ~ MEP_sit, data = dat,family=binomial)
summary(result)
summary(result1)
result1 <- step(result)
extractOR(result)
 # ALSFRS_12(3,2,1각각)
test <- multinom(ALSFRS_12 ~ age_+onset_age+Onset_region+weight+BMI+init_prog_rate_month+MIP_sit+SNIP_sit+MEP_sit, data = dat)
test <- multinom(ALSFRS_12 ~ MEP_sit, data = dat)
result <- step(test)
exp(coef(result))
exp(confint(result))
exp(coef(test))
exp(confint(test))
MASS::dropterm(test,trace=F,test="Chisq")
MASS::dropterm(result,trace=F,test="Chisq")
dat <- dat %>% mutate(ALSFRS_12_prog3=ifelse(
  as.numeric(ALSFRS_12)<4,"1","0"))
dat <- dat %>% mutate(ALSFRS_12_prog2=ifelse(
  as.numeric(ALSFRS_12)<3,"1","0"))
dat <- dat %>% mutate(ALSFRS_12_prog1=ifelse(
  as.numeric(ALSFRS_12)<2,"1","0"))
dat %>% select(ALSFRS_12,ALSFRS_12_prog1,ALSFRS_12_prog2,ALSFRS_12_prog3)
table(dat$ALSFRS_12_prog1)
table(dat$ALSFRS_12_prog2)
table(dat$ALSFRS_12_prog3)
class(dat$ALSFRS_12_prog1)
# ALSFRS_10 2점이상/1점이하, ALSFRS_11 3점이상/2점이하, ALSFRS 3점이상/2점이하의 상태를 구분하는 SNIP,MIP,MEP의 cut off value 
result <- ROC(form=ALSFRS_10_prog~MIP_sit,data=dat,plot="Roc",PV=T)
reportROC(dat$ALSFRS_10_prog,dat$MIP_sit) #cutoff value 37.835, sensitivity 0.627, specificity 0.788, p-value <0001, AUC=0.737
result <- ROC(form=ALSFRS_10_prog~MEP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_10_prog,dat$MEP_sit) #cutoff value 30.835, sensitivity 0.755, specificity 0.556, p-value <0001, AUC=0.682
result <- ROC(form=ALSFRS_10_prog~SNIP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_10_prog,dat$SNIP_sit) #cutoff value 20.165, sensitivity 0.836, specificity 0.515 , p-value <0001, AUC=0.705

result <- ROC(form=ALSFRS_11_prog~MIP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_11_prog,dat$MIP_sit) #cutoff value 37.835, sensitivity 0.627, specificity 0.788, p-value <0001, AUC=0.737
result <- ROC(form=ALSFRS_11_prog~MEP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_11_prog,dat$MEP_sit) #cutoff value 30.835, sensitivity 0.755, specificity 0.556, p-value <0001, AUC=0.682
result <- ROC(form=ALSFRS_11_prog~SNIP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_11_prog,dat$SNIP_sit) #cutoff value 20.165, sensitivity 0.836, specificity 0.515, p-value <0001, AUC=0.705

result <- ROC(form=ALSFRS_12_prog3~MIP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_12_prog3,dat$MIP_sit) #cutoff value 15.165, sensitivity 0.780, specificity 1.000, p-value 0.008, AUC=0.853
result <- ROC(form=ALSFRS_12_prog3~MEP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_12_prog3,dat$MEP_sit) #cutoff value 46.665, sensitivity 0.361, specificity 1.000, p-value 0.766, AUC=0.605
result <- ROC(form=ALSFRS_12_prog3~SNIP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_12_prog3,dat$SNIP_sit) #cutoff value 16.835, sensitivity 0.746, specificity 1.000, p-value 0.002, AUC=0.913

result <- ROC(form=ALSFRS_12_prog2~MIP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_12_prog2,dat$MIP_sit) #cutoff value 26.165, sensitivity 0.610, specificity 1.000, p-value 0.001, AUC=0.805
result <- ROC(form=ALSFRS_12_prog2~MEP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_12_prog2,dat$MEP_sit) #cutoff value 37.165, sensitivity 0.505, specificity 0.889, p-value 0.918, AUC=0.637
result <- ROC(form=ALSFRS_12_prog2~SNIP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_12_prog2,dat$SNIP_sit) #cutoff value 16.835, sensitivity 0.760, specificity 0.889, p-value <0001, AUC=0.826

result <- ROC(form=ALSFRS_12_prog1~MIP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_12_prog1,dat$MIP_sit) #cutoff value 26.165, sensitivity 0.627, specificity 0.938, p-value <0001, AUC=0.823
result <- ROC(form=ALSFRS_12_prog1~MEP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_12_prog1,dat$MEP_sit) #cutoff value 37.165, sensitivity 0.518, specificity 0.875, p-value 0.004, AUC=0.700
result <- ROC(form=ALSFRS_12_prog1~SNIP_sit,data=dat,plot="Roc")
reportROC(dat$ALSFRS_12_prog1,dat$SNIP_sit) #cutoff value 19.165, sensitivity 0.751, specificity 0.875, p-value <0001, AUC=0.828

# survival 기간 data가 있는 환자를 기준으로 해서 survival 기간의 차이가 있는지 확인. SNIP cut-off에 따른 survival분석. 생존기간 분석 및 hazard ratio 
# SNIP cutoff value 이상인 시점에 NIV를 적용한 early NIV group vs. late NIV group의 survival분석. KM curve, hazard ratio, concordance analysis 
# ALSFRS slow,moderate,rapid progression, bulbar onset/limb onset으로 나눠서 분석 
