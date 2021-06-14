library(readxl)
library(dplyr)
install.packages("DescTools")
library(DescTools)
dat <- read_excel("ALS_ALSFRS&호흡기검사-변환.xlsx")
dim(dat) #210 rows, 50 columns
names(dat)
dat$sex <- as.factor(ifelse(dat$성별.x=="M",1,0))
table(dat$sex)
dat[,c("Age_dx","MIP_sit(cmH2O)","MEP_sit(cmH2O)","SNIP_sit(cmH2O)","PEF_sit(L/min)","MIP:MEP ratio","FVC%predicted","FEV1%predicted","height","weight","BMI","MIP_sup(cmH2O)", "MEP_sup(cmH2O)","SNIP_sup(cmH2O)","PEF_sup(L/min)","FEV1_sit(L)","FVC_sit(L)","FEV1/FVC_sit(%)","FEV1_sup(L)","FVC_sup(L)","FEV1/FVC_sup(%)")] <- lapply(dat[,c("Age_dx","MIP_sit(cmH2O)","MEP_sit(cmH2O)","SNIP_sit(cmH2O)","PEF_sit(L/min)","MIP:MEP ratio","FVC%predicted","FEV1%predicted","height","weight","BMI","MIP_sup(cmH2O)", "MEP_sup(cmH2O)","SNIP_sup(cmH2O)","PEF_sup(L/min)","FEV1_sit(L)","FVC_sit(L)","FEV1/FVC_sit(%)","FEV1_sup(L)","FVC_sup(L)","FEV1/FVC_sup(%)")],as.numeric)
table(dat$호흡기검사일) #호흡기 검사일: 2017.10.16~2021.4.12
table(dat$ALSFRS기록일) #ALSFRS체크일: 2017.6.19~2021.4.12
table(dat$`Onset region`) #onset region: bulbar 54; cervical 80;lumbosacral 76 cases
length(unique(dat$patient_ID)) # 142 patients
dat %>% group_by(호흡기질환여부,`심질환여부(심부전 이나 허혈성심질환)`) %>% 
  summarise(n=n_distinct(patient_ID)) #호흡기질환만 있는 환자 2, 심장질환만 있는 환자 5, 호흡기질환과 심장질환이 모두 있는 환자 1명. 
boxplot(dat$'MIP_sit(cmH2O)',dat$'MIP_sup(cmH2O)',dat$'SNIP_sit(cmH2O)',dat$'SNIP_sup(cmH2O)',dat$'MEP_sit(cmH2O)',dat$'MEP_sup(cmH2O)',dat$'FVC%predicted',col=c("blue","red"),names=c("MIP_sit","MIP_sup","SNIP_sit","SNIP_sup","MEP_sit","MEP_sup","FVC%predicted"),main="호흡기능검사결과")
op <- par(no.readonly=T)
par(mfrow=c(4,2),
    mar=c(4,3,3,1),
    oma=c(0.5,0.5,2,0.5))
plot1 <- boxplot(dat$'MIP_sit(cmH2O)'~dat$ALSFRS_10,col=c(2:6),main="relation between ALSFRS_10 and MIP_sit")
plot2 <- boxplot(dat$'MIP_sup(cmH2O)'~dat$ALSFRS_10,col=c(2:6),main="relation between ALSFRS_10 and MIP_sup")
plot3 <- boxplot(dat$'SNIP_sit(cmH2O)'~dat$ALSFRS_10,col=c(2:6),main="relation between ALSFRS_10 and SNIP_sit")
plot4 <- boxplot(dat$'SNIP_sup(cmH2O)'~dat$ALSFRS_10,col=c(2:6),main="relation between ALSFRS_10 and SNIP_sup")
plot5 <- boxplot(dat$'MEP_sit(cmH2O)'~dat$ALSFRS_10,col=c(2:6),main="relation between ALSFRS_10 and MEP_sit")
plot6 <- boxplot(dat$'MEP_sup(cmH2O)'~dat$ALSFRS_10,col=c(2:6),main="relation between ALSFRS_10 and MEP_sup")
plot7 <- boxplot(dat$'FVC%predicted'~dat$ALSFRS_10,col=c(2:6),main="relation between ALSFRS_10 and FVC%predicted")
par(mfrow=c(4,2),
    mar=c(4,3,3,1),
    oma=c(0.5,0.5,2,0.5))
plot8 <- boxplot(dat$'MIP_sit(cmH2O)'~dat$ALSFRS_11,col=c(2:6),main="relation between ALSFRS_11 and MIP_sit")
plot9 <- boxplot(dat$'MIP_sup(cmH2O)'~dat$ALSFRS_11,col=c(2:6),main="relation between ALSFRS_11 and MIP_sup")
plot10 <- boxplot(dat$'SNIP_sit(cmH2O)'~dat$ALSFRS_11,col=c(2:6),main="relation between ALSFRS_11 and SNIP_sit")
plot11 <- boxplot(dat$'SNIP_sup(cmH2O)'~dat$ALSFRS_11,col=c(2:6),main="relation between ALSFRS_11 and SNIP_sup")
plot12 <- boxplot(dat$'MEP_sit(cmH2O)'~dat$ALSFRS_11,col=c(2:6),main="relation between ALSFRS_11 and MEP_sit")
plot13 <- boxplot(dat$'MEP_sup(cmH2O)'~dat$ALSFRS_11,col=c(2:6),main="relation between ALSFRS_11 and MEP_sup")
plot14 <- boxplot(dat$'FVC%predicted'~dat$ALSFRS_11,col=c(2:6),main="relation between ALSFRS_11 and FVC%predicted")
par(mfrow=c(4,2),
    mar=c(4,3,3,1),
    oma=c(0.5,0.5,2,0.5))
plot15 <- boxplot(dat$'MIP_sit(cmH2O)'~dat$ALSFRS_12,col=c(2:6),main="relation between ALSFRS_12 and MIP_sit")
plot16 <- boxplot(dat$'MIP_sup(cmH2O)'~dat$ALSFRS_12,col=c(2:6),main="relation between ALSFRS_12 and MIP_sup")
plot17 <- boxplot(dat$'SNIP_sit(cmH2O)'~dat$ALSFRS_12,col=c(2:6),main="relation between ALSFRS_12 and SNIP_sit")
plot18 <- boxplot(dat$'SNIP_sup(cmH2O)'~dat$ALSFRS_12,col=c(2:6),main="relation between ALSFRS_12 and SNIP_sup")
plot19 <- boxplot(dat$'MEP_sit(cmH2O)'~dat$ALSFRS_12,col=c(2:6),main="relation between ALSFRS_12 and MEP_sit")
plot20 <- boxplot(dat$'MEP_sup(cmH2O)'~dat$ALSFRS_12,col=c(2:6),main="relation between ALSFRS_12 and MEP_sup")
plot21 <- boxplot(dat$'FVC%predicted'~dat$ALSFRS_12,col=c(2:6),main="relation between ALSFRS_12 and FVC%predicted")
par(mfrow=c(4,2),
    mar=c(4,3,3,1),
    oma=c(0.5,0.5,2,0.5))
plot22 <- boxplot(dat$'MIP_sit(cmH2O)'~dat$`ALSFRS-r`,col=c(2:6),main="relation between ALSFRS-r and MIP_sit")
plot23 <- boxplot(dat$'MIP_sup(cmH2O)'~dat$`ALSFRS-r`,col=c(2:6),main="relation between ALSFRS-r and MIP_sup")
plot24 <- boxplot(dat$'SNIP_sit(cmH2O)'~dat$`ALSFRS-r`,col=c(2:6),main="relation between ALSFRS-r and SNIP_sit")
plot25 <- boxplot(dat$'SNIP_sup(cmH2O)'~dat$`ALSFRS-r`,col=c(2:6),main="relation between ALSFRS-r and SNIP_sup")
plot26 <- boxplot(dat$'MEP_sit(cmH2O)'~dat$`ALSFRS-r`,col=c(2:6),main="relation between ALSFRS-r and MEP_sit")
plot27 <- boxplot(dat$'MEP_sup(cmH2O)'~dat$`ALSFRS-r`,col=c(2:6),main="relation between ALSFRS-r and MEP_sup")
plot28 <- boxplot(dat$'FVC%predicted'~dat$`ALSFRS-r`,col=c(2:6),main="relation between ALSFRS-r and FVC%predicted")

number_exam <- dat %>% group_by(patient_ID) %>% 
  mutate(number_check=n())
names(number_exam)
number_exam %>% group_by(number_check) %>% summary(n=n())
left_join(dat,number_exam,by=patient_ID)
?left_join
number_exam %>% group_by(number_check) %>% 
  summarise(n=n(),
            n_distinct_exam=n_distinct(patient_ID)) # # of check: 1;95, 2;32, 3;9, 4;6
t.test(dat$`MIP_sit(cmH2O)`,dat$`MIP_sup(cmH2O)`,paired=T)
t.test(dat$`MEP_sit(cmH2O)`,dat$`MEP_sup(cmH2O)`,paired=T)
t.test(dat$`SNIP_sit(cmH2O)`,dat$`SNIP_sup(cmH2O)`,paired=T)
summary(dat)
aov1 = aov(dat$'FVC%predicted'~factor(dat$ALSFRS_12))
summary(aov1)
PostHocTest(aov1,method='hsd')
tapply(dat$`FVC%predicted`,-dat$ALSFRS_11,mean,na.rm=T)
tapply(dat$`FVC%predicted`,-dat$ALSFRS_11,sd,na.rm=T)
dat %>% group_by(ALSFRS_10) %>% summarise(n=n())
cor.test(dat$`FVC%predicted`,dat$`MIP_sit(cmH2O)`)
cor.test(dat$`FVC%predicted`,dat$`SNIP_sit(cmH2O)`)
cor.test(dat$`FVC%predicted`,dat$`MEP_sit(cmH2O)`)
cor.test(dat$`FVC%predicted`,dat$`MIP_sup(cmH2O)`,na.rm=T)
cor.test(dat$`FVC%predicted`,dat$`SNIP_sup(cmH2O)`,na.rm=T)
cor.test(dat$`FVC%predicted`,dat$`MEP_sup(cmH2O)`,na.rm=T)
par(mfrow=c(3,2),
    mar=c(4,3,3,1),
    oma=c(0.5,0.5,2,0.5))
plot(dat$`FVC%predicted`~dat$`MIP_sit(cmH2O)`)
out <- lm(dat$`FVC%predicted`~dat$`MIP_sit(cmH2O)`)
abline(out,col="red")
plot(dat$`FVC%predicted`~dat$`MIP_sup(cmH2O)`,na.rm=T)
out <- lm(dat$`FVC%predicted`~dat$`MIP_sup(cmH2O)`,na.rm=T)
abline(out,col="red")
plot(dat$`FVC%predicted`~dat$`SNIP_sit(cmH2O)`)
out <- lm(dat$`FVC%predicted`~dat$`SNIP_sit(cmH2O)`)
abline(out,col="red")
plot(dat$`FVC%predicted`~dat$`SNIP_sup(cmH2O)`,na.rm=T)
out <- lm(dat$`FVC%predicted`~dat$`SNIP_sup(cmH2O)`,na.rm=T)
abline(out,col="red")
plot(dat$`FVC%predicted`~dat$`MEP_sit(cmH2O)`)
out <- lm(dat$`FVC%predicted`~dat$`MEP_sit(cmH2O)`)
abline(out,col="red")
plot(dat$`FVC%predicted`~dat$`MEP_sup(cmH2O)`,na.rm=T)
out <- lm(dat$`FVC%predicted`~dat$`MEP_sup(cmH2O)`,na.rm=T)
abline(out,col="red")
result_glm <- glm(dat$ALSFRS_12_r~dat$Age_dx+dat$sex+dat$`MIP_sit(cmH2O)`+dat$`MIP_sup(cmH2O)`+dat$`SNIP_sit(cmH2O)`+dat$`SNIP_sup(cmH2O)`+dat$`MEP_sit(cmH2O)`+dat$`MEP_sup(cmH2O)`+dat$`FVC%predicted`,family=binomial)
dat$ALSFRS_10_r <- ifelse(dat$ALSFRS_10>2,1,0)
result_glm <- glm(dat$ALSFRS_10_r~dat$Age_dx+dat$sex+dat$`MIP_sit(cmH2O)`+dat$`SNIP_sit(cmH2O)`+dat$`MEP_sit(cmH2O)`+dat$`FVC%predicted`,family=binomial)
summary(result_glm)
reduced.model <- step(result_glm)
plot(result_glm)
fit <- glm(formula=dat$ALSFRS_10_r~dat$Age_dx+dat$sex+dat$`MIP_sit(cmH2O)`+dat$`SNIP_sit(cmH2O)`+dat$`FVC%predicted`,family=binomial)
fit.od <- glm(formula=dat$ALSFRS_10_r~dat$Age_dx+dat$sex+dat$`MIP_sit(cmH2O)`+dat$`SNIP_sit(cmH2O)`+dat$`FVC%predicted`,family=quasibinomial)
pchisq(summary(fit.od)$dispersion*fit$df.residual,fit$df.residual,lower=F)
# save.image(file="preparation.RData") #모든 작업데이터 저장
# load("preparation.RData") #이전 작업데이터 불러오기
library(moonBook)
dat[,c("ALSFRS_10","ALSFRS_11","ALSFRS_12","ALSFRS-r","ALSFRS-r bulbar")] <- lapply(dat[,c("ALSFRS_10","ALSFRS_11","ALSFRS_12","ALSFRS-r","ALSFRS-r bulbar")],as.factor)
dat$ALSFRS_10 <- factor(dat$ALSFRS_10,levels=c(0,1,2,3,4))
dat$ALSFRS_11 <- factor(dat$ALSFRS_11,levels=c(0,1,2,3,4))
dat$ALSFRS_12 <- factor(dat$ALSFRS_12,levels=c(0,1,2,3,4))
var <- c(age,height,weight,BMI,onset age,Onset region,ALSFRS-r,initial progression rate,MIP_sit,MEP_sit,SNIP_sit,PEF_sit,MIP_MEP_ratio,FVC_predicted,FEV1_predicted,심질환여부(심부전 이나 허혈성심질환),호흡기질환여부)