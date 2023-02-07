dataset <- read.csv('/Users/vuhoangnguyen/Repo/nhp-measles-analyse/Notebooks/vacc_out.csv')
dataset[dataset == ''] <- 'Unknown'
dataset[is.na(dataset)] <- 'Unknown'

dataset$Distance.from.the.hospital..km. <- as.factor(dataset$Distance.from.the.hospital..km.)
dataset$Distance.from.the.hospital..km. <- relevel(dataset$Distance.from.the.hospital..km., "0 - <20")
dataset$Duration.between.onset.and.admission <- as.factor(dataset$Duration.between.onset.and.admission)
dataset$Duration.between.onset.and.admission <- relevel(dataset$Duration.between.onset.and.admission, '0 - <3')
dataset$Duration.of.stay.within.the.hospital <- as.factor(dataset$Duration.of.stay.within.the.hospital)
dataset$Duration.of.stay.within.the.hospital <- relevel(dataset$Duration.of.stay.within.the.hospital, '0 - <7')

fit <- logistf(data = dataset, outcome_died ~ dataset$Female 
               + dataset$Age.group..in.months. 
               + dataset$New.vaccination 
               + dataset$place_of_exposure 
               #+ dataset$Distance.from.the.hospital..km. 
               + dataset$Duration.between.onset.and.admission
               #+ dataset$Duration.of.stay.within.the.hospital
               #+ dataset$Underlying.conditions...Respiratory.system 
               #+ dataset$Underlying.conditions...Cardiovascular.system 
               #+ dataset$Underlying.condition...Gastrointestinal.system
               #+ dataset$Underlying.condition...Kidney.and.urology.system
               #+ dataset$Underlying.condition...Immunodeficiency
               #+ dataset$Underlying.condition...Neurological.system
               #+ dataset$Underlying.condition...Inherited.metabolic.disorders
               #+ dataset$Underlying.condition...Other.underlying.conditions
               #+ dataset$Underlying.condition...No.underlying.diseases
               #+ dataset$Dianogis.of.admission
               #+ dataset$complication.conjunctivitis
               #+ dataset$complication.gastroentiritis
               #+ dataset$complication.middle.ear.infec
               #+ dataset$complication.pneumonia.bronchitis
               #+ dataset$complication.febrile.seizures
               #+ dataset$complication.septic.shock.sepsis
               #+ dataset$healthcare_associated_infection
               #+ dataset$respiratory_syncytical_virus
               #+ dataset$adenovirus
               #+ dataset$pertussis,
               ,firth = TRUE, pl = TRUE)
summary(fit)
cbind(exp(coef(fit)),exp(confint(fit)))
drop1(fit, test="F")

data(sex2)
fit<-logistf(case ~ age+oc+vic+vicl+vis+dia, data=sex2)
summary(fit)
nobs(fit)
drop1(fit)
plot(profile(fit,variable="dia"))
extractAIC(fit)
fit1<-update(fit, case ~ age+oc+vic+vicl+vis)
extractAIC(fit1)
anova(fit,fit1)
data(sexagg)
fit2<-logistf(case ~ age+oc+vic+vicl+vis+dia, data=sexagg, weights=COUNT)
summary(fit2)
# simulated SNP example
set.seed(72341)
snpdata<-rbind(
  matrix(rbinom(2000,2,runif(2000)*0.3),100,20),
  matrix(rbinom(2000,2,runif(2000)*0.5),100,20))
colnames(snpdata)<-paste("SNP",1:20,"_",sep="")
snpdata<-as.data.frame(snpdata)
for(i in 1:20) snpdata[,i]<-as.factor(snpdata[,i])
snpdata$case<-c(rep(0,100),rep(1,100))
fitsnp<-logistf(data=snpdata, formula=case~1, pl=FALSE)
add1(fitsnp, scope=paste("SNP",1:20,"_",sep=""))
fitf<-forward(fitsnp, scope = paste("SNP",1:20,"_",sep=""))
fitf
