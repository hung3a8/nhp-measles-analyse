options(digits=3)
library(logistf)
dataset <- read.csv('out.csv')
dataset[dataset == ''] <- 'Unknown'
dataset[is.na(dataset)] <- 'Unknown'

year <- format(as.Date(dataset$admission_date, format = '%Y-%m-%d'), '%Y')
fit1 <- logistf(data = dataset, outcome_died ~ year, firth = TRUE, pl = TRUE)
summary(fit1)
cbind(exp(coef(fit1)),exp(confint(fit1)))


fit2 <- logistf(data = dataset, outcome_died ~ Female, firth = TRUE, pl = TRUE)
summary(fit2)
cbind(exp(coef(fit2)),exp(confint(fit2)))
  
fit3 <- logistf(data = dataset, outcome_died ~ Age.group..in.months., firth = TRUE, pl = TRUE)
summary(fit3)
cbind(exp(coef(fit3)),exp(confint(fit3)))

dataset$Vaccination <- as.factor(dataset$Vaccination)
fit4 <- logistf(data = dataset, outcome_died ~ Vaccination, firth = TRUE, pl = TRUE)
summary(fit4)
cbind(exp(coef(fit4)),exp(confint(fit4)))

dataset$place_of_exposure <- as.factor(dataset$place_of_exposure)
dataset$place_of_exposure <- relevel(dataset$place_of_exposure, "community")
fit5 <- logistf(data = dataset, outcome_died ~ place_of_exposure, firth = TRUE, pl = TRUE)
summary(fit5)
cbind(exp(coef(fit5)),exp(confint(fit5)))

dataset$Distance.from.the.hospital..km. <- as.factor(dataset$Distance.from.the.hospital..km.)
dataset$Distance.from.the.hospital..km. <- relevel(dataset$Distance.from.the.hospital..km., "0 - <20")
fit6 <- logistf(data = dataset, outcome_died ~ Distance.from.the.hospital..km., firth = TRUE, pl = TRUE)
summary(fit6)
cbind(exp(coef(fit6)),exp(confint(fit6)))

dataset$region_of_address <- as.factor(dataset$region_of_address)
dataset$region_of_address <- relevel(dataset$region_of_address, "Ha Noi")
fit7 <- logistf(data = dataset, outcome_died ~ region_of_address, firth = TRUE, pl = TRUE)
summary(fit7)
cbind(exp(coef(fit7)),exp(confint(fit7)))

dataset$Duration.between.onset.and.admission <- as.factor(dataset$Duration.between.onset.and.admission)
dataset$Duration.between.onset.and.admission <- relevel(dataset$Duration.between.onset.and.admission, '0 - <3')
fit8 <- logistf(data = dataset, outcome_died ~ Duration.between.onset.and.admission, firth = TRUE, pl = TRUE)
summary(fit8)
cbind(exp(coef(fit8)),exp(confint(fit8)))

dataset$Duration.of.stay.within.the.hospital <- as.factor(dataset$Duration.of.stay.within.the.hospital)
dataset$Duration.of.stay.within.the.hospital <- relevel(dataset$Duration.of.stay.within.the.hospital, '0 - <7')
fit9 <- logistf(data = dataset, outcome_died ~ Duration.of.stay.within.the.hospital, firth = TRUE, pl = TRUE)
summary(fit9)
cbind(exp(coef(fit9)),exp(confint(fit9)))

fit10 <- logistf(data = dataset, outcome_died ~ Underlying.conditions...Respiratory.system, firth = TRUE, pl = TRUE)
summary(fit10)
cbind(exp(coef(fit10)),exp(confint(fit10)))

fit11 <- logistf(data = dataset, outcome_died ~ Underlying.conditions...Cardiovascular.system, firth = TRUE, pl = TRUE)
summary(fit11)
cbind(exp(coef(fit11)),exp(confint(fit11)))

fit12 <- logistf(data = dataset, outcome_died ~ Underlying.condition...Gastrointestinal.system, firth = TRUE, pl = TRUE)
summary(fit12)
cbind(exp(coef(fit12)),exp(confint(fit12)))

fit13 <- logistf(data = dataset, outcome_died ~ Underlying.condition...Kidney.and.urology.system, firth = TRUE, pl = TRUE)
summary(fit13)
cbind(exp(coef(fit13)),exp(confint(fit13)))

fit14 <- logistf(data = dataset, outcome_died ~ Underlying.condition...Immunodeficiency, firth = TRUE, pl = TRUE)
summary(fit14)
cbind(exp(coef(fit14)),exp(confint(fit14)))

fit15 <- logistf(data = dataset, outcome_died ~ Underlying.condition...Neurological.system, firth = TRUE, pl = TRUE)
summary(fit15)
cbind(exp(coef(fit15)),exp(confint(fit15)))

fit16 <- logistf(data = dataset, outcome_died ~ Underlying.condition...Inherited.metabolic.disorders, firth = TRUE, pl = TRUE)
summary(fit16)
cbind(exp(coef(fit16)),exp(confint(fit16)))

fit17 <- logistf(data = dataset, outcome_died ~ Underlying.condition...Other.underlying.conditions, firth = TRUE, pl = TRUE)
summary(fit17)
cbind(exp(coef(fit17)),exp(confint(fit17)))

fit18 <- logistf(data = dataset, outcome_died ~ Underlying.condition...No.underlying.diseases, firth = TRUE, pl = TRUE)
summary(fit18)
cbind(exp(coef(fit18)),exp(confint(fit18)))

fit19 <- logistf(data = dataset, outcome_died ~ oxygen_cannula, firth = TRUE, pl = TRUE)
summary(fit19)
cbind(exp(coef(fit19)),exp(confint(fit19)))

fit20 <- logistf(data = dataset, outcome_died ~ dataset$CPAP, firth = TRUE, pl = TRUE)
summary(fit20)
cbind(exp(coef(fit20)),exp(confint(fit20)))

fit21 <- logistf(data = dataset, outcome_died ~ conventional_mechanical_ventilation, firth = TRUE, pl = TRUE)
summary(fit21)
cbind(exp(coef(fit21)),exp(confint(fit21)))

fit22 <- logistf(data = dataset, outcome_died ~ dataset$hfo_ventilation, firth = TRUE, pl = TRUE)
summary(fit22)
cbind(exp(coef(fit22)),exp(confint(fit22)))

fit23 <- logistf(data = dataset, outcome_died ~ dataset$ECMO, firth = TRUE, pl = TRUE)
summary(fit23)
cbind(exp(coef(fit23)),exp(confint(fit23)))


fit24 <- logistf(data = dataset, outcome_died ~ dataset$Clinical.classification, firth = TRUE, pl = TRUE)
summary(fit24)
cbind(exp(coef(fit24)),exp(confint(fit24)))

dataset$Duration.between.onset.and.test..detection.time...in.hours. <- as.factor(dataset$Duration.between.onset.and.test..detection.time...in.hours.)
dataset$Duration.between.onset.and.test..detection.time...in.hours. <- relevel(dataset$Duration.between.onset.and.test..detection.time...in.hours., '0 - <24')
fit25 <- logistf(data = dataset, outcome_died ~ Duration.between.onset.and.test..detection.time...in.hours., firth = TRUE, pl = TRUE)
summary(fit25)
cbind(exp(coef(fit25)),exp(confint(fit25)))

fit26 <- logistf(data = dataset, outcome_died ~ Measles, firth = TRUE, pl = TRUE)
summary(fit26)
cbind(exp(coef(fit26)),exp(confint(fit26)))

fit27 <- logistf(data = dataset, outcome_died ~ Pneumonia, firth = TRUE, pl = TRUE)
summary(fit27)
cbind(exp(coef(fit27)),exp(confint(fit27)))

fit28 <- logistf(data = dataset, outcome_died ~ Bronchopneumonia, firth = TRUE, pl = TRUE)
summary(fit28)
cbind(exp(coef(fit28)),exp(confint(fit28)))

fit29 <- logistf(data = dataset, outcome_died ~ Other.diagnosis, firth = TRUE, pl = TRUE)
summary(fit29)
cbind(exp(coef(fit29)),exp(confint(fit29)))

fit30 <- logistf(data = dataset, outcome_died ~ complication.gastroentiritis, firth = TRUE, pl = TRUE)
summary(fit30)
cbind(exp(coef(fit30)),exp(confint(fit30)))

fit31 <- logistf(data = dataset, outcome_died ~ dataset$complication.middle.ear.infec, firth = TRUE, pl = TRUE)
summary(fit31)
cbind(exp(coef(fit31)),exp(confint(fit31)))

fit32 <- logistf(data = dataset, outcome_died ~ dataset$complication.conjunctivitis, firth = TRUE, pl = TRUE)
summary(fit32)
cbind(exp(coef(fit32)),exp(confint(fit32)))

fit33 <- logistf(data = dataset, outcome_died ~ dataset$complication.laryngitis, firth = TRUE, pl = TRUE)
summary(fit33)
cbind(exp(coef(fit33)),exp(confint(fit33)))

fit34 <- logistf(data = dataset, outcome_died ~ dataset$complication.pneumonia.bronchitis, firth = TRUE, pl = TRUE)
summary(fit34)
cbind(exp(coef(fit34)),exp(confint(fit34)))

fit35 <- logistf(data = dataset, outcome_died ~ dataset$complication.febrile.seizures, firth = TRUE, pl = TRUE)
summary(fit35)
cbind(exp(coef(fit35)),exp(confint(fit35)))

fit36 <- logistf(data = dataset, outcome_died ~ dataset$complication.septic.shock.sepsis, firth = TRUE, pl = TRUE)
summary(fit36)
cbind(exp(coef(fit36)),exp(confint(fit36)))

fit37 <- logistf(data = dataset, outcome_died ~ dataset$co.infection.influenza.a, firth = TRUE, pl = TRUE)
summary(fit37)
cbind(exp(coef(fit37)),exp(confint(fit37)))

fit38 <- logistf(data = dataset, outcome_died ~ dataset$co.infection.influenza.b, firth = TRUE, pl = TRUE)
summary(fit38)
cbind(exp(coef(fit38)),exp(confint(fit38)))

fit39 <- logistf(data = dataset, outcome_died ~ dataset$co.infection.streptococus.aerius, firth = TRUE, pl = TRUE)
summary(fit39)
cbind(exp(coef(fit39)),exp(confint(fit39)))

fit40 <- logistf(data = dataset, outcome_died ~ dataset$co.infection.streptococus.pneumonia, firth = TRUE, pl = TRUE)
summary(fit40)
cbind(exp(coef(fit40)),exp(confint(fit40)))

fit41 <- logistf(data = dataset, outcome_died ~ dataset$respiratory_syncytical_virus, firth = TRUE, pl = TRUE)
summary(fit41)
cbind(exp(coef(fit41)),exp(confint(fit41)))

fit42 <- logistf(data = dataset, outcome_died ~ dataset$adenovirus, firth = TRUE, pl = TRUE)
summary(fit42)
cbind(exp(coef(fit42)),exp(confint(fit42)))

fit43 <- logistf(data = dataset, outcome_died ~ dataset$pertussis, firth = TRUE, pl = TRUE)
summary(fit43)
cbind(exp(coef(fit43)),exp(confint(fit43)))

fit44 <- logistf(data = dataset, outcome_died ~ dataset$healthcare_associated_infection, firth = TRUE, pl = TRUE)
summary(fit44)
cbind(exp(coef(fit44)),exp(confint(fit44)))


