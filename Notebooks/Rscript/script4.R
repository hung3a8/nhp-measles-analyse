# used to run on HPC services
install.packages("logistf")
library(logistf)

dataset <- read.csv('out.csv')
dataset[dataset == ''] <- 'Unknown'
dataset[is.na(dataset)] <- 'Unknown'

dataset$Duration.between.onset.and.test..detection.time...in.hours. <- as.factor(dataset$Duration.between.onset.and.test..detection.time...in.hours.)
dataset$Duration.between.onset.and.test..detection.time...in.hours. <- relevel(dataset$Duration.between.onset.and.test..detection.time...in.hours., '0 - <24')
dataset$admission_year <- as.factor(format(as.Date(dataset$admission_date), "%Y"))
dataset$region_of_address <- as.factor(dataset$region_of_address)

dataset$New.vaccination <- as.factor(dataset$New.vaccination)
dataset$New.vaccination <- relevel(dataset$New.vaccination, "0")
dataset$Distance.from.the.hospital..km. <- as.factor(dataset$Distance.from.the.hospital..km.)
dataset$Distance.from.the.hospital..km. <- relevel(dataset$Distance.from.the.hospital..km., "0 - <20")
dataset$Duration.between.onset.and.admission <- as.factor(dataset$Duration.between.onset.and.admission)
dataset$Duration.between.onset.and.admission <- relevel(dataset$Duration.between.onset.and.admission, '0 - <3')
dataset$Duration.of.stay.within.the.hospital <- as.factor(dataset$Duration.of.stay.within.the.hospital)
dataset$Duration.of.stay.within.the.hospital <- relevel(dataset$Duration.of.stay.within.the.hospital, '0 - <7')
dataset$New.Age.group..in.months. <- as.factor(dataset$New.Age.group..in.months.)
dataset$New.Age.group..in.months. <- relevel(dataset$New.Age.group..in.months., '>= 60')

dataset$place_of_exposure <- as.factor(dataset$place_of_exposure)
dataset$place_of_exposure <- relevel(dataset$place_of_exposure, 'NHP')

dataset$admission_years <- as.factor(dataset$admission_years)
dataset$admission_years <- relevel(dataset$admission_years, '2017')

fit <- logistf(data = dataset, dataset$outcome_died ~ 
                 dataset$Female 
               + dataset$admission_years
               + dataset$New.Age.group..in.months.
               + dataset$New.vaccination 
               + dataset$place_of_exposure 
               #+ dataset$region_of_address
               + dataset$Distance.from.the.hospital..km.
               #+ dataset$Duration.between.onset.and.admission
               #+ dataset$Duration.between.onset.and.test..detection.time...in.hours.
               + dataset$Duration.of.stay.within.the.hospital
               + dataset$Underlying.conditions...Respiratory.system
               + dataset$Underlying.conditions...Cardiovascular.system
               + dataset$Underlying.condition...Gastrointestinal.system
               + dataset$Underlying.condition...Kidney.and.urology.system
               + dataset$Underlying.condition...Immunodeficiency
               + dataset$Underlying.condition...Neurological.system
               + dataset$Underlying.condition...Inherited.metabolic.disorders
               + dataset$Underlying.condition...Other.underlying.conditions
               + dataset$Underlying.condition...No.underlying.diseases
               + dataset$Measles
               + dataset$Pneumonia
               + dataset$Bronchopneumonia
               + dataset$Other.diagnosis
               #  + dataset$complication.conjunctivitis
               #  + dataset$complication.gastroentiritis
               #  + dataset$complication.middle.ear.infec
               # + dataset$complication.laryngitis
               # + dataset$complication.pneumonia.bronchitis
               # + dataset$complication.febrile.seizures
               # + dataset$complication.septic.shock.sepsis
               + dataset$healthcare_associated_infection
               + dataset$respiratory_syncytical_virus
               + dataset$adenovirus
               + dataset$pertussis
               ,firth = TRUE, pl = TRUE)
summary(fit)
cbind(exp(coef(fit)),exp(confint(fit)))
drop1(fit, test="F")
