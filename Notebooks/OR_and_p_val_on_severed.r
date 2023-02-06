options(digits=3)

# Get data from dataset
dataset <- read.csv("Notebooks/severe_out.csv")

# Replace NA with Unknown
dataset[is.na(dataset)] <- "Unknown"

summary(dataset)

# Automate function
automate <- function(var_name) {
    fit <- logistf::logistf(
        outcome_died ~ var_name,
        data=dataset, firth=TRUE, pl=TRUE)
    sum <- summary(fit)
    confint <- exp(confint(fit))
    coef <- exp(coef(fit))
    print(cbind(confint, coef))
}

# By sex
fit_sex <- logistf::logistf(
    outcome_died ~ Female,
    data=dataset, firth=TRUE, pl=TRUE)

sum_sex <- summary(fit_sex)
confint_sex <- exp(confint(fit_sex))
coef_sex <- exp(coef(fit_sex))
print(cbind(confint_sex, coef_sex))

# By year
dataset$admission_year <- format(as.Date(dataset$admission_date), "%Y")

fit_year <- logistf::logistf(
    outcome_died ~ admission_year,
    data=dataset, firth=TRUE, pl=TRUE)

sum_year <- summary(fit_year)
confint_year <- exp(confint(fit_year))
coef_year <- exp(coef(fit_year))
print(cbind(confint_year, coef_year))

# By age group
fit_age_group <- logistf::logistf(
    outcome_died ~ Age.group..in.months.,
    data=dataset, firth=TRUE, pl=TRUE)

sum_age_group <- summary(fit_age_group)
confint_age_group <- exp(confint(fit_age_group))
coef_age_group <- exp(coef(fit_age_group))
print(cbind(confint_age_group, coef_age_group))

# Vaccination status
dataset$Vaccination <- as.factor(dataset$Vaccination)
fit_vacc <- logistf::logistf(
    outcome_died ~ Vaccination,
    data=dataset, firth=TRUE, pl=TRUE)

sum_vacc <- summary(fit_vacc)
confint_vacc <- exp(confint(fit_vacc))
coef_vacc <- exp(coef(fit_vacc))
print(cbind(confint_vacc, coef_vacc))

# By place of exposure
dataset$place_of_exposure <- as.factor(dataset$place_of_exposure)
dataset$place_of_exposure <- relevel(dataset$place_of_exposure, ref="community")

fit_place_of_exposure <- logistf::logistf(
    outcome_died ~ place_of_exposure,
    data=dataset, firth=TRUE, pl=TRUE)

sum_place_of_exposure <- summary(fit_place_of_exposure)
confint_place_of_exposure <- exp(confint(fit_place_of_exposure))
coef_place_of_exposure <- exp(coef(fit_place_of_exposure))
print(cbind(confint_place_of_exposure, coef_place_of_exposure))

# By distance from the hospital (km)
dataset$Distance.from.the.hospital..km. <- as.factor(dataset$Distance.from.the.hospital..km.)
dataset$Distance.from.the.hospital..km. <- relevel(dataset$Distance.from.the.hospital..km., ref="0 - <20")
fit_distance <- logistf::logistf(
    outcome_died ~ Distance.from.the.hospital..km.,
    data=dataset, firth=TRUE, pl=TRUE)

sum_distance <- summary(fit_distance)
confint_distance <- exp(confint(fit_distance))
coef_distance <- exp(coef(fit_distance))
print(cbind(confint_distance, coef_distance))

# By region of address
dataset$region_of_address <- as.factor(dataset$region_of_address)
dataset$region_of_address <- relevel(dataset$region_of_address, "Ha Noi")
fit_region <- logistf::logistf(
    outcome_died ~ region_of_address,
    data=dataset, firth=TRUE, pl=TRUE)

sum_region <- summary(fit_region)
confint_region <- exp(confint(fit_region))
coef_region <- exp(coef(fit_region))
print(cbind(confint_region, coef_region))

# By duration between onset and admission
dataset$Duration.between.onset.and.admission <- as.factor(dataset$Duration.between.onset.and.admission)
dataset$Duration.between.onset.and.admission <- relevel(dataset$Duration.between.onset.and.admission, ref="0 - <3")
fit_duration <- logistf::logistf(
    outcome_died ~ Duration.between.onset.and.admission,
    data=dataset, firth=TRUE, pl=TRUE)

sum_duration <- summary(fit_duration)
confint_duration <- exp(confint(fit_duration))
coef_duration <- exp(coef(fit_duration))
print(cbind(confint_duration, coef_duration))

# By duration of stay within the hospital
dataset$Duration.of.stay.within.the.hospital <- as.factor(dataset$Duration.of.stay.within.the.hospital)
dataset$Duration.of.stay.within.the.hospital <- relevel(dataset$Duration.of.stay.within.the.hospital, ref="0 - <7")
fit_duration_stay <- logistf::logistf(
    outcome_died ~ Duration.of.stay.within.the.hospital,
    data=dataset, firth=TRUE, pl=TRUE)

sum_duration_stay <- summary(fit_duration_stay)
confint_duration_stay <- exp(confint(fit_duration_stay))
coef_duration_stay <- exp(coef(fit_duration_stay))
print(cbind(confint_duration_stay, coef_duration_stay))

# By underlying conditions
automate(dataset$Underlying.conditions...Respiratory.system)
automate(dataset$Underlying.conditions...Cardiovascular.system)
automate(dataset$Underlying.condition...Gastrointestinal.system)
automate(dataset$Underlying.condition...Kidney.and.urology.system)
automate(dataset$Underlying.condition...Immunodeficiency)
automate(dataset$Underlying.condition...Neurological.system)
automate(dataset$Underlying.condition...Inherited.metabolic.disorders)
automate(dataset$Underlying.condition...Other.underlying.conditions)
automate(dataset$Underlying.condition...No.underlying.diseases)

# By maximal form of respiratory support used
automate(dataset$oxygen_cannula)
automate(dataset$CPAP)
automate(dataset$conventional_mechanical_ventilation)
automate(dataset$hfo_ventilation)
automate(dataset$ECMO)

# By duration between onset and test
dataset$Duration.between.onset.and.test..detection.time...in.hours. <- as.factor(dataset$Duration.between.onset.and.test..detection.time...in.hours.)
dataset$Duration.between.onset.and.test..detection.time...in.hours. <- relevel(dataset$Duration.between.onset.and.test..detection.time...in.hours., ref="0 - <24")
automate(dataset$Duration.between.onset.and.test..detection.time...in.hours.)

# By diagnosis upon admission
automate(dataset$Measles)
automate(dataset$Pneumonia)
automate(dataset$Bronchopneumonia)
automate(dataset$Other.diagnosis)

# By complications
automate(dataset$complication.gastroentiritis)
automate(dataset$complication.middle.ear.infec)
automate(dataset$complication.conjunctivitis)
automate(dataset$complication.laryngitis)
automate(dataset$complication.pneumonia.bronchitis)
automate(dataset$complication.febrile.seizures)
automate(dataset$complication.septic.shock.sepsis)

# By co-infections
automate(dataset$co.infection.influenza.a)
automate(dataset$co.infection.influenza.b)
automate(dataset$co.infection.streptococus.aerius)
automate(dataset$co.infection.streptococus.pneumonia)

# By healthcare-associated infection
automate(dataset$respiratory_syncytical_virus)
automate(dataset$adenovirus)
automate(dataset$pertussis)
automate(dataset$healthcare_associated_infection)
