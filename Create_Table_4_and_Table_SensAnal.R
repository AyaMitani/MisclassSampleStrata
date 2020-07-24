
#---------------------
# Load packages
#---------------------

library(survey)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(xtable)
library(margins)


#---------------------------
# Begin functions
#---------------------------

### produce table with OR and 95% CI

ORtable <- function(sumdesignagnostic, summodelbased, sumdesignbased, variable, p){
  modstarOR <- exp(summodelbased$coef[1:p,1])
  modstarORlcl <- exp(summodelbased$coef[1:p,1] - 1.96 * summodelbased$coef[1:p,2])
  modstarORucl <- exp(summodelbased$coef[1:p,1] + 1.96 * summodelbased$coef[1:p,2])
  modstarORresult <- paste0(format(round(modstarOR, 2), nsmall=2), " (", format(round(modstarORlcl, 2), nsmall=2), ", ", format(round(modstarORucl, 2), nsmall=2), ")")
  
  modOR <- exp(sumdesignagnostic$coef[1:p,1])
  modORlcl <- exp(sumdesignagnostic$coef[1:p,1] - 1.96 * sumdesignagnostic$coef[1:p,2])
  modORucl <- exp(sumdesignagnostic$coef[1:p,1] + 1.96 * sumdesignagnostic$coef[1:p,2])
  modORresult <- paste0(format(round(modOR, 2), nsmall=2), " (", format(round(modORlcl, 2), nsmall=2), ", ", format(round(modORucl, 2), nsmall=2), ")")
  
  desOR <- exp(sumdesignbased$coef[,1])
  desORlcl <- exp(sumdesignbased$coef[1:p,1] - 1.96 * sumdesignbased$coef[1:p,2])
  desORucl <- exp(sumdesignbased$coef[1:p,1] + 1.96 * sumdesignbased$coef[1:p,2])
  desORresult <- paste0(format(round(desOR, 2), nsmall=2), " (", format(round(desORlcl, 2), nsmall=2), ", ", format(round(desORucl, 2), nsmall=2), ")")
  
  ORresult <- cbind(variable, modORresult, modstarORresult, desORresult)
  colnames(ORresult) <- c("Variable", "OR (95% CI)", "OR (95% CI)", "OR (95% CI)")
  ORtable <- xtable(ORresult)
  print.xtable(ORtable, include.rownames = FALSE)
}

### design-based with truncated weights

designbased_tr <- function(cutoff){
  analdata$svywt_tr <- ifelse(analdata$svywt > cutoff, cutoff, analdata$svywt)
  design_tr <- svydesign(id = ~patient.id, strata = ~ e.race.eth.5 + e.agelt35 + e.female + e.rural + e.edugrp, weights = ~svywt_tr, data = analdata)
  designbased_tr <- svyglm(s.trust ~ as.factor(s.race.eth.5) + s.poverty + s.agelt35 + as.factor(s.female) + s.rural + s.edu_L + s.edu_M, design = design_tr, family = quasibinomial("logit") )
  sumdesignbased_tr <- summary(designbased_tr)
}

#----------------------------------
# End of functions
#----------------------------------


#------------------------------
# Load analytical data set
#------------------------------

setwd("") ### ENTER PATH HERE
load("analdata.Rdata")





#------------------------------
# Analysis
#------------------------------

### design agnostic
designagnostic <- glm(s.trust ~ as.factor(s.race.eth.5) + s.poverty + s.agelt35 + as.factor(s.female) + s.rural + s.edu_L + s.edu_M, data = analdata, family = binomial("logit"))
sumdesignagnostic <- summary(designagnostic)

### model-based (adjusting for stratification variables)
modelbased <- glm(s.trust ~ as.factor(s.race.eth.5) + s.poverty + s.agelt35 + as.factor(s.female) + s.rural + s.edu_L + s.edu_M + as.factor(e.race.eth.5) + e.agelt35 + e.female + e.rural + as.factor(e.edugrp), data = analdata, family = binomial("logit"))
summodelbased <- summary(modelbased)
# Calculate average marginal effect
modelbased_marg = margins(modelbased)
# Summary of marginal effect
summary(modelbased_marg)

### design-based (weighted analysis)
design <- svydesign(id = ~patient.id, strata = ~ e.race.eth.5 + e.agelt35 + e.female + e.rural + e.edugrp, weights = ~svywt, data = analdata)
designbased <- svyglm(s.trust ~ as.factor(s.race.eth.5) + s.poverty + s.agelt35 + as.factor(s.female) + s.rural + s.edu_L + s.edu_M, design = design, family = quasibinomial("logit") )
sumdesignbased <- summary(designbased)

### design-based (weighted analysis) with weights truncated at 90th percentile
cutoff <- quantile(analdata$svywt, 0.90)
analdata$svywt_tr <- ifelse(analdata$svywt > cutoff, cutoff, analdata$svywt)
design_tr <- svydesign(id = ~patient.id, strata = ~ e.race.eth.5 + e.agelt35 + e.female + e.rural + e.edugrp, weights = ~svywt_tr, data = analdata)
designbased_tr <- svyglm(s.trust ~ as.factor(s.race.eth.5) + s.poverty + s.agelt35 + as.factor(s.female) + s.rural + s.edu_L + s.edu_M, design = design_tr, family = quasibinomial("logit") )
sumdesignbased_tr <- summary(designbased_tr)


#------------------------------
# Produce Table 4
#------------------------------

variable <- c("Intercept", "Black", "Asian", "Other", "Hispanic", "Poverty", "Age $lt$ 35", "Female", "Rural", "Less than HS", "Some college")
ORtable(sumdesignagnostic, summodelbased, sumdesignbased_tr,  variable, p=11)





#--------------------------------------------------------------------------
# Sensitivity analysis with different truncation cutoffs
#--------------------------------------------------------------------------


sumdesignbased_tr100 <- designbased_tr(quantile(analdata$svywt, 1))
sumdesignbased_tr99 <- designbased_tr(quantile(analdata$svywt, 0.99))
sumdesignbased_tr95 <- designbased_tr(quantile(analdata$svywt, 0.95))

variable <- c("Intercept", "Black", "Asian", "Other", "Hispanic", "Poverty", "Age $lt$ 35", "Female", "Rural", "Less than HS", "Some college")
ORtable(sumdesignbased_tr100, sumdesignbased_tr99, sumdesignbased_tr95,  variable, p=11)

