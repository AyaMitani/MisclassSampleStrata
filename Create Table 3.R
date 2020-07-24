
#---------------------
# Load packages
#---------------------

library(survey)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(xtable)


#------------------------------
# Load analytical data set
#------------------------------

setwd("") ### ENTER PATH HERE
load("analdata.Rdata")


#------------------------------
# Table 3
#-----------------------------

samplesize <- dim(findata)[1]

findata$s.agege35 <- 1 - findata$s.agelt35 

svygender <- as.matrix(table(findata$gender))
svyage <- as.matrix(table(findata$s.agege35))
svyedu <- as.matrix(table(findata$edugrp))
svyrace <- as.matrix(table(findata$s.race.eth.5))
svyincome <- as.matrix(table(findata$income4level))
svyreligious <- as.matrix(table(findata$s.religious))
svyrural <- as.matrix(table(findata$s.rural))

svyfreq <- rbind(svygender, svyage, svyrace, svyedu, svyrural, svyincome)
svyprop <- round(svyfreq/samplesize * 100, 0)
table1svy <- paste0(svyprop, " [", svyfreq, "]")
genderlab <- c("Male", "Female")
agelab <- c("<35", "35+")
racelab <- c("White", "Black", "Asian", "Other", "Hispanic")
edulab <- c("Less than HS", "HS to some college", "At least BS")
incomelab <- c("<30,000", "30,000 to 59,999", "60,000 to 149,999", "150,000+")
rurallab <- c("Suburban/Urban", "Rural")
labs <- c(genderlab, agelab, racelab, edulab, rurallab, incomelab)

table1 <- xtable(cbind(labs, table1svy))
print.xtable(table1, include.rownames = FALSE)


#------------------------------------
# Table S1
#------------------------------------

samplesize <- dim(findata)[1]

design <- svydesign(id = ~contact.id, strata = ~ e.race.eth.5 + e.agelt35 + e.gender + e.rural + e.edugrp, weights = ~svywt, data = findata)
ehrgender <- as.matrix(svytotal(~ as.factor(gender), design, na.rm=TRUE))
ehrage <- as.matrix(svytotal(~ as.factor(s.agege35), design, na.rm=TRUE))
ehredu <- as.matrix(svytotal(~ as.factor(edugrp), design, na.rm=TRUE))
ehrrace <- as.matrix(svytotal(~ as.factor(s.race.eth.5), design, na.rm=TRUE))
ehrincome <- as.matrix(svytotal(~ as.factor(income4level), design, na.rm=TRUE))
ehrreligious <- as.matrix(svytotal(~ as.factor(s.religious), design, na.rm=TRUE))
ehrrural <- as.matrix(svytotal(~ as.factor(s.rural), design, na.rm=TRUE))

svygender <- as.matrix(table(findata$gender))
svyage <- as.matrix(table(findata$s.agege35))
svyedu <- as.matrix(table(findata$edugrp))
svyrace <- as.matrix(table(findata$s.race.eth.5))
svyincome <- as.matrix(table(findata$income4level))
svyreligious <- as.matrix(table(findata$s.religious))
svyrural <- as.matrix(table(findata$s.rural))

svyfreq <- rbind(svygender, svyage, svyrace, svyedu, svyrural, svyincome)
svyprop <- round(svyfreq/samplesize * 100, 0)
ehrfreq <- round(rbind(ehrgender, ehrage, ehrrace, ehredu, ehrrural, ehrincome), 0)
ehrprop <- round(ehrfreq/329672 * 100, 0)
table1svy <- paste0(svyprop, " [", svyfreq, "]")
table1ehr <- paste0(ehrprop, " [", ehrfreq, "]")
genderlab <- c("Male", "Female")
agelab <- c("<35", "35+")
racelab <- c("White", "Black", "Asian", "Other", "Hispanic")
incomelab <- c("<30,000", "30,000 to 59,999", "60,000 to 149,999", "150,000+")
rurallab <- c("Suburban/Urban", "Rural")
labs <- c(genderlab, agelab, racelab, edulab, rurallab, incomelab)

table1 <- xtable(cbind(labs, table1svy))
print.xtable(table1, include.rownames = FALSE)

