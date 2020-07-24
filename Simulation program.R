#----------------------------------------------------------------------------------------------------------------------------
#   Simulation program for stratum misclassification
#      
#   This program runs the simulation study presented in
#   "Survey design and analysis considerations when utilizing a misclassified sampling stratum"
#   Authors: Aya Mitani, Nathanial Mercado, Sebastien Haneuse, Jonathan Schildcrout
#   
#   Code written by: Aya Mitani
#
#
#   Output includes a latex table with simulation results presented in Table 2 of manuscript
#
#   Need to indicate what type of misclassification to simulate in the simulation parameters (Line 36)
#       0 = no misclassification, 1 = non-differential misclassification, 2 = differential misclassification
#----------------------------------------------------------------------------------------------------------------------------

### load packages
library(simex)
library(survey)
library(xtable)
library(tidyverse)


#-----------------------------
# simulation parameters
#-----------------------------


### First read in the arguments listed at the command line
args=(commandArgs(TRUE))
print(args)

### args is now a list of character vectors
### First check to see if arguments are passed.
### Then cycle through each element of the list and evaluate the expressions.

if(length(args)==0){
  print("No arguments supplied.")
  ## supply default values
  simnum=10 # number of simulations to run
  mc_ind=2  # type of misclassification 0 = no misclass, 1 = nondiff misclass, 2 = diff misclass
  baseseed=934
}else{
  for (i in (1:length(args))) eval(parse(text=args[[i]]))
}


popN <- 100000 # population size
bigN <- 10000000
sampN <- 2500 # sample size
prace <- c(0.82, 0.10, 0.01, 0.05, 0.02) # percent true race in population (White, Black, Asian, Other, Hispanic)
sampNeach <- 2500/length(prace)

### poverty model parameters
povbeta0 <- -2
povbeta1 <- 1.25
povbeta2 <- 0.25
povbeta3 <- 1.75
povbeta4 <- 0.50
povbetas <- c(povbeta0, povbeta1, povbeta2, povbeta3, povbeta4)

### trust model parameters
trbeta0 <- -0.75
trbeta1 <- -0.25
trbeta2 <- -0.5
trbeta3 <- 1.25
trbeta4 <- -1.5
trbeta5 <- 1
trbetas <- c(trbeta0, trbeta1, trbeta2, trbeta3, trbeta4, trbeta5)
paramnames <- c("Intercept", "Black", "Asian", "Other", "Hispanic", "Poverty")

### misclassification matrix (marginal) for nondifferential misclassification
mismat_marginal <- matrix(c(0.944, 0.007, 0.000, 0.042, 0.007,
                            0.000, 0.937, 0.000, 0.051, 0.013,
                            0.012, 0.012, 0.768, 0.171, 0.037,
                            0.480, 0.041, 0.130, 0.276, 0.073,
                            0.239, 0.161, 0.017, 0.050, 0.533), nrow = 5, byrow = T)

### misclassification matrix stratified by outcome for differential misclassification
mismat_tr0 <- matrix(c(0.972, 0.000, 0.000, 0.000, 0.028,
                       0.000, 0.870, 0.000, 0.087, 0.043,
                       0.000, 0.000, 0.788, 0.182, 0.030,
                       0.560, 0.020, 0.060, 0.280, 0.080,
                       0.177, 0.145, 0.016, 0.048, 0.613), nrow = 5, byrow = T)

mismat_tr1 <- matrix(c(0.935, 0.009, 0.000, 0.056, 0.000,
                       0.000, 0.964, 0.000, 0.036, 0.000,
                       0.020, 0.020, 0.755, 0.163, 0.041,
                       0.425, 0.055, 0.178, 0.274, 0.068,
                       0.271, 0.169, 0.017, 0.051, 0.492), nrow = 5, byrow = T)

### identity matrix for no misclassification
nomismat_race <- diag(5)

alpha <- 0.05 # alpha used for coverage probability
modp <- 6 - 1
mismodp <- 5 - 1
resultlist <- misresultlist <- list()
simmodnames <- c("full", "fullstar", "srs", "model", "modelstar", "design")

#------------------------------------------------------------------------
# Some functions used in simulation
#------------------------------------------------------------------------

### fucntion to generate misclassified variable 

mcfunc <- function(truevar, mismat){
  truevarf <- factor(truevar)
  colnames(mismat) <- levels(truevarf)
  mismat_list <- list(truevarf = mismat)
  mcout <- misclass(data.org = data.frame(truevarf), mc.matrix = mismat_list, k = 1)
  mcvar <- as.numeric(mcout$truevarf)
  return(mcvar)
}


### function to compute relative bias
relbiasfunc <- function(est, true){
  relbiasout <- ( est - true ) / true
  return(relbiasout)
}

### function to compute alpha% coverage
covfunc <- function(true, est, se, alpha){
  p <- length(true)
  z <- qnorm( 1 - alpha / 2 )
  coverage <- rep(NA, p)
  for(pp in 1:p){
    lcl <- est[pp] - z * se[pp]
    ucl <- est[pp] + z * se[pp]
    coverage[pp] <- ifelse(true[pp] > lcl & true[pp] < ucl, 1, 0)
  }
  return(coverage)
}


### function to compute simulation parameters from regression output (nparam1 is number of parameters - 1)
simparamvec <- c("est", "se", "relbias", "cov")

simparamfunc <- function(modout, nparam1, true, alpha, modname){
  ests <- summary(modout)$coef[c(1:(nparam1+1)),1]
  ses <- summary(modout)$coef[c(1:(nparam1+1)),2]
  relbias <- relbiasfunc(true = true, est = ests)
  cov <- covfunc(true = true, est = ests, se = ses, alpha = alpha)
  names(ests) <- paste0(modname, simparamvec[1], c(0:nparam1))
  names(ses) <- paste0(modname, simparamvec[2], c(0:nparam1))
  names(relbias) <- paste0(modname, simparamvec[3], c(0:nparam1))
  names(cov) <- paste0(modname, simparamvec[4], c(0:nparam1))
  return(c(ests, ses, relbias, cov))
}

modelstarsimparamfunc <- function(modout, nparam1, modname){
  ests <- summary(modout)$coef[c(1:(nparam1+1)),1]
  ses <- summary(modout)$coef[c(1:(nparam1+1)),2]
  names(ests) <- paste0(modname, simparamvec[1], c(0:nparam1))
  names(ses) <- paste0(modname, simparamvec[2], c(0:nparam1))
  return(c(ests, ses))
}

simexsimparamfunc <- function(modout, nparam1, true, alpha, modname){
  ests <- summary(modout)$coef$asymptotic[c(1:(nparam1+1)),1]
  ses <- summary(modout)$coef$asymptotic[c(1:(nparam1+1)),2]
  relbias <- relbiasfunc(true = true, est = ests)
  cov <- covfunc(true = true, est = ests, se = ses, alpha = alpha)
  names(ests) <- paste0(modname, simparamvec[1], c(0:nparam1))
  names(ses) <- paste0(modname, simparamvec[2], c(0:nparam1))
  names(relbias) <- paste0(modname, simparamvec[3], c(0:nparam1))
  names(cov) <- paste0(modname, simparamvec[4], c(0:nparam1))
  return(c(ests, ses, relbias, cov))
}







#######################################     Begin simulation     #########################################


for(s in 1:simnum){
  
  simseed <- baseseed + 25 * s
  names(simseed) <- "simseed"
  
  set.seed(simseed)
  
  
  
  
  #-------------------------------------
  # Generate population data
  #-------------------------------------
  
  truerace <- as.numeric( c(1,2,3,4,5) %*% rmultinom(popN, 1, prace) )
  #trueracef <- as.matrix(dummy_cols(truerace)[,c(-1,-2)]) # create dummy variables for race -- remove original variable and White race
  Black <- ifelse(truerace == 2, 1, 0)
  Asian <- ifelse(truerace == 3, 1, 0)
  Other <- ifelse(truerace == 4, 1, 0)
  Hisp <- ifelse(truerace == 5, 1, 0)
  
  
  ### generate poverty
  povXmat <- cbind(rep(1, popN), Black, Asian, Other, Hisp)
  povXb <- povXmat %*% povbetas
  ppov <- exp(povXb) / ( 1 + exp(povXb) )
  upov <- runif(popN)
  poverty <- ifelse(ppov > upov, 1, 0)
  #glm(poverty ~ Black + Asian + Other + Hisp, family = binomial("logit"))
  
  ### generate trust
  trXmat <- cbind(rep(1, popN), Black, Asian, Other, Hisp, poverty)
  trXb <- trXmat %*% trbetas
  ptr <- exp(trXb) / ( 1 + exp(trXb) )
  utr <- runif(popN)
  trust <- ifelse(ptr > utr, 1, 0)
  
  
  ### generate misclassified race 
  
  if(mc_ind == 1){
    ehrrace <- mcfunc(truerace, mismat_marginal)
    mcdata <- data.frame(truerace, ehrrace, poverty, trust)
  }else if(mc_ind == 2){
    fulldata <- data.frame(truerace, poverty, trust)
    data_tr1 <- subset(fulldata, fulldata$trust == 1)
    data_tr1$ehrrace <- mcfunc(data_tr1$truerace, mismat_tr1)
    data_tr0 <- subset(fulldata, fulldata$trust == 0)
    data_tr0$ehrrace <- mcfunc(data_tr0$truerace, mismat_tr0)
    mcdata <- rbind(data_tr0, data_tr1)
  }else{
    mcdata <- data.frame(truerace, poverty, trust)
    mcdata$ehrrace <- mcdata$truerace
  }
  
  
  
  mcdata$id <- c(1:popN)
  mcdata$Black <- ifelse(mcdata$truerace == 2, 1, 0)
  mcdata$Asian <- ifelse(mcdata$truerace == 3, 1, 0)
  mcdata$Other <- ifelse(mcdata$truerace == 4, 1, 0)
  mcdata$Hisp <- ifelse(mcdata$truerace == 5, 1, 0)
  
  mcdata$ehrBlack <- ifelse(mcdata$ehrrace == 2, 1, 0)
  mcdata$ehrAsian <- ifelse(mcdata$ehrrace == 3, 1, 0)
  mcdata$ehrOther <- ifelse(mcdata$ehrrace == 4, 1, 0)
  mcdata$ehrHisp <- ifelse(mcdata$ehrrace == 5, 1, 0)
  
  
  prop.table(table(mcdata$truerace, mcdata$ehrrace), 2) * 100
  #prop.table(table(mcdata$truerace, mcdata$ehrrace), 2) * 100
  
  #-------------------------------------------------
  # Full data analyses
  #-------------------------------------------------
  
  ### true model
  fullout <- glm(trust ~ Black + Asian + Other + Hisp + poverty, family = binomial("logit"), data = mcdata)
  fullsims <- simparamfunc(modout = fullout, nparam1 = modp, true = trbetas, alpha = alpha, modname = "full")
  
  ### true model with Xstar
  fullstarout <- glm(trust ~ ehrBlack + ehrAsian + ehrOther + ehrHisp + poverty, family = binomial("logit"), data = mcdata)
  fullstarsims <- simparamfunc(modout = fullstarout, nparam1 = modp, true = trbetas, alpha = alpha, modname = "fullstar")

  #---------------------------------------------------------
  # Simple random sampling
  #---------------------------------------------------------
  
  srsid <- sample(mcdata$id, size = sampN)
  srsdat <- mcdata[srsid,]
  srsout <- glm(trust ~ Black + Asian + Other + Hisp + poverty, family = binomial("logit"), data = srsdat)
  srssims <- simparamfunc(modout = srsout, nparam1 = modp, true = trbetas, alpha = alpha, modname = "srs")
  


  #---------------------------------------------------------
  # Sample patients based on misclassified race
  #---------------------------------------------------------
  
  #table(mcdata$ehrrace_diff)
  samplefunc <- function(racenum){
    subdata <- subset(mcdata, mcdata$ehrrace == racenum)
    sampid <- sample(subdata$id, size = sampNeach)
    sampdata <- subdata[which(subdata$id %in% sampid),]
    return(sampdata)
  }
  sampdatalist <- list()
  for(r in 1:length(prace)){
    sampdatalist[[r]] <- samplefunc(r)
  }
  sampledat <- do.call("rbind",sampdatalist)
  
  
  #--------------------------------------------------------
  # Add sampling weights
  #--------------------------------------------------------
  
  sw <- rep(sampNeach/table(mcdata$ehrrace), each = sampNeach)
  sampledat$sw <- 1/sw
  
  
  #---------------------------------------------------
  # Logistic regression analysis
  #---------------------------------------------------
  
  ### model-based
  modelout <- glm(trust ~ Black + Asian + Other + Hisp + poverty, data = sampledat, family = binomial("logit"))
  modelsims <- simparamfunc(modout = modelout, nparam1 = modp, true = trbetas, alpha = alpha, modname = "model")

  ### model-based adjusted for Xstar
  modelstarout <- glm(trust ~ Black + Asian + Other + Hisp + poverty + ehrBlack + ehrAsian + ehrOther + ehrHisp, data = sampledat, family = binomial("logit"))
  modelstarsims <- simparamfunc(modout = modelstarout, nparam1 = modp, true = trbetas, alpha = alpha, modname = "modelstar")

  ### design-based
  desout <- svydesign(id = ~id, strata = ~ ehrrace, weights = ~sw, data = sampledat)
  designout <- svyglm(trust ~ Black + Asian + Other + Hisp + poverty, design = desout, family = quasibinomial("logit") )
  designsims <- simparamfunc(modout = designout, nparam1 = modp, true = trbetas, alpha = alpha, modname = "design")

  
  #---------------------------------------------------
  # Simulation output
  #---------------------------------------------------
  
  result <- c(simseed, fullsims, fullstarsims, srssims, modelsims, modelstarsims, designsims)
  
  
  resultlist[[s]] <- result
  
  
  print(s)
  
}


#------------------------------------------------------------------------
# Generate table for simulation results
#------------------------------------------------------------------------

### True model

simout <- do.call("rbind", resultlist)
#write.csv(simout, paste0("simout", mc_ind, ".csv"), row.names = F)
simsummary <- as.data.frame(matrix(colMeans(simout[,-1]), nrow=modp+1))
allests <- select(as.data.frame(simout, col.names = TRUE), contains(c("est")))
simsd <- as.data.frame(matrix(apply(allests, 2, sd), nrow=modp+1))
cn <- c()
for(s in 1:length(simmodnames)) cn <- c(cn, paste0(simmodnames[s], simparamvec) )
colnames(simsummary) <- cn
rownames(simsummary) <- paramnames
colnames(simsd) <- simmodnames
rownames(simsd) <- paramnames

### table that shows est, se and covp
estsecovlist <- list()
for(s in 1:length(simmodnames)){
  estse <- as.matrix(paste0(format(round(simsummary[,paste0(simmodnames[s], "est")], 3), nsmall=3), " (", format(round(simsd[,paste0(simmodnames[s])], 3), nsmall=3), ")"))
  covp <- as.matrix(paste0(format(round(simsummary[,paste0(simmodnames[s], "cov")] * 100, 1), nsmall=1)))
  estsecov <- cbind(estse, covp)
  colnames(estsecov) <- c(paste0(simmodnames[s], "estse"), paste0(simmodnames[s], "covp"))
  estsecovlist[[s]] <- estsecov
}
simresulttable <- as.data.frame(do.call("cbind", estsecovlist))
rownames(simresulttable) <- paramnames





### Combine both results into one table

if(mc_ind == 0){
  tablecap <- "Simulation results with no misclassification"
}else if(mc_ind == 1){
  tablecap <- "Simulation results with non-differential misclassification"
}else{
  tablecap <- "Simulation results with differential misclassification"
}
thextable <- xtable(simresulttable, caption = tablecap)

print(thextable, file=paste0("resulttable_mc", mc_ind, ".txt"))
