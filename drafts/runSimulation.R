rm(list = ls())
devtools::load_all(".")
library(BayesianFit)
library(blatent)
library(loo)
library(stringr)
library(psych)


# grab command line arguments
# arrayNumber = 219 # leave for debugging on local machine
arrayNumber = as.numeric(commandArgs(trailingOnly = TRUE)[1])


set.seed(arrayNumber)

# simulation specs
nReplicationsPerCondition = 100
simulationsSpecs = conditionInformation(arrayNumber = arrayNumber, nReplicationsPerCondition = nReplicationsPerCondition)

# nth replication
repRound = arrayNumber %% nReplicationsPerCondition
if (repRound == 0) {repRound = nReplicationsPerCondition}

# generate simulation data
simDataList = simulateDCM(trueModel = simulationsSpecs$trueModel,
                          nObs = simulationsSpecs$nObs,
                          quality = simulationsSpecs$quality,
                          seed = repRound
                          )

# save trueParameters for result.RData
trueParameters = as.data.frame(simDataList$trueParameters)

# initial value for estimation
inits = setDefaultInitializeParameters(
  normalMean = 3,
  normalVariance = 1,
  normalCovariance = 0,
  dirichletAlpha = 1
)


# number of MCMC samples by estimation model
if(syntax.model %in% c(syntax.lcdm.correct, syntax.lcdm.under, syntax.lcdm.over)){
  nSampled = 25000
} else {
  nSampled = 10000
}


# estimation
fitModel = blatentEstimate(dataMat = simDataList$responseData,
                           modelText = simulationsSpecs$estiModel,
                           options =  blatentControl(
                             nBurnin = nSampled,
                             nSampled = nSampled,
                             nThin = 5,
                             parallel = TRUE,
                             nCores = 4,
                             nChains = 4,
                             defaultInitializeParameters = inits,
                             defaultPriors = simulationsSpecs$prior,
                             seed = arrayNumber
                             )
                           )

# convergence check
maxRhat = max(fitModel[["parameterSummary"]][,"PSRF"])
print(maxRhat)



#### index related results ###==================================================

# relative fit index
dic = fitModel[["informationCriteria"]][["DIC"]][["DIC"]]
waic = -2 * fitModel[["informationCriteria"]][["WAIC"]][["WAIC"]]
loo = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["looic","Estimate"]

# SE of index
waic.se = waic(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["waic","SE"]
loo.se = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["looic","SE"]

# effective number
dic.p = fitModel[["informationCriteria"]][["DIC"]][["p_D"]]
waic.p = fitModel[["informationCriteria"]][["WAIC"]][["p_WAIC"]]
loo.p = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["p_loo","Estimate"]

# elpd
dic.elpd = -1/2 * dic
waic.elpd = waic(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["elpd_waic","Estimate"]
loo.elpd = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["elpd_loo","Estimate"]



#### respondents related results ### ===========================================

# classification accuracy rate (joint)
estiJ = fitModel[["estimatedLatentVariables"]][c("profileNumber.MAP")]
trueJ = simDataList$trueJointMAP
caJoint = sum(estiJ == trueJ)/ nrow(estiJ) * 100

# classification accuracy rate (marginal)
estiA = fitModel[["estimatedLatentVariables"]][grep("MAP.marginal",names(fitModel[["estimatedLatentVariables"]]))]
trueA = simDataList$trueMarginalMAP
nAccurate = mapply(function(e, t) sum(e == t), estiA, trueA)
caMarginal = sum(nAccurate) / sum(lengths(estiA)) * 100



#### parameter related results ### ========================================

# parameter estimates
parameterEstimates = as.data.frame(fitModel[["parameterSummary"]][,"Mean"])

# Bias
estiJointProb = parameterEstimates[c("joint.prob000","joint.prob001", "joint.prob010", "joint.prob011",
                                 "joint.prob100","joint.prob101","joint.prob110","joint.prob111"),]

trueJointProb = trueParameters[c("joint.prob000","joint.prob001", "joint.prob010", "joint.prob011",
                                 "joint.prob100","joint.prob101","joint.prob110","joint.prob111"),]

diffJointProb = estiJointProb - trueJointProb
biasJointProb = mean(diffJointProb)
rmseJointProb = sqrt(mean(diffJointProb^2))



# combine results into a list
fit.results = list(index = cbind(dic, waic, loo),
                   index.se = cbind(waic.se, loo.se),
                   index.p = cbind(dic.p, waic.p, loo.p),
                   index.elpd = cbind(dic.elpd, waic.elpd, loo.elpd),
                   caRate = cbind(caJoint, caMarginal),
                   evalJointProb = cbind(biasJointProb, rmseJointProb),
                   parameterEstimates = parameterEstimates
                   )


save(maxRhat, trueParameters, fit.results, file = paste0("result_", arrayNumber, ".RData"))
