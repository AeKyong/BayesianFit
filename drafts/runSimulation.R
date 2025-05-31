rm(list = ls())
devtools::load_all(".")
library(BayesianFit)
library(blatent)
library(loo)
library(stringr)
library(psych)
library(foreach)
library(doParallel)


# grab command line arguments
arrayNumber = 1 # leave for debugging on local machine
#arrayNumber = as.numeric(commandArgs(trailingOnly = TRUE)[1])

set.seed(arrayNumber)

# simulation specs
simulationsSpecs = conditionInformation(arrayNumber = arrayNumber, nReplicationsPerCondition = 1)


# generate simulation data
simDataList = simualteDCM(trueModel = simulationsSpecs$trueModel,
                          nObs = simulationsSpecs$nObs,
                          quality = simulationsSpecs$quality
                          )


# initial value for estimation
inits = setDefaultInitializeParameters(
  normalMean = 3,
  normalVariance = 1,
  normalCovariance = 0,
  dirichletAlpha = 1
)


syntax.model = c(syntax.lcdm.correct, syntax.lcdm.under,syntax.lcdm.over,
                 syntax.dina.correct, syntax.dina.under,syntax.dina.over,
                 syntax.crum.correct, syntax.crum.under,syntax.crum.over)

fit.model = list()
model=1
for (model in 1:length(syntax.model)){
  fit.model[[model]] = estimateModel(
    data.mat = simDataList$responseData,
    syntax.model = syntax.model[model],
    inits = inits,
    priors = simulationsSpecs$prior
  )

}

save(fit.model, paste0("fit_", arrayNumber, ".RData"))
