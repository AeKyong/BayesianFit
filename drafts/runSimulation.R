rm(list = ls())
devtools::load_all(".")
library(BayesianFit)
library(blatent)
library(loo)
library(stringr)
library(psych)

# grab command line arguments
arrayNumber = 3 # leave for debugging on local machine
#arrayNumber = as.numeric(commandArgs(trailingOnly = TRUE)[1])

set.seed(arrayNumber)

# simulation specs
simulationsSpecs = conditionInformation(arrayNumber = arrayNumber, nReplicationsPerCondition = 1)


# generate simulation data
simDataList = simualteDCM(trueModel = simulationsSpecs$trueModel,
                          nAttributes = simulationsSpecs$nAttributes,
                          nObs = simulationsSpecs$nObs,
                          quality = simulationsSpecs$quality,
                          nItems = simulationsSpecs$nItems
                          )


# initial value for estimation
inits = setDefaultInitializeParameters(
  normalMean = 3,
  normalVariance = 1,
  normalCovariance = 0,
  dirichletAlpha = 1
)


if(simulationsSpecs$nAttributes == 3){
  syntax.lcdm = syntax.lcdm3
  syntax.dina = syntax.dina3
  syntax.crum = syntax.crum3
} else if(simulationsSpecs$nAttributes == 5){
  syntax.lcdm = syntax.lcdm5
  syntax.dina = syntax.dina5
  syntax.crum = syntax.crum5
}


# estimation
fit.lcdm = estimateModel(
  data.mat = simDataList$responseData,
  syntax.model = syntax.lcdm,
  inits = inits,
  priors = simulationsSpecs$prior
  )

fit.dina = estimateModel(
  data.mat = simDataList$responseData,
  syntax.model = syntax.dina,
  inits = inits,
  priors = simulationsSpecs$prior
)

fit.crum = estimateModel(
  data.mat = simDataList$responseData,
  syntax.model = syntax.crum,
  inits = inits,
  priors = simulationsSpecs$prior
)






