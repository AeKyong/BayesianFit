estimateModel = function(data.mat, syntax.model, inits,  priors){

  nCalibration = 0
  maxRhat = 100000

  while(maxRhat > 1.1){
    nCalibration = nCalibration + 1
    print(nCalibration)
    fitModel = blatentEstimate(dataMat = data.mat,
                               modelText = syntax.model,
                               options =  blatentControl(
                                 nBurnin = 2000 * (1 + nCalibration),
                                 nSampled = 2000,
                                 nThin = 5,
                                 parallel = TRUE,
                                 nCores = 4,
                                 nChains = 4,
                                 defaultInitializeParameters = inits,
                                 defaultPriors = priors,
                                 seed = arrayNumber
                                 )
    )
    # convergence check
    maxRhat = max(fitModel[["parameterSummary"]][,"PSRF"])

  }


  # parameter estimates
  parameterEstimates = as.data.frame(fitModel[["parameterSummary"]][,"Mean"])
  jointMAP = fitModel[["estimatedLatentVariables"]][c("profileNumber.MAP")]
  marginalMAP = fitModel[["estimatedLatentVariables"]][grep("MAP.marginal",names(fitModel[["estimatedLatentVariables"]]))]
  # names(parameterEstimates) = names(jointMAP) = names(marginalMAP)


  # relative fit  !DIC, WAIC value/ number of model parameters check!
  dic = fitModel[["informationCriteria"]][["DIC"]][["DIC"]]
  waic = -2 * fitModel[["informationCriteria"]][["WAIC"]][["WAIC"]]
  loo = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["looic","Estimate"]

  dic.p = fitModel[["informationCriteria"]][["DIC"]][["p_D"]]
  waic.p = fitModel[["informationCriteria"]][["WAIC"]][["p_WAIC"]]
  loo.p = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["p_loo","Estimate"]

  index = cbind(dic, waic, loo)
  indexP = cbind(dic.p, waic.p, loo.p)

  return(list(fitModel = fitModel,
              maxRhat = maxRhat,
              nCalibration = nCalibration,
              parameterEstimates = parameterEstimates,
              jointMAP = jointMAP,
              marginalMAP = marginalMAP,
              index = index,
              indexP = indexP
              )
         )
}
