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
                                 nThin = 1,
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
    print(maxRhat)
  }


  # parameter estimates
  parameterEstimates = as.data.frame(fitModel[["parameterSummary"]][,"Mean"])
  jointMAP = fitModel[["estimatedLatentVariables"]][c("profileNumber.MAP")]
  marginalMAP = fitModel[["estimatedLatentVariables"]][grep("MAP.marginal",names(fitModel[["estimatedLatentVariables"]]))]
  # names(parameterEstimates) = names(jointMAP) = names(marginalMAP)


  # relative fit
  dic = fitModel[["informationCriteria"]][["DIC"]][["DIC"]]
  waic = -2 * fitModel[["informationCriteria"]][["WAIC"]][["WAIC"]]
  loo = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["looic","Estimate"]

  dic.p = fitModel[["informationCriteria"]][["DIC"]][["p_D"]]
  waic.p = fitModel[["informationCriteria"]][["WAIC"]][["p_WAIC"]]
  loo.p = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["p_loo","Estimate"]

  # DIC ELPD check
  dic.elpd = -1/2 * dic
  waic.elpd = waic(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["elpd_waic","Estimate"]
  loo.elpd = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["elpd_loo","Estimate"]

  index = cbind(dic, waic, loo)
  index.p = cbind(dic.p, waic.p, loo.p)
  index.elpd = cbind(dic.elpd, waic.elpd, loo.elpd)



  return(list(fitModel = fitModel,
              maxRhat = maxRhat,
              nCalibration = nCalibration,
              parameterEstimates = parameterEstimates,
              jointMAP = jointMAP,
              marginalMAP = marginalMAP,
              index = index,
              index.p = index.p,
              index.elpd = index.elpd
              )
         )
}
