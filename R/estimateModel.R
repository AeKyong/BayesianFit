estimateModel = function(data.mat, syntax.model, inits,  priors){

  nCalibration = 0
  maxRhat = 10000

  if(syntax.model %in% c(syntax.lcdm.correct, syntax.lcdm.under, syntax.lcdm.over)){
    nSampled = 25000
  } else {
    nSampled = 10000
  }


  # while(maxRhat > 1.1 && nCalibration <= 2){
    nCalibration = nCalibration + 1
    print(nCalibration)

    fitModel = blatentEstimate(dataMat = data.mat,
                               modelText = syntax.model,
                               options =  blatentControl(
                                 nBurnin = nSampled * nCalibration,
                                 nSampled = nSampled * nCalibration,
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
    print(maxRhat)
  # }



  # parameter estimates
  parameterEstimates = as.data.frame(fitModel[["parameterSummary"]][,"Mean"])
  jointMAP = fitModel[["estimatedLatentVariables"]][c("profileNumber.MAP")]
  marginalMAP = fitModel[["estimatedLatentVariables"]][grep("MAP.marginal",names(fitModel[["estimatedLatentVariables"]]))]


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

  waic.se = waic(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["waic","SE"]
  loo.se = loo(fitModel[["logLikelihoods"]][["marginal"]], save_psis = TRUE)$"estimate"["looic","SE"]


  index = cbind(dic, waic, loo, dic.p, waic.p, loo.p, dic.elpd, waic.elpd, loo.elpd, waic.se, loo.se)



  return(list(
    fitModel = fitModel,
    maxRhat = maxRhat,
    nCalibration = nCalibration,
    parameterEstimates = parameterEstimates,
    jointMAP = jointMAP,
    marginalMAP = marginalMAP,
    index = index
    )
    )
}
