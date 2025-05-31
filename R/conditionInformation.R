conditionInformation = function(arrayNumber, nReplicationsPerCondition, nCores = 4){
# browser()
  # convert array number to condition number
  conditionNumber = floor((arrayNumber - 1)/nReplicationsPerCondition) + 1

  # create conditions list
  conditions = list(
    trueModel = c("lcdm", "dina", "crum"),
    quality = c("low", "medium", "high"),
    prior = c("uninformative", "informative"),
    nObs = c(100, 500, 1000, 2000)
  )

  # number of conditions
  nConditions = prod(unlist(lapply(X = conditions, FUN = length)))

  # make conditions matrix
  conditionsMatrix = matrix(NA, nrow = nConditions, ncol = length(conditions))
  colnames(conditionsMatrix) = names(conditions)

  cond=1
  for (cond in 1:nConditions){
    conditionsMatrix[cond,] = dec2bin(
      decimal_number = cond - 1,
      nattributes = length(conditions),
      basevector = unlist(lapply(X = conditions, FUN = length))
    ) + 1
  }


  # populate condition values
  trueModel = conditions$trueModel[conditionsMatrix[conditionNumber,1]]
  quality = conditions$quality[conditionsMatrix[conditionNumber,2]]
  prior =  conditions$prior[conditionsMatrix[conditionNumber,3]]
  nObs = conditions$nObs[conditionsMatrix[conditionNumber,4]]


  # true generation model syntax
  if(trueModel == "lcdm"){
    trueModel = eval(quote(syntax.lcdm.correct))
  } else if(trueModel == "dina"){
    trueModel = eval(quote(syntax.dina.correct))
  } else if(trueModel == "crum"){
    trueModel = eval(quote(syntax.crum.correct))
  }


 # prior
  if(prior == "informative"){
     prior = setDefaultPriors(
      normalMean = 0,
      normalVariance = 5,
      normalCovariance = 0,
      dirichletAlpha = 1
    )
  } else if(prior == "uninformative"){
    prior = setDefaultPriors(
      normalMean = 0,
      normalVariance = 1000,
      normalCovariance = 0,
      dirichletAlpha = 1
    )
  }



  return(list(trueModel = trueModel,
              quality = quality,
              prior = prior,
              nObs = nObs
              )
         )

}

