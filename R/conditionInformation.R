conditionInformation = function(arrayNumber, nReplicationsPerCondition, nCores = 4){
# browser()
  # convert array number to condition number
  conditionNumber = floor((arrayNumber - 1)/nReplicationsPerCondition) + 1

  # create conditions list
  conditions = list(
    trueModel = c("lcdm", "dina", "crum"),
    nAttributes = c(3, 5),
    nObs = c(100, 500, 1000, 2000),
    quality = c("low", "medium", "high"),
    prior = c("uninformative", "informative")
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
  nObs = conditions$nObs[conditionsMatrix[conditionNumber,2]]
  quality = conditions$quality[conditionsMatrix[conditionNumber,3]]
  nAttributes = conditions$nAttributes[conditionsMatrix[conditionNumber,4]]
  prior =  conditions$prior[conditionsMatrix[conditionNumber,5]]


  # true model syntax
  if(nAttributes == 3 & trueModel == "lcdm"){
    trueModel = eval(quote(syntax.lcdm3))
  } else if(nAttributes == 3 & trueModel == "dina"){
    trueModel = eval(quote(syntax.dina3))
  } else if(nAttributes == 3 & trueModel == "crum"){
    trueModel = eval(quote(syntax.crum3))
  } else if(nAttributes == 5 & trueModel == "lcdm"){
    trueModel = eval(quote(syntax.lcdm5))
  } else if(nAttributes == 5 & trueModel == "dina"){
    trueModel = eval(quote(syntax.dina5))
  } else if(nAttributes == 5 & trueModel == "crum"){
    trueModel = eval(quote(syntax.crum5))
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


  # nItems
  trueModel.lines = strsplit(trueModel, "\n")[[1]]
  number = regmatches(trueModel.lines, regexpr("item1-item(\\d+)", trueModel.lines))
  nItems = sub("item1-item", "", number)



  return(list(trueModel = trueModel,
              nAttributes = nAttributes,
              nObs = nObs,
              quality = quality,
              prior = prior,
              # trueParameters = trueParameters,
              nItems = nItems))

}
