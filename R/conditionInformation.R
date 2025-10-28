conditionInformation = function(arrayNumber, nReplicationsPerCondition, nCores = 4){

  # convert array number to condition number
  conditionNumber = floor((arrayNumber - 1)/nReplicationsPerCondition) + 1

  # create conditions list
  conditions = list(
    prior = c("uninformative", "informative"),
    nObs = c(100, 500, 1000, 2000),
    quality = c("low", "medium", "high"),
    trueModel = c("lcdm", "dina", "crum"),
    estiModel = c("lcdmC", "lcdmU", "lcdmO",
                  "dinaC", "dinaU", "dinaO",
                  "crumC", "crumU", "crumO"
                  )
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
  prior =  conditions$prior[conditionsMatrix[conditionNumber,1]]
  nObs = conditions$nObs[conditionsMatrix[conditionNumber,2]]
  quality = conditions$quality[conditionsMatrix[conditionNumber,3]]
  trueModel = conditions$trueModel[conditionsMatrix[conditionNumber,4]]
  estiModel = conditions$estiModel[conditionsMatrix[conditionNumber,5]]



  # true generation model syntax
  if(trueModel == "lcdm"){
    trueModel = eval(quote(syntax.lcdm.correct))
  } else if(trueModel == "dina"){
    trueModel = eval(quote(syntax.dina.correct))
  } else if(trueModel == "crum"){
    trueModel = eval(quote(syntax.crum.correct))
  }


 # prior
   if(prior == "uninformative"){
    prior = setDefaultPriors(
      normalMean = 0,
      normalVariance = 10,
      normalCovariance = 0,
      dirichletAlpha = 1
    )
   } else if(prior == "informative"){
     prior = setDefaultPriors(
       normalMean = 0,
       normalVariance = 1,
       normalCovariance = 0,
       dirichletAlpha = 1
     )
   }


  # estimation model
  if(estiModel == "lcdmC"){
    estiModel = eval(quote(syntax.lcdm.correct))
  } else if(estiModel == "lcdmU"){
    estiModel = eval(quote(syntax.lcdm.under))
  } else if(estiModel == "lcdmO"){
    estiModel = eval(quote(syntax.lcdm.over))
  } else if(estiModel == "dinaC"){
    estiModel = eval(quote(syntax.dina.correct))
  } else if(estiModel == "dinaU"){
    estiModel = eval(quote(syntax.dina.under))
  } else if(estiModel == "dinaO"){
    estiModel = eval(quote(syntax.dina.over))
  } else if(estiModel == "crumC"){
    estiModel = eval(quote(syntax.crum.correct))
  } else if(estiModel == "crumU"){
    estiModel = eval(quote(syntax.crum.under))
  } else if(estiModel == "crumO"){
    estiModel = eval(quote(syntax.crum.over))
  }


  return(list(
    prior = prior,
    nObs = nObs,
    quality = quality,
    trueModel = trueModel,
    estiModel  = estiModel
    )
    )

}

