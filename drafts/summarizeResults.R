rm(list=ls())
devtools::load_all(".")

nReplicationsPerCondition = 100
nModels = 9

# create conditions list
conditions = list(
  prior = c("uninformative", "informative"),
  nObs = c(100, 500, 1000, 2000),
  quality = c("low", "medium", "high"),
  trueModel = c("lcdm", "dina", "crum")
  # estiModel = c("lcdmC", "lcdmU", "lcdmO",
  #               "dinaC", "dinaU", "dinaO",
  #               "crumC", "crumU", "crumO"
  # )
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





# comparison matrix (index, selected rate, selected model's accuracy rate, bias, rmse) averaging array(models x index x replication) across replication for one new condition
comparisonMat = function(condition, nModels, nReplicationsPerCondition){

  # array(model * index * rep) of a new condition
  indexArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))
  selectedModelArray = array(data = 0, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))

  parArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))
  aarArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))
  selectedParArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))
  selectedAarArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))

  biasMat = matrix(NA, nrow = nModels, ncol = nReplicationsPerCondition)
  rmseMat = matrix(NA, nrow = nModels, ncol = nReplicationsPerCondition)

  rep = 1
  model = 1
  # for(rep in 1:nReplicationsPerCondition){
  for(model in 1:nModels){

    # load one file of a new condition
    arrayNumber = ((condition-1) * nModels + (model-1)) * nReplicationsPerCondition + rep
    fileName = paste0("result_", arrayNumber,".RData")
    load(file = fileName)

    #=== 1-1. store three ELPDs of one model  =========================
    indexArray[model,,rep] = fit.results$index

    #=== 3-1. classification accuracy rate of one model ===============
    parArray[model,,rep] = fit.results$caRate[,"caJoint"]
    aarArray[model,,rep] = fit.results$caRate[,"caMarginal"]

    #=== 4-1. Bias/RMSE of one model ==================================
    biasMat[model, rep] = fit.results$evalJointProb[,"biasJointProb"]
    rmseMat[model, rep] = fit.results$evalJointProb[,"rmseJointProb"]

  } # end of model loop

   #=== 2-1. select the model with the max elpd ====================================
   selectedModel  = apply(indexArray[,,rep], 2, which.max)
   selectedModelArray[,, rep][cbind(selectedModel, 1:ncol(indexArray))] = 1

   #=== 3-2. classification accuracy rate of selected model ===========
   selectedParArray[,,rep][cbind(selectedModel, 1:ncol(indexArray))] = parArray[,,rep][cbind(selectedModel, 1:ncol(indexArray))]
   selectedAarArray[,,rep][cbind(selectedModel, 1:ncol(indexArray))] = aarArray[,,rep][cbind(selectedModel, 1:ncol(indexArray))]

  # }  # end of replication loop


  # !!!!!!!DELETE after all replications !!!!!!!!!
  for(rep in 1:nReplicationsPerCondition){
    indexArray[,,rep] = indexArray[,,1]
    selectedModelArray[,,rep] = selectedModelArray[,,1]
    selectedParArray[,,rep] = selectedParArray[,,1]
    selectedAarArray[,,rep] = selectedAarArray[,,1]
    biasMat[,rep] =  biasMat[,1]
    rmseMat[,rep] =  rmseMat[,1]
  }
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  #===1-2. mean of index across replications ====================================
  indexMat = as.data.frame(apply(indexArray, c(1,2), mean))


  #===2-2. percentage of selected model across replications ====================
  selectedRateMat = as.data.frame(100*apply(selectedModelArray, c(1,2), mean))


  #===3-3. mean of classification rate across replications =====================
  selectedParMat = as.data.frame(apply(selectedParArray, c(1,2), mean, na.rm=T))
  selectedAarMat = as.data.frame(apply(selectedAarArray, c(1,2), mean, na.rm=T))


  #==4-2. mean of Bias and RMSE across replications ============================
  biasMat = as.matrix(apply(biasMat, 1, mean, na.rm=T))
  rmseMat = as.matrix(apply(rmseMat, 1, mean, na.rm=T))
  colnames(biasMat) = colnames(rmseMat) = "Mean"


  return(list(indexMat = indexMat,
              selectedRateMat = selectedRateMat,
              selectedParMat = selectedParMat,
              selectedAarMat = selectedAarMat,
              biasMat = biasMat,
              rmseMat = rmseMat)
         )
}



# create a comparison matrix for estimation methods under one new condition
indexList = list()
selectedRateList = list()
selectedParList = list()
selectedAarList = list()
biasList = list()
rmseList = list()
condition=1
for (condition in 1:nConditions){
  comparisonMat = comparisonMat(condition = condition,
                                nModels = nModels,
                                nReplicationsPerCondition = nReplicationsPerCondition)

  indexList[[condition]] = comparisonMat$indexMat
  selectedRateList[[condition]] = comparisonMat$selectedRateMat
  selectedParList[[condition]] = comparisonMat$selectedParMat
  selectedAarList[[condition]] = comparisonMat$selectedAarMat
  biasList[[condition]] = comparisonMat$biasMat
  rmseList[[condition]] = comparisonMat$rmseMat
}



# organize comparison list into a full matrix (refer the format in the excel file)
index.fullMat = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions))
selectedRate.fullMat  = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions))
selectedPar.fullMat  = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions))
selectedAar.fullMat = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions))
eval.fullMat = matrix(NA, ncol = 2*length(conditions$trueModel), nrow = (nModels*nConditions))

condition = 1
for (condition in 1:nConditions){

  colRemain = (condition %% 3)
  if(colRemain==1){ #LCDM
    col = c(1,2,3)
    evalCol = c(1,2)
  } else if(colRemain==2){ #DINA
    col = c(4,5,6)
    evalCol = c(3,4)
  } else if(colRemain==0){ #CRUM
    col = c(7,8,9)
    evalCol = c(5,6)
  }

  rowCeiling = ceiling(condition/3)
  row = seq(1+(rowCeiling-1)*nModels, rowCeiling*nModels, by =1)

  index.fullMat[row, col] = as.matrix(indexList[[condition]])
  selectedRate.fullMat[row, col] = as.matrix(selectedRateList[[condition]])
  selectedPar.fullMat[row, col] = as.matrix(selectedParList[[condition]])
  selectedAar.fullMat[row, col] = as.matrix(selectedAarList[[condition]])

  eval.fullMat[row, evalCol] = cbind(biasList[[condition]], rmseList[[condition]])

}

### need to add handling for cases where non-convergence cases occurs.

