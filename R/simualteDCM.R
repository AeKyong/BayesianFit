# generate data and get a response data, itemParameter, jointMAP, and marginalMAP

simualteDCM = function(trueModel, nAttributes, nObs, quality, nItems){

  # item names
  itemNames = paste0("item", 1:nItems)

  # true joint probability
  if(nAttributes == 3){
    trueParameters = createParameterVector(modelText = trueModel)
    trueParameters[grep(pattern = "joint.prob000", x = names(trueParameters))] = .293
    trueParameters[grep(pattern = "joint.prob001", x = names(trueParameters))] = .075
    trueParameters[grep(pattern = "joint.prob010", x = names(trueParameters))] = .075
    trueParameters[grep(pattern = "joint.prob011", x = names(trueParameters))] = .054
    trueParameters[grep(pattern = "joint.prob100", x = names(trueParameters))] = .075
    trueParameters[grep(pattern = "joint.prob101", x = names(trueParameters))] = .054
    trueParameters[grep(pattern = "joint.prob110", x = names(trueParameters))] = .054
    trueParameters[grep(pattern = "joint.prob111", x = names(trueParameters))] = .321
  } else if(nAttributes == 5){
    trueParameters = createParameterVector(modelText = trueModel)
    trueParameters[grep(pattern = "joint.prob00000", x = names(trueParameters))] = 0.2427
    trueParameters[grep(pattern = "joint.prob00001", x = names(trueParameters))] = 0.0285
    trueParameters[grep(pattern = "joint.prob00010", x = names(trueParameters))] = 0.0285
    trueParameters[grep(pattern = "joint.prob00011", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob00100", x = names(trueParameters))] = 0.0285
    trueParameters[grep(pattern = "joint.prob00101", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob00110", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob00111", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob01000", x = names(trueParameters))] = 0.0285
    trueParameters[grep(pattern = "joint.prob01001", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob01010", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob01011", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob01100", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob01101", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob01110", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob01111", x = names(trueParameters))] = 0.0286
    trueParameters[grep(pattern = "joint.prob10000", x = names(trueParameters))] = 0.0285
    trueParameters[grep(pattern = "joint.prob10001", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob10010", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob10011", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob10100", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob10101", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob10110", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob10111", x = names(trueParameters))] = 0.0286
    trueParameters[grep(pattern = "joint.prob11000", x = names(trueParameters))] = 0.0109
    trueParameters[grep(pattern = "joint.prob11001", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob11010", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob11011", x = names(trueParameters))] = 0.0286
    trueParameters[grep(pattern = "joint.prob11100", x = names(trueParameters))] = 0.0114
    trueParameters[grep(pattern = "joint.prob11101", x = names(trueParameters))] = 0.0286
    trueParameters[grep(pattern = "joint.prob11110", x = names(trueParameters))] = 0.0286
    trueParameters[grep(pattern = "joint.prob11111", x = names(trueParameters))] = 0.2415
  }



  # true item quality
  if(quality == "low"){
    trueParameters[grep(pattern = "(Intercept)", x = names(trueParameters))] = -0.6
    trueParameters[str_count(names(trueParameters), c("A"))==1] = 1.02
    trueParameters[str_count(names(trueParameters), c("A"))==2] = -0.04
    trueParameters[str_count(names(trueParameters), c("A"))==3] = 4.58
  } else if(quality == "medium"){
    trueParameters[grep(pattern = "(Intercept)", x = names(trueParameters))] = -1.1
    trueParameters[str_count(names(trueParameters), c("A"))==1] = 1.3
    trueParameters[str_count(names(trueParameters), c("A"))==2] = 0.23
    trueParameters[str_count(names(trueParameters), c("A"))==3] = 3.4
  } else if(quality == "high"){
    trueParameters[grep(pattern = "(Intercept)", x = names(trueParameters))] = -2.0
    trueParameters[str_count(names(trueParameters), c("A"))==1] = 2.0
    trueParameters[str_count(names(trueParameters), c("A"))==2] = 1.0
  }

  # trueParameters
  trueParameters = eval(quote(trueParameters))

  # generate data
  generatedData = blatentSimulate(modelText = trueModel, nObs = nObs, paramVals = trueParameters)

  # extract respondents' joint/marginal MAP, response data
  trueJointMAP = as.data.frame(generatedData$data["joint"])
  trueMarginalMAP = generatedData$data[c(grep("A", names(generatedData$data)))]
  responseData = generatedData$data[itemNames]



  return(list(trueParameters = trueParameters,
              trueJointMAP = trueJointMAP,
              trueMarginalMAP = trueMarginalMAP,
              responseData = responseData

             )
         )
}


