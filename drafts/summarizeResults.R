# Compare Results ============================================================
# rhat
maxRhatLCDM = singleRep[["resultLCDM"]][["maxRhat"]]
maxRhatDINA = singleRep[["resultDINA"]][["maxRhat"]]
maxRhatCRUM = singleRep[["resultCRUM"]][["maxRhat"]]


# results for item parameter recovery

# grep rows by each item parameter of a generated model
interceptRows = grep(pattern = "(Intercept)", x = rownames(singleRep[["resultTRUE"]][["itemParameter"]]))
A1Rows = grep(pattern = "\\.A1$", x = rownames(singleRep[["resultTRUE"]][["itemParameter"]]))
A2Rows = grep(pattern = "\\.A2$", x = rownames(singleRep[["resultTRUE"]][["itemParameter"]]))
A3Rows = grep(pattern = "\\.A3$", x = rownames(singleRep[["resultTRUE"]][["itemParameter"]]))
A1A2Rows = grep(pattern = "\\.A1:A2$", x = rownames(singleRep[["resultTRUE"]][["itemParameter"]]))
A1A3Rows = grep(pattern = "\\.A1:A3$", x = rownames(singleRep[["resultTRUE"]][["itemParameter"]]))
A2A3Rows = grep(pattern = "\\.A2:A3$", x = rownames(singleRep[["resultTRUE"]][["itemParameter"]]))
A1A2A3Rows = grep(pattern = "\\.A1:A2:A3$", x = rownames(singleRep[["resultTRUE"]][["itemParameter"]]))

# result matrix for each item parameter
interceptResult = matrix(NA, nrow = length(interceptRows), ncol=2, dimnames = list(c(), c("item","true")))
A1Result = matrix(NA, nrow = length(A1Rows), ncol=2, dimnames = list(c(), c("item","true")))
A2Result = matrix(NA, nrow = length(A2Rows), ncol=2, dimnames = list(c(), c("item","true")))
A3Result = matrix(NA, nrow = length(A3Rows), ncol=2, dimnames = list(c(), c("item","true")))
A1A2Result = matrix(NA, nrow = length(A1A2Rows), ncol=2, dimnames = list(c(), c("item","true")))
A2A3Result = matrix(NA, nrow = length(A2A3Rows), ncol=2, dimnames = list(c(), c("item","true")))
A1A3Result = matrix(NA, nrow = length(A1A3Rows), ncol=2, dimnames = list(c(), c("item","true")))
A1A2A3Result = matrix(NA, nrow = length(A1A2A3Rows), ncol=2, dimnames = list(c(), c("item","true")))

itemPara = rownames(singleRep[["resultTRUE"]][["itemParameter"]])

interceptResult[,"item"] = itemPara[grep(pattern = "(Intercept)", x= itemPara)]
interceptResult[,"true"] = singleRep[["resultTRUE"]][["itemParameter"]][interceptRows,1]
interceptResult = merge(x = interceptResult, y = singleRep[["resultLCDM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
interceptResult = merge(x = interceptResult, y = singleRep[["resultDINA"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
interceptResult = merge(x = interceptResult, y = singleRep[["resultCRUM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
colnames(interceptResult) = c("item", "true", "LCDM", "DINA", "CRUM")

A1Result[,"item"] = itemPara[grep(pattern = "\\.A1$", x= itemPara)]
A1Result[,"true"] = singleRep[["resultTRUE"]][["itemParameter"]][A1Rows,1]
A1Result = merge(x = A1Result, y = singleRep[["resultLCDM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A1Result = merge(x = A1Result, y = singleRep[["resultDINA"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A1Result = merge(x = A1Result, y = singleRep[["resultCRUM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
colnames(A1Result) = c("item", "true", "LCDM", "DINA", "CRUM")

A2Result[,"item"] = itemPara[grep(pattern = "\\.A2$", x= itemPara)]
A2Result[,"true"] = singleRep[["resultTRUE"]][["itemParameter"]][A2Rows,1]
A2Result = merge(x = A2Result, y = singleRep[["resultLCDM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A2Result = merge(x = A2Result, y = singleRep[["resultDINA"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A2Result = merge(x = A2Result, y = singleRep[["resultCRUM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
colnames(A2Result) =c("item", "true", "LCDM", "DINA", "CRUM")

A3Result[,"item"] = itemPara[grep(pattern = "\\.A3$", x= itemPara)]
A3Result[,"true"] = singleRep[["resultTRUE"]][["itemParameter"]][A3Rows,1]
A3Result = merge(x = A3Result, y = singleRep[["resultLCDM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A3Result = merge(x = A3Result, y = singleRep[["resultDINA"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A3Result = merge(x = A3Result, y = singleRep[["resultCRUM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
colnames(A3Result) = c("item", "true", "LCDM", "DINA", "CRUM")

A1A2Result[,"item"] = itemPara[grep(pattern = "\\.A1:A2$", x= itemPara)]
A1A2Result[,"true"] = singleRep[["resultTRUE"]][["itemParameter"]][A1A2Rows,1]
A1A2Result = merge(x = A1A2Result, y = singleRep[["resultLCDM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A1A2Result = merge(x = A1A2Result, y = singleRep[["resultDINA"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A1A2Result = merge(x = A1A2Result, y = singleRep[["resultCRUM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
colnames(A1A2Result) = c("item", "true", "LCDM", "DINA", "CRUM")

A2A3Result[,"item"] = itemPara[grep(pattern = "\\.A2:A3$", x= itemPara)]
A2A3Result[,"true"] = singleRep[["resultTRUE"]][["itemParameter"]][A2A3Rows,1]
A2A3Result = merge(x = A2A3Result, y = singleRep[["resultLCDM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A2A3Result = merge(x = A2A3Result, y = singleRep[["resultDINA"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A2A3Result = merge(x = A2A3Result, y = singleRep[["resultCRUM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
colnames(A2A3Result) = c("item", "true", "LCDM", "DINA", "CRUM")

A1A3Result[,"item"] = itemPara[grep(pattern = "\\.A1:A3$", x= itemPara)]
A1A3Result[,"true"] = singleRep[["resultTRUE"]][["itemParameter"]][A1A3Rows,1]
A1A3Result = merge(x = A1A3Result, y = singleRep[["resultLCDM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A1A3Result = merge(x = A1A3Result, y = singleRep[["resultDINA"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A1A3Result = merge(x = A1A3Result, y = singleRep[["resultCRUM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
colnames(A1A3Result) = c("item", "true", "LCDM", "DINA", "CRUM")

A1A2A3Result[,"item"] = itemPara[grep(pattern = "\\.A1:A2:A3$", x= itemPara)]
A1A2A3Result[,"true"] = singleRep[["resultTRUE"]][["itemParameter"]][A1A2A3Rows,1]
A1A2A3Result = merge(x = A1A2A3Result, y = singleRep[["resultLCDM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A1A2A3Result = merge(x = A1A2A3Result, y = singleRep[["resultDINA"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
A1A2A3Result = merge(x = A1A2A3Result, y = singleRep[["resultCRUM"]][["itemParameter"]], by.x = "item", by.y = "item", all.x=TRUE, all.y=FALSE)
colnames(A1A2A3Result) = c("item", "true", "LCDM", "DINA", "CRUM")

# calculate Bias
interceptBiasLCDM = mean(interceptResult$LCDM - as.numeric(interceptResult$true), na.rm = TRUE)
interceptBiasDINA = mean(interceptResult$DINA - as.numeric(interceptResult$true), na.rm = TRUE)
interceptBiasCRUM = mean(interceptResult$CRUM - as.numeric(interceptResult$true), na.rm = TRUE)

A1BiasLCDM = mean(A1Result$LCDM - as.numeric(A1Result$true), na.rm = TRUE)
A1BiasDINA = mean(A1Result$DINA - as.numeric(A1Result$true), na.rm = TRUE)
A1BiasCRUM = mean(A1Result$CRUM - as.numeric(A1Result$true), na.rm = TRUE)

A2BiasLCDM = mean(A2Result$LCDM - as.numeric(A2Result$true), na.rm = TRUE)
A2BiasDINA = mean(A2Result$DINA - as.numeric(A2Result$true), na.rm = TRUE)
A2BiasCRUM = mean(A2Result$CRUM - as.numeric(A2Result$true), na.rm = TRUE)

A3BiasLCDM = mean(A3Result$LCDM - as.numeric(A3Result$true), na.rm = TRUE)
A3BiasDINA = mean(A3Result$DINA - as.numeric(A3Result$true), na.rm = TRUE)
A3BiasCRUM = mean(A3Result$CRUM - as.numeric(A3Result$true), na.rm = TRUE)

A1A2BiasLCDM = mean(A1A2Result$LCDM - as.numeric(A1A2Result$true), na.rm = TRUE)
A1A2BiasDINA = mean(A1A2Result$DINA - as.numeric(A1A2Result$true), na.rm = TRUE)
A1A2BiasCRUM = mean(A1A2Result$CRUM - as.numeric(A1A2Result$true), na.rm = TRUE)

A1A3BiasLCDM = mean(A1A3Result$LCDM - as.numeric(A1A3Result$true), na.rm = TRUE)
A1A3BiasDINA = mean(A1A3Result$DINA - as.numeric(A1A3Result$true), na.rm = TRUE)
A1A3BiasCRUM = mean(A1A3Result$CRUM - as.numeric(A1A3Result$true), na.rm = TRUE)

A2A3BiasLCDM = mean(A2A3Result$LCDM - as.numeric(A2A3Result$true), na.rm = TRUE)
A2A3BiasDINA = mean(A2A3Result$DINA - as.numeric(A2A3Result$true), na.rm = TRUE)
A2A3BiasCRUM = mean(A2A3Result$CRUM - as.numeric(A2A3Result$true), na.rm = TRUE)

A1A2A3BiasLCDM = mean(A1A2A3Result$LCDM - as.numeric(A1A2A3Result$true), na.rm = TRUE)
A1A2A3BiasDINA = mean(A1A2A3Result$DINA - as.numeric(A1A2A3Result$true), na.rm = TRUE)
A1A2A3BiasCRUM = mean(A1A2A3Result$CRUM - as.numeric(A1A2A3Result$true), na.rm = TRUE)


# calculate RMSE
interceptRMSELCDM = sqrt(mean((as.numeric(interceptResult$true) - interceptResult$LCDM)^2, na.rm = TRUE))
interceptRMSEDINA = sqrt(mean((as.numeric(interceptResult$true) - interceptResult$DINA)^2, na.rm = TRUE))
interceptRMSECRUM = sqrt(mean((as.numeric(interceptResult$true)- interceptResult$CRUM)^2, na.rm = TRUE))

A1RMSELCDM = sqrt(mean((as.numeric(A1Result$true) - A1Result$LCDM)^2, na.rm = TRUE))
A1RMSEDINA = sqrt(mean((as.numeric(A1Result$true) - A1Result$DINA)^2, na.rm = TRUE))
A1RMSECRUM = sqrt(mean((as.numeric(A1Result$true) - A1Result$CRUM)^2, na.rm = TRUE))

A2RMSELCDM = sqrt(mean((as.numeric(A2Result$true) - A2Result$LCDM)^2, na.rm = TRUE))
A2RMSEDINA = sqrt(mean((as.numeric(A2Result$true) - A2Result$DINA)^2, na.rm = TRUE))
A2RMSECRUM = sqrt(mean((as.numeric(A2Result$true) - A2Result$CRUM)^2, na.rm = TRUE))

A3RMSELCDM = sqrt(mean((as.numeric(A3Result$true) - A3Result$LCDM)^2, na.rm = TRUE))
A3RMSEDINA = sqrt(mean((as.numeric(A3Result$true) - A3Result$DINA)^2, na.rm = TRUE))
A3RMSECRUM = sqrt(mean((as.numeric(A3Result$true) - A3Result$CRUM)^2, na.rm = TRUE))

A1A2RMSELCDM = sqrt(mean((as.numeric(A1A2Result$true) - A1A2Result$LCDM)^2, na.rm = TRUE))
A1A2RMSEDINA = sqrt(mean((as.numeric(A1A2Result$true) - A1A2Result$DINA)^2, na.rm = TRUE))
A1A2RMSECRUM = sqrt(mean((as.numeric(A1A2Result$true) - A1A2Result$CRUM)^2, na.rm = TRUE))

A1A3RMSELCDM = sqrt(mean((as.numeric(A1A3Result$true) - A1A3Result$LCDM)^2, na.rm = TRUE))
A1A3RMSEDINA = sqrt(mean((as.numeric(A1A3Result$true) - A1A3Result$DINA)^2, na.rm = TRUE))
A1A3RMSECRUM = sqrt(mean((as.numeric(A1A3Result$true) - A1A3Result$CRUM)^2, na.rm = TRUE))

A2A3RMSELCDM = sqrt(mean((as.numeric(A2A3Result$true) - A2A3Result$LCDM)^2, na.rm = TRUE))
A2A3RMSEDINA = sqrt(mean((as.numeric(A2A3Result$true) - A2A3Result$DINA)^2, na.rm = TRUE))
A2A3RMSECRUM = sqrt(mean((as.numeric(A2A3Result$true) - A2A3Result$CRUM)^2, na.rm = TRUE))

A1A2A3RMSELCDM = sqrt(mean((as.numeric(A1A2A3Result$true) - A1A2A3Result$LCDM)^2, na.rm = TRUE))
A1A2A3RMSEDINA = sqrt(mean((as.numeric(A1A2A3Result$true) - A1A2A3Result$DINA)^2, na.rm = TRUE))
A1A2A3RMSECRUM = sqrt(mean((as.numeric(A1A2A3Result$true) - A1A2A3Result$CRUM)^2, na.rm = TRUE))


# results for classification accuracy
jointMAPresult = matrix(NA, nrow = nObs, ncol=4, dimnames = list(c(), c("true", "LCDM", "DINA", "CRUM")))
marginalMAPresult = matrix(NA, nrow = nObs, ncol=12, dimnames = list(c(), c("true.A1", "true.A2", "true.A3",
                                                                            "LCDM.A1", "LCDM.A2", "LCDM.A3","DINA.A1", "DINA.A2", "DINA.A3", "CRUM.A1", "CRUM.A2", "CRUM.A3")))


jointMAPresult[,"true"] = singleRep[["resultTRUE"]][["jointMAP"]][,1]
jointMAPresult[,"LCDM"] = singleRep[["resultLCDM"]][["jointMAP"]][,1]
jointMAPresult[,"DINA"] = singleRep[["resultDINA"]][["jointMAP"]][,1]
jointMAPresult[,"CRUM"] = singleRep[["resultCRUM"]][["jointMAP"]][,1]

marginalMAPresult[, "true.A1"] = singleRep[["resultTRUE"]][["marginalMAP"]][,"A1"]
marginalMAPresult[, "true.A2"] = singleRep[["resultTRUE"]][["marginalMAP"]][,"A2"]
marginalMAPresult[, "true.A3"] = singleRep[["resultTRUE"]][["marginalMAP"]][,"A3"]

marginalMAPresult[, "LCDM.A1"] = singleRep[["resultLCDM"]][["marginalMAP"]][,1]
marginalMAPresult[, "LCDM.A2"] = singleRep[["resultLCDM"]][["marginalMAP"]][,2]
marginalMAPresult[, "LCDM.A3"] = singleRep[["resultLCDM"]][["marginalMAP"]][,3]

marginalMAPresult[, "DINA.A1"] = singleRep[["resultDINA"]][["marginalMAP"]][,1]
marginalMAPresult[, "DINA.A2"] = singleRep[["resultDINA"]][["marginalMAP"]][,2]
marginalMAPresult[, "DINA.A3"] = singleRep[["resultDINA"]][["marginalMAP"]][,3]

marginalMAPresult[, "CRUM.A1"] = singleRep[["resultCRUM"]][["marginalMAP"]][,1]
marginalMAPresult[, "CRUM.A2"] = singleRep[["resultCRUM"]][["marginalMAP"]][,2]
marginalMAPresult[, "CRUM.A3"] = singleRep[["resultCRUM"]][["marginalMAP"]][,3]

# classification rate: joint, marginal
jointClRateLCDM = length(which(jointMAPresult[,"true"] == jointMAPresult[,"LCDM"]))/nObs
jointClRateDINA = length(which(jointMAPresult[,"true"] == jointMAPresult[,"DINA"]))/nObs
jointClRateCRUM = length(which(jointMAPresult[,"true"] == jointMAPresult[,"CRUM"]))/nObs

marginalClRateLCDM = (length(which(marginalMAPresult[,"true.A1"] == marginalMAPresult[,"LCDM.A1"]))+
                        length(which(marginalMAPresult[,"true.A2"] == marginalMAPresult[,"LCDM.A2"]))+
                        length(which(marginalMAPresult[,"true.A3"] == marginalMAPresult[,"LCDM.A3"])))/(3*nObs)

marginalClRateDINA = (length(which(marginalMAPresult[,"true.A1"] == marginalMAPresult[,"DINA.A1"]))+
                        length(which(marginalMAPresult[,"true.A2"] == marginalMAPresult[,"DINA.A2"]))+
                        length(which(marginalMAPresult[,"true.A3"] == marginalMAPresult[,"DINA.A3"])))/(3*nObs)

marginalClRateCRUM = (length(which(marginalMAPresult[,"true.A1"] == marginalMAPresult[,"CRUM.A1"]))+
                        length(which(marginalMAPresult[,"true.A2"] == marginalMAPresult[,"CRUM.A2"]))+
                        length(which(marginalMAPresult[,"true.A3"] == marginalMAPresult[,"CRUM.A3"])))/(3*nObs)


# result
simResults = list()
simResults$maxRhat = list(maxRhatLCDM = maxRhatLCDM, maxRhatDINA = maxRhatDINA, maxRhatCRUM = maxRhatCRUM)
simResults$itemResult = list(interceptResult, A1Result, A2Result, A3Result, A1A2Result, A2A3Result, A1A3Result)
simResults$thetaResult = list(jointMAPresult, marginalMAPresult)
simResults$results = data.frame(interceptBiasLCDM = interceptBiasLCDM, interceptBiasDINA = interceptBiasDINA, interceptBiasCRUM = interceptBiasCRUM,
                                A1BiasLCDM = A1BiasLCDM,  A1BiasDINA = A1BiasDINA,  A1BiasCRUM = A1BiasCRUM,
                                A2BiasLCDM = A2BiasLCDM,  A2BiasDINA = A2BiasDINA,  A2BiasCRUM = A2BiasCRUM,
                                A3BiasLCDM = A3BiasLCDM,  A3BiasDINA = A3BiasDINA,  A3BiasCRUM = A3BiasCRUM,
                                A1A2BiasLCDM = A1A2BiasLCDM,  A1A2BiasDINA = A1A2BiasDINA,  A1A2BiasCRUM = A1A2BiasCRUM,
                                A1A3BiasLCDM = A1A3BiasLCDM,  A1A3BiasDINA = A1A3BiasDINA,  A1A3BiasCRUM = A1A3BiasCRUM,
                                A2A3BiasLCDM = A2A3BiasLCDM,  A2A3BiasDINA = A2A3BiasDINA,  A2A3BiasCRUM = A2A3BiasCRUM,
                                A1A2A3BiasLCDM = A1A2A3BiasLCDM,  A1A2A3BiasDINA = A1A2A3BiasDINA,  A1A2A3BiasCRUM = A1A2A3BiasCRUM,
                                interceptRMSELCDM = interceptRMSELCDM, interceptRMSEDINA = interceptRMSEDINA, interceptRMSECRUM = interceptRMSECRUM,
                                A1RMSELCDM = A1RMSELCDM,  A1RMSEDINA = A1RMSEDINA,  A1RMSECRUM = A1RMSECRUM,
                                A2RMSELCDM = A2RMSELCDM,  A2RMSEDINA = A2RMSEDINA,  A2RMSECRUM = A2RMSECRUM,
                                A3RMSELCDM = A3RMSELCDM,  A3RMSEDINA = A3RMSEDINA,  A3RMSECRUM = A3RMSECRUM,
                                A1A2RMSELCDM = A1A2RMSELCDM,  A1A2RMSEDINA = A1A2RMSEDINA,  A1A2RMSECRUM = A1A2RMSECRUM,
                                A1A3RMSELCDM = A1A3RMSELCDM,  A1A3RMSEDINA = A1A3RMSEDINA,  A1A3RMSECRUM = A1A3RMSECRUM,
                                A2A3RMSELCDM = A2A3RMSELCDM,  A2A3RMSEDINA = A2A3RMSEDINA,  A2A3RMSECRUM = A2A3RMSECRUM,
                                A1A2A3RMSELCDM = A1A2A3RMSELCDM,  A1A2A3RMSEDINA = A1A2A3RMSEDINA,  A1A2A3RMSECRUM = A1A2A3RMSECRUM,
                                jointClRateLCDM = jointClRateLCDM, jointClRateDINA = jointClRateDINA, jointClRateCRUM = jointClRateCRUM,
                                marginalClRateLCDM = marginalClRateLCDM, marginalClRateDINA = marginalClRateDINA, marginalClRateCRUM = marginalClRateCRUM
)

simResults$relativeFit = matrix(c(singleRep[["resultLCDM"]][["relativeFit"]],
                                  singleRep[["resultDINA"]][["relativeFit"]],
                                  singleRep[["resultCRUM"]][["relativeFit"]]),
                                nrow= 3, ncol = 3, dimnames=list(c("LCDM","DINA","CRUM"), c("DIC","WAIC","LOO")))

simResults$relativeFitRank = data.frame(DIC = rank(unlist(simResults[["relativeFit"]][,1])),
                                        WAIC = rank(unlist(simResults[["relativeFit"]][,2])),
                                        LOO = rank(unlist(simResults[["relativeFit"]][,3])))



save(simResults, file = paste0("simResults", arrayNumber, ".RData"))
