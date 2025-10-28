rm(list=ls())
# devtools::load_all(".")
library(abind)

dec2bin = function(decimal_number, nattributes, basevector){
  dec = decimal_number
  profile = matrix(NA, nrow = 1, ncol = nattributes)
  for (i in nattributes:1){
    profile[1,i] =  dec %% basevector[i]
    dec = (dec - dec %% basevector[i])/basevector[i]
  }
  return(profile)
}


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


conditionsCharMatrix = conditionsMatrix

# loop through each column by its name (e.g., "prior", "nObs", etc.)
for (col_name in colnames(conditionsMatrix)) {

  # Get the vector of character labels for the current condition
  # e.g., for "prior", this would be c("uninformative", "informative")
  character_labels = conditions[[col_name]]

  # Get the column of numeric indices from the original matrix
  numeric_indices = conditionsMatrix[, col_name]

  # Replace the numbers in the new matrix's column with the looked-up character labels
  conditionsCharMatrix[, col_name] = character_labels[numeric_indices]
}



#------------------------------------------------------------------
# Function : merge 9 esti model results from 1 true model into an array
#------------------------------------------------------------------

# selected rate matrix and elpd matrix averaging array(models x index x replication) across replication for one new condition
comparisonMat = function(condition, nModels, nReplicationsPerCondition){

  # array(model * index * rep) of a new condition
  indexArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))
  selectedModelArray = array(data = 0, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))

  elpdArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))
  elpdSDArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))

  parArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))
  aarArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))
  selectedParArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))
  selectedAarArray = array(data = NA, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))

  biasMat = matrix(NA, nrow = nModels, ncol = nReplicationsPerCondition)
  rmseMat = matrix(NA, nrow = nModels, ncol = nReplicationsPerCondition)

  maxRhatArray = array(data = 0, dim = c(nModels, 3, nReplicationsPerCondition),dimnames = list(paste0("eModel",1:nModels), c("dic","waic","loo"), paste0("rep",1:nReplicationsPerCondition)))


  for(rep in 1:nReplicationsPerCondition){
    for(model in 1:nModels){

      # load file
      arrayNumber = ((condition-1)*nModels + (model-1)) * nReplicationsPerCondition + rep
      fileName = paste0("result_", arrayNumber,".RData")
      print(fileName)

      # if array does not exist, next
      if(!file.exists(fileName)){
        indexArray[model,,rep] = NA
        elpdArray[model,,rep] = NA
        elpdSDArray[model,,rep] = NA
        selectedModelArray[model,,rep] = NA
        parArray[model,,rep] = NA
        aarArray[model,,rep] = NA
        selectedParArray[model,,rep] = NA
        selectedAarArray[model,,rep] = NA
        biasMat[model,rep] = NA
        rmseMat[model,rep] = NA
        next
      }
      load(file = fileName)

      # if maxRhat is larger than 1.1, next
      if(maxRhat > 1.1){
        maxRhatArray[model,,rep] = 1
        indexArray[model,,rep] = NA
        elpdArray[model,,rep] = NA
        elpdSDArray[model,,rep] = NA
        selectedModelArray[model,,rep] = NA
        parArray[model,,rep] = NA
        aarArray[model,,rep] = NA
        selectedParArray[model,,rep] = NA
        selectedAarArray[model,,rep] = NA
        biasMat[model,rep] = NA
        rmseMat[model,rep] = NA
        next
      }

      #=== 1-1. store three index of one model  ================================
      indexArray[model,,rep] = fit.results$index
      elpdArray[model,,rep] = fit.results$index.elpd

      #=== 3-1. classification accuracy rate of one model ===============
      parArray[model,,rep] = fit.results$caRate[,"caJoint"]
      aarArray[model,,rep] = fit.results$caRate[,"caMarginal"]

      #=== 4-1. Bias/RMSE of one model ==================================
      biasMat[model, rep] = fit.results$evalJointProb[,"biasJointProb"]
      rmseMat[model, rep] = fit.results$evalJointProb[,"rmseJointProb"]
    } # end of model loop

    #=== 2-1. select the model with the min index ================================
    selectedModel  = apply(indexArray[,,rep], 2, function(x) {
      if (all(is.na(x))) {
        return(NA)
      } else {
        return(which(x == min(x, na.rm = TRUE)))
      }
    })
    selectedModelArray[,,rep][cbind(selectedModel, 1:ncol(indexArray))] = 1

    #=== 3-2. classification accuracy rate of selected model ===========
    valid_indices = !is.na(selectedModel)
    selectedParArray[,,rep][cbind(selectedModel[valid_indices], 1:ncol(indexArray))] = parArray[,,rep][cbind(selectedModel[valid_indices], 1:ncol(indexArray))]
    selectedAarArray[,,rep][cbind(selectedModel[valid_indices], 1:ncol(indexArray))] = aarArray[,,rep][cbind(selectedModel[valid_indices], 1:ncol(indexArray))]

  }  # end of replication loop


  #===1-2. mean of elpd across replications =================================================
  indexMat = as.data.frame(apply(indexArray, c(1,2), mean, na.rm = T))
  elpdMat = as.data.frame(apply(elpdArray, c(1,2), mean, na.rm = T))
  elpdSDMat = as.data.frame(apply(elpdArray, c(1,2), sd, na.rm = T))

  #===2-2. the number (percentage) of selected model across replications ====================
  selectedRateMat = as.data.frame(apply(selectedModelArray, c(1,2), sum, na.rm =T))

  #===3-3. mean of classification rate across replications =====================
  mean_na = function(x) {
    # Remove all NA values from the vector
    vals = x[!is.na(x)]

    # Check if the resulting vector has any values left
    if (length(vals) == 0) {
      # If it's empty, return NA
      return(NA)
    } else {
      # Otherwise, calculate the mean of the remaining values
      return(mean(vals))
    }
  }
  selectedParMat = as.data.frame(apply(selectedParArray, c(1,2), mean_na))
  selectedAarMat = as.data.frame(apply(selectedAarArray, c(1,2), mean_na))

  #===3-4. mean of classification rate across replications =====================
  parMat = as.data.frame(apply(parArray, c(1,2), mean, na.rm=T))
  aarMat = as.data.frame(apply(aarArray, c(1,2), mean, na.rm=T))

  #===4-2. mean of Bias and RMSE across replications ============================
  biasMat = as.matrix(apply(biasMat, 1, mean, na.rm=T))
  rmseMat = as.matrix(apply(rmseMat, 1, mean, na.rm=T))
  colnames(biasMat) = colnames(rmseMat) = "Mean"

  #===5-1. the number (percentage) of condition with large max Rhat (or non-convergence) across replications =====
  maxRhatMat = as.data.frame(apply(maxRhatArray, c(1,2), sum, na.rm =T))


  return(list(indexMat = indexMat,
              elpdMat = elpdMat,
              elpdSDMat = elpdSDMat,
              selectedRateMat = selectedRateMat,
              selectedParMat = selectedParMat,
              selectedAarMat = selectedAarMat,
              parMat = parMat,
              aarMat = aarMat,
              biasMat = biasMat,
              rmseMat = rmseMat,
              maxRhatMat = maxRhatMat,
              selectedParArray = selectedParArray,
              selectedAarArray = selectedAarArray
  )
  )
}


#-------------------------------------
# Combine each condition into a list
#-------------------------------------
# elpdMeanList = list()
# elpdSdList = list()
# selectedRateList = list()

indexList = list()
elpdList = list()
elpdSDList = list()
selectedRateList = list()
selectedParList = list()
selectedAarList = list()
parList = list()
aarList = list()
biasList = list()
rmseList = list()
maxRhatList = c()
combinedParArray = NULL
combinedAarArray = NULL
combinedParList = list()
combinedAarList = list()

condition=1
for (condition in 1:nConditions){
  comparison = comparisonMat(condition = condition,
                             nModels = nModels,
                             nReplicationsPerCondition = nReplicationsPerCondition)


  indexList[[condition]] = comparison$indexMat
  elpdList[[condition]] = comparison$elpdMat
  elpdSDList[[condition]] = comparison$elpdSDMat
  selectedRateList[[condition]] = comparison$selectedRateMat
  selectedParList[[condition]] = comparison$selectedParMat
  selectedAarList[[condition]] = comparison$selectedAarMat
  parList[[condition]] = comparison$parMat
  aarList[[condition]] = comparison$aarMat
  biasList[[condition]] = comparison$biasMat
  rmseList[[condition]] = comparison$rmseMat
  maxRhatList[[condition]] = comparison$maxRhatMat
  combinedParArray=abind(combinedParArray, comparison$selectedParArray, along = 1)
  combinedAarArray=abind(combinedAarArray, comparison$selectedAarArray, along = 1)
  combinedParList[[condition]] = comparison$selectedParArray
  combinedAarList[[condition]] = comparison$selectedAarArray
}

#------------------------------------------------------------------------------
# Mean and SD of classification accuracy rate of selected model by each index
#------------------------------------------------------------------------------
library(purrr)
# This code assumes 'combinedParList' and 'conditionsCharMatrix' already exist.
create_summary_table = function(data_list, condition_labels) {

  # --- Part A: Calculate Mean and SD for each condition ---
  summary_df = purrr::map_dfr(data_list, .f = function(condition_array) {
    index_means = apply(condition_array, MARGIN = 2, FUN = mean, na.rm = TRUE)
    index_sds = apply(condition_array, MARGIN = 2, FUN = sd, na.rm = TRUE)
    tibble(
      DIC_Mean = index_means[1], WAIC_Mean = index_means[2], LOO_Mean = index_means[3],
      DIC_SD = index_sds[1],   WAIC_SD = index_sds[2],   LOO_SD = index_sds[3]
    )
  }, .id = "Condition")

  # Combine condition labels with the summary statistics
  results_table = cbind(condition_labels, summary_df)

  # --- Part B: Calculate and structure the GRAND summary row ---
  total_dic_mean = purrr::map(data_list, ~ .x[, 1, ]) %>% unlist() %>% mean(na.rm = TRUE)
  total_waic_mean = purrr::map(data_list, ~ .x[, 2, ]) %>% unlist() %>% mean(na.rm = TRUE)
  total_loo_mean = purrr::map(data_list, ~ .x[, 3, ]) %>% unlist() %>% mean(na.rm = TRUE)

  total_dic_sd = purrr::map(data_list, ~ .x[, 1, ]) %>% unlist() %>% sd(na.rm = TRUE)
  total_waic_sd = purrr::map(data_list, ~ .x[, 2, ]) %>% unlist() %>% sd(na.rm = TRUE)
  total_loo_sd = purrr::map(data_list, ~ .x[, 3, ]) %>% unlist() %>% sd(na.rm = TRUE)

  # Create a single row for both grand mean and grand SD
  total_stat_row = tibble(
    prior = "Total",
    nObs = "Grand_Summary",
    quality = NA,
    trueModel = NA,
    Condition = NA,
    DIC_Mean = total_dic_mean,   WAIC_Mean = total_waic_mean,   LOO_Mean = total_loo_mean,
    DIC_SD = total_dic_sd,       WAIC_SD = total_waic_sd,       LOO_SD = total_loo_sd
  )

  # --- Part C: Bind the summary row and return the final table ---
  results_table = as_tibble(results_table)
  final_table_with_total = dplyr::bind_rows(results_table, total_stat_row)

  return(final_table_with_total)
}



par72_results = create_summary_table(
  data_list = combinedParList,
  condition_labels = conditionsCharMatrix
)

write.csv(par72_results, "table_selectedPar72.csv", row.names = FALSE)

aar_results = create_summary_table(
  data_list = combinedAarList,
  condition_labels = conditionsCharMatrix
)

write.csv(aar_results, "table_selectedAar72.csv", row.names = FALSE)



#-------------------------------------------------------
# organize comparison list into a full condition matrix
#-------------------------------------------------------
index.fullMat = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions/3))
elpd.fullMat = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions/3))
elpdSD.fullMat = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions/3))
selectedRate.fullMat  = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions/3))
selectedPar.fullMat  = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions/3))
selectedAar.fullMat = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions/3))
par.fullMat  = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions/3))
aar.fullMat = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions/3))
eval.fullMat = matrix(NA, ncol = 2*length(conditions$trueModel), nrow = (nModels*nConditions/3))
maxRhat.fullMat = matrix(NA, ncol = 3*length(conditions$trueModel), nrow = (nModels*nConditions/3))

condition = 1
for (condition in 1:nConditions){

  colRemain = (condition %% 3)
  if(colRemain==1){
    col = c(1,2,3)
    evalCol = c(1,2)
  } else if(colRemain==2){
    col = c(4,5,6)
    evalCol = c(3,4)
  } else if(colRemain==0){
    col = c(7,8,9)
    evalCol = c(5,6)
  }

  rowCeiling = ceiling(condition/3)
  row = seq(1+(rowCeiling-1)*nModels,rowCeiling*nModels, by =1)

  index.fullMat[row, col] = as.matrix(indexList[[condition]])
  elpd.fullMat[row, col] = as.matrix(elpdList[[condition]])
  elpdSD.fullMat[row, col] = as.matrix(elpdSDList[[condition]])
  selectedRate.fullMat[row, col] = as.matrix(selectedRateList[[condition]])
  selectedPar.fullMat[row, col] = as.matrix(selectedParList[[condition]])
  selectedAar.fullMat[row, col] = as.matrix(selectedAarList[[condition]])
  par.fullMat[row, col] = as.matrix(parList[[condition]])
  aar.fullMat[row, col] = as.matrix(aarList[[condition]])
  eval.fullMat[row, evalCol] = cbind(biasList[[condition]], rmseList[[condition]])
  maxRhat.fullMat[row, col] = as.matrix(maxRhatList[[condition]])
}
colnames(index.fullMat) =
  colnames(elpd.fullMat) =
  colnames(elpdSD.fullMat) =
  colnames(selectedRate.fullMat) =
  colnames(selectedPar.fullMat) =
  colnames(selectedAar.fullMat) =
  colnames(par.fullMat) =
  colnames(aar.fullMat) =
  colnames(maxRhat.fullMat) =
  c("LCDM_DIC", "LCDM_WAIC", "LCDM_LOO",
    "DINA_DIC", "DINA_WAIC", "DINA_LOO",
    "CRUM_DIC", "CRUM_WAIC", "CRUM_LOO")


#---------------------------------------------
# attach the condition names on the matrices
#---------------------------------------------
# create conditions list
conditionsFull = list(
  prior = c("uninformative", "informative"),
  nObs = c(100, 500, 1000, 2000),
  quality = c("low", "medium", "high"),
  # trueModel = c("lcdm", "dina", "crum"),
  estiModel = c("lcdmC", "lcdmU", "lcdmO",
                "dinaC", "dinaU", "dinaO",
                "crumC", "crumU", "crumO"
  )
)

# number of conditions
nConditionsFull = prod(unlist(lapply(X = conditionsFull, FUN = length)))

# make conditions matrix
conditionsFullMatrix = matrix(NA, nrow = nConditionsFull, ncol = length(conditionsFull))
colnames(conditionsFullMatrix) = names(conditionsFull)

cond=1
for (cond in 1:nConditionsFull){
  conditionsFullMatrix[cond,] = dec2bin(
    decimal_number = cond - 1,
    nattributes = length(conditionsFull),
    basevector = unlist(lapply(X = conditionsFull, FUN = length))
  ) + 1
}

# change the numeric condition matrix to character
conditionsCharMatrix=conditionsFullMatrix
for (col_name in colnames(conditionsFullMatrix)) {
  numeric_indices = conditionsFullMatrix[, col_name]
  name_labels = conditionsFull[[col_name]]
  conditionsCharMatrix[, col_name] = name_labels[numeric_indices]
}

index.fullMat.cond = cbind(conditionsCharMatrix, index.fullMat)
elpd.fullMat.cond = cbind(conditionsCharMatrix, elpd.fullMat)
elpdSD.fullMat.cond = cbind(conditionsCharMatrix, elpdSD.fullMat)
selectedRate.fullMat.cond = cbind(conditionsCharMatrix, selectedRate.fullMat)
selectedPar.fullMat.cond = cbind(conditionsCharMatrix, selectedPar.fullMat)
selectedAar.fullMat.cond = cbind(conditionsCharMatrix, selectedAar.fullMat)
par.fullMat.cond = cbind(conditionsCharMatrix, par.fullMat)
aar.fullMat.cond = cbind(conditionsCharMatrix, aar.fullMat)
eval.fullMat.cond = cbind(conditionsCharMatrix, eval.fullMat)
maxRhat.fullMat.cond = cbind(conditionsCharMatrix, maxRhat.fullMat)


write.csv(index.fullMat.cond, "table_index.csv")
write.csv(elpd.fullMat.cond, "table_elpd.csv")
write.csv(elpdSD.fullMat.cond, "table_elpdSD.csv")
write.csv(selectedRate.fullMat.cond, "table_selectedRate.csv")
write.csv(selectedPar.fullMat.cond, "table_selectedPar.csv")
write.csv(selectedAar.fullMat.cond, "table_selectedAar.csv")
write.csv(par.fullMat.cond, "table_par.csv")
write.csv(aar.fullMat.cond, "table_aar.csv")
write.csv(eval.fullMat.cond, "table_eval.csv")
write.csv(maxRhat.fullMat.cond, "table_maxRhat.csv")

#--------------------------------------------------------
# Mean and SD of selected rate by each index
#--------------------------------------------------------
library(tidyr)
library(dplyr)
# 1. Reshape the data from a wide format to a long format.
#    This makes it easier to group by the model selection index.
long_data = as.data.frame(selectedRate.fullMat.cond) %>%
  pivot_longer(
    cols = ends_with("_DIC") | ends_with("_WAIC") | ends_with("_LOO"),
    names_to = "selection_metric",
    values_to = "selection_count"
  )

# 2. Separate the 'selection_metric' column into the model that was selected
#    and the index that was used (e.g., 'LCDM_DIC' -> 'LCDM' and 'DIC').
separated_data = long_data %>%
  separate(selection_metric, into = c("true_model", "index"), sep = "_")%>%
  mutate(selection_count = as.numeric(as.character(selection_count)))


# 3. Create a 'true_model' column based on the 'estiModel' column.
#    We are only interested in the correctly specified models (ending in 'C').
#    We also standardize the names to match 'selected_model' (e.g., 'lcdmC' -> 'LCDM').
prepared_data = separated_data %>%
  mutate(
    esti_model = substr(estiModel, 1, 4),
    esti_model = case_when(
      esti_model == "lcdm" ~ "LCDM",
      esti_model == "dina" ~ "DINA",
      esti_model == "crum" ~ "CRUM",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(grepl("C$", estiModel))

# 4. Filter for only the rows where the index correctly identified the true model.
correctly_selected_data = prepared_data %>%
  filter(esti_model == true_model)

# 5. Group by the index and calculate the mean and standard deviation of the
#    selection counts, which now represent the correct selection rates.
summary_stats = correctly_selected_data %>%
  group_by(index) %>%
  summarise(
    Mean_Correct_Rate = mean(selection_count, na.rm = TRUE),
    SD_Correct_Rate = sd(selection_count, na.rm = TRUE)
  ) %>%
  # Ensure the factor levels are in a logical order
  mutate(index = factor(index, levels = c("DIC", "WAIC", "LOO"))) %>%
  arrange(index)

reshaped_summary = summary_stats %>%
  pivot_longer(
    cols = c(Mean_Correct_Rate, SD_Correct_Rate),
    names_to = "Statistic",
    values_to = "Value"
  )

# Now, pivot wider, using the index names as the new columns.
final_summary_tidy = reshaped_summary %>%
  pivot_wider(
    names_from = index,
    values_from = Value
  )

write.csv(final_summary_tidy, "table_selectedRate_total.csv")


#---------------------------------------------------------
# Plot of Correct Selection Rate
#---------------------------------------------------------
library(tidyverse)
library(stringr)


selectedRate.fullMat.cond = read.csv("table_selectedRate.csv")

# Reshape the data and apply final label formatting.
results_long_formatted = selectedRate.fullMat.cond %>%
  pivot_longer(
    cols = ends_with("_DIC") | ends_with("_WAIC") | ends_with("_LOO"),
    names_to = c("SelectedModel", "Criterion"),
    names_sep = "_",
    values_to = "SelectionRate"
  ) %>%
  mutate(
    # Create a new column named `True Model` with a space for the facet label.
    `True Model` = str_to_upper(str_sub(estiModel, 1, 4)),
    Q_Spec = str_to_upper(str_sub(estiModel, 5, 5)),

    # Set the desired order for the True Model factor
    `True Model` = factor(`True Model`, levels = c("LCDM", "DINA", "CRUM")),

    # Create the full, desired label for 'prior'
    prior = paste("Prior:", str_to_title(prior)),
    # Re-factor to ensure the order is correct for plotting
    prior = factor(prior, levels = c("Prior: Uninformative", "Prior: Informative")),

    quality = factor(quality, levels = c("low", "medium", "high")),

    # Recode 'LOO' to 'PSIS-LOO' for the legend label
    Criterion = recode(Criterion, "LOO" = "PSIS-LOO")
  ) %>%
  # Filter for correct model selection
  filter(`True Model` == SelectedModel & Q_Spec == "C")


# --- 3. Visualization by Quality Level ---
# The plotting code is updated to remove the redundant labeller.

# Plot for LOW quality data
plot_low_quality = results_long_formatted %>%
  filter(quality == "low") %>%
  ggplot(aes(x = factor(nObs), y = SelectionRate, group = Criterion, color = Criterion)) +
  geom_line(aes(linetype = Criterion), size = 1.2) +
  geom_point(size = 3.5, aes(shape = Criterion)) +
  facet_grid(`True Model` ~ prior) +
  labs(
    # title = "Correct Model Selection Accuracy (Low Quality Data)",
    x = "Sample Size (N)",
    y = "Correct Selection Rate (%)"
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Plot for MEDIUM quality data
plot_medium_quality = results_long_formatted %>%
  filter(quality == "medium") %>%
  ggplot(aes(x = factor(nObs), y = SelectionRate, group = Criterion, color = Criterion)) +
  geom_line(aes(linetype = Criterion), size = 1.2) +
  geom_point(size = 3.5, aes(shape = Criterion)) +
  facet_grid(`True Model` ~ prior) +
  labs(
    # title = "Correct Model Selection Accuracy (Medium Quality Data)",
    x = "Sample Size (N)",
    y = "Correct Selection Rate (%)"
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Plot for HIGH quality data
plot_high_quality = results_long_formatted %>%
  filter(quality == "high") %>%
  ggplot(aes(x = factor(nObs), y = SelectionRate, group = Criterion, color = Criterion)) +
  geom_line(aes(linetype = Criterion), size = 1.2) +
  geom_point(size = 3.5, aes(shape = Criterion)) +
  facet_grid(`True Model` ~ prior) +
  labs(
    # title = "Correct Model Selection Accuracy (High Quality Data)",
    x = "Sample Size (N)",
    y = "Correct Selection Rate (%)"
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )


# --- 4. Display and Save the Plots ---

# Display the plots in RStudio
print(plot_low_quality)
print(plot_medium_quality)
print(plot_high_quality)

# Save each plot to a separate, high-resolution file
ggsave("plot_selectedRate_low.png", plot = plot_low_quality, width = 11, height = 8, dpi = 300)
ggsave("plot_selectedRate_med.png", plot = plot_medium_quality, width = 11, height = 8, dpi = 300)
ggsave("plot_selectedRate_high.png", plot = plot_high_quality, width = 11, height = 8, dpi = 300)

