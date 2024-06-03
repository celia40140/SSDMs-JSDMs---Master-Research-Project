# function to save prediction from hmsc multivariate spatial model

load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/cov_med.RData")
load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/occ_med.RData")
occurrence = occ_med
covariates = cov_med 


base_dir <- "outputs/occurrence_prediction/"

species_names <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/species_names.csv")
species_name <- species_names$species_names

pred_hmsc_multivariate_function <- function(occurrence_validation,
                                            covariates,
                                            species_name,
                                            base_dir){
  
  # then load every models from localDir in a list
  model_file <- "outputs/hmsc_models/shmsc_models/multivariate/shmsc_multi_finalv2.Rdata"

  # Load the model
  load(model_file)
  
  #load dataset
  validation <- occurrence$validation
  
  # because studydesign between fitting and validation must be the same we need to add rows from fitting here
  # Calculate the number of rows to add
  num_rows_to_add <- 432 - nrow(validation)
  
  # Sample the required number of rows from occurrence$fitting
  rows_to_add <- occurrence$fitting[sample(nrow(occurrence$fitting), num_rows_to_add), ]
  
  # Combine the sampled rows with occurrence_validation
  validation <- rbind(validation, rows_to_add)
  
  # add covariates
  occurrence_validation <- dplyr::inner_join(validation, covariates, by = "id_spygen")
  
  # Randomly select 453 rows using slice_sample
  # occurrence_validation <- dplyr::slice_sample(occurrence_validation, n = 432)
  
  occurrence_validation$id_spygen <- as.factor(occurrence_validation$id_spygen)
  
  # create a new set of data from validation set
  # Create X matrix with validation data
  X_val <- occurrence_validation |>
    dplyr::select(colnames(covariates)[!colnames(covariates) %in% c("id_spygen", "mid_latitude", "mid_longitude", "turbidity", "dist_shore")])
  X_val <- as.data.frame(X_val)
  rownames(X_val) <- occurrence_validation$id_spygen
  
  # Create Y matrix with validation data
  Y_val <- occurrence_validation |>
    dplyr::select(-c("id_spygen", "mid_latitude", "mid_longitude", "fishing_pressure","bathymetry", "temperature", "chlorophyll", "salinity", "dist_fully_protected_MPA", "protection", "nb_substrats", "principal_substrat", "gravity", "dist_shore", "turbidity")) |>
    data.matrix()
  rownames(Y_val) <- occurrence_validation$id_spygen
  
  # setting model structure with spatial structure ‘Nearest Neighbour Gaussian Process (NNGP)’ 
  # take the same as initial model so we don't need to create it again here

  studyDesign <- data.frame(spatial = as.factor(occurrence_validation$id_spygen), associations = as.factor(occurrence_validation$id_spygen))

  
  # we set up expected=FALSE because we compute the mean of the predicted occurrences on 100 samples (=occurrence probability)
  pred <- predict(model_fit_mcmc,
                  XData = X_val,
                  Yc = Y_val,
                  mcmcStep = 10, 
                  studyDesign = studyDesign,
                  # ranLevels = list(spatial = rL.nngp),#, associations = rL), #take from model
                  expected = FALSE,
                  predictEtaMean= TRUE,
                  nParallel = 10)

  
  # Step 1: Create matrices for each sublist
  matrices <- lapply(pred, function(sublist) {
    as.matrix(sublist)
  })
  
  # Step 2: Combine matrices column-wise into a single dataset
  final_dataset <- do.call(cbind, matrices)
  
  # Get unique column names (species names)
  unique_species <- unique(colnames(final_dataset))
  
  # Initialize an empty dataframe to store the results
  species_means <- data.frame(row.names = rownames(final_dataset), stringsAsFactors = FALSE)
  
  # Iterate over unique species names
  for (species_name in unique_species) {
    # Get columns corresponding to the current species
    species_columns <- final_dataset[, colnames(final_dataset) == species_name]
    # Compute mean row for the species
    species_mean_row <- rowMeans(species_columns, na.rm = TRUE)
    # Assign mean row to the corresponding species in the result dataframe
    species_means[species_name] <- species_mean_row
  }
  
  # Initialize list to store results
  validation_prediction <- list()
  
  # save final predictions like in others models
  for (i in seq_along(unique_species)) {
    
    validation_predict <- data.frame(id_spygen = occurrence_validation$id_spygen,
                                     validation_predict = species_means[i])
    validation_predict <- validation_predict |>
      dplyr::rename(validation_predict = unique_species[i])
    
    validation_observed <- occurrence_validation[,c("id_spygen", unique_species[i])]
    validation_observed <- validation_observed |>
      dplyr::rename(validation_observed = unique_species[i])
    
    validation_obs_prd <- dplyr::inner_join(validation_predict, validation_observed, by = "id_spygen")
    
    validation_prediction[[i]] <- validation_obs_prd
    
  }
  
  extracted_predictions <- dplyr::tibble(species_name = unique_species,
                                         fitted_model = 'SHMSC',
                                         validation_observed = lapply(validation_prediction, '[[', "validation_observed"),
                                         validation_predict = lapply(validation_prediction, '[[', "validation_predict"))
  
  # save prediction output in same file structure
  
  model_dir <- "shmsc"
  
  dir.create(base_dir, recursive = T)
  
  save(extracted_predictions, file = paste0(base_dir, model_dir, "_extracted_predictions.RData"))
  
  rm(list=ls())
  gc()
  
}

