#' Title glm_function
#' 
#' This function fit a glm  witout cross validation procedure
#'
#' @param occurrence a dataframe with all occurrences for all species
#' @param covariates a datagrame containg all covariates to fit the model
#' @param species_name a vector containg the name of all species contain in @param occurrence
#' @param base_dir the path to save the data
#'
#' @return a dataframe with as many row as the length of @param species_name . Each row is a species with its occurrence observation and prediction from each cross validation fold
#' @export
#'
#' @examples
# 

load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/cov_med.RData")
load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/occ_med.RData")
occurrence = occ_med
covariates = cov_med 
base_dir <- "outputs/occurrence_prediction/"

species_names <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/species_names.csv")
species_name <- species_names$species_names

glm_function <- function(occurrence,
                         covariates,
                         species_name,
                         base_dir){

  
  # create raw occurrence object
  species_j <- list()
  raw_occurrence <- occurrence
  
  # Define the model formula with all qualitative variables
  model_formula <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + factor(protection) + factor(principal_substrat)
  
  # Define alternative model formulas if one of the qualitative variables has only one category
  form1 <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + factor(principal_substrat)
  form2 <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + factor(protection)
  form3 <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity
  
  for (j in 1:length(species_name)) {
    # Run the iteration for each species
    species_j[[j]] <- pbmcapply::pbmclapply(1, function(i) {
      
      # select the jth species from the fitting set
      fitting <- raw_occurrence$fitting[,c("id_spygen", species_name[j])]
      
      # add covariates
      fitting <- dplyr::inner_join(fitting, covariates, by = "id_spygen")
      
      # get occurrence data
      occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
      
      # keep only 2 times more absences than observation
      # get absence
      
      n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2
      
      absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]
      
      if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
        
        absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]
        
      }
      
      # combine absence and presence
      occurrence_final <- rbind(occurrence_only, absence_fit)
      
      names(occurrence_final)[names(occurrence_final) == species_name[j]] <- "occurrence"
      
      #Fit the model :
      # Count occurrences of each category in occurrence_final
      # Loop over the unique categories of 'protection'
      for (category in unique(occurrence_final$protection)) {
        # Count occurrences of the category in occurrence_final
        category_counts <- sum(occurrence_final$protection == category)
        
        # Filter out rows associated with this category if it has less than two occurrences
        if (category_counts < 4) {
          occurrence_final <- occurrence_final |>
            dplyr::filter(protection != category)
        }
      }
      
      # Loop over the unique categories of 'principal_substrat'
      for (category in unique(occurrence_final$principal_substrat)) {
        # Count occurrences of the category in occurrence_final
        category_counts <- sum(occurrence_final$principal_substrat == category)
        
        # Filter out rows associated with this category if it has less than two occurrences
        if (category_counts < 4) {
          occurrence_final <- occurrence_final |>
            dplyr::filter(principal_substrat != category)
        }
      }
      
      # Check if either protection or principal_substrat has only one unique value
      if (length(unique(occurrence_final$protection)) == 1 && length(unique(occurrence_final$principal_substrat)) == 1) {
        formula <- form3
      } else if (length(unique(occurrence_final$protection)) == 1) {
        formula <- form1
      } else if (length(unique(occurrence_final$principal_substrat)) == 1) {
        formula <- form2
      } else {
        formula <- model_formula
      }
      
    model_fit <- tryCatch(
      suppressWarnings(glm(formula = formula, family = binomial(link = "probit"), data = occurrence_final)),
      error = function(e) NA
    )
    # model_fit <- tryCatch(glm(formula = model_formula, family = binomial(link = "probit"), data = occurrence_final), error = function(e) NA)
    
    if(!any(is.na(model_fit) == TRUE)){
      
      # select the jth species from the validation set
      validation <- raw_occurrence$validation[,c("id_spygen", species_name[j])]
      
      occurrence_only_val <- validation[which(validation[,species_name[j]] > 0),]
      n_subsample_val <- nrow(validation[which(validation[, species_name[j]] == 0),]) * 2   # same
      absence_val <- validation[which(validation[, species_name[j]] == 0),]   
      
      if(nrow(absence_val) > n_subsample_val) {
        
        replacement_val <- ifelse(length(which(validation[, species_name[j]] == 0)) < n_subsample_val, T, F)
        
        absence_val <- absence_val[sample(which(absence_val[, species_name[j]] == 0), n_subsample_val, replace = FALSE),]
        
      }
      
      occurrence_validation <- rbind(occurrence_only_val, absence_val)
      
      # add covariates
      occurrence_validation <- dplyr::inner_join(validation, covariates, by = "id_spygen")
      names(occurrence_validation)[names(occurrence_validation) == species_name[j]] <- "occurrence"
      
      #test 
      test <- unique(occurrence_validation$protection) %in% unique(occurrence_final$protection) # test if you have the same protection factors in the fitting and validation set
      if(any(test == FALSE)){
        occurrence_validation <- occurrence_validation |>
          dplyr::filter(protection %in% unique(occurrence_final$protection))
      }
      test2 <- unique(occurrence_validation$principal_substrat) %in% unique(occurrence_final$principal_substrat)
      if(any(test2 == FALSE)){
        occurrence_validation <- occurrence_validation |>
          dplyr::filter(principal_substrat %in% unique(occurrence_final$principal_substrat))
      }
      
      validation_predict <- predict(model_fit, occurrence_validation, type = 'response')
      
      # Create data frame with predictions
      validation_predict <- data.frame(id_spygen = occurrence_validation$id_spygen,
                                       validation_predict = validation_predict)
      # Extract observed values from the validation set
      validation_observed <- occurrence_validation[,c("id_spygen", "occurrence")]
      
      # Rename columns for consistency
      validation_observed <- validation_observed |>
        dplyr::rename(validation_observed = occurrence)
      
      # Inner join observed and predicted values
      validation_obs_prd <- validation_predict |>
        dplyr::inner_join(validation_observed, multiple = "first")
      
      validation_obs_prd

    }else{
      validation_obs_prd  <- NA
      } 
      
    }, mc.cores = parallel::detectCores() - 1)
    
    }
  
  # Initialize an empty list to store the tibbles
  extracted_predictions <- list()

  # Iterate over each sublist in species_j
  for (j in 1:length(species_j)) {
    # Extract "validation_observed" and "validation_predict" for the jth species
    validation_observed <- lapply(species_j[[j]], '[[', "validation_observed")
    validation_predict <- lapply(species_j[[j]], '[[', "validation_predict")
    
    # Construct a tibble for the jth species
    tibble_entry <- dplyr::tibble(
      species_name = species_name[j],
      fitted_model = 'GLM',
      validation_observed = validation_observed,
      validation_predict = validation_predict
    )
    
    # Store the tibble in the result list
    extracted_predictions[[j]] <- tibble_entry
  }
  
  # Combine all tibbles into a single tibble
  extracted_predictions <- do.call(dplyr::bind_rows, extracted_predictions)

  # save prediciton output in same file structure

  model_dir <- "glm"

  dir.create(base_dir, recursive = T)

  save(extracted_predictions, file = paste0(base_dir, model_dir, "_extracted_predictions.RData"))

  rm(list=ls())
  gc()

}




















#' Title glm_function
#' 
#' This function fit a glm  with a k fold balanced cross validation procedure
#'
#' @param occurrence a list in which each elements is a fold of the cross validation procededure. Each fold is split into two subset, the first one named "fitting" to
#' train the model and the second one named "validation" to test the model
#' @param covariates a datagrame containg all covariates to fit the model
#' @param species_name a vector containg the name of all species contain in @param occurrence
#' @param base_dir the path to save the data
#'
#' @return a dataframe with as many row as the length of @param species_name . Each row is a species with its occurrence observation and prediction from each cross validation fold
#' @export
#'
#' @examples






# glm_function_cv <- function(occurrence,
#                             covariates,
#                             species_name,
#                             base_dir){
#   
#   species_j <- list()
#   
#   for(i in 1:length(occurrence)) {
#     
#     print(paste0("cv ", i))
#     
#     # create raw occurrence object and select cross validation set i
#     raw_occurrence <- occurrence[[i]]
#     
#     model_formula <- occurrence ~ habitat_div + mean_bathy + logland + chloroDay + tempDay + factor(protection)
#     
#     
#     species_j[[i]] <- pbmcapply::pbmclapply(1:length(species_name), function(j){
#       
#       # select the jth species from the fitting set
#       fitting <- raw_occurrence$fitting[,c("id_spygen", species_name[j])]
#       
#       # add covariates
#       fitting <- dplyr::inner_join(fitting, covariates, by = "Row.names")
#       
#       # select the jth species from the validation set
#       validation <- raw_occurrence$validation[,c("Row.names", species_name[j])]
#       
#       # add covariates
#       validation <- dplyr::inner_join(validation, covariates, by = "Row.names")
#       
#       # get occurrence data
#       occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
#       occurrence_only_val <- validation[which(validation[,species_name[j]] > 0),]
#       
#       
#       # keep only two times more absences than observation
#       # get absence
#       
#       n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2   # how much absences can i put in my model
#       n_subsample_val <- nrow(validation[which(validation[, species_name[j]] == 0),]) * 2   # same
#       
#       absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]     # all the absences
#       absence_val <- validation[which(validation[, species_name[j]] == 0),]   # same
#       
#       if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
#         
#         # replacement_fit <- ifelse(length(which(fitting[, species_name[j]] == 0)) < n_subsample_fit, T, F)   # true if absences < 2 * presences
#         
#         absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]
#         
#       }
#       
#       if(nrow(absence_val) > n_subsample_val) {
#         
#         # replacement_val <- ifelse(length(which(validation[, species_name[j]] == 0)) < n_subsample_val, T, F)
#         
#         absence_val <- absence_val[sample(which(absence_val[, species_name[j]] == 0), n_subsample_val, replace = FALSE),]
#         
#       }
#       
#       # combine absence and presence
#       occurrence_final <- rbind(occurrence_only, absence_fit)
#       occurrence_validation <- rbind(occurrence_only_val, absence_val)
#       
#       names(occurrence_final)[names(occurrence_final) == species_name[j]] <- "occurrence"
#       names(occurrence_validation)[names(occurrence_validation) == species_name[j]] <- "occurrence"
#       
#       #Fit the model :
#       test <- unique(occurrence_validation$protection) %in% unique(occurrence_final$protection) # test if you have the same protection factors in the fitting and validation set
#       if(any(test == FALSE)){
#         occurrence_validation <- occurrence_validation |>
#           dplyr::filter(protection %in% unique(occurrence_final$protection))
#       }
#       # test2 <- unique(occurrence_validation$Confinement) %in% unique(occurrence_final$Confinement)
#       #  if(any(test2 == FALSE)){
#       #   occurrence_validation <- occurrence_validation |>
#       #     dplyr::filter(Confinement %in% unique(occurrence_final$Confinement))
#       #  }
#       
#       model_fit <- tryCatch(glm(formula = model_formula, family = binomial(link = "probit"), method = "ML", data = occurrence_final), error = function(e) NA)
#       
#       if(!any(is.na(model_fit) == TRUE)){
#         
#         validation_predict <- predict(model_fit, occurrence_validation, type = 'response')
#         
#         # Create data frame with predictions
#         validation_predict <- data.frame(Row.names = occurrence_validation$Row.names,
#                                          validation_predict = validation_predict)
#         # Extract observed values from the validation set
#         validation_observed <- occurrence_validation[,c("Row.names", "occurrence")]
#         
#         # Rename columns for consistency
#         validation_observed <- validation_observed |>
#           dplyr::rename(validation_observed = occurrence)
#         
#         # Inner join observed and predicted values
#         validation_obs_prd <- validation_predict |>
#           dplyr::inner_join(validation_observed, multiple = "first")
#         
#         validation_obs_prd
#         
#       }else{
#         
#         validation_obs_prd  <- NA
#         
#       }
#       
#     }, mc.cores = parallel::detectCores() - 1)
#     
#   }
#   
#   validation_prediction <- parallel::mclapply(1:length(species_j[[1]]), function(i){
#     
#     species_i <- lapply(species_j, `[[`, i)
#     
#     species_i_bind <- do.call(rbind, species_i)
#     
#   }, mc.cores = 10)
#   
#   
#   extracted_predictions <- dplyr::tibble(species_name = species_name,
#                                          fitted_model = 'cv_GLM',
#                                          validation_observed = lapply(validation_prediction, '[[', "validation_observed"),
#                                          validation_predict = lapply(validation_prediction, '[[', "validation_predict"))
#   
#   # save prediciton output in same file structure
#   
#   model_dir <- "cv_glm"
#   
#   dir.create(base_dir, recursive = T)
#   
#   save(extracted_predictions, file = paste0(base_dir, model_dir, "_extracted_predictions.RData"))
#   
#   rm(list=ls())
#   gc()
#   
# }
# 
