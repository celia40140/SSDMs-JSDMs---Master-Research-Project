# function to fit glmm (SPAMM)

# 
#' Title spamm_function
#' 
#' This function fit a glmm using the R package `spaMM` with a k fold spatial cross validation procedure
#'
#' @param occurrence a list in which each elements is a fold of the spatial cross validation procededure. Each fold is split into two subset, the first one named "fitting" to
#' train the model and the second one named "validation" to test the model
#' @param covariates a datagrame containg all covariates to fit the model
#' @param species_name a vector containg the name of all species contain in @param occurrence
#' @param base_dir the path to save the data
#'
#' @return a dataframe with as many row as the length of @param species_name . Each row is a species with its occurrence observation and prediction from each cross validation fold
#' @export
#'
#' @examples


load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/cov_med.RData")
load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/occ_med.RData")
occurrence = occ_med
covariates = cov_med 
base_dir <- "outputs/occurrence_prediction/"

species_names <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/species_names.csv")
species_name <- species_names$species_names


spamm_function <- function(occurrence,
                         covariates,
                         species_name,
                         base_dir){

  # create raw occurrence object
  species_j <- list()
  raw_occurrence <- occurrence

  # Define the model formula with all qualitative variables
  model_formula <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + factor(protection) + factor(principal_substrat) + Matern(1|mid_longitude+mid_latitude)
  
  # Define alternative model formulas if one of the qualitative variables has only one category
  form1 <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + factor(principal_substrat) + Matern(1|mid_longitude+mid_latitude)
  form2 <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + factor(protection) + Matern(1|mid_longitude+mid_latitude)
  form3 <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + Matern(1|mid_longitude+mid_latitude)
  
  for (j in 1:length(species_name)) {
    # Run the iteration for each species
    species_j[[j]] <- pbmcapply::pbmclapply(1, function(i) {

      # select the jth species from the fitting set
      fitting <- raw_occurrence$fitting[,c("id_spygen", species_name[j])]
      
      # add covariates
      fitting <- dplyr::inner_join(fitting, covariates, by = "id_spygen")
      
      # get occurrence data
      occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
      
      # # keep only 2 times more absences than observation
      # # get absence
      n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2
      
      absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]
      
      # # ensure that the selected absence sites maintain the same proportion of categories as the entire absence dataset for both categorical variables
      # # Determine the proportions of categories within the absences dataset for each categorical variable
      # prop_table_var1 <- prop.table(table(absence_fit$your_categorical_variable1))
      # prop_table_var2 <- prop.table(table(absence_fit$your_categorical_variable2))
      # 
      # # Sample absences based on the proportions of categories for each categorical variable
      # selected_absences <- integer(0)
      # for (category_var1 in names(prop_table_var1)) {
      #   for (category_var2 in names(prop_table_var2)) {
      #     # Calculate the number of absences to select for this category combination
      #     n_category_absences_var1 <- min(prop_table_var1[category_var1] * n_subsample_fit, n_subsample_fit)
      #     n_category_absences_var2 <- min(prop_table_var2[category_var2] * n_subsample_fit, n_subsample_fit)
      # 
      #     # If there are absences in this category combination
      #     if (n_category_absences_var1 > 0 && n_category_absences_var2 > 0) {
      #       # Get the indices of absences for this category combination
      #       category_absence_indices <- which(absence_fit$your_categorical_variable1 == category_var1 &
      #                                           absence_fit$your_categorical_variable2 == category_var2)
      # 
      #       # Randomly select absences from this category combination
      #       selected_indices <- sample(category_absence_indices,
      #                                  min(n_category_absences_var1, n_category_absences_var2),
      #                                  replace = FALSE)
      # 
      #       # Add the selected absences to the list
      #       selected_absences <- c(selected_absences, selected_indices)
      #     }
      #   }
      # }
      # 
      # # Subset the absence data using the selected indices
      # balanced_absences <- absence_fit[selected_absences, ]
      
       
      
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
      
      # model_fit <- tryCatch(
      #   suppressWarnings(spaMM::fitme(formula, data = occurrence_final, family = binomial(link = "probit"), method = "PQL/L", control=list(fix_predVar=FALSE))),
      #   error = function(e) NA
      # )
      
      model_fit <- suppressWarnings(
        spaMM::fitme(formula, 
                     data = occurrence_final, 
                     family = binomial(link = "probit"), 
                     method = "PQL/L", 
                     control=list(fix_predVar=FALSE)))
      
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
  
  # # Initialize an empty list to store the tibbles
  # extracted_predictions <- list()
  # 
  # # Iterate over each sublist in species_j
  # for (j in 1:length(species_j)) {
  #   # Extract "validation_observed" and "validation_predict" for the jth species
  #   validation_observed <- lapply(species_j[[j]], '[[', "validation_observed")
  #   validation_predict <- lapply(species_j[[j]], '[[', "validation_predict")
  #   #lignes qui fonctionnent quand pas message d'erreur sinon ajouter [[1]]
  #   
  #   # Construct a tibble for the jth species
  #   tibble_entry <- dplyr::tibble(
  #     species_name = species_name[j],
  #     fitted_model = 'SPAMM',
  #     validation_observed = validation_observed,
  #     validation_predict = validation_predict
  #   )
  #   
  #   # Store the tibble in the result list
  #   extracted_predictions[[j]] <- tibble_entry
  # }
  
  extracted_predictions <- list()
  
  # Iterate over each sublist in species_j
  for (j in 1:length(species_j)) {
    if (length(species_j[[j]]) > 1) {
      # Extract "validation_observed" and "validation_predict" for the first element of the sublist
      validation_observed <- lapply(species_j[[j]][[1]], '[[', "validation_observed")
      validation_predict <- lapply(species_j[[j]][[1]], '[[', "validation_predict")
    } else {
      # Extract "validation_observed" and "validation_predict" for the jth species
        validation_observed <- lapply(species_j[[j]], '[[', "validation_observed")
        validation_predict <- lapply(species_j[[j]], '[[', "validation_predict")
    }
    
    # Construct a tibble for the jth species
    tibble_entry <- dplyr::tibble(
      species_name = species_name[j],
      fitted_model = 'SPAMM',
      validation_observed = validation_observed,
      validation_predict = validation_predict
    )
    
    # Store the tibble in the result list
    extracted_predictions[[j]] <- tibble_entry
  }
  
  
  # Combine all tibbles into a single tibble
  extracted_predictions <- do.call(dplyr::bind_rows, extracted_predictions)
  
  # save prediciton output in same file structure

  model_dir <- "spamm"

  dir.create(base_dir, recursive = T)

  save(extracted_predictions, file = paste0(base_dir, model_dir, "_extracted_predictions.RData"))

  rm(list=ls())
  gc()

}





# spamm_function_cv <- function(occurrence,
#                               covariates,
#                               species_name,
#                               base_dir){
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
#     # create formula with new covariate names
#     fmla <- as.formula(paste0("occurrence ~ ", paste0(colnames(covariates)[!colnames(covariates) %in% c("Row.names", "latitude_start_DD", "longitude_start_DD")], collapse = " + ")))
#     
#     species_j[[i]] <- pbmcapply::pbmclapply(1:length(species_name), function(j){
#       
#       # select the jth species from the fitting set
#       fitting <- raw_occurrence$fitting[,c("Row.names", species_name[j])]
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
#       
#       fitting <- fitting |>
#         dplyr::select("Row.names", "longitude_start_DD", "latitude_start_DD", species_name[j], colnames(med_covariates_no_na)[!colnames(med_covariates_no_na) %in% c("Row.names")]) |>
#         dplyr::rename(X = longitude_start_DD,
#                       Y = latitude_start_DD)
#       
#       validation <- validation |>
#         dplyr::select("Row.names", "longitude_start_DD", "latitude_start_DD", species_name[j], colnames(med_covariates_no_na)[!colnames(med_covariates_no_na) %in% c("Row.names")]) |>
#         dplyr::rename(X = longitude_start_DD,
#                       Y = latitude_start_DD)
#       
#       # get occurrence data
#       occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
#       occurrence_only_val <- validation[which(validation[,species_name[j]] > 0),]
#       
#       if(nrow(occurrence_only_val) == 0){
#         
#         occurrence_only_val <- validation
#         
#       }
#       
#       # keep only two times more absences than observation
#       n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2   # how much absences can i put in my model
#       n_subsample_val <- nrow(validation[which(validation[, species_name[j]] == 0),]) * 2   # same
#       
#       absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]     # all the absences
#       absence_val <- validation[which(validation[, species_name[j]] == 0),]   # same
#       
#       if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
#         
#         absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]
#         
#       }
#       
#       if(nrow(absence_val) > n_subsample_val) {
#         
#         absence_val <- absence_val[sample(which(absence_val[, species_name[j]] == 0), n_subsample_val, replace = FALSE),]
#         
#       }
#       
#       # combine absence and presence
#       occurrence_final <- rbind(occurrence_only, absence_fit) |>
#         as.data.frame()
#       occurrence_validation <- rbind(occurrence_only_val, absence_val) |>
#         as.data.frame()
#       
#       names(occurrence_final)[names(occurrence_final) == species_name[j]] <- "occurrence"
#       names(occurrence_validation)[names(occurrence_validation) == species_name[j]] <- "occurrence"
#       
#       
#       # Fit the model
#       
#       model_fit <- spaMM::fitme(fmla, data = occurrence_final,family = binomial(link = "probit"), method = "REML")
#       
#       
#       if(!any(is.na(model_fit) == TRUE)){
#         
#         validation_predict <- predict(model_fit, occurrence_validation, type = 'response')
#         
#         validation_predict <- data.frame(Row.names = occurrence_validation$Row.names,
#                                          validation_predict = validation_predict)
#         
#         validation_observed <- occurrence_validation[,c("Row.names", "occurrence")]
#         
#         validation_observed <- validation_observed |>
#           dplyr::rename(validation_observed = occurrence)
#         
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
#                                          fitted_model = 'cv_SPAMM',
#                                          validation_observed = lapply(validation_prediction, '[[', "validation_observed"),
#                                          validation_predict = lapply(validation_prediction, '[[', "validation_predict"))
#   
#   # save prediciton output in same file structure
#   
#   model_dir <- "cv_spamm"
#   
#   dir.create(base_dir, recursive = T)
#   
#   save(extracted_predictions, file = paste0(base_dir, model_dir, "_extracted_predictions.RData"))
#   
#   rm(list=ls())
#   gc()
#   
# }


