# function to fit Random Forest and assess covariates relative importance


load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/cov_med.RData")
load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/occ_med.RData")
occurrence = occ_med
covariates = cov_med 

species_names <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/species_names.csv")
species_name <- species_names$species_names

base_dir <- "outputs/occurrence_contribution/"


rf_function_cont <- function(occurrence, 
                             covariates, 
                             species_name, 
                             base_dir_cont){
  
  # create raw occurrence object 
  raw_occurrence <- occurrence
  
  # Define the model formula with all qualitative variables
  model_formula <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + protection + principal_substrat
  
  # Define alternative model formulas if one of the qualitative variables has only one category
  form1 <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + principal_substrat
  form2 <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity + protection
  form3 <- occurrence ~ fishing_pressure + nb_substrats + bathymetry + temperature + chlorophyll + salinity + dist_fully_protected_MPA + gravity 
  
  contribution <- pbmcapply::pbmclapply(1:length(species_name), function(j){
    
    # select the jth species from the fitting set
    fitting <- raw_occurrence$fitting[,c("id_spygen", species_name[j])]
    
    # add covariates
    fitting <- dplyr::inner_join(fitting, covariates, by = "id_spygen")
    
    # get occurrence data
    occurrence_only <- fitting[which(fitting[,species_name[j]] > 0),]
    
    
    # keep only two times more absences than observation
    n_subsample_fit <- nrow(fitting[which(fitting[, species_name[j]] > 0),]) * 2   # how much absences can i put in my model
    
    absence_fit <- fitting[which(fitting[, species_name[j]] == 0),]     # all the absences
    
    if(nrow(absence_fit) > n_subsample_fit) {    # if specie j has more absences than 2 x occurrences
      
      absence_fit <- absence_fit[sample(which(absence_fit[, species_name[j]] == 0), n_subsample_fit, replace = FALSE),]
      
    }
    
    # combine absence and presence
    occurrence_final <- rbind(occurrence_only, absence_fit)
    
    names(occurrence_final)[names(occurrence_final) == species_name[j]] <- "occurrence"
    
    # Fit model: (you don't need to explicitly mention that you're building a classification tree within the randomForest function)
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
      suppressWarnings(randomForest::randomForest(x = occurrence_final[, setdiff(colnames(occurrence_final), c("id_spygen", "occurrence", "dist_shore", "turbidity", "mid_longitude", "mid_latitude"))],
                                                  formula = formula,
                                                  y = occurrence_final$occurrence,
                                                  ntree = 1000,
                                                  importance = FALSE,
                                                  norm.votes = TRUE)), error = function(e) NA)
    
    # Use the package DALEX to assess covariates relative importance
    # First create an explain object (a representation of your model, depend on the structure of the algorithm used)
    explainer_rf <- DALEX::explain(model = model_fit,
                                    data = occurrence_final[, !(names(occurrence_final) %in% c("id_spygen","occurrence", "turbidity", "dist_shore", "mid_latitude", "mid_longitude"))],
                                    y = occurrence_final[,"occurrence"],
                                    label = "rf",
                                   type = "classification")
                                 

        # Compute a 25-permutation-based value of the RMSE for all explanatory variables
    vip.25_rf <- DALEX::model_parts(explainer = explainer_rf, 
                                     observed = occurrence_final[,"occurrence"], 
                                     predicted = explainer_rf[["model"]][["predicted"]],
                                     loss_function = DALEX::loss_one_minus_auc,
                                     B = 25,
                                     type = "difference")
    
    # From the model_parts function you get 25 RMSE values for each covariates. 
    # Take the mean and assess the standard-deviation of the RMSE for each covariates to assess the error of the permutation method
    vip.25_rf <- vip.25_rf |> 
      dplyr::mutate(variable = ifelse(variable %in% c("principal_substrat", "nb_substrats"), "substrat", variable)) |> 
      dplyr::mutate(variable = ifelse(variable %in% c("protection", "dist_fully_protected_MPA"), "protection", variable)) |> 
      dplyr::group_by(variable) |> 
      dplyr::summarise(Dropout_loss = mean(dropout_loss),
                       sd_dropout_loss = sd(dropout_loss))
    
    vip.25_rf <- vip.25_rf |> 
      dplyr::filter(!variable %in% c("_baseline_", "_full_model_"))
    
  }, mc.cores = 15)
  
  extracted_contributions <- dplyr::tibble(species_name = species_name, 
                                           fitted_model = "RF", 
                                           # estimate contribution
                                           contributions_and_sd = contribution)
  
  # save contribution output in same file structure
  
  model_dir <- "rf"
  
  dir.create(base_dir)
  
  save(extracted_contributions, file = paste0(base_dir, model_dir, "_extracted_contributions.RData"))
  
  rm(list=ls())
  gc()
  
}
    