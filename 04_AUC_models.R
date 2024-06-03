#' Title AUC_models_functions
#' 
#' This function is for models evaluations with AUC (ROC curve), Precision and Recall.
#' We want to calculate AUC value for all species using validation set and pROC::roc function
#' For imbalanced classification with a severe skew and few examples of the minority class, the ROC AUC can be misleading and may provide an excessively optimistic view of the performance.
#' AUC per model are calculated with the mean AUC of all species and correspond to the macro-weighted average (hence treating all classes equally). 
#' Finally, we compare the mean AUC of all models
#' we also measure Precision and Recall using multiROC::cal_confus function and TP/TP+FP and TP/TP+FN formulas respectively.
#' Both the precision and the recall are focused on the positive class (the minority class) and are unconcerned with the true negatives (majority class) and make possible to assess the performance of a classifier on the minority class..


# recall = sensitivity = true positive rate = TP/TP+FN -> presence identify correctly by model
# high recall -> your model is sensitive, but it may include some false positives


# precision = what proportion of positive prediction 
# high precision -> your model is accurate, but it may miss some positives

#################################################################################
######################   AUC measure and plot   #################################

species_names <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/species_names.csv")
species_name <- species_names$species_names

elasmo_names <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/elasmo_names.csv")
elasmo_names <- elasmo_names$elasmo_names



# base_dir = "outputs/auc/"

# load files 
output_files <- list.files("outputs/occurrence_prediction", full.names = T)

load_outputs <- lapply(1:length(output_files), function(i) {
  
  load(output_files[i])
  assign(unique(extracted_predictions$fitted_model), extracted_predictions)
  
})

# Create an empty list to store results for each model
model_i <- list()

# Iterate over each model output
for(i  in 1:length(load_outputs)) {
  
  # Extract the current model output
  model_output <- load_outputs[[i]]
  
  
  # Create an empty list to store results for each species
  all_species <- list()
  
  # Iterate over each species
  for(j in 1:length(species_name)) {
    
    # Extract observed and predicted values for the current species
    validation_observed <- unlist(model_output[j, "validation_observed"])
    validation_predict <- unlist(model_output[j, "validation_predict"])
      
    # Check if at least one occurrence has a value of "1" in the validation_observed column
    if ("1" %in% validation_observed) {
      # Create a data frame for the current species
      species_data <- data.frame(
        validation_observed = validation_observed,
        validation_predict = validation_predict,
        row.names = NULL  # Suppress row names
      )
      
      species_data <- species_data |>
        dplyr::mutate(validation_observed = factor(validation_observed, levels = c("0", "1")))
      

 
    
    pr <- yardstick::pr_auc(data=species_data, 
                             truth=validation_observed, 
                             estimator="binary", 
                             event_level="second", 
                             validation_predict)

    pr_auc <- pr$.estimate
    
    
    roc <- yardstick::roc_auc(data=species_data, 
                             truth=validation_observed, 
                             estimator="binary", 
                             event_level="second", 
                             validation_predict)
    
    roc_auc <- roc$.estimate
  
    
    # # Compute AUC and CI for the current species
    roc_obj <- pROC::roc(validation_observed ~ validation_predict, data=species_data, algorithm= "4")
    auc_value <- pROC::auc(roc_obj)
    # ci_value <- pROC::ci.auc(roc_obj)
    
    # PRROC::pr.curve(scores.class0= validation_observed,
    #                 scores.class1=validation_observed,
    #                 weights.class0= validation_predict,
    #                 weights.class1=1-validation_predict)
    # PRROC::roc.curve(scores.class0= validation_observed,
    #                 scores.class1=validation_observed,
    #                 weights.class0= validation_predict,
    #                 weights.class1=1-validation_predict,
    #                 max.compute=TRUE,
    #                 min.compute=TRUE)

    
    
    # # Compute Prevalence and detection rate
    # # Convert predicted values to binary class labels
    # binary_predictions <- as.factor(ifelse(validation_predict > 0.5, 1, 0))
    # 
    # #data
    # validation_observed <- as.factor(validation_observed)
    # validation_predict2 <- as.factor(binary_predictions)
    # 
    # # Compute confusion matrix
    # confusion <- caret::confusionMatrix(data = validation_predict2, reference = validation_observed, positive = "1")
    # 
    # prevalence <- confusion$byClass[["Prevalence"]]
    # detection_rate <- confusion$byClass[["Detection Rate"]]
    # # precision <- confusion$byClass[["Precision"]]
    # # sensitivity <- confusion$byClass[["Sensitivity"]]
    # # balanced_accuracy <- confusion$byClass[["Balanced Accuracy"]]
     
    
    
    # Store results for the current species
    species_result <- list(
      species_name = unique(species_name[j]),
      fitted_model = unique(model_output$fitted_model),
      roc_auc = roc_auc,
      pr_auc = pr_auc,
      auc_value =auc_value
      # prevalence = prevalence,
      # detection_rate = detection_rate
      # precision = precision,
      # sensitivity = sensitivity,
      # balanced_accuracy = balanced_accuracy
    )
    
    } else {
      # Move on to the next species if there are no occurrences with a value of "1"
      next
    }
    
    
    # Append results for the current species to the list
    all_species[[j]] <- species_result
  }
  
  
  # Compute mean AUC for all species within the current model
  model_roc_auc <- sapply(all_species, function(species) species$roc_auc)
  mean_roc_auc <- mean(model_roc_auc, na.rm = TRUE)
  
  model_auc_value <- sapply(all_species, function(species) species$auc_value)
  mean_auc_value <- mean(model_auc_value, na.rm = TRUE)
  
  model_pr_auc <- sapply(all_species, function(species) species$pr_auc)
  mean_pr_auc <- mean(model_pr_auc, na.rm = TRUE)
  
  # model_prevalence <- sapply(all_species, function(species) species$prevalence)
  # mean_prevalence <- mean(model_prevalence, na.rm = TRUE)
  # 
  # model_detectionRate <- sapply(all_species, function(species) species$detection_rate)
  # mean_detectionRate <- mean(model_detectionRate, na.rm = TRUE)
  
  model_name <- unique(model_output$fitted_model)
  
  # Store results for the current model
  model_i[[i]] <- list(
    all_species = all_species,
    mean_roc_auc = mean_roc_auc,
    mean_pr_auc = mean_pr_auc,
    mean_auc_value = mean_auc_value,
    # mean_prevalence=mean_prevalence,
    # mean_detectionRate=mean_detectionRate,
    model_name = model_name
  )
}


library(ggplot2)
library(dplyr)

# Create an empty list to store combined data for all models
combined_data <- list()


# Iterate over each model
for (i in 1:length(model_i)) {
  
  # Create an empty list to store combined data for the current model
  model_data <- list()
  
  # Iterate over each species within the current model
  for (j in 1:length(model_i[[i]]$all_species)) {
    
    # Extract data for the current specie
    species_data <- model_i[[i]]$all_species[[j]]
    
    if (!is.null(species_data)) {
  
    # Extract values
    roc_auc_value <- species_data$roc_auc
    pr_auc_value <- species_data$pr_auc
    # prevalence_value <- species_data$prevalence
    # detectionRate_value <- species_data$detection_rate
    
    # Create a data frame for the current species
    species_df <- data.frame(
      Model = model_i[[i]]$model_name,
      Species = species_data$species_name,
      roc_AUC = roc_auc_value,
      pr_AUC = pr_auc_value
      # Prevalence = prevalence_value,
      # DetectionRate = detectionRate_value
    )
    
    # Append data for the current species to the list
    model_data[[j]] <- species_df
    
    } else {
      
      next 
    }
  }
  
  # Combine data for all species within the current model into a single data frame
  model_data <- dplyr::bind_rows(model_data)
  
  # Add data for the current model to the list
  combined_data[[i]] <- model_data
}


# Combine data for all models into a single data frame
combined_data <- dplyr::bind_rows(combined_data)

# reorganize dataset for ggplot
# combined_data$Model <- factor(combined_data$Model, levels = c("cv_GLM", "GLM", "cv_GAM", "GAM", "cv_GBM", "cv_SPAMM", "SPAMM", "cv_RF", "RF", "cv_SPRF", "SPRF", "cv_HMSC", "HMSC"))
combined_data$Model <- factor(combined_data$Model, levels = c("GLM", "SPAMM", "RF", "SRF", "HMSC", "SHMSC"))

auc_elasmo <- combined_data |>
  filter(Species %in% elasmo_names)
readr::write_csv(auc_elasmo, "auc_elasmo.csv")

# Group by Model and calculate mean AUC and CI for each group

model_summary <- combined_data |>
  dplyr::group_by(Model) |>
  dplyr::summarise(mean_roc_AUC = mean(roc_AUC, na.rm = TRUE),
                   mean_pr_AUC = mean(pr_AUC, na.rm = TRUE)
                   # mean_Prevalence = mean(Prevalence, na.rm = TRUE),
                   # mean_Detection_Rate = mean(DetectionRate, na.rm = TRUE),
  )


# Define a named vector mapping model names to colors
# model_colors <- c(cv_GAM = "#d18975", GAM = "#d18975", cv_GLM ="lightgrey", GLM ="lightgrey", cv_SPAMM ="#758bd1", SPAMM ="#758bd1", cv_GBM="", GBM="", cv_RF = "#8fd175", RF = "#8fd175", cv_SPRF="#DDCC77", SPRF="#DDCC77", cv_HMSC= "purple" , HMSC ="purple" )
model_colors <- c(GLM = "#758bd1", SPAMM = "lightgrey", RF="#758bd1", SRF="lightgrey", HMSC="#758bd1", SHMSC="lightgrey")

# Add more colors as needed
ci_colors <- c(GLM = "black", SPAMM = "black", RF = "black", SRF = "black", HMSC = "black", SHMSC = "black")


library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming 'combined_data' is your original dataframe
# Reshape the data to long format
long_data <- combined_data |>
  pivot_longer(cols = c(roc_AUC, pr_AUC), 
               names_to = "Metric", values_to = "Value")

# Plotting
ggplot(long_data, aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.6) +
  geom_jitter(color = "black", size = 1, alpha = 0.5, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.25)) +
  geom_point(data = subset(long_data, Species %in% elasmo_names),
             aes(x = Model, y = Value), color = "red", size = 1,
  alpha = 0.8, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), show.legend = FALSE) +
  facet_wrap(~ Metric, scales = "free_x") +
  scale_fill_manual(values = model_colors) +
  labs(x = "Models", y = "Value", fill = "") +
  ggtitle("Comparison of Metrics by Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(values = model_colors) +
  scale_y_continuous(breaks = seq(0, max(long_data$Value, na.rm = TRUE), by = 0.1))


library(ggplot2)

ggplot(long_data, aes(x = Model, y = Value, fill = Model)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.6) +
  geom_jitter(color = "black", size = 1, alpha = 0.5, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.25)) +
  geom_point(data = subset(long_data, Species %in% elasmo_names),
             aes(x = Model, y = Value), color = "red", size = 1,
             alpha = 0.8, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), show.legend = FALSE) +
  facet_wrap(~ Metric, scales = "free_x") +
  scale_fill_manual(values = model_colors) +
  labs(x = "Models", y = "Value") +
  # ggtitle("Comparison of Metrics by Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_y_continuous(breaks = seq(0, max(long_data$Value, na.rm = TRUE), by = 0.1)) +
  guides(fill = FALSE, color = FALSE)













#' Title AUC_models_functions
#' 
#' This function is for models evaluations with AUC (ROC curve).
#' We want to calculate AUC value for all species and compare cv with model fitted without cv
#' AUC per model are calculated with the mean AUC of all species and correspond to the macro-weighted average (hence treating all classes equally). 
#' Finally, we compare the mean AUC of all models with or without cross validation step.


#################################################################################
######################   AUC measure and plot   #################################

load("data/derived_data/med_covariates_no_na.RData")
load("data/derived_data/occ_bcv_2.RData")
load("data/derived_data/biodivmed_occ_2.RData")
load("data/derived_data/med_biodiv_2.RData")

species_name = colnames(biodivmed_occ_2)[!colnames(biodivmed_occ_2) %in% c("Row.names", "Confinement", "protection", "transect","habitat_principal", "latitude_start_DD", "longitude_start_DD", "R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]
specieshmsc <- c("Anthias_anthias", "Apogon_imberbis", "Belone_belone", "Boops_boops", "Ceratoscopelus_maderensis", "Chelon_auratus", "Chelon_labrosus", "Chelon_ramada", "Chromis_chromis", "Conger_conger")
base_dir = "outputs/auc/"

# load files 
output_files <- list.files("outputs/occurrence_prediction", full.names = T)

load_outputs <- lapply(1:length(output_files), function(i) {
  
  load(output_files[i])
  assign(unique(extracted_predictions$fitted_model), extracted_predictions)
  
})


# Create an empty list to store results for each model
model_i <- list()

# Iterate over each model output
for(i  in 1:length(load_outputs)) {
  
  # Extract the current model output
  model_output <- load_outputs[[i]]
  
  # Create an empty list to store results for each species
  all_species_species <- list()  # For species from species_name
  all_species_specieshmsc <- list()  # For species from specieshmsc
  
  # Check if the current model output corresponds to load output 4 or 11
  if (i %in% c(4, 11)) {
    # Handle load output 4 and 11 differently
    
    # Iterate over each species in specieshmsc
    for(j in 1:length(specieshmsc)) {
      
      # Extract observed and predicted values for the current species
      validation_observed <- unlist(model_output[j, "validation_observed"])
      validation_predict <- unlist(model_output[j, "validation_predict"])
      
      # Create a data frame for the current species
      species_data <- data.frame(
        species_name = rep(specieshmsc[j], length(validation_observed)),
        fitted_model = rep(unique(model_output$fitted_model), length(validation_observed)),
        validation_observed = validation_observed,
        validation_predict = validation_predict,
        row.names = NULL  # Suppress row names
      )
      
      # Compute AUC and CI for the current species
      roc_obj <- pROC::roc(species_data$validation_observed, species_data$validation_predict)
      auc_value <- pROC::auc(roc_obj)
      ci_value <- pROC::ci.auc(roc_obj)
      
      # Store results for the current species in a named list
      species_result <- list(
        species_name = unique(specieshmsc[j]),
        fitted_model = unique(model_output$fitted_model),
        auc = auc_value,
        ci = ci_value
      )
      
      # Append results for the current species to the list
      all_species_specieshmsc[[j]] <- species_result
    }
  } else {
    # Handle other load outputs
    
    # Iterate over each species in species_name
    for(j in 1:length(species_name)) {
      
      # Extract observed and predicted values for the current species
      validation_observed <- unlist(model_output[j, "validation_observed"])
      validation_predict <- unlist(model_output[j, "validation_predict"])
      
      # Create a data frame for the current species
      species_data <- data.frame(
        species_name = rep(species_name[j], length(validation_observed)),
        fitted_model = rep(unique(model_output$fitted_model), length(validation_observed)),
        validation_observed = validation_observed,
        validation_predict = validation_predict,
        row.names = NULL  # Suppress row names
      )
      
      # Compute AUC and CI for the current species
      roc_obj <- pROC::roc(species_data$validation_observed, species_data$validation_predict)
      auc_value <- pROC::auc(roc_obj)
      ci_value <- pROC::ci.auc(roc_obj)
      
      # Store results for the current species in a named list
      species_result <- list(
        species_name = unique(species_name[j]),
        fitted_model = unique(model_output$fitted_model),
        auc = auc_value,
        ci = ci_value
      )
      
      # Append results for the current species to the list
      all_species_species[[j]] <- species_result
    }
  }
  
  # Combine lists for both species sets into a single list
  all_species_combined <- c(all_species_species, all_species_specieshmsc)
  
  # Compute mean AUC for all species within the current model
  model_auc_combined <- sapply(all_species_combined, function(species) species$auc)
  mean_auc_combined <- mean(model_auc_combined, na.rm = TRUE)
  model_name_combined <- unique(model_output$fitted_model)
  
  # Store results for the current model
  model_i[[i]] <- list(
    all_species_combined = all_species_combined,
    mean_auc_combined = mean_auc_combined,
    model_name_combined = model_name_combined
  )
}


library(ggplot2)
library(dplyr)

# Create an empty list to store combined data for all models
combined_data <- list()


# Iterate over each model
for (i in 1:length(model_i)) {

  # Create an empty list to store combined data for the current model
  model_data <- list()


  # Iterate over each species within the current model
  for (j in 1:length(model_i[[i]]$all_species)) {

    # Extract data for the current specie
    species_data <- model_i[[i]]$all_species[[j]]


    # Extract AUC and CI values
    auc_value <- as.numeric(sub(".*: (\\d+\\.\\d+)-.*", "\\1", species_data$auc))
    ci_value <- as.numeric(sub(".*-(\\d+\\.\\d+).*", "\\1", species_data$ci))

    # Create a data frame for the current species
    species_df <- data.frame(
              Model = paste0(model_i[[i]]$model_name),
              Species = species_data$species_name,
              AUC = auc_value,
              CI_lower = ci_value[1],
              CI_upper = ci_value[3]
            )

    # Append data for the current species to the list
    model_data[[j]] <- species_df
          }

  # Combine data for all species within the current model into a single data frame
  model_data <- bind_rows(model_data)

  # Add data for the current model to the list
  combined_data[[i]] <- model_data
  }

        
# Combine data for all models into a single data frame
combined_data <- bind_rows(combined_data)

# reorganize dataset for ggplot
combined_data$Model <- factor(combined_data$Model, levels = c("cv_GLM", "GLM", "cv_GAM", "GAM", "cv_SPAMM", "SPAMM","cv_GBM","GBM", "cv_RF", "RF", "cv_SPRF", "SPRF", "cv_HMSC_univ", "HMSC_univ"))
        
# Group by Model and calculate mean AUC and CI for each group
        
model_summary <- combined_data |>
  dplyr::group_by(Model) |>
  summarise(mean_AUC = mean(AUC, na.rm = TRUE),
            mean_upper_CI = mean(CI_upper, na.rm = TRUE),
            mean_lower_CI = mean(CI_lower, na.rm = TRUE)
          )

# Define a named vector mapping model names to colors
model_colors <- c(cv_GAM = "#BEAED4", GAM = "#BEAED4", cv_GLM ="#FC8D62", GLM ="#FC8D62", cv_SPAMM ="#7FC97F", SPAMM ="#7FC97F", cv_GBM="#FFD92F", GBM="#FFD92F", cv_RF = "#386CB0", RF = "#386CB0", cv_SPRF="#FF7F00", SPRF="#FF7F00",cv_HMSC_univ= "#E78AC3" , HMSC_univ ="#E78AC3" ) 
# Add more colors as needed
ci_colors <- c(cv_GAM = "black", GAM = "black", cv_GLM ="black", GLM ="black", cv_SPAMM ="black", SPAMM ="black", cv_GBM = "black", GBM = "black", cv_RF = "black", RF = "black", cv_SPRF="black", SPRF="black", cv_HMSC_univ = "black" , HMSC_univ ="black" )  # Add more colors as needed
        
# create a vector with species name we want to highlight
# special_species <- c("Belone_belone", "Boops_boops")
    

# library(RColorBrewer)
display.brewer.pal(8, "Paired")
color_palette <- brewer.pal(8, "Paired")
"#7FC97F" "#BEAED4" "#FDC086" "#FFFF99" "#386CB0" "#F0027F" "#BF5B17"

"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494"

"#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F""#FF7F00"


# Plotting
ggplot(combined_data, aes(x = Model, y = AUC, fill = Model)) +
          geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.6) +
          geom_point(data = combined_data, aes(x = Model, y = AUC), 
                     color = "black", size = 1, alpha = 0.5, 
                     position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), show.legend = FALSE) +
          # geom_point(data = subset(combined_data, Species %in% special_species), 
          #            aes(x = Model, y = AUC), color = "red", size = 1, 
                     # alpha = 0.8, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), show.legend = FALSE) +
          scale_fill_manual(values = model_colors) +
          labs(x = "Models", y = "AUC", fill = "Model") +
          ggtitle("AUC Values with Confidence Intervals by Model") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid.major.x = element_blank(),  
                panel.grid.minor.x = element_blank())
        
    










# create a vector with species name we want to highlight
# special_species <- c("Belone_belone", "Boops_boops")

# Plotting
# ggplot(combined_data, aes(x = Model, y = AUC, fill = Model)) +
#           geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.6) +
#           geom_point(data = combined_data, aes(x = Model, y = AUC),
#                      color = "black", size = 1, alpha = 0.5,
#                      position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), show.legend = FALSE) +
#           # geom_point(data = subset(combined_data, Species %in% special_species),
#           #            aes(x = Model, y = AUC), color = "red", size = 1,
#                      # alpha = 0.8, position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), show.legend = FALSE) +
#           scale_fill_manual(values = model_colors) +
#           labs(x = "Models", y = "AUC", fill = "Model") +
#           ggtitle("AUC Values with Confidence Intervals by Model") +
#           theme_minimal() +
#           theme(axis.text.x = element_text(angle = 45, hjust = 1),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.minor.x = element_blank())










        # # set categories 
        # species_10_20 <- rownames(subset(total_occurrences, total_occurrences >= 10 & total_occurrences <= 20)) #15 faible occurrence
        # species_21_40 <- rownames(subset(total_occurrences, total_occurrences >= 21 & total_occurrences <= 40)) #13 faible 
        # species_41_70 <- rownames(subset(total_occurrences, total_occurrences >= 41 & total_occurrences <= 70)) #12 moyenne
        # species_71_90 <- rownames(subset(total_occurrences, total_occurrences >= 71 & total_occurrences <= 90)) #12 moyenne
        # species_91_140 <- rownames(subset(total_occurrences, total_occurrences >= 91 & total_occurrences <= 140)) #15 forte 
        # species_141_162 <-rownames(subset(total_occurrences, total_occurrences >= 141 & total_occurrences <= 162)) #7 forte
        # 
        # # Assign categories based on the total occurrences
        # combined_data$category[combined_data$Species %in% species_10_20] <- "species_10_20"
        # combined_data$category[combined_data$Species %in% species_21_40] <- "species_21_40"
        # combined_data$category[combined_data$Species %in% species_41_70] <- "species_41_70"
        # combined_data$category[combined_data$Species %in% species_71_90] <- "species_71_90"
        # combined_data$category[combined_data$Species %in% species_91_140] <- "species_91_140"
        # combined_data$category[combined_data$Species %in% species_141_162] <- "species_141_162"
        # 
        # combined_data$category <- factor(combined_data$category, levels = c("species_10_20", "species_21_40", "species_41_70", "species_71_90", "species_91_140", "species_141_162"))
        # # combined_data$Model <- factor(combined_data$Model, levels = c("cv_GAM", "GAM" , "cv_GLM", "GLM" , "cv_RF", "RF", "cv_SPAMM", "SPAMM", "cv_SPR", "SPR"))
        # 
        # ggplot(combined_data, aes(x = Model, y = AUC, fill = category)) +
        #   geom_violin(alpha = 0.6, drop = FALSE) +
        #   labs(x = "Models", y = "AUC", fill = "Species") +
        #   ggtitle("AUC Values by Model and Categories of Species Occurrences") +
        #   theme_minimal() +
        #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
        #         panel.grid.major.x = element_blank(),
        #         panel.grid.minor.x = element_blank()) +
        #   facet_wrap(~Model, scales = "free", ncol = 4)
        # 
        # ggplot2::ggsave(filename= "filename", device = "svg", width = 15, height = 10) 
        # 
        


