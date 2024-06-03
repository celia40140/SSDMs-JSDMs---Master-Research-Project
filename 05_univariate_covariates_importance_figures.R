# function for evaluating covariates importance
list_files_path <- list.files("outputs/occurrence_contribution", full.names = T)
bind_files <- lapply(1:length(list_files_path), function(i) {
  
  load(list_files_path[i])
  assign(paste0("model_", i), extracted_contributions)
  
})
bind_files <- do.call(rbind, bind_files)

plot_data = bind_files

# Define specific colors for each covariate
# pal_contribution <- PNWColors::pnw_palette("Bay", 1, type = "discrete")
specific_colors <- c(
  "bathymetry" = "#bebada",
  "substrat" = "#bc80bd",
  "Random: associations" = "#ffffb3",
  "chlorophyll" = "#b3de69",
  "salinity" = "#8dd3c7",
  "temperature" = "#80b1d3",
  "gravity" = "#fb8072",
  "protection" = "#fccde5",
  "Random: spatial"= "#d9d9d9",
  "fishing_pressure" = "#fdb462"
)

color = specific_colors
labs_y = ""
labs_fill = ""
ylim = c(0,0.15)
legend.position = "none"

species_names <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/species_names.csv")
species_name <- species_names$species_names

teleost_names <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/teleost_names.csv")
teleost_names <- teleost_names$teleost_names

elasmo_names <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/elasmo_names.csv")
elasmo_names <- elasmo_names$elasmo_names





####################################################################################
########### variance partitioning plot for the model 
library(ggplot2)

# covariates relative importance by mean
# for glm and rf: "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"

fitted_model = "GLM"

fitted_model = "RF"



plot_level <- fitted_model   

only_model <- plot_data |>
  dplyr::filter(fitted_model == plot_level)


ENV <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$Dropout_loss})
ENV_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$sd_dropout_loss})
ENV <- do.call(rbind, ENV)
ENV_sd <- do.call(rbind, ENV_sd)
ENV <- dplyr::tibble(value = colMeans(ENV),
                     sd = colMeans(ENV_sd),
                     var = c("bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),
                     VAR = rep("ENV", 8),
                     plot_level = rep(plot_level, 8))

cont <- ENV 

importance_plot <- ggplot(cont) +
  geom_col(aes(x = reorder(var, value), y = value, fill = var)) +
  geom_errorbar(aes(x = var, y = value, ymin = value - sd, ymax = value + sd), width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = specific_colors) +  # Use specific_colors here
  theme_bw() +
  coord_flip(ylim = c(0,0.04)) +
  facet_grid(~ plot_level) +
  labs(y = "loss function: 1 - AUC", x = "", fill = labs_fill) +
  theme(legend.position = legend.position,
        title = element_text(size = 18),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# covariates relative importance by mean
# for spamm and srf : "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature", "Random: spatial"

fitted_model = "SPAMM"

fitted_model = "SPRF"


plot_level <- fitted_model   # spamm and srf have "Random: spatial"

only_model <- plot_data |>
  dplyr::filter(fitted_model == plot_level)


ENV <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$Dropout_loss})
ENV_sd <- lapply(1:nrow(only_model), function(i) { only_model$contributions_and_sd[[i]][only_model$contributions_and_sd[[i]]$variable %in% c("Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$sd_dropout_loss})
ENV <- do.call(rbind, ENV)
ENV_sd <- do.call(rbind, ENV_sd)
ENV <- dplyr::tibble(value = colMeans(ENV),
                     sd = colMeans(ENV_sd),
                     var = c("Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),
                     plot_level = rep("SRF", 9))  # attention ici changer pour SPAMM

cont <- ENV 

importance_plot <- ggplot(cont) +
  geom_col(aes(x = reorder(var, value), y = value, fill = var)) +
  geom_errorbar(aes(x = var, y = value, ymin = value - sd, ymax = value + sd), width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = specific_colors) +  # Use specific_colors here
  theme_bw() +
  coord_flip(ylim = c(0,0.03)) +
  facet_grid(~ plot_level) +
  labs(y = "loss function: 1 - AUC", x = "", fill = labs_fill) +
  theme(legend.position = legend.position,
        title = element_text(size = 18),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






####################################################################################
########### Mean variance partitioning plot for elasmo 
library(ggplot2)

# covariates relative importance by mean
# for glm and rf: "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"

fitted_model = "GLM"

fitted_model = "RF"


plot_level <- fitted_model   

only_model <- plot_data |>
  dplyr::filter(fitted_model == plot_level)

only_elasmo_model <-  only_model|>
  dplyr::filter(species_name %in% elasmo_names)


ENV <- lapply(1:nrow(only_elasmo_model), function(i) { only_elasmo_model$contributions_and_sd[[i]][only_elasmo_model$contributions_and_sd[[i]]$variable %in% c("bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$Dropout_loss})
ENV_sd <- lapply(1:nrow(only_elasmo_model), function(i) { only_elasmo_model$contributions_and_sd[[i]][only_elasmo_model$contributions_and_sd[[i]]$variable %in% c("bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$sd_dropout_loss})
ENV <- do.call(rbind, ENV)
ENV_sd <- do.call(rbind, ENV_sd)
ENV <- dplyr::tibble(value = colMeans(ENV),
                     sd = colMeans(ENV_sd),
                     var = c("bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),
                     plot_level = rep(plot_level, 8))

cont <- ENV 

ggplot(cont) +
  geom_col(aes(x = reorder(var, value), y = value, fill = var)) +
  geom_errorbar(aes(x = var, y = value, ymin = value - sd, ymax = value + sd), width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = specific_colors) +  # Use specific_colors here
  theme_bw() +
  coord_flip(ylim = c(0,0.03)) +
  facet_grid(~ plot_level) +
  labs(y = "loss function: 1 - AUC", x = "", fill = labs_fill) +
  ggtitle("Relative Importance of Covariates for Elasmobranchs") +  
  theme(legend.position = legend.position,
        title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# for spamm and srf : "Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"
fitted_model = "SPAMM"

fitted_model = "SPRF"


plot_level <- fitted_model   # spamm and srf have "Random: spatial"

only_model <- plot_data |>
  dplyr::filter(fitted_model == plot_level)

only_elasmo_model <-  only_model|>
  dplyr::filter(species_name %in% elasmo_names)


ENV <- lapply(1:nrow(only_elasmo_model), function(i) { only_elasmo_model$contributions_and_sd[[i]][only_elasmo_model$contributions_and_sd[[i]]$variable %in% c("Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$Dropout_loss})
ENV_sd <- lapply(1:nrow(only_elasmo_model), function(i) { only_elasmo_model$contributions_and_sd[[i]][only_elasmo_model$contributions_and_sd[[i]]$variable %in% c("Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$sd_dropout_loss})
ENV <- do.call(rbind, ENV)
ENV_sd <- do.call(rbind, ENV_sd)
ENV <- dplyr::tibble(value = colMeans(ENV),
                     sd = colMeans(ENV_sd),
                     var = c("Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),
                     plot_level = rep(plot_level, 9))  # changer pour SRF et non SPRF

cont <- ENV 

ggplot(cont) +
  geom_col(aes(x = reorder(var, value), y = value, fill = var)) +
  geom_errorbar(aes(x = var, y = value, ymin = value - sd, ymax = value + sd), width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = specific_colors) +  # Use specific_colors here
  theme_bw() +
  coord_flip(ylim = c(0,0.13)) +
  facet_grid(~ plot_level) +
  labs(y = "loss function: 1 - AUC", x = "", fill = labs_fill) +
  ggtitle("Relative Importance of Covariates for Elasmobranchs") +  
  theme(legend.position = legend.position,
        title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



####################################################################################
########### Mean variance partitioning plot for teleost
library(ggplot2)

# covariates relative importance by mean
# for glm and rf: "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"

fitted_model = "GLM"

fitted_model = "RF"

plot_level <- fitted_model   

only_model <- plot_data |>
  dplyr::filter(fitted_model == plot_level)

only_teleost_model <- only_model|>
  dplyr::filter(species_name %in% teleost_names)


ENV <- lapply(1:nrow(only_teleost_model), function(i) { only_teleost_model$contributions_and_sd[[i]][only_teleost_model$contributions_and_sd[[i]]$variable %in% c("bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$Dropout_loss})
ENV_sd <- lapply(1:nrow(only_teleost_model), function(i) { only_teleost_model$contributions_and_sd[[i]][only_teleost_model$contributions_and_sd[[i]]$variable %in% c("bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$sd_dropout_loss})
ENV <- do.call(rbind, ENV)
ENV_sd <- do.call(rbind, ENV_sd)
ENV <- dplyr::tibble(value = colMeans(ENV),
                     sd = colMeans(ENV_sd),
                     var = c("bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),
                     plot_level = rep(plot_level, 8))

cont <- ENV 

ggplot(cont) +
  geom_col(aes(x = reorder(var, value), y = value, fill = var)) +
  geom_errorbar(aes(x = var, y = value, ymin = value - sd, ymax = value + sd), width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = specific_colors) +  # Use specific_colors here
  theme_bw() +
  coord_flip(ylim = c(0,0.04)) +
  facet_grid(~ plot_level) +
  labs(y = "loss function: 1 - AUC", x = "", fill = labs_fill) +
  ggtitle("Relative Importance of Covariates for Teleosts") +  
  theme(legend.position = legend.position,
        title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# for spamm and srf : "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature", "Random: spatial"
fitted_model = "SPAMM"

fitted_model = "SPRF"


plot_level <- fitted_model   

only_model <- plot_data |>
  dplyr::filter(fitted_model == plot_level)

only_teleost_model <- only_model|>
  dplyr::filter(species_name %in% teleost_names)


ENV <- lapply(1:nrow(only_teleost_model), function(i) { only_teleost_model$contributions_and_sd[[i]][only_teleost_model$contributions_and_sd[[i]]$variable %in% c("Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$Dropout_loss})
ENV_sd <- lapply(1:nrow(only_teleost_model), function(i) { only_teleost_model$contributions_and_sd[[i]][only_teleost_model$contributions_and_sd[[i]]$variable %in% c("Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),]$sd_dropout_loss})
ENV <- do.call(rbind, ENV)
ENV_sd <- do.call(rbind, ENV_sd)
ENV <- dplyr::tibble(value = colMeans(ENV),
                     sd = colMeans(ENV_sd),
                     var = c("Random: spatial", "bathymetry", "chlorophyll", "fishing_pressure", "gravity", "protection", "salinity", "substrat", "temperature"),
                     plot_level = rep(plot_level, 9))  # changer pour SRF 

cont <- ENV 

ggplot(cont) +
  geom_col(aes(x = reorder(var, value), y = value, fill = var)) +
  geom_errorbar(aes(x = var, y = value, ymin = value - sd, ymax = value + sd), width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = specific_colors) +  # Use specific_colors here
  theme_bw() +
  coord_flip(ylim = c(0,0.10)) +
  facet_grid(~ plot_level) +
  labs(y = "loss function: 1 - AUC", x = "", fill = labs_fill) +
  ggtitle("Relative Importance of Covariates for Teleosts") +  
  theme(legend.position = legend.position,
        title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






grid_arrange <- gridExtra::grid.arrange(
  covariates_importance_GLM,
  covariates_importance_GAM,
  covariates_importance_SPAMM,
  covariates_importance_RF,
  covariates_importance_GBM,
  covariates_importance_SPRF,
  ncol = 2  # Adjust the number of columns as needed
)

