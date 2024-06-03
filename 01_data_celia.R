# This script load the needed datasets, (covariates & occurrences), 
# select the covariates used in the models 
# plot the data 


# occ <- rbind(occ_med$fitting, occ_med$validation)
# occ_with_coords <- occ |>
#   dplyr::left_join(cov_med |> dplyr::select(id_spygen, mid_latitude, mid_longitude), by = "id_spygen")
# 
# save(occ_with_coords, file = "occ_with_coords.RData")

#################################################################################
#                                                                               #
#               Choice of Sites : 2023, coastal, France                         #
#                     From BioDivMed metadata                                   #
#                                                                               #
#################################################################################
###################  New metadata 2018 - 2023 BioDIvMEd   #######################

## upload dataset ##
biodivmed_full <- readr::read_csv("data/raw_data/Med_metadonnees_ADNe - v2.0_2018-2023.csv")
## Calculate midpoints ##
# if latitude and longitude start and end : we calculate new coordinates at the center (not important for some methods because transect are under 2km!!)
biodivmed_full <- biodivmed_full |>
  dplyr::mutate(
    mid_latitude = (latitude_start_DD + latitude_end_DD) / 2,
    mid_longitude = (longitude_start_DD + longitude_end_DD) / 2
  )
# for samples without both coordinates : only keep longitude and latitude
biodivmed_full <- biodivmed_full |>
  dplyr::mutate(mid_longitude = ifelse(is.na(mid_longitude), longitude_start_DD, mid_longitude),
                mid_latitude = ifelse(is.na(mid_latitude), latitude_start_DD, mid_latitude)) |>
  dplyr::select(spygen_code, pool, date, lockdown, country, region, protection, component, method, depth_sampling, project, mid_latitude, mid_longitude, latitude_start_DD, longitude_start_DD, latitude_end_DD, longitude_end_DD)|>
  dplyr::rename(id_spygen = spygen_code)
# save(biodivmed_full, file = "biodivmed_full.RData")



#####################################################################################
########################  version 2023 only  

## create a dataset for 2023 ## 
biodivmed_2023 <- biodivmed_full |>
  dplyr::filter(substr(date, 1, 4) == "2023")    # 685 !!!

# Selection for fitting set : component=coastal, and country=France 
biodivmed_fitting <- biodivmed_2023 |>
  dplyr::filter(
    component %in% c("coastal", "open_ocean", "offshore") & 
      country == "France" & 
      depth_sampling < 50
  ) |> 
  dplyr::select(
    id_spygen, pool, date, region, protection, method, depth_sampling, 
    component, project, mid_latitude, mid_longitude, 
    latitude_start_DD, longitude_start_DD, latitude_end_DD, longitude_end_DD
  )

## add covariates & occurrences to create final dataset : biodivmed_fitting # biodivmed_occ

# save(biodivmed_fitting, file = "biodivmed_fitting.RData")


## Select validation set from previous year : component=coastal, and country=France 
biodivmed_2018_19_2022 <- biodivmed_full |>
  dplyr::filter(substr(date, 1, 4) %in% c("2018", "2019", "2020", "2021", "2022") & 
                  lockdown == "0")
 
biodivmed_validation <- biodivmed_2018_19_2022 |>
  dplyr::filter(
    component %in% c("coastal", "open_ocean", "offshore") & 
      country == "France" & 
      depth_sampling < 50
  ) |> 
  dplyr::select(
    id_spygen, pool, date, region, protection, method, depth_sampling, 
    component, project, mid_latitude, mid_longitude, 
    latitude_start_DD, longitude_start_DD, latitude_end_DD, longitude_end_DD
  )

## add covariates & occurrences to create final dataset for validation : biodivmed_validation

# save(biodivmed_validation, file = "biodivmed_validation.RData")


#################################################################################
#                                                                               #
#                        Loads SpyGen Occurrences                               #
#                          BioDivMed 2018-2023                                  #
#                                 Y                                             #
#                                                                               #
#################################################################################
## load occurrences & select id_spygen for fitting and validation set
occ <- read.csv("data/raw_data/data_BIODIVMED_presence.csv", sep = ";", header = TRUE)
occ <- occ|>
  dplyr::rename(id_spygen = code_spygen) |>  
  dplyr::rename_all(~ stringr::str_replace_all(., "\\.", "_"))


#################################################################################
################## create occ_fitting & occ_validation   ########################
## Sélectionner les lignes dans occ_fitting correspondant aux valeurs de id_spygen dans biodivmed_2023
occ_fitting <- occ |>
  dplyr::semi_join(biodivmed_fitting, by = c("id_spygen")) |> ####### 492 need to be 555 !
  dplyr::rename_all(~ stringr::str_replace_all(., "\\.", "_")) #|>
#dplyr::select(-id_spygen)

id_spygen_col <- occ_fitting[["id_spygen"]]
occ_fitting <- occ_fitting|>
  dplyr::mutate(across(everything(), as.numeric))
# Calculate column sums excluding the first non-numeric column
col_sums <- colSums(occ_fitting)#, drop = FALSE])
# Identify columns with sum inferior to 10 occ
cols_to_erase <- which(col_sums < 1)
# Remove columns
occ_fitting <- occ_fitting[, -cols_to_erase]
#bind id_spygen back
occ_fitting$id_spygen <- dplyr::coalesce(as.character(occ_fitting$id_spygen), id_spygen_col)


# for validation set
occ_validation <- occ |>
  dplyr::semi_join(biodivmed_validation, by = c("id_spygen")) |>  ####### 637 need to be 737 !
  dplyr::rename_all(~ stringr::str_replace_all(., "\\.", "_"))

# # select same species for both datasets
common_column_names <- intersect(names(occ_fitting), names(occ_validation))
occ_validation <- occ_validation |>
  dplyr::select(all_of(common_column_names))


# ## verifier si dans validation on a bien toutes les espèces avec 10 occ
# occ_validation <- occ_validation|>
#   dplyr::mutate(across(everything(), as.numeric))
# 
# # Calculate column sums excluding the first non-numeric column
# col_sums <- colSums(occ_validation)#, drop = FALSE])
# # Identify columns with sum inferior to 10 occ
# cols_to_erase <- which(col_sums < 10)  # 6 espèces :Coryphaena_hippurus, Capros_aper, Istiophorus_platypterus, Chauliodus_sloani, Xyrichtys_novacula, Raja_brachyura
# # Remove columns
# occ_validation <- occ_validation[, -cols_to_erase]
# #bind id_spygen back
# occ_validation$id_spygen <- dplyr::coalesce(as.character(occ_validation$id_spygen), id_spygen_col)
# 



#################################################################################
#                                                                               #
#                         Loads Covariates Data                                 #
#                                  X                                            #
#                                                                               #
#################################################################################

# Upload dataset with covariates

##### investigate the site for which we don't have values for covariates ####

# fishing pressure
load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/eDNA_fishing.Rdata")
fishing_pressure <- eDNA_fishing |>
  dplyr::select(spygen_id, fishing_density) |>
  dplyr::rename(id_spygen = spygen_id) |>
  dplyr::rename(fishing_pressure = fishing_density)

fishing_pressure$fishing_pressure <- as.numeric(fishing_pressure$fishing_pressure)

# Set the noise level
noise_level <- 0.0001

# Generate random noise for each zero value
zero_indices <- which(fishing_pressure$fishing_pressure == 0)
num_zeros <- length(zero_indices)
noise <- abs(rnorm(num_zeros, mean = 0, sd = noise_level))

# Add the noise to the zero values in the fishing_pressure column
fishing_pressure$fishing_pressure[zero_indices] <- 
  fishing_pressure$fishing_pressure[zero_indices] + noise


# gravity_dshore
dshore <- readr::read_delim("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/mtdt_all.csv",
                            delim = ";")
dshore <- dshore |>
  dplyr::select(spygen_code, dshore)|>
  dplyr::rename(id_spygen = spygen_code) |>
  dplyr::rename(dist_shore = dshore)

dshore <- dshore |>
  dplyr::mutate(dist_shore = as.numeric(gsub(",", ".", dist_shore)))

# Set the noise level
noise_level <- 0.01
# Generate random noise for each zero value
zero_indices <- which(dshore$dist_shore == 0)
num_zeros <- length(zero_indices)
noise <- abs(rnorm(num_zeros, mean = 0, sd = noise_level))
# Add the noise to the zero values in the fishing_pressure column
dshore$dist_shore[zero_indices] <- dshore$dist_shore[zero_indices] + noise

gravity <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/gravityADNe_2018-2023.csv")
gravity <- gravity |>
  dplyr::select(spygen_code, gravity_mean)
colnames(gravity) <- c("id_spygen", "gravity")

gravity_dshore <- dplyr::left_join(dshore, gravity, by = "id_spygen")

# Temp salinity bathymetry
# load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/cov_lolo.Rdata")
# medEdna_bathy <- cov_lolo |>
#   dplyr::select(code_spygen, bathymetry)
# colnames(medEdna_bathy) <- c("id_spygen", "bathymetry")

medEdna_bathy <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/medEdna_bathy.csv", col_names = FALSE)
colnames(medEdna_bathy) <- c("id_spygen", "bathymetry")

medEdna_temp_Week <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/medEdna_temp_Week.csv", col_names = FALSE)
colnames(medEdna_temp_Week) <- c("id_spygen", "temperature")


medEdna_sal_Week <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/medEdna_sal_Week.csv", col_names = FALSE)
colnames(medEdna_sal_Week) <- c("id_spygen", "salinity")


# bbp chl dino
# load("C:/Users/celia/Desktop/Final Project/data/dino.Rdata")
load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/chl.Rdata")
load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/bbp.Rdata")

bbp <- bbp |>
  dplyr::select(id_spygen, mean_7days_BBP443)|>
  dplyr::rename(turbidity = mean_7days_BBP443)
chl <- chl |>
  dplyr::select(id_spygen, mean_7days_CHL)|>
  dplyr::rename(chlorophyll = mean_7days_CHL)
# dino <- dino |>
#   dplyr::select(id_spygen, mean_7days_DINO)
bbp_chl <- dplyr::left_join(chl, bbp, by = "id_spygen")#|>
#dplyr::left_join(dino, by = "id_spygen")

# substrate
data_popo <- sf::st_read("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/data_spygen_substrat.geojson")
data_popo <- data_popo |>
  dplyr::select(id_spygen, nb_substrats, substrat_principal)|>
  dplyr::rename(principal_substrat = substrat_principal)

data_popo$principal_substrat <- ifelse(
  is.na(data_popo$principal_substrat), NA,  # Keep NA as NA
  ifelse(
    data_popo$principal_substrat == "Muddy sand", "mud",
    ifelse(
      data_popo$principal_substrat == "Sand", "sand",
      ifelse(
        data_popo$principal_substrat == "[Posidonia oceanica] meadows", "posidonia_meadows",
        ifelse(
          data_popo$principal_substrat == "Coarse & mixed sediment", "coarse_mixed_sediment",
          ifelse(
            data_popo$principal_substrat == "Fine mud", "mud",
            ifelse(
              data_popo$principal_substrat == "Rock or other hard substrata", "rock",
              ifelse(
                data_popo$principal_substrat == "Sandy mud", "mud",
                ifelse(
                  data_popo$principal_substrat == "Seabed", "sand",
                  data_popo$principal_substrat  # Keep other values unchanged
                )
              )
            )
          )
        )
      )
    )
  )
)

# reserve
reserve <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/data/MetaData_AMP.csv")
reserve <- reserve |>
  dplyr::select(spygen_code, dist_min_fully_protected_MPA, protection)|>
  dplyr::rename(id_spygen = spygen_code)|>
  dplyr::rename(dist_fully_protected_MPA = dist_min_fully_protected_MPA)

# Set the noise level
noise_level <- 0.01
# Generate random noise for each zero value
zero_indices <- which(reserve$dist_fully_protected_MPA == 0)
num_zeros <- length(zero_indices)
noise <- abs(rnorm(num_zeros, mean = 0, sd = noise_level))
# Add the noise to the zero values in the fishing_pressure column
reserve$dist_fully_protected_MPA[zero_indices] <- reserve$dist_fully_protected_MPA[zero_indices] + noise


# combine datasets
cov_med <- dplyr::left_join(gravity_dshore, bbp_chl, by = "id_spygen")|>
  dplyr::left_join(data_popo, by = "id_spygen") |>
  dplyr::left_join(reserve, by = "id_spygen") |>
  dplyr::left_join(medEdna_bathy, by = "id_spygen") |>
  dplyr::left_join(medEdna_sal_Week, by = "id_spygen") |>
  dplyr::left_join(medEdna_temp_Week, by = "id_spygen")|>
  dplyr::left_join(fishing_pressure, by = "id_spygen")

cov_med <- dplyr::select(cov_med, -geometry.x, -geometry.y)

# no na filter 
cov_med <- cov_med |>
  dplyr::filter(!rowSums(dplyr::across(everything(), is.na))) #######  1323 sites

# add mid_latitude and mid_longitude
cov_med <- dplyr::left_join(cov_med, biodivmed_full |> dplyr::select(id_spygen, mid_latitude, mid_longitude), by = "id_spygen")

# salinity errors 
# Calculate the mean of the salinity column, excluding negative values
mean_salinity <- mean(cov_med$salinity[cov_med$salinity >= 1])
# Replace negative values in the salinity column with the mean
cov_med$salinity[cov_med$salinity < 1] <- mean_salinity

# log
variables_quantitatives <- c("dist_shore","fishing_pressure", "bathymetry", "dist_fully_protected_MPA", "gravity")
# cov_med[, variables_quantitatives] <- scale(cov_med[, variables_quantitatives], center = TRUE, scale = TRUE)
cov_med[, variables_quantitatives] <- log(cov_med[, variables_quantitatives] +1)

cov_med$protection <- as.factor(cov_med$protection)
cov_med$principal_substrat <- as.factor(cov_med$principal_substrat)
cov_med$id_spygen <- as.factor(cov_med$id_spygen)

noise_magnitude <- 0.00001
cov_med <- cov_med |>
  dplyr::rowwise() |>
  dplyr::mutate(mid_latitude = mid_latitude + runif(1, -noise_magnitude, noise_magnitude),
                mid_longitude = mid_longitude + runif(1, -noise_magnitude, noise_magnitude)) |>
  dplyr::ungroup()

# save(cov_med, file= "~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/cov_med.RData")



##################################################################################
##################################################################################
#Sélectionner les lignes de fitting
cov_med_fitting <- occ_fitting |>
  dplyr::semi_join(cov_med, by = c("id_spygen")) |> ####### # 518 sur 558
  dplyr::rename_all(~ stringr::str_replace_all(., "\\.", "_"))

cov_med_validation <- occ_validation |>
  dplyr::semi_join(cov_med, by = c("id_spygen")) |> ####### # 557 sur 737
  dplyr::rename_all(~ stringr::str_replace_all(., "\\.", "_"))

cov_med_fitting <- cov_med_fitting |>
  dplyr::left_join(cov_med |> dplyr::select(id_spygen, mid_latitude, mid_longitude), by = "id_spygen")
cov_med_validation <- cov_med_validation |>
  dplyr::left_join(cov_med |> dplyr::select(id_spygen, mid_latitude, mid_longitude), by = "id_spygen")


# Remove rows not in cov_med for occ_fitting
occ_fitting <- occ_fitting |>
  dplyr::filter(id_spygen %in% cov_med_fitting$id_spygen)

# Remove rows not in cov_med for occ_validation
occ_validation <- occ_validation |>
  dplyr::filter(id_spygen %in% cov_med_validation$id_spygen)


# verify number of species 
id_spygen_col <- occ_fitting[["id_spygen"]]

occ_validation <- occ_validation|>
  dplyr::mutate(across(everything(), as.numeric))

# Calculate column sums excluding the first non-numeric column
col_sums <- colSums(occ_validation)#, drop = FALSE])
# Identify columns with sum inferior to 10 occ
cols_to_erase <- which(col_sums < 10)
# Remove columns 
occ_fitting <- occ_fitting[, -cols_to_erase]
#bind id_spygen back 
occ_fitting$id_spygen <- dplyr::coalesce(as.character(occ_fitting$id_spygen), id_spygen_col)


# for validation set
# select same species for both datasets
common_column_names <- intersect(names(occ_fitting), names(occ_validation))

occ_validation <- occ_validation |>
  dplyr::select(all_of(common_column_names))


# create occ_med dataset with both fitting and validation occurrence (id_spygen)
occ_med <- list(
  fitting = occ_fitting,
  validation = occ_validation
)

# save(occ_med, file= "~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/data/derived_data/occ_med.RData")




#################################################################################
#######################  List of species_names   ########################

# species names 
#create species names object
species_names <- names(occ_med$fitting)[-which(names(occ_med$fitting) == "id_spygen")] #201

species_names <- names(occ_fitting)[-which(names(occ_fitting) == "id_spygen")] #201


# start by finding elasmo species 
elasmo_spygen <- readr::read_csv("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/extraction/elasmo_spygen.csv")
elasmo_spygen <- elasmo_spygen |>
  dplyr::mutate(taxon = stringr::str_replace_all(taxon, " ", "_")) |>
  dplyr::distinct(taxon, .keep_all = TRUE)
# create object with elasmo species names 
elasmo_names <- unlist(strsplit(elasmo_spygen$taxon, split = " "))
elements_to_remove <- c("Bathytoshia", "Hypanus", "Amblyraja", "Torpedinidae", "Raja", "Dasyatidae", "Amblyraja", "Dasyatoidea")
elasmo_names <- elasmo_names[!elasmo_names %in% elements_to_remove] # 34


# calculate number of occurrences per species 
occ_fitting_elasmo <- occ_med$fitting |>
  dplyr::select(one_of(c("id_spygen", elasmo_names))) 
# il manque: `Hexanchus_griseus`, `Hexanchus_nakamurai`, `Alopias_vulpinus`, `Cetorhinus_maximus`, `Isurus_oxyrinchus`, `Ginglymostoma_cirratum`, `Dipturus_oxyrinchus`, `Leucoraja_naevus`, `Raja_radula`, `Raja_asterias_Raja_clavata_Raja_polystigma`, `Etmopterus_spinax`, `Squalus_blainville`, `Squalus_acanthias`, `Centrophorus_squamosus`, `Galeorhinus_galeus` 

elasmo_names <- names(occ_fitting_elasmo)[-which(names(occ_fitting_elasmo) == "id_spygen")] #total 18 mais 10 au dessus de 10 occ
teleost_names <- setdiff(species_names, elasmo_names) #104

# col_sums <- colSums(occ_fitting_elasmo, na.rm = TRUE)
# dans fitting : Dasyatis_tortonesei, Torpedo_marmorata, Tetronarce_nobiliana, Rostroraja_alba, Raja_asterias_R_clavata_R_polystigma, Raja_brachyura, Myliobatis_aquila, Aetomylaeus_bovinus, Dasyatis_pastinaca, Scyliorhinus_canicula     


file_path <- "species_names.csv"
# # Write the teleost species names to the CSV file
write.csv(data.frame(species_names), file_path, row.names = FALSE)
# 


#################################################################################
# HOW TO HAVE SPECIES NAMES 

species_names <- read_csv("species_names.csv")
species_names <- species_names$species_names

elasmo_names <- read_csv("elasmo_species_names.csv")
elasmo_names <- elasmo_names$elasmo_names

teleost_names <- read_csv("teleost_species_names.csv")
teleost_names <- teleost_names$teleost_names




#################################################################################
#############################  Mapp of sites   ##################################
## set the map ##
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

occ_fitting <- dplyr::inner_join(occ_fitting, cov_med, by = "id_spygen")
occ_validation <- dplyr::inner_join(occ_validation, cov_med, by = "id_spygen")

## plot sites ##
library(ggplot2)
plot <- ggplot2::ggplot(data = world) +
  geom_sf(color = "black", fill = "light grey") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(2.79123253648454, 10.647334299871368), ylim = c(41.1906737866009, 44.44029498045467), expand = FALSE) +
  theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2),
        panel.background = element_rect(fill = "light blue")) +
  geom_point(data = cov_med_fitting, aes(x = mid_longitude, y = mid_latitude, color = "Fitting sites"), size = 1, alpha = 0.7) +
  geom_point(data = cov_med_validation, aes(x = mid_longitude, y = mid_latitude, color = "Validation sites"), size = 1, alpha = 0.7) +
  labs(title = paste("BioDivMed Sites"),
       x = "Longitude",
       y = "Latitude") +
  guides(color = guide_legend(title = "", override.aes = list(size = 4)))

# Update the scale_color_manual and scale_fill_manual to set custom colors and labels for the legend
plot <- plot + scale_color_manual(values = c("Fitting sites" = "#AA3377", "Validation sites" = "#4477aa"),
                                  labels = c("Fitting sites", "Validation sites")) +
  scale_fill_manual(values = c("Fitting sites" = "#AA3377", "Validation sites" = "#4477aa"),
                    labels = c("Validation sites", "Fitting sites"))

ggsave("biodiv_med.svg", plot, device = "svg", dpi = 1200,  width = 10, height = 5)


## visualize sites using sf ##
sf_fitting <- sf::st_as_sf(cov_med_fitting, coords=c("mid_longitude", "mid_latitude"), crs=4326)  |>
  dplyr::select(c("id_spygen", "geometry"))

sf_validation <- sf::st_as_sf(cov_med_validation, coords=c("mid_longitude", "mid_latitude"), crs=4326) |>
  dplyr::select(c("id_spygen", "geometry"))


# Add the year column based on your condition
# sf <- sf |> dplyr::mutate(year = ifelse(grepl("\\b\\d{2}/\\d{2}/2023\\b", date), 2023, other_year_value))
# replace the "other_year_value" with the actual year from the "date" column, you can extract it using string manipulation functions

# sf <- sf |>
#   dplyr::mutate(year = ifelse(grepl("\\b\\d{2}/\\d{2}/2023\\b", date), 2023, as.integer(substring(date, nchar(date)-3, nchar(date)))))
# sf$year <- as.character(sf$year)
#lubridate

tmap::tmap_mode("view") #html
# tmap::tmap_mode("plot") #to save in svg 


# # # Create the map
library(tmap)
library(sf)
# Create the map
carte <- tm_fill() +
  tm_borders(alpha = 0) +  # Add country contour
  tm_shape(sf_fitting) +
  tm_dots(col = "#AA3377") +  # Fitting sites color
  tm_shape(sf_validation) +
  tm_dots(col = "#4477aa") +  # Validation sites color
  tm_scale_bar(position = c("left", "bottom")) +  # Add scale bar
  tm_compass(type = "8star", position = c("left", "top"))  # Add north arrow


# tmap::tmap_save(tm = carte, filename = "carte.svg")



sf_fitting <- sf::st_as_sf(cov_med_fitting, coords = c("mid_longitude", "mid_latitude"), crs = 4326) |>
  dplyr::select(id_spygen, geometry)

sf_validation <- sf::st_as_sf(cov_med_validation, coords = c("mid_longitude", "mid_latitude"), crs = 4326) |>
  dplyr::select(id_spygen, geometry)

# Create the map
carte <- tm_shape(sf_fitting) +
  tm_dots(col = "#AA3377", size = 0.05) +  # Fitting sites color
  tm_shape(sf_validation) +
  tm_dots(col = "#4477aa", size = 0.05) +  # Validation sites color
  tm_scale_bar(position = c("left", "bottom"), size=5) +  # Add scale bar
  tm_compass()  # Add north arrow

# View the map
tmap_mode("view")
carte
#################################################################################
#######################   Elasmo Component & Method    ##########################
# for the whole dataset investigate where elasmo species are and what method/component seems to be more apropriate to detect them

# components
occ_elasmo <- occ |>
  dplyr::select(id_spygen, one_of(elasmo_names))
# occ_elasmo$col_sums <- colSums(occ_elasmo, na.rm = TRUE)
# species_above_10 <- names(col_sums[col_sums >= 10])

occ_elasmo$row_sum <- rowSums(occ_elasmo[, -which(names(occ_elasmo) == "id_spygen")])

empty <- occ_elasmo$id_spygen[occ_elasmo$row_sum == 0] # 633
sites_elasmo <- occ_elasmo$id_spygen[occ_elasmo$row_sum > 0] # 497
  
# retreive metadata for the sites with elasmo and compute stats
sites_metadata <- biodivmed_full |>
  dplyr::filter(id_spygen %in% sites_elasmo)

# Compute statistics for method, component, and depth_sampling
stats_elasmo <- sites_metadata |>
  dplyr::group_by(component) |>
  dplyr::summarise(count = dplyr::n()) |>
  dplyr::mutate(percent = count / sum(count) * 100)

# add an extra column to have real info on the distribution 
all <- occ$id_spygen
metadata <- biodivmed_full |>
  dplyr::filter(id_spygen %in% all)

stat_all <- metadata |>
  dplyr::group_by(component) |>
  dplyr::summarise(count_all = dplyr::n())

stats_elasmo <- stats_elasmo |>          ################### save table with with final data !
  dplyr::left_join(stat_all, by = "component") |>
  dplyr::mutate(mean_component = count / count_all * 100)

# plotting 
library(ggplot2)
# Plotting with manual color scale
# Get unique components in the order of mean_component
unique_components <- unique(stats_elasmo[order(stats_elasmo$mean_component), ]$component)

# Plotting with manual color scale
ggplot(stats_elasmo, aes(x = reorder(component, mean_component), y = mean_component, fill = component)) +
  geom_bar(stat = "identity") +
  labs(title = "Contribution of Components to Elasmobranch Species Detection",
       x = "Component",
       y = "Mean Component (%)") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Display y-axis labels as percentages
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12)) +
  scale_fill_manual(values = viridis::viridis(length(unique_components), direction = -1),
                    breaks = unique_components)  # Specify the order of breaks


###############################################################################################
# methods
stats_elasmo <- sites_metadata |>
  dplyr::group_by(method) |>
  dplyr::summarise(count = dplyr::n()) |>
  dplyr::mutate(percent = count / sum(count) * 100)

# add an extra column to have real info on the distribution 
all <- occ$id_spygen
metadata <- biodivmed_full |>
  dplyr::filter(id_spygen %in% all)

stat_all <- metadata |>
  dplyr::group_by(method) |>
  dplyr::summarise(count_all = dplyr::n())

stats_elasmo <- stats_elasmo |>          ################### save table with with final data !
  dplyr::left_join(stat_all, by = "method") |>
  dplyr::mutate(mean_method = count / count_all * 100)

# plotting 
library(ggplot2)
# Plotting with manual color scale
# Get unique components in the order of mean_component
unique_components <- unique(stats_elasmo[order(stats_elasmo$mean_method), ]$method)

# Plotting with manual color scale
ggplot(stats_elasmo, aes(x = reorder(method, mean_method), y = mean_method, fill = method)) +
  geom_bar(stat = "identity") +
  labs(title = "Contribution of Methods to Elasmobranch Species Detection",
       x = "Method",
       y = "Mean Method (%)") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Display y-axis labels as percentages
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12)) +
  scale_fill_manual(values = viridis::viridis(length(unique_components), direction = -1),
                    breaks = unique_components)  # Specify the order of breaks




#################################################################################
######################  Co-occurrences measure   #################################
# choose species that have significative co-occurence values with elasmobranchs


# Remove duplicate rows : not needed with new version
# occ <- dplyr::distinct(occ, id_spygen, .keep_all = TRUE)
# Set row names to id_spygen column and remove the column
rownames(occ) <- occ$id_spygen
occ <- occ[, -1]  # Remove the id_spygen


# package asnipe : association index -> Simple Ratio Index (A U B/A & B)
adj=asnipe::get_network(occ, association_index="SRI")

perm <- asnipe::network_permutation(adj, data_format = "GBI", permutations = 1000, association_matrix= adj)

MA<-apply(perm,2:3,function(x){quantile(x,c(0.975))}) # differences between observed matrix and quantile of 1000 random networks

## positive values are preferred associations
PrefAsso = adj - MA
PrefAsso[which(PrefAsso<0)]=0
rowSums(PrefAsso)
PrefAsso[which(PrefAsso>0)]=1
sum(PrefAsso) # 352 = total count of positive associations

# Calculate column sums excluding the first non-numeric column
col_sums <- colSums(PrefAsso)
# Identify columns with sum equal to 0
cols_to_erase <- which(col_sums == 0)
# Remove columns and rows 
PrefAsso <- PrefAsso[-cols_to_erase, -cols_to_erase]

assoc.g=igraph::graph_from_adjacency_matrix(PrefAsso, "undirected", weighted=T) #create a graph object

elasmo_names_in_graph <- intersect(rownames(PrefAsso), elasmo_names)

# Assign labels to all nodes but only display the names of elasmobranch species
igraph::V(assoc.g)$label <- ifelse(igraph::V(assoc.g)$name %in% elasmo_names_in_graph, igraph::V(assoc.g)$name, "")

# Plot the graph with labels of elasmobranch species
com=igraph::cluster_fast_greedy(assoc.g) #community detection method
node.colors=igraph::membership(com) #assign node color based on community membership

# layouts = c("igraph::layout_with_fr", "igraph::layout_with_mds", igraph::layout_with_kk", "igraph::layout_with_dh", "igraph::layout_with_gem", "igraph::layout_as_star", "igraph::layout_as_tree", "igraph::layout_in_circle", "igraph::layout_on_grid")
set.seed(2)
plot(assoc.g, layout = igraph::layout_with_mds, edge.width = igraph::E(assoc.g)$weight * 5, 
     vertex.label = igraph::V(assoc.g)$label, vertex.size = 5, vertex.color = node.colors, vertex.label.cex = 0.7)





# package cooccur : observed and expected frequencies of co-occurrence between each pair of species
# randomisation with null model 
# Create a matrix with the same dimensions as df, filled with 1s
# site_mask <- matrix(1, nrow = nrow(df), ncol = ncol(df))
# colnames(site_mask) <- colnames(df)
# rownames(df) <- NULL
# 
# # Now you can use this matrix as input to the cooccur function
# cooccur_result <- cooccur::cooccur(mat = df, type = "spp_site", spp_names = TRUE, prob = "hyper", site_mask = site_mask)
# 
# cooccur = cooccur::cooccur(mat = df, type="site_spp", spp_names=TRUE, prob="hyper", only_effects=TRUE, site_mask = matrix)
# par(mfrow=c(2,1))
# plot(cooccur)
# 
# cormat<-matrix(data = NA, nrow=16, ncol=16)
# for(i in 1:16)
# {
#   for(j in 1:16)
#   {
#     cormat1[i,j]<-sum(as.numeric(try32[,i]>0)*as.numeric(try32[,j]>0))/(sum(as.numeric(try32[,i]>0))+sum(as.numeric(try32[,j]>0)))
#   }
# }
# network <- graph_from_adjacency_matrix(cormat1, weighted=T, mode="undirected", diag=F)
# plot(network,edge.arrow.size=.4,vertex.label=colnames(try31), vertex.color=my_color,
#      vertex.label.color="black",vertex.label.font=2)

# # package ecospat::ecospat.co_occurrences()
# eco.co <- ecospat::ecospat.co_occurrences(occ)
# # calculate pairwise co-occurrence with C-score index using ecospat.Cscore()
# outpath <- getwd()
# Cscore <- ecospat::ecospat.Cscore(occ, nperm, outpath, verbose=TRUE)




#################################################################################
######################  Mapp of biodiversity   ##################################

# the goal here is to see where species are located, and if, some of the absences can be erased: big issue for us will to have too much absences for our models to be able to work
# for exemple, we already know that Squatina squatina (angel shark) is only located in Corsica so all 0 outside can't be taken for real 0

# For all the occurrences of ALL SPECIES 
#This is for the map of just the Med 
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# combine occurences and metadata
occ_fitting_with_metadata <- occ_fitting |>
  dplyr::left_join(biodivmed_fitting |> dplyr::select(id_spygen, mid_latitude, mid_longitude), by = "id_spygen")

# Melt the dataset to long format for easier plotting
melted_data <- reshape2::melt(occ_fitting_with_metadata, id.vars = c("id_spygen", "mid_latitude", "mid_longitude"))   # for all species

# erased rows when column value = 0 (if you just want to see where species occurs)
data <- melted_data |>
  dplyr::filter(value != 0)

# transform as factor column value
data$value <- factor(data$value)


# # Separate elamsobranchs species to create 2 specific melted dataset for teleosts and elasmobranchs
species_name <- setdiff(names(occ_fitting), "id_spygen")

melted_elasmo <- data |>
  dplyr::filter(variable %in% elasmo_names)

elasmo <- unique(melted_elasmo$variable) # 18
elasmo <- c("Mobula_mobular", "Torpedo_marmorata", "Prionace_glauca", "Dasyatis_pastinaca", "Aetomylaeus_bovinus", "Myliobatis_aquila", "Tetronarce_nobiliana", "Scyliorhinus_canicula", "Pteroplatytrygon_violacea", "Raja_undulata", "Raja_miraletus", "Squatina_squatina", "Scyliorhinus_stellaris", "Mustelus_mustelus", "Raja_brachyura", "Rostroraja_alba", "Dasyatis_tortonesei", "Raja_asterias_Raja_clavata_Raja_polystigma")

teleost <- setdiff(species_name, elasmo_names) #120 = 131 en tout 

melted_teleost <- data |>
  dplyr::filter(variable %in% teleost)


# Create a vector of unique colors for each species group
elasmo_color <- "pink"
teleost_color <- "black"

# Plotting using ggplot2
library(ggplot2)
ggplot2::ggplot(data = world) +
  geom_sf(color = "black", fill = "light grey") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(2.79123253648454, 10.647334299871368), ylim = c(41.1906737866009, 44.44029498045467), expand = FALSE) +
  ggtitle("Mediterranean Sea Occurrences") +
  theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank",
                                        size = 0.2), panel.background = element_rect(fill = "light blue")) +
  geom_point(data = melted_teleost, aes(x = mid_longitude, y = mid_latitude, color = "Teleosts", fill = "Teleosts"), size = 2, alpha = 0.7) +
  geom_point(data = melted_elasmo, aes(x = mid_longitude, y = mid_latitude, color = "Elasmobranchs", fill = "Elasmobranchs"), size = 2, alpha = 0.7) +  
  labs(title = "Species Occurrences",
       x = "Longitude",
       y = "Latitude",
       color = "Species",
       fill = "Species") +
  scale_color_manual(values = c("Elasmobranchs" = elasmo_color, "Teleosts" = teleost_color), labels = c("Elasmobranchs", "Teleosts")) +
  scale_fill_manual(values = c("Elasmobranchs" = elasmo_color, "Teleosts" = teleost_color), labels = c("Elasmobranchs", "Teleosts")) +
  guides(shape = guide_legend(title = "Legend Title", override.aes = list(size = 4)))


# Plot for ALL ELASMOBRANCHS
neutral_color <- "grey"
elasmo_highlight <- "pink"

# Specify an absolute path
save_directory <- "/Users/celia/Desktop/Final Project/figures/elasmobranchs_occurrences"
plots_list <- list()  # Initialize an empty list to store plots
plots_per_grid <- 9  # Set the number of plots per grid

for (i in seq_along(elasmo)) {
  species <- elasmo[i]
  
  # Filter melted_elasmo for the current species
  subset_data <- melted_elasmo[melted_elasmo$variable == species, ]
  data <- subset(melted_elasmo, variable != species)
  
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Generate the plot
  plot <- ggplot(data = world) +
    geom_sf(color = "black", fill = "light grey") +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf(xlim = c(2.79123253648454, 10.647334299871368), ylim = c(41.1906737866009, 44.44029498045467), expand = FALSE) +
    ggtitle(paste("Occurrences of", species)) +
    theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2),
          panel.background = element_rect(fill = "light blue")) +
    geom_point(data = data, aes(x = mid_longitude, y = mid_latitude, color = "grey", fill = "grey"), size = 1, alpha = 0.7) +
    geom_point(data = subset_data, aes(x = mid_longitude, y = mid_latitude, color = "pink", fill = "pink"), size = 3, alpha = 0.7) +
    scale_color_manual(values = c(neutral_color, elasmo_highlight), labels = c("Other Elasmobranchs", species)) +
    scale_fill_manual(values = c(neutral_color, elasmo_highlight), labels = c("Other Elasmobranchs", species)) +
    labs(title = paste("Occurrences of", species),
         x = "Longitude",
         y = "Latitude",
         color = "Presence",
         fill = "Presence") +
    guides(shape = guide_legend(title = "Legend Title", override.aes = list(size = 4)))
  # Add the plot to the list
  plots_list[[length(plots_list) + 1]] <- plot
  
  # Check if we've reached the desired number of plots per grid or it's the last species
  if (length(plots_list) %% plots_per_grid == 0 || i == length(elasmo)) {
    # Arrange and display the plots in a grid
    grid_arrange <- gridExtra::grid.arrange(grobs = plots_list, ncol = 3)
    
    # Save the grid to a file
    save_filename <- paste0(save_directory, "grid_", (i - 1) %/% plots_per_grid + 1, ".svg")
    ggsave(save_filename, grid_arrange, device = "svg", width = 15, height = 10)  # Adjust width and height as needed
    
    # Clear the plots_list for the next grid
    plots_list <- list()
  }
}



# script for krigning interpolation of ? biodiversity indice

# Calculate richness indice (lequel)


# interpolation and plot
library(tidyverse)
library(akima)
library(broom)
library(oce)
library(gstat)
library(sp)
library(patchwork)

hydro = read_csv("hydro_201311.csv")
coastline = read_csv("gshhg_calcofi_h.csv")

# indice de richesse avec lat long et id_spygen
hydro_wgs_84 = SpatialPointsDataFrame(coords = select(hydro, lon, lat), data = select(hydro, SiO3, Temp, O2, ChlorA),
                                      proj4string = CRS("+proj=longlat +datum=WGS84")) # To use geodesic distances

# set up la grid de zone étude
lon_grid = seq(from = min(hydro$lon), to = max(hydro$lon), length.out = 10*3)
lat_grid = seq(from = min(hydro$lat), to = max(hydro$lat), length.out = 7*3)
grid = crossing(lon_grid, lat_grid)
grid_wgs_84 = SpatialPoints(coords = select(grid, lon_grid, lat_grid), proj4string = CRS("+proj=longlat +datum=WGS84"))
#st_intersect pour supprimer ce qui n'est pas dans le buffer de distance à la cote

# indice de richesse/biodiversité
vario_hydro = variogram(SiO3 ~ 1, data = hydro_wgs_84)

vario_hydro_fitted = fit.variogram(vario_hydro, vgm(model= "Exp", psill = 6.2, range = 150, nugget = 3))

kriging = krige(formula = SiO3 ~ 1, locations = hydro_wgs_84, newdata = grid_wgs_84, model = vario_hydro_fitted)

ggplot(data = kriging, aes(x = lon_grid, y = lat_grid)) + geom_tile(aes(fill = var1.pred)) +
  geom_point(data = hydro, aes(x = lon, y = lat, colour = SiO3, size = SiO3)) +
  stat_contour(aes(z = var1.pred), geom = "path", bins = 10) +
  geom_polygon(data = coastline, aes(x = lon, y = lat)) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA, limits = c(1,15)) +
  coord_map() + scale_colour_gradient(low = "yellow", high = "red", na.value = NA, limits = c(1,15)) + theme_light()






##################################################################################
####################### Study of covariates with VIF & ACP #######################

# see if some covariates are too coorelated and choose 6 to 7 predictors (compare with the mean number of occurences)
# dataframe with all occurences and all covariates + Row.names

id_spygen_values <- c(occ_fitting$id_spygen, occ_validation$id_spygen)
filtered_occ <- occ[occ$id_spygen %in% id_spygen_values, ]
filtered_cov <- data.frame(cov_med$id_spygen)

common_id_spygen <- intersect(filtered_occ$id_spygen, filtered_cov$cov_med.id_spygen)

# filtered_occ[, -1] <- as.data.frame(lapply(filtered_occ[, -1], as.numeric))

filtered_occ_2 <- occ[occ$id_spygen %in% common_id_spygen, ]

combined_data <- dplyr::full_join(filtered_occ_2, cov_med, by = "id_spygen")

## RDA
# utiliser data_MED_final
RDA2=vegan::capscale(combined_data[,-c(1,238:250)] ~ nb_substrats + protection + principal_substrat + bathymetry + dist_fully_protected_MPA + chlorophyll  + temperature + salinity, combined_data, dist="jaccard", na.action = na.omit, add =TRUE)
summary(RDA2)
plot(RDA2)

## AFC 
afc <- FactoMineR::CA(occ[,-1], graph = FALSE, axes = c(1,2))
factoextra::fviz_ca_biplot(afc, axes = c(1, 2), col.row = "blue", col.col = "black", repel= FALSE, ellipse.type="confidence", addELLipse=TRUE)


# ACP
# ici problème avec les NA vérifier qu'on a bien aucun NA dans le fichier final et sauter cette étape 
# Create a dataset with all quantitative covariates
data_quantitative <- cov_med[c("gravity", "dist_fully_protected_MPA", "dist_shore", "fishing_pressure", "chlorophyll", "turbidity","salinity", "temperature", "nb_substrats", "bathymetry")]

pca = FactoMineR::PCA(data_quantitative[,-1], scale.unit = TRUE, ncp = 5, graph = FALSE)
# Get eigenvalues
eig.val <- factoextra::get_eigenvalue(pca)
eig.val
# Draw screeplot
factoextra::fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

# Variable contributions to axes
var <- factoextra::get_pca_var(pca)
# Corrplot of the cos2 of variables
corrplot::corrplot(var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 to Dim.3
factoextra::fviz_cos2(pca, choice = "var", axes = 1:3)
# Contributions of variables to PC1 : 
factoextra::fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2 :
factoextra::fviz_contrib(pca, choice = "var", axes = 2, top = 10)
# Variable contributions to axes
factoextra::fviz_pca_var(pca, col.var = "contrib",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


# VIF avec package usdm
usdm::vifcor(data_quantitative, th = 0.9, keep = NULL, method = 'pearson')


# Check correlation between quantitative covariates
psych::pairs.panels(data_quantitative,
                    method = "pearson", # correlation method
                    hist.col = "#00AFBB",
                    density = TRUE,  # show density plots
                    ellipses = FALSE) # show correlation ellipses
# on peut voir qu'il est pas necessaire de garder toutes les var de température et de chlorophyle (garder seulement pour le jour et l'année)
# atttention chloro année et temp année fortement corrélé aussi !
# attention a moyenne profondeur transect et moyenne bathy corrélées


corrplot::corrplot.mixed(cor(data_quantitative),
               lower = "number", 
               upper = "circle",
               tl.col = "black", 
               order = "hclust",
               tl.cex = 0.6) 


#############################################################################
#################### Dataset Analyse : PLOTS ################################

# do all of those plots with the variables chosen (or not if preliminary work) to investigate covariates 
# don't forget to calculate R (=richness) and Chondri (=elasmobranchs diversity) with Alicia Dalongeville indices (https://doi.org/10.1111/1365-2664.14276)

# New dataset with scaled quantitative covariates
data_quantitative <- data_MED_final[c("R","Chondri", "LFI", "least_dist_reserve", "habitat_div", "mean_depth_transect", "mean_bathy", "logland", "logport","chloroDay",   "chloroYear", "tempDay",  "tempYear")]

# Plot data
#confinement montre une différence de richesse spé
#protection ne montre pas de diff de richesse
#richesse spé semble augmenter en cotier (transect)
cowplot::plot_grid(nrow = 1, ncol = 3,
                   ggplot(data_MED_final, aes(y = Confinement, x = R, fill = R)) +
                     geom_boxplot(show.legend = FALSE)+
                     xlab("richesse spé") +
                     ylab("Confinement"),
                   ggplot(data_MED_final, aes(y = protection, x = R, fill = R)) +
                     geom_boxplot(show.legend = FALSE)+
                     xlab("richesse spé") +
                     ylab("protection"),
                   ggplot(data_MED_final, aes(y = R, x = transect)) +
                     geom_point() +
                     xlab("transect") +
                     ylab("richesse"))

#ploter plus en détail
ggplot(data_MED, aes(x=R, y=LFI, shape=Confinement, colour=Confinement, fill=Confinement)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("richesse") +
  ylab("LFI") +
  ggtitle("Une regression par confinement couleur") #lorsqu'on est en confinement, interieur ou dehors des reserves on a globalement une plus grande richesse associé avec des poissons de plus grande taille
#les transect offshore ont un LFI de 20 max et un R de 25 max (faible du a pb detection? effort d'echantillonnage plus faible?)

ggplot(data_MED_final, aes(x=R, y=Chondri, shape=transect, colour=transect, fill=transect)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("Chondri") +
  ggtitle("Une regression par confinement couleur")# quand richesse augmente richesse en chondri augmente (logique)
#observé pour reserve et en dehors, en confinement et en dehors, en cotier ou offshore

ggplot(data_MED_final, aes(x=R, y=least_dist_reserve, shape=protection, colour=protection, fill=protection)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("distance reserve") +
  ggtitle("Une regression par confinement couleur") #distance à la reserve semble pas expliquer richesse spé

ggplot(data_MED, aes(x=R, y=habitat_div, shape=habitat_principal, colour=habitat_principal, fill=habitat_principal)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("div habitat") +
  ggtitle("Une regression par confinement couleur") #pas de lien clair

ggplot(data_MED, aes(x=R, y=mean_depth_transect, shape=protection, colour=protection, fill=protection)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("profondeur moyenne transect") +
  ggtitle("Une regression par confinement couleur") #pas de lien clair

ggplot(data_MED, aes(x=R, y=logland, shape=Confinement, colour=Confinement, fill=Confinement)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("logland") +
  ggtitle("Une regression par confinement couleur") #en confinement il ne semble pas y avoir de lien entre richesse et distance à la côte, par contre hors confinement une diminution de la distance à la côte entraine une augmentation de la richesse
# la richesse augmente quand le log de la distance a la cote diminue en dehors des reserves (distance cst car reserve côtière) , quand on est offshore ou en cotier, distance à la cote nexplique pas modif de richesse

ggplot(data_MED_final, aes(x=R, y=logport, shape=protection, colour=protection, fill=protection)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("logport") +
  ggtitle("Une regression par confinement couleur") #logport explique pas changement de R

ggplot(data_MED_final, aes(x=R, y=chloroDay, shape=transect, colour=transect, fill=transect)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("chloroDay") +
  ggtitle("Une regression par confinement couleur") #pas de lien clair chloro et richesse

ggplot(data_MED, aes(x=R, y=tempDay, shape=protection, colour=protection, fill=protection)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  geom_point() +
  xlab("R") +
  ylab("tempDay") +
  ggtitle("Une regression par confinement couleur") #pas de lien clair entre temperature et richesse

# close to balanced dataframe
mosaicplot(table(cov_med$protection, cov_med$principal_substrat), main="Mosaic Plot")
table <- table(cov_med$protection, cov_med$principal_substrat)
# boxplot
boxplot(least_dist_reserve ~ protection, data = data_MED_final,
        xlab = "Protection", ylab = "Least Distance to Reserve",
        main = "Boxplot of Least Distance to Reserve by Protection")







#########################################################
############## Plot absences vs presences ###############
#########################################################

combined <- rbind(occ_med$fitting, occ_med$validation)

# for our models it's more than important to have an idea of the number of occurrences we are working with: the following code will help us understand the proportion of absence compare to presence for all species
# we will also focus on the difference between elasmobranchs (cartilaginous fish) and teleosts
# we will also extract the names of the species (=colnames) of which the numbers of occurrences is less than 30 (global rules is 10 occurences/covariates)
# we also compute prevalence of species 

#save for later occurences in a different dataset
# med_biodiv<-biodivmed_occ
# save(med_biodiv_2, file = "data/derived_data/med_biodiv_2.RData")

# Identify columns for occurrences
#species_names <- colnames(med_biodiv)[!colnames(med_biodiv) %in% c("Row.names", "longitude_start_DD", "latitude_start_DD")]
# species_name <- colnames(data_biodiv)[!colnames(data_biodiv) %in% c("Row.names")] #sans espèces sans occurences

# Calculate total occurrences for each species
total_occurrences <- colSums(combined[, species_names], na.rm = TRUE)
total_occurrences <- as.data.frame(total_occurrences)

# compute number of site 
num_rows <- nrow(combined)

## compute prevalence (=the proportion of presence records out of the total records for each species)
raw_prevalence <- total_occurrences$total_occurrences / num_rows
per_prevalence <- (total_occurrences$total_occurrences / num_rows) * 100

# Add this as a new column to your total_occurrences dataframe
total_occurrences$raw_prevalence <- raw_prevalence
total_occurrences$per_prevalence <- per_prevalence

total_occurrences_elasmo <- total_occurrences[rownames(total_occurrences) %in% elasmo_names, ]
total_occurrences_elasmo$Species <- rownames(total_occurrences_elasmo)

readr::write_csv(total_occurrences_elasmo, "total_occurrences_elasmo.csv")
# compute mean raw prevalence for elasmo and teleo (to be plot later) and total
mean_tot_prevalence <- mean(total_occurrences$raw_prevalence)
# Define the row names for which you want to compute the mean raw prevalence
elasmo <- c("Aetomylaeus_bovinus", "Bathytoshia_lata", "Dasyatis_pastinaca", "Dasyatis_tortonesei",
                   "Etmopterus_spinax", "Galeus_melastomus", "Mobula_mobular", "Mustelus_mustelus",
                   "Myliobatis_aquila", "Prionace_glauca", "Pteroplatytrygon_violacea", "Raja_brachyura",
                   "Raja_undulata", "Rostroraja_alba", "Scyliorhinus_canicula", "Scyliorhinus_stellaris",
                   "Squatina_squatina", "Torpedo_marmorata")

# Subset the dataframe based on the specified row names and calculate the mean raw prevalence
mean_elasmo_prevalence <- mean(total_occurrences[row.names(total_occurrences) %in% elasmo, "raw_prevalence"])
mean_teleo_prevalence <- mean(total_occurrences[!(row.names(total_occurrences) %in% elasmo), "raw_prevalence"])

# add a column 
total_occurrences$group <- ifelse(row.names(total_occurrences) %in% elasmo, "elasmo", "teleo")

# plot prevalence 
prevalence_comparison <- ggplot2::ggplot(total_occurrences, ggplot2::aes(x = group, y = prevalence, fill = group)) +
  ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 0.8), alpha = 0.6) +
  ggplot2::geom_point(data = total_occurrences, ggplot2::aes(x = group, y = prevalence), 
             color = "black", size = 1, alpha = 0.5, 
             position = ggplot2::position_jitterdodge(dodge.width = 0.8, jitter.width = 0.5), show.legend = FALSE) +
  ggplot2::labs(x = "group", y = "prevalence", fill = "group") +
  ggplot2::ggtitle("Percentages of Elasmobranchs and Teleosts Prevalence") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank(),  
        panel.grid.minor.x = ggplot2::element_blank())

# Specify an absolute path
save_directory <- "/home/celia/Documents/M2_internship/celia_project/celia_project/outputs/figures/prevalence/"
ggplot2::ggsave(filename= "prevalence_comparison", device = "pdf", width = 15, height = 10) 



# Extract species names for all categories
species_less <- rownames(subset(total_occurrences, total_occurrences < 10)) # 57 species to not put in the model
species_10_20 <- rownames(subset(total_occurrences, total_occurrences >= 10 & total_occurrences <= 20)) #15 faible occurrence
species_21_40 <- rownames(subset(total_occurrences, total_occurrences >= 21 & total_occurrences <= 40)) #13 faible 
species_41_70 <- rownames(subset(total_occurrences, total_occurrences >= 41 & total_occurrences <= 70)) #12 moyenne
species_71_90 <- rownames(subset(total_occurrences, total_occurrences >= 71 & total_occurrences <= 90)) #12 moyenne
species_91_140 <- rownames(subset(total_occurrences, total_occurrences >= 91 & total_occurrences <= 140)) #15 forte 
species_141_162 <-rownames(subset(total_occurrences, total_occurrences >= 141 & total_occurrences <= 162)) #7 forte
# 74 species in total

# create a new dataset with species with more than 10 occurences 
biodivmed_occ_2 <- data_biodiv[, !colnames(data_biodiv) %in% species_less]
biodivmed_occ_2$Row.names <- as.factor(biodivmed_occ_2$Row.names)
save(biodivmed_occ_2, file = "data/derived_data/biodivmed_occ_2.RData")

med_biodiv_2 <- med_biodiv[, !colnames(data_biodiv) %in% species_less]


# Create a data frame for plotting
total_occurrences$species <- rownames(total_occurrences)

# Calculate mean occurrences
mean_occurrences <- mean(total_occurrences$total_occurrences)
# mean_occ_elasmo <- mean(total_occurrences[row.names(total_occurrences) %in% elasmo, "mean"])
# mean_teleo_prevalence <- mean(total_occurrences[!(row.names(total_occurrences) %in% elasmo), "raw_prevalence"])

# Calculate percentage of species with occurrences above the mean
total_occurrences$above_mean <- ifelse(total_occurrences$total_occurrences > mean_occurrences, 1, 0)
total_occurrences$below_mean <- ifelse(total_occurrences$total_occurrences < mean_occurrences, 1, 0)
# Calculate percentages
percentage_above_mean <- (sum(total_occurrences$above_mean) / length(total_occurrences$species)) * 100
percentage_below_mean <- (sum(total_occurrences$below_mean) / length(total_occurrences$species)) * 100

# Define elasmobranch species to be colored differently
highlight_species <- c("Aetomylaeus_bovinus", "Bathytoshia_lata", "Dasyatis_pastinaca", "Dasyatis_tortonesei",
                        "Etmopterus_spinax", "Galeus_melastomus", "Mobula_mobular", "Mustelus_mustelus",
                        "Myliobatis_aquila", "Prionace_glauca", "Pteroplatytrygon_violacea", "Raja_brachyura",
                        "Raja_undulata", "Rostroraja_alba", "Scyliorhinus_canicula", "Scyliorhinus_stellaris",
                        "Squatina_squatina", "Torpedo_marmorata")

# Plotting using ggplot2
library(ggplot2)

ggplot(total_occurrences, aes(x = reorder(species, -total_occurrences), y = total_occurrences)) +
  geom_bar(stat = "identity", aes(fill = ifelse(species %in% highlight_species, "pink", "skyblue"))) +
  geom_hline(yintercept = mean_occurrences, linetype = "dashed", color = "black", size = 0.5) +
  annotate("text", x = max(seq_along(total_occurrences)) + 90, y = mean_occurrences,
           label = paste("Mean Occurrence:", round(mean_occurrences, 0)),
           vjust = -1, hjust = 0, color = "black", size = 4) +
  annotate("text", label = paste(round(percentage_above_mean, 1), "% above mean"),
           x = max(seq_along(total_occurrences)) + 90, y = mean_occurrences - 8,
           vjust = 0, hjust = 0, color = "black", size = 4) +
  annotate("text", label = paste(round(percentage_below_mean, 1), "% below mean"),
           x = max(seq_along(total_occurrences)) + 90, y = mean_occurrences - 10,
           vjust = 1, hjust = 0, color = "black", size = 4) +
  scale_fill_identity() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(color = "black", size = 10, hjust = 0.5)) +
  labs(title = "Total Occurrences for all Species",
       x = "Species",
       y = "Total Occurrences",
       caption = "Occurrences of teleosts (blue) and elasmobranchs (pink) species.")
save_directory <- "/home/celia/Documents/M2_internship/celia_project/celia_project/outputs/figures/prevalence/"
ggplot2::ggsave(filename= "occurrences2", device = "svg", width = 15, height = 10) 






  





###########################################################################################
#################### Calculate Alicia Dalongeville indices ################################
###########################################################################################
# https://github.com/AliciaDalongeville/eDNA_indicators_med/blob/main/01_indicators/indicators.R

## Load the eDNA data (matrix species per sample)
adne <- read.csv("Data/eDNA.csv", header=T, row.names = 1 )

# list of species
species <- rownames(adne)
# list of samples
samples <- colnames(adne)

## Calculate the indicators for each sample
##########################################################
## 1 - Species Richness R
##########################################################
indicators[,1] <- apply(adne, 2, sum)

##########################################################
## 2 - Functional diversity FD
##########################################################
for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # Get the functional groups of these species
  fd_i <- as.factor(traits[which(traits$Species %in% s_i), "GF"])
  
  # Number of unique functional groups
  indicators[i,2] <- nlevels(fd_i)
}

##########################################################
## 3 - Large Reef Fish Index - LFI
## 5 - Ratio Demerso-pelagic / benthic
## 7 - Chondrichtyen species
## 8 - Commercial species
## 9 - Highly commercial species 
##########################################################
for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # Calculate indicators
  indicators[i,"LFI"] <- sum(traits[which(traits$Species %in% s_i), "LRFI"]) 
  indicators[i,"DP_B_ratio"] <- sum(traits[which(traits$Species %in% s_i), "DP"]) / (sum(traits[which(traits$Species %in% s_i), "B"])+1)
  indicators[i,"Chondri"] <- sum(traits[which(traits$Species %in% s_i), "SHarK"])
  indicators[i,"Commercial"] <- sum(traits[which(traits$Species %in% s_i), "all_commercial_level"])
  indicators[i,"High_commerc"] <- sum(traits[which(traits$Species %in% s_i), "highly_commercial_only"])
}

###########################################################
## 4 Cryptobenthic (definition Brandl et al. 2018)
# Brandl SJ, Goatley CHR, Bellwood DR, Tornabene L. 2018 The hidden half: ecology and evolution of cryptobenthic fishes on coral reefs. Biol. Rev. 93, . (doi:10.1111/brv.12423))
###########################################################
crypto_families = c("Tripterygiidae", "Grammatidae", "Creediidae", "Aploactinidae", "Gobiidae", 
                    "Chaenopsidae", "Gobiesocidae", "Labrisomidae", "Pseudochromidae", "Bythitidae", 
                    "Plesiopidae", "Blenniidae", "Apogonidae", "Callionymidae", "Opistognathidae", "Syngnathidae")

traits <- traits %>%
  mutate(crypto_Brandl = if_else(Family %in% crypto_families, 1,0))

for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # Calculate indicator
  indicators[i,"Crypto"] <- sum(traits[which(traits$Species %in% s_i), "crypto_Brandl"]) 
}

##########################################################
## 11 - Vulnerability
##########################################################
for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # calculate indicators
  indicators[i,"Vulner"] <- mean(traits[which(traits$Species %in% s_i), "Vulnerability"], na.rm=T)
}

##########################################################
## 6 - Red List IUCN
##########################################################
## Count all species listed VU, EN or CR on the Red List of Threatened Species

### Make a dummy variable for IUCN categories
traits <- dummy_cols(traits, select_columns = 'IUCN_Red_List_Category')

## Calculate indicator
for (i in 1:nrow(indicators)) { # for each sample
  # list species present in the sample
  s_i <- adne %>%
    filter(adne[,i] == 1) %>%
    tibble::rownames_to_column(var="Sp") %>%
    pull(Sp)
  
  # Number of species per category
  VU <- sum(traits[which(traits$Species %in% s_i), "IUCN_Red_List_Category_VU"], na.rm=T)
  EN <- sum(traits[which(traits$Species %in% s_i), "IUCN_Red_List_Category_EN"], na.rm=T)
  CR <- sum(traits[which(traits$Species %in% s_i), "IUCN_Red_List_Category_CR"], na.rm=T)
  
  # Calculate indicator
  indicators[i,"RedList"] <- VU + EN + CR
}

##########################################################
## 10 - Phylogenetic Diversity - PD
##########################################################
# Retrieve the phylogeny of only native reef species across all three oceans.
phy <- fishtree_phylogeny(species = species)

plot(phy, show.tip.label = FALSE)
tiplabels(tip = which(phy$tip.label %in% species),
          pch=19, cex=2)

rownames(adne) <- gsub(" ", "_", rownames(adne), fixed = TRUE)

## check that phylogeny and data have matching names
#nc <- geiger::name.check(phy, adne) # 24 species not in tree (chondrychtyans + synonyms)

# Manually check synonyms and find the species
species[species == "Mullus barbatus"] <- "Mullus barbatus barbatus"
rownames(adne)[rownames(adne) == "Mullus_barbatus"] <- "Mullus_barbatus_barbatus"

species[species == "Diplodus sargus"] <- "Diplodus sargus sargus"
rownames(adne)[rownames(adne) == "Diplodus_sargus"] <- "Diplodus_sargus_sargus"

species[species == "Diplodus cervinus"] <- "Diplodus cervinus cervinus"
rownames(adne)[rownames(adne) == "Diplodus_cervinus"] <- "Diplodus_cervinus_cervinus"

species[species == "Chelon auratus"] <- "Liza aurata"
rownames(adne)[rownames(adne) == "Chelon_auratus"] <- "Liza_aurata"

species[species == "Chelon ramada"] <- "Liza ramada"
rownames(adne)[rownames(adne) == "Chelon_ramada"] <- "Liza_ramada"

# Retrieve the missing phylogeny 
phy <- fishtree_phylogeny(species = species, type="phylogram")
nc <- geiger::name.check(phy, adne) # 19 species not in the tree

# Remove from the data the species that are not in the tree
adne2 <- adne[which(rownames(adne) %in% nc$data_not_tree == F),]

# Transpose the ADNe matrix 
adne2 <- t(adne2)

# prune the tree
prunedTree <- prune.sample(adne2,phy)

# Calculate Faith's PD
pd.result <- pd(adne2, prunedTree, include.root=T)

# Add PD to indicator dataframe
indicators[,"PD"] <- pd.result$PD/pd.result$SR  #percentage of species richness with only species that are in the tree

# Save the indicator table
write.csv(indicators, file="01_indicators/indicators_updated.csv")







# # Plot for TELEOSTS
# # Create a neutral color for other teleosts
# neutral_color <- "grey"
# highlight <- "skyblue"
# 
# # Specify an absolute path
# save_directory <- "/home/celia/Documents/M2_internship/celia_project/celia_project/outputs/figures/teleosts_occurrences/"
# 
# plots_list <- list()  # Initialize an empty list to store plots
# plots_per_grid <- 12  # Set the number of plots per grid
# 
# for (i in seq_along(teleost)) {
#   species <- teleost[i]
#   
#   # Filter melted_elasmo for the current species
#   subset_data <- melted_teleost[melted_teleost$variable == species, ]
#   data <- subset(melted_teleost, variable != species)
#   
#   # Generate the plot
#   plot <- ggplot(data = world) +
#     geom_sf(color = "black", fill = "light grey") +
#     xlab("Longitude") + ylab("Latitude") +
#     coord_sf(xlim = c(2.79123253648454, 10.647334299871368), ylim = c(41.1906737866009, 44.44029498045467), expand = FALSE) +
#     ggtitle(paste("Occurrences of", species)) +
#     theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2),
#           panel.background = element_rect(fill = "light blue")) +
#     geom_point(data = data, aes(x = longitude_start_DD, y = latitude_start_DD, color = "grey", fill = "grey"), size = 1, alpha = 0.7) +
#     geom_point(data = subset_data, aes(x = longitude_start_DD, y = latitude_start_DD, color = "skyblue", fill = "skyblue"), size = 3, alpha = 0.7) +
#     scale_color_manual(values = c(neutral_color, highlight), labels = c("Other Teleosts", species)) +
#     scale_fill_manual(values = c(neutral_color, highlight), labels = c("Other Teleosts", species)) +
#     labs(title = paste("Occurrences of", species),
#          x = "Longitude",
#          y = "Latitude",
#          color = "Presence",
#          fill = "Presence") +
#     guides(shape = guide_legend(title = "Legend Title", override.aes = list(size = 4)))
#   # Add the plot to the list
#   plots_list[[length(plots_list) + 1]] <- plot
#   
#   # Check if we've reached the desired number of plots per grid or it's the last species
#   if (length(plots_list) %% plots_per_grid == 0 || i == length(teleost)) {
#     # Arrange and display the plots in a grid
#     grid_arrange <- gridExtra::grid.arrange(grobs = plots_list, ncol = 3)
#     
#     # Save the grid to a file
#     save_filename <- paste0(save_directory, "grid_", (i - 1) %/% plots_per_grid + 1, ".svg")
#     ggsave(save_filename, grid_arrange, device = "svg", width = 15, height = 10)  # Adjust width and height as needed
#     
#     # Clear the plots_list for the next grid
#     plots_list <- list()
#   }
# }












