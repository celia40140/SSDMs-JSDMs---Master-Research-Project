
################################################################################
#                 Check for model parameter convergence
##################

# load models
load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/outputs/hmsc_models/hmsc_models/multivariate/hmsc_multi_finalv2.Rdata")

# ou
load("~/Bureau/LAST VERSION FROM PERSONNAL LAPTOP/Final Project/celia_project/outputs/hmsc_models/shmsc_models/multivariate/shmsc_multi_finalv2.Rdata")

## MCMC convergence
# species niches Beta; residual species associations Omega
# Histograms of effective sample sizes and potential scale reduction factors (psrf) for Beta and Omega parameters
ns = 114 # nbr espèces
mpost = Hmsc::convertToCodaObject(model_fit_mcmc)
par(mfrow=c(2,2))
ess.beta = coda::effectiveSize(mpost$Beta)
psrf.beta = coda::gelman.diag(mpost$Beta, multivariate=FALSE)$psrf
hist(ess.beta)
hist(psrf.beta)
sppairs = matrix(sample(x = 1:ns^2, size = 100))
tmp = mpost$Omega[[1]]
for (chain in 1:length(tmp)){
  tmp[[chain]] = tmp[[chain]][,sppairs]
}
ess.omega = coda::effectiveSize(tmp)
psrf.omega = coda::gelman.diag(tmp, multivariate=FALSE)$psrf
hist(ess.omega)
hist(psrf.omega)

# tmp = mpost$Alpha
# ess.alpha = coda::effectiveSize(tmp)
# psrf.alpha = coda::gelman.diag(tmp, multivariate=FALSE)$psrf
# hist(ess.alpha)
# hist(ess.alpha)


# ggplot2 to MCMC diagnostics, 
S <- ggmcmc::ggs(mpost$Beta)
cov <- "dist_fully_protected_MPA"
S <- S[grep(cov, S$Parameter),]

################################################################################
#           Variables contribution with variance partionning
##################

## Variance partionning
head(model_fit_mcmc$X)   # follow the order to assign group
VP = Hmsc::computeVariancePartitioning(model_fit_mcmc) #, group = c(1,2,2,1,3,3), groupnames = c("gravity","habitat", "environment"))
Hmsc::plotVariancePartitioning(model_fit_mcmc, VP = VP, main = "Variance Partitioning", )


# Variance partitioning ggplot
VP_long <- as.data.frame(VP[["vals"]]) |>
  tibble::rownames_to_column(var = "Covariate") |>
  tidyr::pivot_longer(
    cols = - Covariate,
    names_to = "Response",
    values_to = "Value"
  )
VP_long$Covariate <- forcats::fct_relevel(VP_long$Covariate, c(
  unique(VP_long$Covariate)[!grepl("Random", unique(VP_long$Covariate))],
  "Random: associations"))

# Rename categories in the "Covariate" column
VP_long <- VP_long |>
  dplyr::mutate(Covariate = dplyr::case_when(
    Covariate %in% c("nb_substrats", "principal_substrat") ~ "substrat",
    Covariate %in% c("dist_fully_protected_MPA", "protection") ~ "protection",
    TRUE ~ Covariate
  ))

# Calculate mean values for each covariate
summary_VP <- VP_long |>
  dplyr::group_by(Covariate) |>
  dplyr::summarise(
    Mean_Value = mean(Value),
    SD_Value = sd(Value)
  )

####################################################################################
########### variance partitioning plot for the model 
library(ggplot2)

# Define specific colors for each covariate
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

# Use specific colors in your plot
ggplot(summary_VP) +
  geom_col(data = summary_VP, aes(x = reorder(Covariate, Mean_Value), y = Mean_Value, fill = Covariate)) +
  geom_errorbar(data = summary_VP, aes(x = Covariate, y = Mean_Value, ymin = Mean_Value - SD_Value, ymax = Mean_Value + SD_Value), width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = specific_colors) +  # Assign specific colors to covariates
  theme_bw() +
  coord_flip() +
  labs(y = "Mean Explained Variance ", x = "") +
  ggtitle("SHMSC") +
  theme(legend.position = "none",
        plot.title = element_text(size = 18, hjust = 0.5),
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

# Filter data for elasmo_names
elasmo_names <- readr::read_csv("~/Bureau/hmsc-hpc/examples/basic_example/data/derived_data/elasmo_names.csv")
elasmo_names <- elasmo_names$elasmo_names

elasmo_VP <- subset(VP_long, Response %in% elasmo_names)

# Calculate mean values for each covariate
summary_elasmo <- elasmo_VP |>
  dplyr::group_by(Covariate) |>
  dplyr::summarise(
    Mean_Value = mean(Value),
    SD_Value = sd(Value)
  )

ggplot(summary_elasmo) +
  geom_col(data = summary_elasmo, aes(x = reorder(Covariate, Mean_Value), y = Mean_Value, fill = Covariate)) +
  geom_errorbar(data = summary_elasmo, aes(x = Covariate, y = Mean_Value, ymin = Mean_Value - SD_Value, ymax = Mean_Value + SD_Value), width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = specific_colors) +  # Assign specific colors to covariates
  theme_bw() +
  coord_flip() +
  labs(title = "SHMSC", y = "Mean Explained Variance for Elasmobranchs", x = "") +
  theme(legend.position = "none",
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

####################################################################################
########### variance partitioning plot for all elasmo species independently
# Plot for elasmo_names with specific colors and reordered levels
ggplot(elasmo_VP, aes(x = Response, y = Value, fill = Covariate)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.2) +
  scale_fill_manual(values = specific_colors) +  # Use specific colors
  labs(
    title = "SHMSC Variance partitioning for Elasmobranchs",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 50, hjust = 0.5, vjust = 0.5, size = 12),
    legend.position = "right",
    legend.text = element_text(size = 10)
  )


####################################################################################
########### Mean variance partitioning plot for teleost

# Filter data for teleost_names
teleost_names <- readr::read_csv("~/Bureau/hmsc-hpc/examples/basic_example/data/derived_data/teleost_names.csv")
teleost_names <- teleost_names$teleost_names

teleost_VP <- subset(VP_long, Response %in% teleost_names)

# Calculate mean values for each covariate
summary_teleost <- teleost_VP |>
  dplyr::group_by(Covariate) |>
  dplyr::summarise(
    Mean_Value = mean(Value),
    SD_Value = sd(Value)
  )

ggplot(summary_teleost) +
  geom_col(data = summary_teleost, aes(x = reorder(Covariate, Mean_Value), y = Mean_Value, fill = Covariate)) +
  geom_errorbar(data = summary_teleost, aes(x = Covariate, y = Mean_Value, ymin = Mean_Value - SD_Value, ymax = Mean_Value + SD_Value), width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(values = specific_colors) +  # Assign specific colors to covariates
  theme_bw() +
  coord_flip() +
  labs(title = "SHMSC", y = "Mean Explained Variance for Teleosts", x = "") +
  theme(legend.position = "none",
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        strip.background = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


####################################################################################
########### variance partitioning plot for all teleost species independently
# Plot for teleost_names with specific colors and reordered levels
ggplot(teleost_VP, aes(x = Response, y = Value, fill = Covariate)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.2) +
  scale_fill_manual(values = specific_colors) +  # Use specific colors
  labs(
    title = "SHMSC Variance partitioning for Teleosts",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 50, hjust = 0.5, vjust = 0.5, size = 8),
    legend.position = "right",
    legend.text = element_text(size = 10)
  )





































## Parameters estimates
# species response to covariates
postBeta = Hmsc::getPostEstimate(model_fit_mcmc, parName = "Beta")

elasmo <- c("Scyliorhinus_canicula", "Dasyatis_pastinaca", "Dasyatis_tortonesei", "Pteroplatytrygon_violacea"   ,     "Mobula_mobular" , "Myliobatis_aquila" ,"Raja_brachyura", "Rostroraja_alba" ,"Tetronarce_nobiliana" , "Torpedo_marmorata" )

teleost_names <- readr::read_csv("~/Bureau/hmsc-hpc/examples/basic_example/data/derived_data/teleost_names.csv")
teleost_names <- teleost_names$teleost_names

matching_names <- elasmo[elasmo %in% colnames(postBeta[["mean"]])]

matching_names <- teleost_names[teleost_names %in% colnames(postBeta[["mean"]])]

# select values for elasmo
# Subset each component in postBeta to include only the matching species names
postBeta_elasmo <- list(
  mean = postBeta[["mean"]][, matching_names, drop = FALSE],
  support = postBeta[["support"]][, matching_names, drop = FALSE],
  supportNeg = postBeta[["supportNeg"]][, matching_names, drop = FALSE]
)
postBeta_teleost <- list(
  mean = postBeta[["mean"]][, matching_names, drop = FALSE],
  support = postBeta[["support"]][, matching_names, drop = FALSE],
  supportNeg = postBeta[["supportNeg"]][, matching_names, drop = FALSE]
)

#Plots heatmaps of parameter estimates or posterior support values of species' environmental responses, i.e. how species in Y responds to covariates in X
#"Mean" for posterior mean estimate, "Support" for the level of statistical support measured by posterior probability for a positive or negative response, and "Sign" to indicate whether the response is positive, negative
# Define a custom color palette with more than two colors
colfunc <- colorRampPalette(c("red","white","royalblue"))
# Specify the number of color levels
num_color_levels <- 15

Hmsc::plotBeta(model_fit_mcmc, 
               post = postBeta_elasmo, 
               param = "Support", 
               supportLevel = 0.9,
               SpeciesOrder = "Vector", 
               SpVector = matching_names,  
               spNamesNumbers = c(TRUE, FALSE),
               covNamesNumbers = c(TRUE, FALSE),
               cex = c(0.7, 0.7, 0.8),
               main = "Elasmobranchs Environmental Responses",
               colors = colfunc,
               colorLevels = num_color_levels,
               newplot = TRUE)

Hmsc::plotBeta(model_fit_mcmc, 
               post = postBeta_teleost, 
               param = "Support", 
               supportLevel = 0.9,
               SpeciesOrder = "Vector", 
               SpVector = matching_names,  
               spNamesNumbers = c(TRUE, FALSE),
               covNamesNumbers = c(TRUE, FALSE),
               cex = c(0.7, 0.7, 0.8),
               main = "Teleosts Environmental Responses",
               colors = colfunc,
               colorLevels = num_color_levels,
               newplot = TRUE)



colfunc <- colorRampPalette(c("red","white","royalblue"))
# Specify the number of color levels
num_color_levels <- 15

# residual associations among species
OmegaCor = Hmsc::computeAssociations(model_fit_mcmc)
supportLevel = 0.9

toPlot = ((OmegaCor[[1]]$support>supportLevel)
          + (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean

toPlot_elasmo <- toPlot[elasmo, elasmo, drop = FALSE]

corrplot::corrplot(toPlot_elasmo, method = "color",
                   col=colorRampPalette(c("red","white","royalblue"))(200),
                   tl.cex=.6, tl.col="black",
                   title=paste("Random effect:", model_fit_mcmc$rLNames[1]), mar=c(0,0,1,0))

# plot elasmo and associated species : 
# Identify species associated with elasmo
# Initialize a vector to store associated teleost species
associated_teleost <- character(0)

# Iterate over each elasmo species
for (elasmo_species in elasmo) {
  # Extract the row corresponding to the current elasmo species
  elasmo_row <- as.data.frame(toPlot["Torpedo_marmorata", ])
  
  # Find teleost species associated with the current elasmo species
  associated_teleost <- c(associated_teleost, row.names(elasmo_row)[elasmo_row != 0])
}

# Remove duplicates from the associated teleost vector
associated_teleost <- unique(associated_teleost)
associated_sp <- c("Muraena_helena","Synodus_saurus","Belone_belone","Aidablennius_sphynx", "Lipophrys_trigloides", "Parablennius_gattorugine","Parablennius_incognitus","Parablennius_pilicornis","Parablennius_sanguinolentus","Parablennius_zvonimiri","Lepadogaster_candolii","Tripterygion_delaisi","Tripterygion_melanurum" ,"Tripterygion_tripteronotum","Seriola_dumerili","Coryphaena_hippurus", "Sardina_pilchardus","Sardinella_aurita","Merluccius_merluccius","Aphia_minuta","Buenia_affinis","Corcyrogobius_liechtensteini","Crystallogobius_linearis","Gobius_ater","Gobius_bucchichi","Gobius_niger","Gobius_xanthocephalus","Millerigobius_macrocephalus","Odondebuenia_balearica","Pseudaphya_ferreri","Zebrus_zebrus","Apogon_imberbis",            
"Coris_julis","Symphodus_ocellatus","Symphodus_tinca","Thalassoma_pavo",             
"Chelon_auratus",               "Chelon_labrosus",              "Chelon_ramada",                "Oedalechilus_labeo",          
"Ceratoscopelus_maderensis",    "Myctophum_punctatum",          "Scorpaena_notata",             "Scorpaena_porcus",            
"Scorpaena_scrofa",             "Anthias_anthias",             "Epinephelus_marginatus",       "Serranus_cabrilla",           
"Serranus_hepatus"   ,          "Serranus_scriba" ,             "Cepola_macrophthalma" ,        "Euthynnus_alletteratus" ,     
"Sarda_sarda" ,                 "Scomber_colias"  ,             "Thunnus_thynnus"  ,            "Spicara_maena"    ,           
"Boops_boops"      ,            "Diplodus_annularis"     ,      "Diplodus_puntazzo"   ,         "Diplodus_sargus" ,            
"Diplodus_vulgaris"   ,         "Lithognathus_mormyrus" ,       "Oblada_melanura" ,             "Pagellus_acarne"   ,          
"Pagellus_erythrinus"  ,        "Sarpa_salpa"       ,           "Sparus_aurata"  ,              "Spondyliosoma_cantharus"   ,  
"Maurolicus_muelleri"  ,        "Mullus_barbatus"    ,          "Mullus_surmuletus" ,           "Gymnammodytes_semisquamatus" ,
"Uranoscopus_scaber"   ,        "Dicentrarchus_labrax"  ,       "Chromis_chromis"   ,           "Sciaena_umbra"   , "Sphyraena_sphyraena"   ,       "Sphyraena_viridensis"  , "Engraulis_encrasicolus"  ,     "Conger_conger"     ,           "Parablennius_rouxi"     ,      "Lepadogaster_lepadogaster" , "Istiophorus_platypterus" ,     "Symphodus_bailloni"  ,         "Lampanyctus_crocodilus" ,      "Bothus_podas"  , "Pegusa_nasuta"    ,            "Pomatomus_saltatrix" ,         "Pagellus_bogaraveo" ,    "Gobius_cruentatus"  , "Xyrichtys_novacula"  , "Trachinus_draco" ,"Gaidropsarus_mediterraneus","Blennius_ocellaris" ,         
"Lebetus_guilleti" , "Scyliorhinus_canicula", "Dasyatis_pastinaca", "Dasyatis_tortonesei", "Pteroplatytrygon_violacea"   ,     "Mobula_mobular" , "Myliobatis_aquila" ,"Raja_brachyura", "Rostroraja_alba" ,"Tetronarce_nobiliana" , "Torpedo_marmorata" )     

# plot 
toPlot_asso <- toPlot[associated_sp, associated_sp, drop = FALSE]
corrplot::corrplot(toPlot_asso, method = "color",
                   col=colorRampPalette(c("red","white","royalblue"))(200),
                   tl.cex=.35, tl.col="black",
                   title=paste("Random effect:", model_fit_mcmc$rLNames[1]), mar=c(0,0,1,0))


## Plotting variation over environmental gradients
Gradient <- Hmsc::constructGradient(model_fit_mcmc,
                                    focalVariable = "tempDay",
                                    non.focalVariables = list("protection"=list(1,"reserve"),
                                                              "habitat_div"=list(1),
                                                              "mean_bathy"=list(1),
                                                              "logland"=list(1),
                                                              "chloroDay"=list(1)))


Gradient$XDataNew
#XFormula = ~ protection + habitat_div + mean_bathy + logland + chloroDay + tempDay,

predY = predict(model_fit_mcmc, XData=Gradient$XDataNew, studyDesign=Gradient$studyDesignNew,
                ranLevels=Gradient$rLNew, expected=FALSE)

Hmsc::plotGradient(model_fit_mcmc, Gradient, pred=predY, measure="S", showData = TRUE)
#measure="S" to plot the summed expected species richness, i.e. the row sum of the predicted communities.
# predicted response peaks where ?

# measure="Y" to visualize the same predictions for individual species and by using index to select the species to be visualized (as ordered in the matrix m$Y).
Hmsc::plotGradient(model_fit_mcmc, Gradient, pred=predY, measure="Y", index = 15, showData = TRUE)
