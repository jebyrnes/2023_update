#' =========================
#' Maps of Data locations
#' =========================

library(dplyr)
library(meowR)
library(ggplot2)
library(sf)
library(meowR)
library(rnaturalearth)

theme_set(theme_classic(base_size = 20) )

#let's work in sf, shall we?
ecoregions <- st_as_sf(regions)
countries <- ne_countries(returnclass = "sf") 
coast <- ne_coastline(returnclass = "sf") 

#open the data
source("scripts/open_all_data_for_analysis.R")

makeDataSummary <- function(adf){
  ret <- adf %>%
    group_by(ECOREGION) %>%
    dplyr::summarise(Studies = length(unique(Authors..Year.)),
                     `Data Points` = length(Authors..Year.),
                     log_data_points=log(`Data Points`))
  ret
  
}

make_data_map <- function(dat){
  left_join(ecoregions, makeDataSummary(dat )) |>
    filter(!is.na(`Data Points`)) |>
    ggplot() +
    geom_sf(data = countries, color = NA) +
    geom_sf(data = coast, linewidth = 0.1) +
    geom_sf(aes(fill = `Data Points`)) + 
    scale_fill_distiller(trans = "log10",
                         palette = "Spectral") +
    labs(title = "Data Points by Ecoregion",
         fill = "Data Points")
}

##Fig 1. Total Map using data points by ecoregion
make_data_map(masterData)
ggsave(file = "figures/Figure1_all_data_points.jpg",
       dpi=300)

##Sup Fig 2. Abund map using data points by ecoregion
make_data_map(totalabund)
ggsave(file = "figures/Sup_2_abund_data_points.jpg",
       dpi=300)


##Sup Fig 3. Richness map using data points by ecoregion
make_data_map(richness)
ggsave(file = "figures/Sup_3_rich_data_points.jpg",
       dpi=300)


##Sup Fig 4. Single sp abundance map using data points by ecoregion
make_data_map(singleSpAbund)
ggsave(file = "figures/Sup_4_single_sp_abund_data_points.jpg",
       dpi=300)

##Sup Fig 5. Updated Map using data points by ecoregion
make_data_map(biomass)
ggsave(file = "figures/Sup_5_biomass_data_points.jpg",
       dpi=300)