################################################################################
# 0. Library and data loading
library(geobr)
library(dplyr)
library(ggforce)
library(ggplot2)
library(rmapshaper)
library(readxl)
library(sf)
library(tidyr)
library(purrr)

## Defines some layout constants
width <- 10 ### Plot width in inches
height <- 10.5 ### Plot height in inches

## Defines color palette based on Du Bois palette as presented by Anthony Starks
## (obtained from https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf)
palette <- c(
  "brown" = "#654321",
  "red" = "#dc143c",
  "green" = "#00aa00",
  "blue" = "#4682b4",
  "tan" = "#d2b48c",
  "gold" = "#ffd700",
  "pink" = "#ffc0cb",
  "mint" = "#afffb4",
  "cyan" = "#bae8ff"
)

## Data on enslaved Blacks comes from the Slave Voyages Project.
## Query made in https://www.slavevoyages.org/voyage/database filtering only
## travels with purchases in Africa and landings in Brazil
rawdata <- readxl::read_xlsx("2022/day03/data.xlsx", sheet = "Data")
br_regions <- readxl::read_xlsx("2022/day03/data.xlsx", sheet = "Brazil")
af_regions <- readxl::read_xlsx("2022/day03/data.xlsx", sheet = "Africa")

## Unites the African id info. and the enslaved people data.
## Keeps columns only of the number of embarked
enslaved <- rawdata %>% 
  dplyr::left_join(af_regions) %>% 
  dplyr::select(-c("id_AF","Embarked_Total","port_AF"), -starts_with("Disembarked"))

## Sums the data for each African region
enslaved <- enslaved %>% 
  dplyr::group_by(region_AF) %>% 
  dplyr::summarise(across(.fns = sum)) %>% 
  dplyr::ungroup() 

## Arranges all embarked people data in a few columns
enslaved <- enslaved %>% 
  tidyr::pivot_longer(cols = starts_with("Embarked"),
                      names_prefix = "Embarked_",
                      names_to = "id_BR",
                      values_to = "people") %>% 
  dplyr::mutate(id_BR = as.numeric(id_BR))

## Unites the Brazilian id info. and the enslaved data
enslaved <- enslaved %>% 
  dplyr::left_join(br_regions) %>% 
  dplyr::select(-c("id_BR","port_BR"))

## Sums the data for each Brazilian region
enslaved <- enslaved %>% 
  dplyr::group_by(region_AF, region_BR) %>% 
  dplyr::summarise(people = sum(people)) %>% 
  dplyr::ungroup() 

## Keeps only data on landings in Bahia and
## gets the distribution of enslaved people by origin
enslaved <- enslaved %>% 
  dplyr::filter(region_BR == "Bahia") %>% 
  dplyr::mutate(pct = round(100*people/sum(people), 1)) %>% 
  dplyr::arrange(desc(pct))
enslaved

## Gets Bahia's shape and simplifies it
bahia <- geobr::read_state(code_state = 29) %>% 
  rmapshaper::ms_simplify(keep = 0.03, weighting = 1)

## Gets Bahia's boundaries and places it in a tibble
bound <- sf::st_geometry(bahia)
bound <- bound[[1]][[1]]
colnames(bound) <- c("x", "y")
bound <- bound %>% as_tibble()

## Gets Bahia's area size (without units)
bahia_area <- bound %>% 
  as.matrix() %>% 
  list() %>% 
  sf::st_polygon() %>% 
  sf::st_area()

## Define points to represent the African regions in
## such way that the Voronoi tiles approximately represent 
## the size of enslaved population that landed in Bahia
df <- tibble(
  x = c(-41.5,-38.3,-39,
        -39.5,-38.2,-37.9,
        -37.9,-37.6,-39.5),
  y = c(-10.5,-11.6,-10.5,
        -8.8,-10.3,-11.6,
        -10.3,-11.6,-8.6),
  fill = palette,
  color_name = names(palette),
  region_AF = enslaved$region_AF
)

## Creates a simple draft of the plot and obtains the areas of the generated
## Voronoi tiles. Uses this info. to retroactively define the coordinates of
## the points that represent the African regions so the sizes are proportional
### Creates the draft and extracts the coordinates of the regions
draft <- df %>% 
  ggplot() +
  ggforce::geom_voronoi_tile(aes(x = x, y = y, group = 1L, fill = I(fill)),
                             color = "white", size = 1.5,
                             bound = bound, normalize = TRUE)
area <- ggplot_build(draft)
area <- area$data[[1]]

### Obtains the area of the regions
area <- area %>% 
  dplyr::select(fill, x, y) %>% 
  dplyr::group_by(fill) %>% 
  dplyr::slice(c(1:n(),1)) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(data = purrr::map(data, function(coords) {
    
    coords %>% 
      as.matrix() %>% 
      list() %>% 
      sf::st_polygon() %>% 
      sf::st_area()
    
  })) %>% 
  tidyr::unnest(cols = data)

### Shows the percentage in the map from the total area and
### joins with coordinates and enslaved data
area %>%
  dplyr::mutate(pct_map = round(100*data/bahia_area, 1)) %>% 
  dplyr::left_join(df) %>% 
  dplyr::left_join(enslaved) %>% 
  dplyr::arrange(desc(pct))

shape <- draft +
  coord_sf(xlim = c(-47,-37), ylim = c(-8,-18.5)) +
  theme_void() +
  theme(
    plot.margin = margin(0,0,0,0, "pt")
  )
  
ggsave("2022/day03/shape.svg", plot = shape, dpi = "retina",
       width = width, height = height)
