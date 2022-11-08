################################################################################
# 0. Library and data loading
library(dplyr)
library(GDAtools)
library(ggplot2)
library(ggsvg)
library(hues)
library(lubridate)
library(purrr)
library(readr)
library(rsvg)
library(sf)
library(tidyr)
library(USAboundaries)

## Gets the data on UFO sightings
sights <- readr::read_csv("2022/day04/ufo_sighting_data.csv")

## Gets the US states shapes
us_states <- USAboundaries::us_states(resolution = "low")

################################################################################
# 1. Data handling
## Keeps only the US states in the mainland
us_states <- us_states %>% 
  dplyr::filter(!(name %in% c("Hawaii","Alaska","Puerto Rico")))

## Keeps only sightings within US mainland and that have coordinates
sights <- sights %>% 
  dplyr::filter(country == "us", !(`state/province` %in% c("hi","ak","pr"))) %>% 
  dplyr::filter(if_all(.cols = c("latitude","longitude"), .fns = ~!is.na(.)))

## Keeps only sightings from 2013 and on
sights <- sights %>% 
  dplyr::mutate(Date_time = lubridate::mdy_hm(Date_time)) %>% 
  dplyr::filter(lubridate::year(Date_time) >= 2013)

## Keeps only the coordinates
sights <- sights %>% dplyr::select("longitude","latitude")

## Converts the data.frame to an sf object and
## applies the CRS of the US map to it
sights <- sf::st_as_sf(sights, coords = c("longitude","latitude"))
st_crs(sights) <- sf::st_crs(us_states)

## Calculates the distance matrix between points and
## converts it to a dist object
dist <- sf::st_distance(sights)
dist <- as.dist(dist)

## Creates 10 clusters that aggregate the points
hc <- fastcluster::hclust(dist, "median")
memb <- stats::cutree(hc, k = 10)

## Add the membership info. to the database
sights <- sights %>% dplyr::mutate(member = factor(memb))

## Gets medoids for each cluster
medoids <- GDAtools::medoids(dist, memb)

## Keeps only the medoids and their coordinates
ufos <- sights %>% 
  dplyr::slice(medoids) %>% 
  sf::st_geometry() %>% 
  purrr::map(~tibble(
    longitude = .x[1],
    latitude = .x[2])
  )

################################################################################
# 2. Produces the plot
## Loads the svg image as text
svg_url <- '2022/day04/alien.svg'
svg_txt <- paste(readLines(svg_url), collapse = "\n")

## Places the US map and the actual points by cluster membership
plot <- us_states %>% 
  ggplot() +
  geom_sf(fill = "black", lwd = 0.1) +
  geom_sf(aes(color = member), size = 0.5, data = sights) +
  hues::scale_color_iwanthue(hmin = 80, hmax = 200,
                             lmin = 30, cmin = 20)

## Places the medoids as "little green aliens"
purrr::walk(ufos, function(data) {
  p = plot + ggsvg::geom_point_svg(aes(longitude, latitude), svg = svg_txt, data = data) 
  assign("plot", p, envir = .GlobalEnv)
})

## Places texts and customizations of theme
plot <- plot + 
  theme_void() +
  theme(
    plot.margin = margin(0,0,0,0),
    legend.position = "none"
  )

## Saves the plot
ggsave("2022/day04/us.png", plot = plot, dpi = "retina",
       width = 3000, height = 2000, units = "px", bg = NA)
