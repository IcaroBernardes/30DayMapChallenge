################################################################################
# 0. Library and data loading
library(dplyr)
library(cartogram)
library(tmap)
library(geobr)
library(glue)
library(sf)
library(sp)
library(stringr)
library(tidytext)

## Loads the data. Obtained from https://data.brasil.io/dataset/socios-brasil/empresas.csv.gz
## and filtered to keep only business which have 'point' in their official name
rawdata <- readRDS("2022/day01/rawdata.RDS")

## Loads the shapes of the states
ufs <- geobr::read_state()

## Loads the shapes of the country
br <- geobr::read_country()

################################################################################
# 1. Data handling
## Converts the sf object to a SpatialPolygonsDataFrame
ufs <- sf::as_Spatial(ufs)

## Projects the SpatialPolygonsDataFrame
carto <- sp::spTransform(ufs, sp::CRS("+init=epsg:3395"))

## Counts the number of business per state
df <- rawdata %>% dplyr::count(uf, name = "Business")

## Joins the data inside the SpatialPolygonsDataFrame
carto@data <- dplyr::left_join(carto@data, df, by = c("abbrev_state" = "uf"))

## Makes NA's as zeros
carto@data <- carto@data %>%
  dplyr::mutate(Business = ifelse(is.na(Business), 0, Business))

## Creates the cartogram
carto <- cartogram::cartogram_dorling(x = carto, weight = "Business")

## Finds the most frequent meaningful words in the names
stopwords <- c(
  "ltda", "s", "a", "sa", "eireli", ### types of business
  "e", "de", "do", "da", "dos", "das", "com", "em", "para", ### connection-words (e.g.: 'of', 'and')
  "c", "m", ### initials?
  "point"
)
topwords <- rawdata %>% dplyr::distinct(razao_social)
n_points <- dim(topwords)[1]
topwords <- topwords %>% 
  tidytext::unnest_tokens(output = "words", input = "razao_social") %>% 
  dplyr::filter(!(words %in% stopwords)) %>% 
  dplyr::count(words) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::mutate(pct = round(100*cumsum(n)/n_points)) %>% 
  dplyr::filter(pct <= 70)

################################################################################
# 2. Creates and saves the map
## Opens the saving device
ragg::agg_png(
  filename = "2022/day01/day01.png",
  width = 2000,
  height = 2400,
  res = 320
)

## Defines the subtitle
subtitle <- 'Brazilian business love to use English words in their names.
The map bellow shows the number of companies that
use the term "point" in their official name by state.'

## Defines the detail
detail <- rep("\n", 21) %>% glue::glue_collapse()
detail <- glue::glue('{detail}
They mostly have generic names
(e.g.: commercial point) or
sell Food and Beverages
(e.g.: bar point, snacks point).')

## Defines the credits
credits <- 'Data from: Brasil.io
Author: Ícaro Bernardes (@IcaroBSC)'

## Makes the map
tm_style("cobalt") +
  tm_shape(carto) + tm_polygons("Business") +
  tm_shape(ufs) + tm_borders(col = "gray", lwd = 0.5) +
  tm_shape(br) + tm_borders(col = "white", lwd = 3) +
  tm_credits(
    text = subtitle,
    size = 1,
    position = c("left", "top")
  ) +
  tm_credits(
    text = detail,
    size = 0.65,
    position = c("left", "top")
  ) +
  tm_credits(
    text = credits,
    size = 0.65,
    align = "right",
    position = c("right", "bottom")
  ) +
  tm_layout(
    title = 'BRAZILIAN "POINTS"',
    title.size = 20,
    frame = FALSE,
    fontfamily = "Comfortaa",
    legend.position = c("left", "bottom"),
    inner.margins = c(0.08, 0.13, 0.33, 0.13),
    outer.margins = 0.03
  )

## Unloads the map to save it
dev.off()
