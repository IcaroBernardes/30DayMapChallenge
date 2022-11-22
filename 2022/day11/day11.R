# 0. Library and data loading ##################################################
library(cartogram)
library(cowplot)
library(dplyr)
library(fastDummies)
library(ggforce)
library(ggpath)
library(ggplot2)
library(ggtext)
library(glue)
library(junebug)
library(magick)
library(patchwork)
library(ragg)
library(readxl)
library(rmapshaper)
library(rnaturalearth)
library(sf)
library(sp)
library(stringr)
library(systemfonts)
library(tidyr)
library(tmap)
library(tmaptools)

## Loads the data collected from Wikipedia in
## the original language of each country
data <- readxl::read_xlsx("2022/day11/data.xlsx")

## Loads shapes of a couple of countries and keeps only names and coordinates
shapes <- rnaturalearth::ne_countries(
  scale = "large",
  country = c("Argentina", "Bolivia", "Brazil", "Chile",
              "Colombia", "Ecuador", "Guyana", "Paraguay",
              "Peru", "Suriname", "Uruguay", "Venezuela"), 
  returnclass = "sf"
) %>% 
  dplyr::select(name_en = name, pop_est, geometry) %>% 
  dplyr::mutate(pop_est = as.numeric(pop_est)) %>% 
  dplyr::arrange(name_en)

## Defines the labels of the left-right spectrum
lab_spectrum <- c(" extreme-left left center-left center center-right right extreme-right ")
vec_spectrum <- lab_spectrum %>%
  stringr::str_trim() %>% 
  stringr::str_split(" ", simplify = TRUE)

## Defines colors
redpals <- c("#E86533","#F55D3D","#DB463B","#F54259","#EB4294")
color_titles <- "#E01200"
color_bodies <- "#AD2000"
color_credits <- "#6E1400"
color_bg <- "#FFAA80"
color_borders <- "#FFC3A6"

## Makes special styled fonts available to R (e.g.: Medium, Solid, etc)
### Lists fonts visible to {systemfonts}
fonts_list <- systemfonts::system_fonts()

### Takes all font styles that share that exact family name and
### registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Free")
junebug::font_hoist("Font Awesome 6 Brands")

### Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

## Defines the fonts
font_titles <- "Dosis"
font_bodies <- "Comfortaa"
font_solid_glyphs <- "Font Awesome 6 Free Solid"
font_brands_glyphs <- "Font Awesome 6 Brands Regular"

# 1. Data handling #############################################################
## Combines data and shapes
shapes <- dplyr::bind_cols(shapes, data)

## Greatly simplifies the shapes
countries <- rmapshaper::ms_simplify(shapes, keep = 0.0025, weighting = 0)

## Randomly assigns colors to the countries
set.seed(13)
countries <- countries %>% 
  dplyr::mutate(fill = redpals[sample.int(5, 12, replace = TRUE)])

## Gets the results by year, associates Font Awesome glyphs to them,
## label the years and defines coordinates
results <- data %>% 
  dplyr::select(position, year) %>% 
  dplyr::arrange(year, position) %>% 
  dplyr::mutate(glyph = ifelse(position == 1, "\uf091", "\uf165"),
                color = ifelse(position == 1, "#FF5719", "#FF1921"),
                year = stringr::str_replace(year, "^20", "'")) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(x = -10 + 15.2*cur_group_id(),
                y = 50 - 10*(1:n())) %>% 
  dplyr::ungroup()

## Defines coordinates for the years labels
results_labels <- dplyr::tibble(
  x = unique(results$x),
  y = max(results$y) + 12,
  yend = y - 34,
  label = unique(results$year)
)

## Defines the coordinates for commentaries on the map on pct of votes
commentaries <- dplyr::tibble(
  x = c(-34,-92,-50),
  y = c(-30,-16,-46),
  halign = c(0,1,0),
  label = c(
    "Lula won the elections by the smallest margin (50.9% x 49.1%) since
    the redemocratization of <span style='color:#FFAA80;'>Brazil.</span>
    He also faces resistance of coup supporters even before taking office.",
    "Political instability in <span style='color:#FFAA80;'>Peru</span> became
    the norm. After a phyrric victory, president Castillo had to face two
    impeachment processes in eight months.",
    "The current president of <span style='color:#FFAA80;'>Argentina,</span>
    Alberto Fernandez, was in 4th place in the Opinaia poll for the 2023's
    election with 8% as of April of 2022."
  )
)

## Defines the coordinates for commentaries highlights on the map on pct of votes
commentaries_lines <- dplyr::tibble(
  x = c(-40.2,-44.5,
        -45,-50,-50,
        
        -53.5,-60.5,
        -61,-69,
        
        -84.6,-81.5,
        -81,-74),
  y = c(-30.7,-30.7,
        -30.7,-30.7,-25,
        
        -44.2,-44.2,
        -44.2,-44.2,
        
        -11.6,-11.6,
        -11.6,-11.6),
  group = c(1,1,
            2,2,2,
            
            3,3,
            4,4,
            
            5,5,
            6,6),
  size = c(3.2,3.2,
           0.8,0.8,0.8,
           
           3.2,3.2,
           0.8,0.8,
           
           3.2,3.2,
           0.8,0.8),
  linetype = c("solid","solid",
               "dashed","dashed","dashed",
               
               "solid","solid",
               "dashed","dashed",
               
               "solid","solid",
               "dashed","dashed")
)

## Creates the database of ideological positions
ideologies <- data %>% 
  dplyr::mutate(ideology = str_replace_all(ideology, ",", " "),
                ideology = glue::glue(" {ideology} "),
                first = str_extract(ideology, "^ ([:graph:]+) "),
                last = str_extract(ideology, " ([:graph:]+) $"),
                regex_str = glue::glue('(^.+(?={first}))|((?<={last}).+$)'),
                ideology = stringr::str_remove_all(lab_spectrum, regex_str),
                ideology = str_trim(ideology)) %>% 
  dplyr::select(country, ideology) %>% 
  fastDummies::dummy_cols(select_columns = "ideology", split = " ", remove_selected_columns = TRUE) %>% 
  tidyr::pivot_longer(cols = starts_with("ideology"),
                      names_prefix = "ideology_",
                      names_to = "ideology",
                      values_to = "yes") %>% 
  dplyr::filter(yes == 1) %>% 
  dplyr::select(-yes) %>% 
  dplyr::mutate(pos = factor(ideology, levels = vec_spectrum),
                pos = as.numeric(pos))

## Defines constants that help define coordinates of ideological positions
r <- 2
theta <- seq(180, 0, length.out = 7)/180
alpha <- seq(-0.5*pi, -1.5*pi, length.out = 7)

## Defines coordinates for the ideological positions (points)
ideologies_pts <- ideologies %>% 
  dplyr::mutate(x = r*cospi(theta[pos]),
                y = -sqrt(r^2 - x^2))

## Defines coordinates for the ideological positions (curves)
ideologies_cvs <- ideologies %>% 
  dplyr::group_by(country) %>% 
  dplyr::arrange(pos) %>% 
  dplyr::filter(n() > 1) %>% 
  dplyr::slice(-n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(start = alpha[pos],
                end = alpha[pos+1])

## Defines coordinates for the ideological spectrum (points)
spectrum_pts <- dplyr::tibble(
  pos = 1:7,
  x = r*cospi(theta[pos]),
  y = -sqrt(r^2 - x^2)
)

## Defines coordinates for the ideological spectrum (curves)
spectrum_cvs <- dplyr::tibble(
  start = alpha[1:6],
  end = alpha[2:7],
)

## Standardizes the dimensions of the parties symbols
img <- list.files("2022/day11/images", full.names = TRUE)
img %>% 
  purrr::walk(function (image) {
    magick::image_read(image) %>% 
      magick::image_scale(geometry = "200x200") %>%
      magick::image_write(image)
  })

## Associates parties and their symbols
parties <- data %>% 
  dplyr::mutate(path = glue::glue("2022/day11/images/{party}.png")) %>% 
  dplyr::select(country, path)

# 2. Cartogram object creation #################################################
## Selects the relevant data to make the cartogram
carto <- countries %>% dplyr::select(abs)

## Converts the sf object to a SpatialPolygonsDataFrame
carto <- sf::as_Spatial(carto)

## Projects the SpatialPolygonsDataFrame
carto <- sp::spTransform(carto, sp::CRS("+init=epsg:3395"))

## Creates the cartogram
carto <- cartogram::cartogram_dorling(x = carto, weight = "abs", k = 20)

# 3. Texts definition ##########################################################
## Defines texts of the "art cover" section
p1_title <- "SCARLET\nAMERICA"
p1_body <- c(
  'South America took a turn to the left in the latest presidential elections.
  The "crimsonization" of the region, however, is not uniform.'
)
p1_body <- p1_body %>% stringr::str_wrap(width = 38)
p_credits <- "Graphic by: Ícaro Bernardes | Data by: Wikipedia"
social_css <- "<span style='font-family:Comfortaa;'>"
p_social <- glue::glue("\uf099 {social_css}@IcaroBSC</span>
                        \uf09b {social_css}@IcaroBernardes</span> 
                        \uf08c {social_css}@icarobsc</span>")

## Defines texts of the "position timeline" section
p2_title <- "RECENT\nVICTORIES"
p2_body <- c(
  'Of the last 12 general elections in South America,
  left-led coalitions won 7.'
)
p2_body <- p2_body %>% stringr::str_wrap(width = 28)

## Defines texts of the "absolute votes" section
p3_title <- "BIGGEST\nCOUNTRIES"
p3_body <- c(
  'Left-wing candidates received many votes across the subcontinent.
  Each vote was fundamental for the victories in gigantic Brazil
  and enormous Argentina and Colombia.'
)
p3_body <- p3_body %>% stringr::str_wrap(width = 35)

## Defines texts of the "percentage of votes" section
p4_title <- "POLARIZED\nSOCIETIES"
p4_body <- c(
  'Aside from the staggering victory of Maduro in Venezuela and the
  major wins in Chile and Bolivia, the left attained slim victories.'
)
p4_body <- p4_body %>% stringr::str_wrap(width = 35)

## Defines texts of the "ideology" section
p5_title <- "DIVERSE\nBLOCK"
p5_body <- c(
  'Left-wing parties occupy a broad position in the left-right spectrum.
  While the Argentinian Partido Justicialista even has a small presence
  in the center-right, the Partido Socialista Unido de Venezuela is in
  the extreme-left.'
)
p5_body <- p5_body %>% stringr::str_wrap(width = 35)

# 4. Poster creation ###########################################################
## Creates the "art cover" section
p1 <- countries %>% 
  ggplot() +
  # annotation_custom(bg, xmin = -Inf, xmax = Inf, ymin = -335, ymax = 15) +
  
  ### Places a background point
  annotate("point", x = -53, y = -19, size = 190, color = color_borders) +
  
  ### Places the simplified and randomly colorized shapes
  geom_sf(aes(fill = I(fill)), color = color_borders, linewidth = 2) + 
  
  ### Places the texts
  geom_text(aes(x = -180, y = -10, label = p1_title),
            size = 33, hjust = 0, lineheight = 0.7,
            color = color_titles, family = font_titles) +
  geom_text(aes(x = -180, y = -38, label = p1_body),
            size = 6.3, hjust = 0, lineheight = 0.9,
            color = color_bodies, family = font_bodies) +
  geom_text(aes(x = -180, y = -50, label = p_credits),
            size = 3.5, hjust = 0,
            color = color_credits, family = font_bodies) +
  ggtext::geom_richtext(aes(x = -180, y = -53, label = p_social),
                        size = 3.5, hjust = 0, fill = NA, label.colour = NA,
                        color = color_credits, family = font_brands_glyphs) +
  
  ### Defines axes limits and theme elements
  coord_sf(xlim = c(-180,-40), ylim = c(-59,11)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_bg, color = color_bg)
  )

## Creates the "position timeline" section
p2 <- results %>% 
  ggplot() +
  
  ### Places the years labels
  geom_segment(aes(x = x, xend = x, y = y, yend = yend), color = color_credits,
               size = 20, lineend = "round", data = results_labels) +
  geom_segment(aes(x = x, xend = x, y = y, yend = yend + 0.2),
               color = "#FAB693", size = 18,
               lineend = "round", data = results_labels) +
  geom_point(aes(x = x, y = y), color = color_credits,
             size = 25, data = results_labels) +
  geom_text(aes(x = x, y = y, label = label), color = color_bg,
            nudge_x = -0.1, family = font_bodies,
            size = 10, data = results_labels) +
  
  ### Places the glyphs
  geom_text(aes(x = x, y = y, label = glyph, color = I(color)),
            family = font_solid_glyphs, size = 9) +
  
  ### Places the texts
  geom_text(aes(x = 85, y = 46, label = p2_title),
            size = 20, hjust = 0, lineheight = 0.8,
            color = color_titles, family = font_titles) +
  geom_text(aes(x = 85, y = 26, label = p2_body),
            size = 5, hjust = 0, 
            color = color_bodies, family = font_bodies) +
  
  ### Defines axes limits and theme elements
  coord_equal(xlim = c(0,140), ylim = c(0,70)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_bg, color = color_bg)
  )

## Creates the "absolute votes" section
### Creates a map with {tmap}
p3 <- tmap::tm_shape(countries,
                     bbox = tmaptools::bb(c(-100,-55,-30,15))) +
  tmap::tm_polygons(col = "#FFC5A8", border.col = color_bg, lwd = 3) +
  tmap::tm_shape(carto) +
  tmap::tm_polygons(col = "abs", border.col = "#FFC5A8", lwd = 1,
                    palette = c("#FFD4D7","#FF0015"), title = "Votes received", 
                    style = "log10_pretty",
                    legend.format = list(scientific = TRUE,
                                         big.num.abbr = c("mln" = 5, "bln" = 9))) +
  tmap::tm_layout(asp = 0,
                  bg.color = "#FFFFFF00",
                  outer.margins = rep(0,4),
                  frame = FALSE)

### Converts the tmap to a grob
p3 <- tmap::tmap_grob(p3)

p3 <- ggplot() +
  
  ### Places the grob of the tmap
  annotation_custom(p3, xmin = 70) +
  
  ### Places the texts
  geom_text(aes(x = 0, y = 50, label = p3_title),
            size = 20, hjust = 0, lineheight = 0.8,
            color = color_titles, family = font_titles) +
  geom_text(aes(x = 0, y = 20, label = p3_body),
            size = 5, hjust = 0, 
            color = color_bodies, family = font_bodies) +
  
  ### Defines axes limits and theme elements
  coord_equal(xlim = c(0,140), ylim = c(0,70), ratio = 1/1.24) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_bg, color = color_bg)
  )

## Creates the "percentage of votes" section
p4 <- shapes %>% 
  ggplot() +

  ### Places the map colorized with the percentages
  geom_sf(aes(fill = pct), color = color_bg, linewidth = 1) +
  
  ### Places pointers to the commentaries on the map
  geom_path(aes(x = x, y = y, group = group, linetype = I(linetype), size = I(size)),
            color = color_credits, data = commentaries_lines) +
  
  ### Places commentaries on the map
  ggtext::geom_textbox(aes(x = x, y = y, label = label, halign = halign),
                       size = 2, box.colour = NA, fill = NA, lineheight = 1.5,
                       color = color_credits, family = font_bodies,
                       width = unit(0.15, "npc"), data = commentaries) +
  
  ### Places the texts
  geom_text(aes(x = -17, y = -11, label = p4_title),
            size = 20, hjust = 0, lineheight = 0.8,
            color = color_titles, family = font_titles) +
  geom_text(aes(x = -17, y = -31, label = p4_body),
            size = 5, hjust = 0, 
            color = color_bodies, family = font_bodies) +
  
  ### Defines colors for the map gradient
  scale_fill_gradient(low = "#FFBBB3", high = "#FF1E00", guide = "none") +
  
  ### Defines axes limits and theme elements
  coord_sf(xlim = c(-100,40), ylim = c(-55,15)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_bg, color = color_bg)
  )

## Creates the "ideology" section
p5 <- NULL %>% 
  ggplot(aes(x0 = 0, y0 = 0, r = 2)) +
  
  ### Places the symbols from the parties and circles around them
  annotate("point", x = 0, y = 0, size = 18, color = "#FF1E00") +
  annotate("point", x = 0, y = 0, size = 17, color = "white") +
  ggpath::geom_from_path(aes(x = 0, y = 0, path = path),
                         width = 0.3, height = 0.3, data = parties) +
  
  ### Places points and curves to represent the full spectrum
  geom_point(aes(x = x, y = y), color = "#FFC5A8", size = 4, data = spectrum_pts) +
  ggforce::geom_arc(aes(start = start, end = end), color = "#FFC5A8",
                    linewidth = 2, data = spectrum_cvs) +
  
  ### Places points and curves to represent the ideologies
  geom_point(aes(x = x, y = y), color = "#FF1E00", size = 4, data = ideologies_pts) +
  ggforce::geom_arc(aes(start = start, end = end), color = "#FF1E00",
                    linewidth = 2, data = ideologies_cvs) +
  geom_point(aes(x = x, y = y), color = "white", size = 3, data = ideologies_pts) +
  ggforce::geom_arc(aes(start = start, end = end), color = "white",
                    linewidth = 1, data = ideologies_cvs) +
  
  ### Facets by country
  facet_wrap(~ country, nrow = 4) +
  
  ### Defines axes limits and theme elements
  coord_equal(xlim = c(-2.5,2.5), ylim = c(-2.5,1.3)) +
  theme_void() +
  theme(
    strip.text = element_text(family = font_bodies, color = color_credits,
                              margin = margin(2, 0, 2, 0, "pt"))
  )

### Converts the ggplot to a grop
p5 <- cowplot::as_grob(p5)

p5 <- NULL %>% 
  ggplot() +
  
  ### Places the grob of the ggplot
  annotation_custom(p5, xmin = 75,xmax = 145,ymin=0,ymax=70) +
  
  ### Places the texts
  geom_text(aes(x = 0, y = 55, label = p5_title),
            size = 20, hjust = 0, lineheight = 0.8,
            color = color_titles, family = font_titles) +
  geom_text(aes(x = 0, y = 25, label = p5_body),
            size = 5, hjust = 0, 
            color = color_bodies, family = font_bodies) +
  
  ### Defines axes limits and theme elements
  coord_equal(xlim = c(0,140), ylim = c(0,70)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = color_bg, color = color_bg)
  )

## Stacks all plots on one column (patchwork)
p <- p1 / p2 / p3 / p4 / p5 

## Saves the poster
ggsave("2022/day11/day11.png", plot = p, device = ragg::agg_png,
       width = 10, height = 25, dpi = 320, bg = "white")

## Adds the credits to the individual plots and saves them
### "art cover" section
ggsave("2022/day11/p1.png", plot = p1, device = ragg::agg_png,
       width = 10, height = 5.48, dpi = 320, bg = "white")
### "position timeline" section
p2 <- p2 +
  geom_text(aes(x = 85, y = 17, label = p_credits),
            size = 3.5, hjust = 0,
            color = color_credits, family = font_bodies) +
  ggtext::geom_richtext(aes(x = 85, y = 14, label = p_social),
                        size = 3.5, hjust = 0, fill = NA, label.colour = NA,
                        color = color_credits, family = font_brands_glyphs)
ggsave("2022/day11/p2.png", plot = p2, device = ragg::agg_png,
       width = 10, height = 5.00, dpi = 320, bg = "white")
### "absolute votes" section
p3 <- p3 +
  geom_text(aes(x = 0, y = 4, label = p_credits),
            size = 3.5, hjust = 0,
            color = color_credits, family = font_bodies) +
  ggtext::geom_richtext(aes(x = 0, y = 0, label = p_social),
                        size = 3.5, hjust = 0, fill = NA, label.colour = NA,
                        color = color_credits, family = font_brands_glyphs)
ggsave("2022/day11/p3.png", plot = p3, device = ragg::agg_png,
       width = 10, height = 4.04, dpi = 320, bg = "white")
### "percentage of votes" section
p4 <- p4 +
  geom_text(aes(x = -17, y = -45, label = p_credits),
            size = 3.5, hjust = 0,
            color = color_credits, family = font_bodies) +
  ggtext::geom_richtext(aes(x = -17, y = -48, label = p_social),
                        size = 3.5, hjust = 0, fill = NA, label.colour = NA,
                        color = color_credits, family = font_brands_glyphs)
ggsave("2022/day11/p4.png", plot = p4, device = ragg::agg_png,
       width = 10, height = 5.32, dpi = 320, bg = "white")
### "ideology" section
p5 <- p5 +
  geom_text(aes(x = 0, y = 6, label = p_credits),
            size = 3.5, hjust = 0,
            color = color_credits, family = font_bodies) +
  ggtext::geom_richtext(aes(x = 0, y = 3, label = p_social),
                        size = 3.5, hjust = 0, fill = NA, label.colour = NA,
                        color = color_credits, family = font_brands_glyphs)
ggsave("2022/day11/p5.png", plot = p5, device = ragg::agg_png,
       width = 10, height = 5.0, dpi = 320, bg = "white")

# 5. Poster post-processing ####################################################
## Reads the image
p <- magick::image_read("2022/day11/day11.png")

## Crops the white space that {patchwork} creates
## around the plot grid and saves the image
p %>% magick::image_crop(geometry = "3152x7824",
                         gravity = "Center") %>% 
  magick::image_write("2022/day11/day11.png")
