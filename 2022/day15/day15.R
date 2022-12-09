# 0. Library and data loading ##################################################
library(dplyr)
library(ggplot2)
library(ggtext)
library(ggpath)
library(geobr)
library(ggfx)
library(cropcircles)
library(rmapshaper)
library(ggview)
library(ragg)
library(sf)
library(readxl)
library(glue)
library(junebug)
library(systemfonts)
library(stringr)

## Reads the data on restaurants
restaurants <- readxl::read_excel("2022/day15/data.xlsx", sheet = "restaurants")

## Reads the data on the foods
foods <- readxl::read_excel("2022/day15/data.xlsx", sheet = "foods")

## Gets Brazil shape
brasil_shp <- geobr::read_country()

## Gets the shape of Salvador's regions (mainland)
salvador_shp <- sf::read_sf("2022/day15/Prefeituras_Bairro.shp") %>% 
  dplyr::slice(-c(4,6,8,9))

## Defines some saturated colors
red <- "#941B10"
orange <- "#F5A70F"
lime <- "#E0CC14"
brown <- "#8e3d0b"

## Makes special styled fonts available to R (e.g.: Medium, Solid, etc)
### Lists fonts visible to {systemfonts}
fonts_list <- systemfonts::system_fonts()

### Takes all font styles that share that exact family name and
### registers them (makes them visible to {systemfonts})
junebug::font_hoist("Font Awesome 6 Brands")

### Gets the info used to register the font families
fonts_register <- systemfonts::registry_fonts()

# 1. Data handling #############################################################
## Defines coordinates of the texts on the "front cover" section
social_css <- "<span style='font-family:Nunito;'>"
cover_texts <- tibble(
  x = c(30, 30, 28.5, 0, 0, 0),
  y = c(84, 68, 48, 38, 31, 6),
  family = c("JAMES FONTS", "JAMES FONTS", "Nunito", "Nunito", "Nunito", "Font Awesome 6 Brands Regular"),
  hjust = c(0.5, 0.5, 0.5, 0, 0, 0),
  color = c("white", "white", "black", "white", "gray", "white"),
  fill = c(NA, NA, "white", NA, NA, NA),
  size = c(25, 25, 6, 5, 5, 3),
  label = c("STREET", 
            "FOOD",
            
            "<strong>LATIN AMERICA</strong>",
            
            '<strong>S1:E3 <span style="color:gray;">"Salvador, Brazil"</span></strong>',
            
            "Hearty feijoada. Spicy moqueca.<br>
            Beachside pirão. Bahia's food<br>
            is awash with both African and<br>
            Portuguese influences — and lots<br>
            and lots of dendê oil.",
            
            glue::glue(
              "{social_css}Show by: Netflix | Data by: Wikipedia and Google Maps | Photo by: Caio Vilela</span><br>
              {social_css}Graphic by: Ícaro Bernardes:</span>
              \uf099 {social_css}@IcaroBSC</span>
              \uf09b {social_css}@IcaroBernardes</span> 
              \uf08c {social_css}@icarobsc</span>"
            ))
)

## Simplifies the country shape
brasil_shp <- rmapshaper::ms_simplify(brasil_shp, keep = 0.004, weighting = 0.01)

## Simplifies the city shape and dissolves the regions
salvador_shp <- rmapshaper::ms_simplify(salvador_shp, keep = 0.05, weighting = 0.5) %>% 
  rmapshaper::ms_dissolve()

## Composes the chefs' info
restaurants <- restaurants %>% 
  dplyr::mutate(label = glue::glue("<strong style='font-size:25px;'>{chef}</strong><br>
                                   <span style='font-size:17px;'>{restaurant}</span>"))

## Defines coordinates for the chefs' photos and info
restaurants_coords <- tibble(
  chef = c("Dona Suzana", "Martinha Rodrigues", "Cláudia Bárbara", "Kabaça Clementino"),
  image = c("dona_suzana", "martinha_rodrigues", "claudia_barbara", "kabaca_clementino"),
  ximg = c(546000, 547000, 566500, 560000),
  yimg = c(8566000, 8557000, 8563000, 8557000),
  hjust = c(1, 1, 0, 0)
)
restaurants <- restaurants %>% dplyr::left_join(restaurants_coords)

## Creates paths to the chefs' photos and crops them into circles
restaurants <- restaurants %>% 
  dplyr::mutate(path = glue::glue("2022/day15/photos/{image}.png"),
                path = cropcircles::circle_crop(path))

## Defines coordinates for the decorative ellipse, points, foods' info and photos
a <- 75
b <- 53
foods <- foods %>% 
  dplyr::mutate(theta = c(90,72,52,28,0)/180,
                x = a*cospi(theta) + 10,
                y = b*sinpi(theta) + 5,
                ximg = (a+13)*cospi(theta) + 10,
                yimg = (b+13)*sinpi(theta) + 5)
ellipse <- tibble(
  theta = seq(90, -20, length.out = 100)/180,
  x = a*cospi(theta) + 10,
  y = b*sinpi(theta) + 5
) %>% 
  dplyr::add_row(x = c(-10,10), y = c(58,58), .before = 1L)

## Composes the foods' info
foods <- foods %>% 
  dplyr::mutate(label = glue::glue("<strong style='font-size:18px;'>{dish}</strong><br><br>
                                   <span style='font-size:12px;'>{ingredients}</span><br>
                                   <span style='font-size:7px;'>Photo by: {img_owner}</span>"))

## Creates paths to the foods' photos and crops them into circles
foods <- foods %>% 
  dplyr::mutate(path = glue::glue("2022/day15/photos/{image}.png"),
                path = cropcircles::circle_crop(path))

# 4. Poster creation ###########################################################
## Creates the "front cover" section
p1 <- cover_texts %>% 
  ggplot() +
  
  ### Places a photo of the city and applies a blur effect to it
  ggfx::as_reference(
    x = ggfx::with_blur(x = ggpath::geom_from_path(
      aes(x = 70, y = 50, path = "2022/day15/photos/baia.jpg"),
      width = 0.7, height = 1, hjust = 0),
      sigma = 30),
    id = "bg"
  ) +
  ggpath::geom_from_path(
    aes(x = 70, y = 50, path = "2022/day15/photos/baia.jpg"),
    width = 0.7, height = 1, hjust = 0) +
  ggfx::as_reference(
    x = ggfx::with_blur(
      x = annotate("rect", xmin = -Inf, xmax = 90,
                   ymin = -Inf, ymax = Inf, fill = "black", color = NA),
      sigma = 30
    ),
    id = "left_bg"
  ) +
  ggfx::as_reference(
    x = ggfx::with_blur(
      x = annotate("rect", xmin = 199, xmax = Inf,
                   ymin = -Inf, ymax = Inf, fill = "black", color = NA),
      sigma = 30
    ),
    id = "right_bg"
  ) +
  ggfx::with_interpolate(
    x = ggfx::with_blur(
      x = ggpath::geom_from_path(
        aes(x = 70, y = 50, path = "2022/day15/photos/baia.jpg"),
        width = 0.7, height = 1, hjust = 0),
      sigma = 50),
    bg_layer = "left_bg", src_percent = 2,
  ) +
  ggfx::with_interpolate(
    x = ggfx::with_blur(
      x = ggpath::geom_from_path(
        aes(x = 70, y = 50, path = "2022/day15/photos/baia.jpg"),
        width = 0.7, height = 1, hjust = 0),
      sigma = 50),
    bg_layer = "right_bg", src_percent = 2,
  ) +
  
  ### Places the texts
  ggtext::geom_richtext(
    aes(x = x, y = y, label = label, family = family, hjust = hjust,
        color = I(color), fill = I(fill), size = I(size)),
    vjust = 1, label.colour = NA,
    label.padding = unit(c(0.5, 0.85, 0.25, 0.85), "lines")
  ) +
  
  ### Defines axes limits and theme elements
  coord_cartesian(xlim = c(0,200), ylim = c(0,100)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = "black")
  )

## Previews the plot
ggview::ggview(p1, device = ragg::agg_png(res = "320"), dpi = 320,
               units = "px", width = 4000, height = 2000)

## Saves the plot
ggsave("2022/day15/p1.png", plot = p1, device = ragg::agg_png(res = "320"),
       width = 4000, height = 2000, units = "px", dpi = 320)

## Creates the "Brazil" section
### Defines some axes limits and coordinates for the decorative line
p2_ymin <- -34
p2_ymax <- 6
p2_line <- 0.42*p2_ymin + 0.58*p2_ymax

p2 <- brasil_shp %>% 
  ggplot() +
  
  ### Places a decorative line
  ggfx::with_outer_glow(
    x = annotate("segment", x = c(-55,-55), xend = c(-30,-55),
                 y = c(p2_line,p2_line), yend = c(p2_line,p2_line+30),
                 linewidth = 2, color = "white"),
    colour = "white", sigma = 10, expand = 2
  ) +
  
  ### Places the country shape twice with an outer glow
  ggfx::with_outer_glow(
    x = geom_sf(fill = "black", color = "black", linewidth = 3.5),
    colour = "black", sigma = 10, expand = 2
  ) +
  ggfx::with_outer_glow(
    x = geom_sf(fill = "black", color = red, linewidth = 1.2),
    colour = red, sigma = 10, expand = 2
  ) +
  
  ### Places a point to represent Salvador
  annotate("point", x = -39, y = -12.3,
           color = "#F5A70F", size = 4) +
  
  ### Places the city name
  ggtext::geom_textbox(
    aes(x = -40, y = -15, label = "SALVADOR, BRAZIL"),
    colour = "white", fill = "black", box.colour = NA, size = 12.5,
    width = unit(0.95, "npc"), hjust = 1, halign = 1,
    box.padding = unit(c(12, 0, 12, 0), "pt"),
    family = "Nunito", fontface = "bold.italic"
  ) +
  
  ### Defines axes limits and theme elements
  coord_sf(xlim = c(-74,-34), ylim = c(p2_ymin,p2_ymax), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = "black")
  )

## Previews the plot
ggview::ggview(p2, device = "png", dpi = 320, units = "px",
               width = 2000, height = 2000)

## Saves the plot
ggsave("2022/day15/p2.png", plot = p2, device = ragg::agg_png(res = "320"),
       width = 2000, height = 2000, units = "px", dpi = 320)

## Creates the "Salvador" section
### Defines some axes limits and coordinates for the decorative line
p3_ymin <- 8554000
p3_ymax <- 8587334
p3_line <- 0.42*p3_ymin + 0.58*p3_ymax

p3 <- salvador_shp %>% 
  ggplot() +
  
  ### Places a decorative line
  ggfx::with_outer_glow(
    x = annotate("segment", x = 527000, xend = 584000,
                 y = p3_line, yend = p3_line,
                 linewidth = 2, color = "white"),
    colour = "white", sigma = 10, expand = 2
  ) +
  
  ### Places the title
  annotate("text", x = 551000, y = 8574500, label = "The Chefs",
           family = "Nunito", fontface = "bold", color = "white",
           vjust = 0, hjust = 1, size = 18) +
  
  ### Places the subtitle
  annotate("text", x = 551000, y = 8572300, family = "Nunito",
           fontface = "bold.italic", color = "white",
           vjust = 1, hjust = 1, size = 5,
           label = "Keeping traditions. Feeding the people",) +
  
  ### Places the country shape twice with an outer glow
  ggfx::with_outer_glow(
    x = geom_sf(fill = "black", color = "black", linewidth = 3),
    colour = "black", sigma = 8, expand = 1
  ) +
  ggfx::with_outer_glow(
    x = geom_sf(fill = "black", color = orange, linewidth = 1.1),
    colour = orange, sigma = 8, expand = 1
  ) +
  
  ### Places lines that tie the chefs' photos and their restaurant locations
  geom_segment(aes(x = x, xend = ximg, y = y, yend = yimg),
               color = lime, linewidth = 1, linetype = "dotted", data = restaurants) +
  
  ### Places the chefs' photos and a border around them
  geom_point(aes(x = ximg, y = yimg), size = 23,
             color = lime, data = restaurants) +
  ggpath::geom_from_path(aes(x = ximg, y = yimg, path = path),
                         width = 0.1, height = 0.1, data = restaurants) +
  
  ### Places the chefs' infos
  ggtext::geom_richtext(aes(x = ximg, y = yimg, label = label, hjust = hjust),
                        label.padding = unit(c(0, 33, 0, 33), "pt"),
                        size = 4, lineheight = 1.5, family = "Nunito", 
                        color = "white", label.colour = NA, fill = NA,
                        data = restaurants) +
  
  ### Places the restaurants locations
  geom_point(aes(x = x, y = y), size = 3,
             color = lime, data = restaurants) +
  
  ### Defines axes limits and theme elements
  coord_sf(xlim = c(530099,580100), ylim = c(p3_ymin,p3_ymax), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = "black")
  )

## Previews the plot
ggview::ggview(p3, device = "png", dpi = 320, units = "px",
               width = 3000, height = 2000)

## Saves the plot
ggsave("2022/day15/p3.png", plot = p3, device = ragg::agg_png(res = "320"),
       width = 3000, height = 2000, units = "px", dpi = 320)

## Creates the "Foods" section
p4 <- foods %>% 
  ggplot() +
  
  ### Places a decorative line
  ggfx::with_outer_glow(
    x = geom_path(aes(x = x, y = y), linewidth = 2,
                  color = "white", data = ellipse),
    colour = "white", sigma = 10, expand = 2
  ) +
  
  ### Places the title
  annotate("text", x = 84, y = 97, family = "Nunito",
           fontface = "bold", color = "white", vjust = 1, hjust = 0,
           size = 18, lineheight = 0.8, label = "The\nDishes") +
  
  ### Places the subtitle
  annotate("text", x = 123, y = 72, family = "Nunito",
           fontface = "bold.italic", color = "white", vjust = 1, hjust = 1,
           size = 5, lineheight = 0.9,
           label = "Blessed by the ocean\nand Dendê-fuelled") +
  
  ### Places the points and lines associated to the foods
  geom_point(aes(x = x, y = y), size = 5, color = brown) +
  geom_segment(aes(x = x, xend = ximg, y = y, yend = yimg),
               linewidth = 1, color = brown, linetype = "dotted") +
  
  ### Places the foods' photos and a border around them
  geom_point(aes(x = ximg, y = yimg), size = 23, color = brown) +
  ggpath::geom_from_path(aes(x = ximg, y = yimg, path = path),
                         width = 0.1, height = 0.1) +
  
  ### Places the foods' infos
  ggtext::geom_textbox(aes(x = ximg, y = yimg, label = label), 
                       size = 2, lineheight = 1.6, family = "Nunito", 
                       color = "white", box.colour = NA, fill = NA,
                       width = unit(0.16, "npc"), halign = 0.5, vjust = 0,
                       nudge_y = 7, data = foods) +
  
  ### Defines axes limits and theme elements
  coord_cartesian(xlim = c(0,125), ylim = c(0,100), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = "black")
  )

## Previews the plot
ggview::ggview(p4, device = "png", dpi = 320, units = "px",
               width = 2500, height = 2000)

## Saves the plot
ggsave("2022/day15/p4.png", plot = p4, device = ragg::agg_png(res = "320"),
       width = 2500, height = 2000, units = "px", dpi = 320)
