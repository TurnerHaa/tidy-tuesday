# data import
if(!require(pacman))install.packages("pacman")

pacman::p_load(tidyverse, tidytuesdayR, extrafont, janitor, hrbrthemes, cowplot,
               ggforce, ggview, ggtext, ggimage, ggpath, extrafont)

# ---- prepare fonts ----
loadfonts()

font <- "Oswald"
title_font <- "Source Sans 3"

# ---- import data ----
tuesdata <- tidytuesdayR::tt_load('2025-04-01')

pokemon <- tuesdata$pokemon_df |> 
  clean_names() |> 
  mutate(url_icon = paste0("https:", url_icon))



evolutions <- read.csv("data/gen1Evolutions.csv")

# alternative Pokemon charts
gen1 <- evolutions |> 
  left_join(evolutions |> select(pokemon, height),
            by = c("evolved_from" = "pokemon"),
            suffix = c("", "_prev")) |> 
  mutate(
    height = height * 10,
    height_prev = height_prev * 10,
    growth = height - height_prev) |> 
  select(-height_prev, -Unnamed..0) |> 
  mutate(growth = ifelse(is.na(growth), 0, growth)) |> 
  left_join(pokemon |> select(pokemon, url_icon),
            by = c("pokemon" = "pokemon")) |> 
  filter(pokemon %in% starters_gen1_7)

gen1 <- pokemon |> 
  filter(generation_id == 1) |> 
  select(id, pokemon, type_1, height) |> 
  arrange(id)

# ---- Gen1 only ----
gen1 <- gen1 |>  
  mutate(
    x = rep(seq(1, 9), length.out = nrow(gen1)),
    y = rep(19:1, length.out = 151, each = 9)
  )

P_gen1 <- ggplot(gen1, aes(x = x, y = y, size = height, colour = type_1)) +
  geom_point(shape = 15, alpha = 0.8) +
  scale_size(range = c(1, 15)) +
  scale_colour_manual(values = pokemon_colours) +
  coord_sf() +
  theme_modern_rc() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
    
  )

P_gen1

# ---- GenAll only ----
genAll <- pokemon |> 
  select(id, pokemon, type_1, height) |> 
  arrange(desc(height))

genAll <- genAll |>  
  mutate(
    x = rep(seq(1, 8), length.out = nrow(genAll)),
    y = rep(119:1, length.out = 949, each = 8)
  )

ggplot(genAll, aes(x = x, y = y, size = height, colour = type_1)) +
  geom_point(shape = 15) +
  # coord_fixed() +
  theme_modern_rc()

#---- Common types pairings ----
types <- pokemon |> 
  select(pokemon, type_1, type_2) |> 
  filter((!is.na(type_1)), (!is.na(type_2))) |> 
  count(type_1, type_2)

ggplot(types, aes(x = type_1, y = type_2, fill = n)) +
  geom_tile(color = "white") +
  theme_ipsum()
