# ---- load packages ----
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

# ---- supporting datasets ----
# Colour lookups
pokemon_colours <- c(normal= '#A8A77A',
                     fire= '#EE8130',
                     water= '#6390F0',
                     electric= '#F7D02C',
                     grass= '#7AC74C',
                     ice= '#96D9D6',
                     fighting= '#C22E28',
                     poison= '#A33EA1',
                     ground= '#E2BF65',
                     flying= '#A98FF3',
                     psychic= '#F95587',
                     bug= '#A6B91A',
                     rock= '#B6A136',
                     ghost= '#735797',
                     dragon= '#6F35FC',
                     dark= '#705746',
                     steel= '#B7B7CE',
                     fairy= '#D685AD')  


evolutions <- read.csv("data/gen1Evolutions.csv")

starters_gen1_7 <- c(
  # Gen 1 - Kanto
  "bulbasaur", "ivysaur", "venusaur",
  "charmander", "charmeleon", "charizard",
  "squirtle", "wartortle", "blastoise",
  
  # Gen 2 - Johto
  "chikorita", "bayleef", "meganium",
  "cyndaquil", "quilava", "typhlosion",
  "totodile", "croconaw", "feraligatr",
  
  # Gen 3 - Hoenn
  "treecko", "grovyle", "sceptile",
  "torchic", "combusken", "blaziken",
  "mudkip", "marshtomp", "swampert",
  
  # Gen 4 - Sinnoh
  "turtwig", "grotle", "torterra",
  "chimchar", "monferno", "infernape",
  "piplup", "prinplup", "empoleon",
  
  # Gen 5 - Unova
  "snivy", "servine", "serperior",
  "tepig", "pignite", "emboar",
  "oshawott", "dewott", "samurott",
  
  # Gen 6 - Kalos
  "chespin", "quilladin", "chesnaught",
  "fennekin", "braixen", "delphox",
  "froakie", "frogadier", "greninja",
  
  # Gen 7 - Alola
  "rowlet", "dartrix", "decidueye",
  "litten", "torracat", "incineroar",
  "popplio", "brionne", "primarina"
)

starters <- pokemon |> 
  filter(pokemon %in% starters_gen1_7)

starters <- starters |> 
  mutate(
    group = rep(1:(nrow(starters) %/% 3 + 1), each = 3, length.out = nrow(starters))) |> 
  select(
    id, pokemon, type_1, height, url_image, generation_id, group, weight
  ) |> 
  group_by(group) |> 
  arrange(id) |> 
  mutate(
    height = height * 100,
    previous_form = lag(pokemon),
    previous_height = lag(height),
    growth = height - previous_height,
    growth = ifelse(is.na(growth), 0, growth),
    y = generation_id,
    pokemon = factor(pokemon, levels = starters$pokemon[order(starters$id)])
  ) |> 
  ungroup() |> 
  group_by(generation_id) |> 
  mutate(
    x = rep(1:9, length.out = n())
  )

evolutionRange <- starters |> 
  group_by(group) |> 
  mutate(size_change = max(height) - min(height),
         starter = first(pokemon),
         starter_url = first(url_image)
         ) |> 
  filter(x %% 3 == 0) |> 
  mutate(pokemon = if_else(row_number() %% 2 == 0, " ", pokemon))


# WIP viz
 ggplot(evolutionRange, aes(x = size_change, y = reorder(pokemon, size_change))) +
  geom_segment(
    aes(x = 0, xend = size_change, y = reorder(pokemon, size_change), yend = reorder(pokemon, size_change), colour = type_1),
    linewidth = 1.5
  ) +
  geom_point(aes(colour = type_1), size = 0.8) +
   scale_size_continuous(range = c(2, 15)) +
  labs(
    title = "Growwwww Bulbasaur!",
    subtitle = "Change in height of starter Pokemon between 1st and 3rd evolution",
    colour = "Type:"
    ) +
   xlab("Change in height (cm)") + 
  scale_y_discrete(
    labels = setNames(
      glue::glue('<img src="{evolutionRange$starter_url}" height="13">    &#8594;    <img src="{evolutionRange$url_image}" height="13">'),
      evolutionRange$pokemon
    )
    ) +
   scale_x_continuous(
     expand = c(0, 0),
     limits = c(0, 280),
     breaks = seq(0, 280, by = 40)  # Adjust the step size as needed
   ) +
  scale_color_manual(values = c("#EE8130", "#7AC74C", "#6390F0")) +
  theme_modern_rc() +

  theme(
    plot.title.position = "plot",
    text = element_text(family = font),
    axis.text.y = ggtext::element_markdown(margin = margin(0, 0, 0, 0,)),
    axis.text.x = element_text(size = 12),
    legend.position = c(0.92, 0.2),
    legend.title = element_text(),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(colour = "white", size = 12, face = "bold"),
    panel.spacing = unit(0.3, "lines"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(5, 0, 2, 0), family = font),
    plot.title = element_text(margin = margin(0, 0, 0, 0), family = title_font, size = 22),
    plot.subtitle = element_text(margin = margin(5, 0, 8, 0), family = font, size = 14),
    plot.margin = margin(10, 20, 10, 10),
    plot.background = element_rect(fill = "#28292D"),
    panel.background = element_rect(fill = "#28292D", colour = NA),
    
  ) +
  canvas(2250, 1500, units = "px")

ggsave("Pokemon.png", width = 2250, height = 1500, units = "px", dpi = 300)
