# load packages
if(!require(pacman))install.packages("pacman")

pacman::p_load(tidyverse, tidytuesdayR, janitor, hrbrthemes, palmerpenguins, extrafont,
               ggview)

# fonts
loadfonts()


title_font <- "Oswald"
font <- "Source Serif 4"

# 
penguinsRAW <- penguins |> 
  clean_names() |> 
  filter(!is.na(sex)) |> 
  mutate(
    mass_kg = body_mass_g / 1000,
    sex = case_when(
      sex == "female" ~ "Female",
      sex == "male" ~ "Male",
      TRUE ~ sex
    )
      )

# append kg to last value
label_with_kg <- function(breaks) {
  # Convert to character
  labels <- as.character(breaks)
  # Modify the last one
  labels[length(labels)] <- paste0(labels[length(labels)], "kg")
  return(labels)
}


# gender x bodymass
ggplot(penguinsRAW, aes(x = sex, y = mass_kg, fill = sex)) +
  # annotate('rect', xmin=0.5, xmax=2.5, ymin=2.6, ymax=6.5, alpha=0.7, fill= "grey") +
  # annotate('rect', xmin=4.5, xmax=6.5, ymin=2.6, ymax=6.5, alpha=0.7, fill="grey") +
  geom_violin(alpha = 0.8, colour = NA) +
  facet_wrap(~ species) +
  scale_fill_manual(values = c("#FF4A1C", "#B2FFA9")) +
  scale_x_discrete(labels = c(
    "female.Adelie" = "Adelie - F",
    "male.Adelie" = "Adelie - M",
    "female.Chinstrap" = "Chinstrap - F",
    "male.Chinstrap" = "Chinstrap - M",
    "female.Gentoo" = "Gentoo - F",
    "male.Gentoo" = "Gentoo - M"
  )) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 6.5),
    breaks = seq(0, 6.5, by = 2), 
    labels = label_with_kg) +
  labs(
    title = "Breaking the ice",
    subtitle = "Penguin weights by species and sex",
    caption = "Source: Gorman et al. (2014) | {palmerpenguins} R package"
  ) +
  ylab("Mass (kg)") +
  theme_ipsum() +
  theme(
    text = element_text(colour = "#1A181B", family = font),
    plot.background = element_rect(fill = "#f9f9f9", colour = NA),
    panel.background = element_rect(fill = "#f9f9f9", colour = NA),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    legend.title = element_blank(),
    plot.title = element_text(family = title_font, size = 20, face = "bold", margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(family = font, margin = margin(10, 0, 10, 0), size = 12, colour = "#808080"),
    legend.position = "None",
    plot.margin = margin(0, 10, 10, 5),
    axis.text.y = element_text(size = 8, family = font, colour = "#1A181B"),
    axis.text.x = element_text(size = 8, family = font, colour = "#1A181B", margin = margin(8, 0, 0, 0)),
    strip.text = element_text(hjust = 0.5, size = 10, face = "bold", margin = margin(8, 0, 2, 0)),
    panel.spacing.x = unit(3.5, "lines"),
    plot.caption = element_text(size = 6, family = font, face = "plain", margin = margin(10, 0, 0, 0))
  ) +
  canvas(1920, 1080, units = "px")

ggsave("PalmerPenguins.png", width = 1920, height = 1080, units = "px")
