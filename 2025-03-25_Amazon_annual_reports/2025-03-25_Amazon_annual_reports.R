if(!require(pacman))install.packages("pacman")

pacman::p_load(tidyverse, tidytuesdayR, extrafont, hrbrthemes, scales, cowplot, ggtext, ggview)

# font_import()
loadfonts()

# set fonts
title_font <- "Oswald"
font <- "Source Sans 3 ExtraLight"

tuesdata <- tidytuesdayR::tt_load('2025-03-25')

report_words_clean <- tuesdata$report_words_clean

grouped_year <-  report_words_clean |> 
  group_by(year, word) |> 
  summarise(n = n(), .groups = "drop") |>
  group_by(word) |> 
  arrange(year) |> 
  mutate(
    baseline = first(n),
    pct_change = ((n - baseline) / baseline * 100)
  )

m_billion <- grouped_year |>
  filter(word == "million" | word == "billion")

ggplot(m_billion, aes(x = year, y = n, colour = word)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(
    expand = c(0,0),
    breaks = seq(2005, 2023, by = 2)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 400)
  ) +
  scale_colour_manual(values = c("#F14950", "#86C1D3")) +
  scale_fill_manual(values = c("#F14950", "#86C1D3")) +
  labs(
    title = "M to the B",
    subtitle = "Mentions of <span style='color:#86C1D3;'>million</span> and <span style='color:#F14950;'>billion</span> in Amazon's annual reports",
    caption = "Source: aboutamazon.com via Gregory Vander Vinne   |   turnerhaa.bsky"
  ) +
  theme_modern_rc() +
  theme(
    legend.position = "none",
    text = element_text(family = font, colour = "#E1E3E4", size = 11),  # Increased from 10 to 11
    plot.title = element_text(family = title_font, face = "bold", colour = "#E1E3E4", size = 20, hjust = 0),  # Increased from 19 to 20
    plot.subtitle  = element_markdown(family = font, colour = "#E1E3E4", margin = margin(-5, 0, 16, 0), size = 11),  # Increased from 10 to 11
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(margin = margin(6, 0, 0, 0), colour = "#E1E3E4", size = 7.5),  # Increased from 8 to 9
    axis.text.y = element_text(margin = margin(0, 2, 0, 0), colour = "#E1E3E4", size = 8),  # Increased from 8 to 9
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.background = element_rect(fill = "#28292D"),
    panel.background = element_rect(fill = "#28292D", colour = NA),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.1, colour = "white"),
    plot.margin = margin(10, 14, 12, 10),
    plot.caption = element_text(size = 7, margin = margin(12, 0, -5, 0)),  # Increased from 6 to 7
    plot.title.position = "plot"
  ) +
  canvas(width = 2000, height = 1250, units = "px")

ggsave("2025-03-25_Amazon_annual_reports.png")
