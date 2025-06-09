# install packages
if(!require(pacman))install.packages("pacman")

pacman::p_load(tidyverse, tidytuesdayR, janitor, readxl, hrbrthemes, scales, ggview, extrafont, patchwork,
               ggpath)

# load TT data
careRAW <- tt_load("2025-04-08")

care <- careRAW$care_state |> 
  clean_names()

median_waits <- care |> 
  filter(measure_id == "OP_18b") |> 
  arrange(desc(score))

# load population data
USA_pop_cols <- c("state", "2020_base", "yr_2020", "yr_2021", "yr_2022", "yr_2023", "yr_2024") 

USA_population <- read_excel("data/NST-EST2024-POP.xlsx", skip = 8, n_max = 53) |> 
  setNames(USA_pop_cols) |> 
  filter(!is.na(state)) |> 
  mutate(
    state = str_sub(state, 2, -1)
  ) |> 
  select(1, 7)

state_lookup <- tibble::tibble(
  state = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "American Samoa", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Guam", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
    "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Northern Mariana Islands", "Montana", "Nebraska", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico", "New York",
    "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota",
    "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Virgin Islands", "Washington",
    "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"
  ),
  abbreviation = c(
    "AL", "AK", "AZ", "AR", "AS", "CA", "CO", "CT", "DE", "FL", "GA", "GU", "HI", "ID",
    "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
    "MO", "MP", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
    "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "VI", "WA", "WV",
    "WI", "WY", "DC"
  )
)

USA_population <- USA_population |> 
  left_join(state_lookup, by = join_by(state == state)) |> 
  select(3, 1, 2)

# join pop to median_waits
median_waits <- median_waits |> 
  left_join(USA_population, by = join_by(state == abbreviation)) |> 
  filter(!is.na(score)) |> 
  rename(population = yr_2024)



  

care <- care |> 
  left_join(USA_population, by = join_by(state == abbreviation)) |> 
  rename(acronym = state) |> 
  rename(state = state.y) |> 
  mutate(
    vote = 
      case_when(
        state %in% c("California", "Colorado", "Connecticut", "Delaware", "District of Columbia",
                       "Hawaii", "Illinois", "Maine", "Maryland", "Massachusetts", "Minnesota", "New Hampshire", "New Jersey", "New Mexico", "New York", "Oregon", "Rhode Island",
                       "Vermont", "Virginia", "Washington", "Puerto Rico") ~ "Blue",
        TRUE ~ "Red"
      )
  )
  


  
flu_shot <- care |> 
  filter(measure_id == "IMM_3") |> 
  arrange(desc(score)) |> 
  filter(!is.na(state)) |> 
  mutate(
    non_vax = 100 - score
  ) |> 
  pivot_longer(
    cols = c(score, non_vax),
    names_to = "category",
    values_to = "percentage"
  ) |> 
  group_by(state) |> 
  mutate(score = max(percentage)) |> 
  ungroup() |> 
  mutate(
    state = 
      case_when(
        state == "District of Columbia" ~ "D.C.",
        TRUE ~ state
      )
  ) |> 
  filter(
   ! state %in% c("VI", "MP", "GU", "AS")
  )


# font_import()
loadfonts()

title_font <- "Oswald"
font <- "Source Serif 4"

title <- "Who's getting their flu shot?"
subtitle <- "% of healthcare workers given influenza vaccination"


ggplot(flu_shot, aes(x = percentage, y = reorder(state, score), fill = category)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 25, colour = "#342A21", alpha = 0.4, linetype = 1, linewidth = 0.2) +
  geom_vline(xintercept = 50, colour = "#342A21", alpha = 0.4, linetype = 1, linewidth = 0.2) +
  geom_vline(xintercept = 75, colour = "#342A21", alpha = 0.4, linetype = 1, linewidth = 0.2) +
  # geom_text(data = top_bottom, aes(x = score + 2.5, y = reorder(state, score), label = paste0(score, "%")), size = 1.5, colour = "#342A21", family = font) +
  # geom_text(aes(x = 0 + 1, y = reorder(state, score), label = state), size = 1.5, colour = "white", family = font, hjust = 0, vjust = 0.5) +
  scale_x_continuous(
    expand = c(0,0)
  ) +
  scale_fill_manual(values = c("#C9B79C", "#DA667B")) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = "Source: Centers for Medicare and Medicaid Services | Note: Values as of 6 April, 2025"
  ) +
  theme_ipsum_pub() +
  theme(
    text = element_text(family = font, size = 8, colour = "#342A21"),
    axis.text.y = element_text(family = font, size = 4, colour = "#342A21", margin = margin(0, -1, 0, 0)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = font, size = 5, colour = "#342A21"),
    axis.title.x = element_blank(),
    legend.position = "NONE",
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(10, 8, 5, 5),
    plot.title = element_text(size = 7, family = title_font, margin = margin(-5, 0, 3, 0), hjust = 0),
    plot.subtitle = element_text(size = 5, family = font, margin = margin(0, 0, 4, 0), hjust = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_text(family = title_font, size = 3.5, margin = margin(3, 0, 0, 0))
  ) +
  canvas(800, 1000, unit = "px", dpi = 300)

ggsave("US_care.png", width = 800, height = 1000, unit = "px", dpi = 300)
