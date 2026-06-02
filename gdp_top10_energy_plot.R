# Author: Scottie Szewczyk
# Tidy Tuesday: Sustainable Energy for All (May 26, 2026)
# Source: https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-05-26
# Created with the assistance of Claude Sonnet 4.6 through Posit Assistant in RStudio.

library(tidyverse)
library(countrycode)

# ── Data sources ───────────────────────────────────────────────────────────────
# Energy data:  SE4ALL / World Bank Energy Data
#               https://energydata.info/dataset/538a3ba2-f218-42b2-a79c-3a5b7603556e
#               Downloaded locally as clean_energy_data.csv
#
# GDP data:     World Bank, indicator NY.GDP.MKTP.CD (GDP, current USD)
#               https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
#               Downloaded via World Bank API and saved locally as gdp_2010.csv

# ── Load energy data ─────────────────────────────────────────────────

raw <- read.csv("clean_energy_data.csv")

# ── Load GDP data & identify top 10 countries ──────────────────────────────────

gdp_df <- read.csv("gdp_2010.csv")

top10_countries <- gdp_df |>
  arrange(desc(gdp_usd)) |>
  slice_head(n = 10) |>
  pull(country_name)

# ── Filter energy data to top 10 GDP countries ─────────────────────────────────
energy_df <- raw |>
rename(
  energy_intensity = energy_intensity_level_final_energy_megajoules_per_usd_2005_ppp,
  renewable_pct    = renewable_energy_consumption_tfec_pct
) |>
  select(country_name, country_code, yr, energy_intensity, renewable_pct) |>
  mutate(yr = as.integer(yr)) |>
  filter(!is.na(energy_intensity), !is.na(renewable_pct)) |>
  group_by(country_name) |>
  filter(n() == n_distinct(yr)) |>
  ungroup()
plot_df <- energy_df |>
  filter(country_name %in% top10_countries) |>
  arrange(country_name, yr)

points_1990 <- plot_df |> filter(yr == 1990)
points_mid  <- plot_df |> filter(yr > 1990, yr < 2010)  # intermediate years only
labels_2010 <- plot_df |> filter(yr == 2010)

# ── Manual label positions ─────────────────────────────────────────────────────

label_coords <- tribble(
  ~country_name,    ~label_x, ~label_y,
  "United Kingdom",     -5.1,      3.5,
  "Japan",               1,      1.0,
  "United States",      3,      7.6,
  "Italy",               7.7,      2.7,
  "Germany",            0,      5.6,
  "France",             15,      3.2,
  "China",              25,     15.5,
  "Canada",             24,     7.0,
  "India",              48,      7.0,
  "Brazil",             50,      3.5
)

labels_2010 <- labels_2010 |> left_join(label_coords, by = "country_name")

# ── Manual country colors ──────────────────────────────────────────────────────

country_colors <- c(
  "Brazil"         = "#66a61e",
  "Canada"         = "#d95f02",
  "China"          = "#7570b3",
  "France"         = "#e7298a",
  "Germany"        = "#1b9e77",
  "India"          = "#fa8128",
  "Italy"          = "#a6761d",
  "Japan"          = "#daa520",
  "United Kingdom" = "#1f78b4",
  "United States"  = "#e31a1c"
)

# ── Plot ───────────────────────────────────────────────────────────────────────

ggplot(plot_df, aes(x = renewable_pct, y = energy_intensity,
                    color = country_name, group = country_name)) +

  # Path with arrowhead at the 2010 point of each country
  geom_path(
    linewidth = 0.8,
    alpha     = 0.85,
    arrow     = arrow(type = "closed", length = unit(0.12, "inches"), ends = "last")
  ) +

  # Intermediate year points (1991–2009)
  geom_point(data = points_mid, size = 1.6) +

  # Open circle marks the 1990 starting position
  geom_point(data = points_1990, shape = 21, size = 3.5,
             fill = "white", stroke = 1.2) +

  # Country name labels at manually specified coordinates
  geom_text(
    data     = labels_2010,
    aes(x = label_x, y = label_y, label = country_name),
    size     = 3.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # ── Free-standing line segment for Japan
  annotate("segment",
           x = 3.8, xend = 1.5, y = 3.2, yend = 1.4,
           linewidth = 0.35, alpha = 0.6, color = "#daa520") +

  # ── Corner quadrant interpretation labels ──────────────────────────────────
  annotate("text", x = -10,  y = 25.5,
           label = "Fewer renewables,\nworse efficiency",
           hjust = 0, vjust = 1, size = 5, color = "black", fontface = "italic") +
  annotate("text", x =  64, y = 25.5,
           label = "More renewables,\nworse efficiency",
           hjust = 1, vjust = 1, size = 5, color = "black", fontface = "italic") +
  annotate("text", x = -10,  y = -2.5,
           label = "Fewer renewables,\nbetter efficiency",
           hjust = 0, vjust = 0, size = 5, color = "black", fontface = "italic") +
  annotate("text", x =  64, y = -2.5,
           label = "More renewables,\nbetter efficiency",
           hjust = 1, vjust = 0, size = 5, color = "black", fontface = "italic") +

  # ── Manual legend (open circle = 1990, arrow = 2010, trajectory note) ────────
  annotate("rect",
           xmin = 35, xmax = 61.5, ymin = 12.0, ymax = 19,
           fill = "white", color = "grey80", alpha = 0.9) +
  annotate("text",
           x = 48, y = 15.9, label = "Each path traces a country's\ntrajectory over the period\nof 1990-2010.",
           hjust = 0.5, vjust = 0, size = 4.5, color = "black", fontface = "italic") +
  annotate("point",
           x = 39.5, y = 14.7, shape = 21, size = 3.5,
           fill = "white", color = "grey30") +
  annotate("text",
           x = 41, y = 14.7, label = "Start (1990)",
           hjust = 0, vjust = 0.5, size = 3.8, color = "grey30") +
  annotate("segment",
           x = 38.5, xend = 40.5, y = 13, yend = 13,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "grey30", linewidth = 0.8) +
  annotate("text",
           x = 41, y = 13, label = "End (2010)",
           hjust = 0, vjust = 0.5, size = 3.8, color = "grey30") +

  # ── Scales & theme ─────────────────────────────────────────────────────────
  scale_color_manual(values = country_colors) +
  scale_x_continuous(limits = c(-11, 65), breaks = seq(0, 60, by = 10),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(0)) +
  scale_y_continuous(limits = c(-4, 27),  breaks = seq(0, 25, by = 5),
                     expand = expansion(0)) +
  labs(
    title    = "Economic Energy Intensity vs. Renewable Energy Consumption (1990–2010)",
    subtitle = "Top 10 economies by 2010 GDP",
    x        = "Renewable Energy (% of Total Final Energy Consumption)",
    y        = "Economic Energy Intensity (Megajoules per $1 GDP in 2005 prices)",
    caption  = paste0(
      "Energy data: SE4ALL / World Bank Energy Data (energydata.info)\n",
      "GDP rankings: World Bank, NY.GDP.MKTP.CD indicator (data.worldbank.org)"
    )
  ) +
  theme_bw() +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.caption     = element_text(color = "grey50", size = 8, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )
