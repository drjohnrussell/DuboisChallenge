library(tidyverse)
library(showtext)
library(ggbrace)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggview)

font_add_google("Play", family = "dubois")
showtext_auto()
showtext_opts(dpi = 600)

## DuBois color palette
brown      <- "#654321"
tan        <- "#d2b48c"
crimson    <- "#dc143c"
gold       <- "#ffd700"
green      <- "#006400"
lightblue  <- "#add8e6"
blue       <- "#000C7B"
pink       <- "#ffc0cb"
lighttan   <- "#D2C3AF"
background <- "#E6D4C3"

## Category fill colors matching the plate legend (left to right)
cat_levels <- c("Less than 1", "1-4", "4-8", "8-15", "15-25")
cat_colors <- c(
  "Less than 1" = gold,
  "1-4"         = blue,
  "4-8"         = crimson,
  "8-15"        = brown,
  "15-25"       = "black"
)

## Load data
data <- read_csv("2026/data/challenge08.csv") |>
  mutate(Category = factor(Category, levels = cat_levels))

## US states (lower 48) with category data joined
us_states <- ne_states(country = "United States of America", returnclass = "sf") |>
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico")) |>
  left_join(data, by = c("postal" = "State")) |>
  mutate(Category = factor(Category, levels = cat_levels))

## ── Choropleth map ────────────────────────────────────────────────────────────
usmap <- ggplot() +
  geom_sf(data = us_states, aes(fill = Category), color = "black", linewidth = 0.4) +
  scale_fill_manual(values = cat_colors, na.value = background, drop = FALSE) +
  coord_sf(crs = sf::st_crs("+proj=laea +lat_0=45 +lon_0=-100")) +
  theme_void() +
  theme(
    legend.position  = "none",
    plot.background  = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = NA, color = NA)
  )

## ── Brace spanning the legend — opens downward beneath the labels ─────────────
brace_df   <- data.frame(x = c(0.05, 0.95), y = c(0, 0.6))
brace_plot <- ggplot(brace_df, aes(x = x, y = y)) +
  stat_brace(outside = FALSE, rotate = 180) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(plot.background = element_rect(fill = NA, color = NA))

## x positions for the 5 legend circles / labels
legend_x <- seq(0.20, 0.80, length.out = 5)

## ── Assemble with cowplot ─────────────────────────────────────────────────────
final <- ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  theme(plot.background = element_rect(fill = background, color = background)) +

  ## Title
  draw_label(
    "DISTRIBUTION OF BLACK AMERICANS IN THE UNITED STATES.",
    x = 0.5, y = 0.978, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", fontface = "bold", size = 32, color = "black"
  ) +
  ## French subtitle
  draw_label(
    "DISTRIBUTION DES NEGRES DAN LES ETATS UNIS.",
    x = 0.5, y = 0.947, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 20, color = "black"
  ) +

  ## Legend circles
  draw_label("\u25CF", x = legend_x[1], y = 0.814, color = gold,    size = 100) +
  draw_label("\u25CF", x = legend_x[2], y = 0.814, color = blue,    size = 100) +
  draw_label("\u25CF", x = legend_x[3], y = 0.814, color = crimson, size = 100) +
  draw_label("\u25CF", x = legend_x[4], y = 0.814, color = brown,   size = 100) +
  draw_label("\u25CF", x = legend_x[5], y = 0.814, color = "black", size = 100) +
  ## Legend labels
  draw_label("LESS THAN\n1", x = legend_x[1], y = 0.789, hjust = 0.5, vjust = 1,
             fontfamily = "dubois", size = 20, lineheight = 1.0, color = "black") +
  draw_label("1-4",   x = legend_x[2], y = 0.789, hjust = 0.5, vjust = 1,
             fontfamily = "dubois", size = 20, color = "black") +
  draw_label("4-8",   x = legend_x[3], y = 0.789, hjust = 0.5, vjust = 1,
             fontfamily = "dubois", size = 20, color = "black") +
  draw_label("8-15",  x = legend_x[4], y = 0.789, hjust = 0.5, vjust = 1,
             fontfamily = "dubois", size = 20, color = "black") +
  draw_label("15-25", x = legend_x[5], y = 0.789, hjust = 0.5, vjust = 1,
             fontfamily = "dubois", size = 20, color = "black") +
  ## Brace below legend labels
  draw_plot(brace_plot, x = 0.10, y = 0.760, width = 0.80, height = 0.018) +

  ## Unit label below brace
  draw_label(
    "BLACK AMERICANS TO THE SQUARE MILE.",
    x = 0.5, y = 0.75, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 25, color = "black"
  ) +

  ## Choropleth map
  draw_plot(usmap, x = 0.0, y = 0.02, width = 1.0, height = 0.81)

ggsave(
  filename = "2026/final/challenge08.png",
  plot     = final,
  width    = 22, height = 28, units = "in", dpi = 600,
  bg       = background
)
