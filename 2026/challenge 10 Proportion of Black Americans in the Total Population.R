library(tidyverse)
library(showtext)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

font_add_google("Play", family = "dubois")
showtext_auto()
showtext_opts(dpi = 600)

## DuBois color palette
crimson    <- "#dc143c"
green      <- "#006400"
background <- "#E6D4C3"

## Load data
data <- read_csv("2026/data/challenge10.csv")

## ── US outline (projected) ────────────────────────────────────────────────────
laea <- "+proj=laea +lat_0=45 +lon_0=-100"

us_outline <- ne_states(country = "United States of America", returnclass = "sf") |>
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico")) |>
  summarise(geometry = st_union(geometry)) |>
  st_transform(crs = laea)

## Centroid for scaling — keeps the silhouette centred inside the red outline
us_center  <- st_centroid(us_outline)
cx <- st_coordinates(us_center)[1]
cy <- st_coordinates(us_center)[2]

## Scale the US outline to represent a given population proportion.
## Area ∝ factor², so linear scale = sqrt(proportion).
scale_us <- function(outline_sf, proportion) {
  g <- st_geometry(outline_sf)
  scaled_g <- (g - c(cx, cy)) * sqrt(proportion) + c(cx, cy)
  st_crs(scaled_g) <- st_crs(outline_sf)
  st_set_geometry(outline_sf, scaled_g)
}

us_1800 <- scale_us(us_outline, 1/5)
us_1830 <- scale_us(us_outline, 1/6)
us_1860 <- scale_us(us_outline, 1/7)
us_1890 <- scale_us(us_outline, 1/8)

## ── Map factory: red-outlined USA + scaled black silhouette ──────────────────
make_map <- function(scaled_us) {
  ggplot() +
    geom_sf(data = us_outline, fill = background, color = crimson, linewidth = 1.5) +
    geom_sf(data = scaled_us,  fill = "black",    color = NA) +
    theme_void() +
    theme(
      legend.position  = "none",
      plot.background  = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA)
    )
}

p_1800 <- make_map(us_1800)
p_1830 <- make_map(us_1830)
p_1860 <- make_map(us_1860)
p_1890 <- make_map(us_1890)

## ── Assemble with cowplot ─────────────────────────────────────────────────────
final <- ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  theme(plot.background = element_rect(fill = background, color = background)) +

  ## ── Titles ──────────────────────────────────────────────────────────────────
  draw_label(
    "PROPORTION OF BLACK AMERICANS IN THE TOTAL POPULATION OF THE UNITED STATES .",
    x = 0.5, y = 0.978, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", fontface = "bold", size = 27, color = "black"
  ) +
  draw_label(
    "RAPPORT DES N\u00c8GRES \u00c0 LA POPULATION TOTALE DES ETATS UNIS .",
    x = 0.5, y = 0.948, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 22, color = "black"
  ) +
  draw_label(
    "DONE BY ATLANTA UNIVERSITY .",
    x = 0.5, y = 0.921, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 18, color = "black"
  ) +

  ## ── 1800 (top-left, smallest) ────────────────────────────────────────────
  draw_label(
    "1800",
    x = 0.155, y = 0.696, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 27, color = "black"
  ) +
  draw_plot(p_1800, x = 0.11, y = 0.510, width = 0.16, height = 0.180) +
  draw_label(
    "ONE\u00b7FIFTH",
    x = 0.185, y = 0.495, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 22, fontface="bold", color = green
  ) +

  ## ── 1830 (top-right, medium) ─────────────────────────────────────────────
  draw_label(
    "1830",
    x = 0.695, y = 0.700, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 32, color = "black"
  ) +
  draw_plot(p_1830, x = 0.555, y = 0.495, width = 0.295, height = 0.235) +
  draw_label(
    "ONE\u00b7SIXTH",
    x = 0.675, y = 0.476, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 22, fontface="bold", color = green
  ) +

  ## ── 1890 year label (large, positioned above bottom-right map) ────────────
  draw_label(
    "1890",
    x = 0.755, y = 0.425, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 52, color = "black"
  ) +

  ## ── 1860 (bottom-left, large) ────────────────────────────────────────────
  draw_label(
    "1860",
    x = 0.225, y = 0.440, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 48, color = "black"
  ) +
  draw_plot(p_1860, x = 0.01, y = 0.175, width = 0.415, height = 0.258) +
  draw_label(
    "ONE\u00b7SEVENTH",
    x = 0.225, y = 0.156, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", fontface= "bold", size = 22, color = green
  ) +

  ## ── 1890 (bottom-right, largest) ─────────────────────────────────────────
  draw_plot(p_1890, x = 0.455, y = 0.175, width = 0.445, height = 0.272) +
  draw_label(
    "ONE\u00b7EIGHTH",
    x = 0.725, y = 0.150, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", fontface = "bold", size = 34, color = green
  )

ggsave(
  filename = "2026/final/challenge10.png",
  plot     = final,
  width    = 22, height = 28, units = "in", dpi = 600,
  bg       = background
)
