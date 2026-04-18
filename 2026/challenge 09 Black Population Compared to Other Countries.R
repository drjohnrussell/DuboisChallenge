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
background <- "#E6D4C3"

## Load data
data <- read_csv("2026/data/challenge09.csv")

## ── Helper: create a shape plot ───────────────────────────────────────────────
make_shape_plot <- function(shape, fill = NA, outline = crimson, lw = 1.5) {
  ggplot() +
    geom_sf(data = shape, fill = fill, color = outline, linewidth = lw) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA)
    )
}

## ── Fetch country / region shapes ────────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

## Spain mainland only — crop out the Canary Islands (≈27–29°N)
spain <- filter(world, admin == "Spain") |>
  st_crop(st_bbox(c(xmin = -10, xmax = 5, ymin = 35, ymax = 44), crs = st_crs(4326)))

australia <- filter(world, admin == "Australia")

## Norway + Sweden: crop to Scandinavian peninsula, dropping Svalbard / Jan Mayen
scan_box   <- st_bbox(c(xmin = 4, xmax = 32, ymin = 54, ymax = 72), crs = st_crs(4326))
norway_main <- filter(world, admin == "Norway") |> st_crop(scan_box)
sweden_main <- filter(world, admin == "Sweden") |> st_crop(scan_box)
nordswe     <- bind_rows(norway_main, sweden_main)

## Netherlands + Belgium shown together — crop to European mainland only
## (Netherlands admin entry includes Caribbean BES islands which shrink the map)
benelux_box <- st_bbox(c(xmin = 2, xmax = 8, ymin = 49, ymax = 54), crs = st_crs(4326))
neth     <- filter(world, admin == "Netherlands") |> st_crop(benelux_box)
belgium  <- filter(world, admin == "Belgium")     |> st_crop(benelux_box)
nethbelg <- bind_rows(neth, belgium)

swiss   <- filter(world, admin == "Switzerland")
hungary <- filter(world, admin == "Hungary")

## USA lower 48 — filled black for the Negro Population silhouette
us48 <- ne_states(country = "United States of America", returnclass = "sf") |>
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico"))

## Bavaria (Bayern) — a German state
bavaria <- ne_states(country = "Germany", returnclass = "sf") |>
  filter(name == "Bayern")

## England — filter by geonunit and union all districts into one shape
england <- ne_states(country = "United Kingdom", returnclass = "sf") |>
  filter(geonunit == "England") |>
  st_union()

## ── Create individual shape plots ─────────────────────────────────────────────
p_spain     <- make_shape_plot(spain)
p_australia <- make_shape_plot(australia)
p_nordswe   <- make_shape_plot(nordswe)
p_nethbelg  <- make_shape_plot(nethbelg)
p_swiss     <- make_shape_plot(swiss)
p_hungary   <- make_shape_plot(hungary)
p_bavaria   <- make_shape_plot(bavaria)
p_england   <- make_shape_plot(england)
p_usa       <- make_shape_plot(us48, fill = "black", outline = "black")

## ── Population lookup ─────────────────────────────────────────────────────────
pop <- setNames(data$Population, data$Country)
fmt <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)

## ── Layout constants ──────────────────────────────────────────────────────────
## 3-column grid; cx = column centres; pw = plot width
cx <- c(0.17, 0.45, 0.75)
pw <- 0.305

## ── Assemble with cowplot ─────────────────────────────────────────────────────
final <- ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  theme(plot.background = element_rect(fill = background, color = background)) +

  ## ── Titles ────────────────────────────────────────────────────────────────
  draw_label(
    "BLACK POPULATION OF THE UNITED STATES COMPARED WITH THE TOTAL POPULATION OF OTHER COUNTRIES .",
    x = 0.5, y = 0.95, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", fontface = "bold", size = 29, lineheight = 1.2, color = "black"
  ) +
  draw_label(
    "POPULATION NÈGRE DES ÉTATS UNIS COMPARÉE À LA POPULATION TOTALE DES AUTRES PAYS .",
    x = 0.5, y = 0.93, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 29, lineheight = 1.2, color = "black"
  ) +
  draw_label(
    "DONE BY ATLANTA UNIVERSITY .",
    x = 0.5, y = 0.910, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 25, color = "black"
  ) +

  ## ── Row 1: España | Australia | Norvège + Suède ───────────────────────────
  ## shapes
  draw_plot(p_spain,     x = cx[1] - pw/2, y = 0.675, width = .9*pw, height = 0.220) +
  draw_plot(p_australia, x = cx[2] - pw/5, y = 0.675, width = .5*pw, height = 0.200) +
  draw_plot(p_nordswe,   x = cx[3] - pw/2, y = 0.675, width = .9*pw, height = 0.220) +
  ## labels
  draw_label("ESPAÑA",           x=cx[1]-pw/5, y=0.79, hjust=0, vjust=1, fontfamily="dubois", size=19, color="black") +
  draw_label(fmt(pop["ESPAÑA"]), x=cx[1]-pw/4, y=0.775, hjust=0, vjust=1, fontfamily="dubois", size=16, color="black") +
  draw_label("AUSTRALIA",              x=cx[2], y=0.80, hjust=0.5, vjust=1, fontfamily="dubois", size=19, color="black") +
  draw_label(fmt(pop["AUSTRALIA"]),    x=cx[2], y=0.785, hjust=0.5, vjust=1, fontfamily="dubois", size=16, color="black") +
  draw_label("NORVÈGE   SUÈDE",        x=cx[3]-pw/6, y=0.77, hjust=0.5, vjust=1, fontfamily="dubois", size=19, color="black") +
  draw_label(
    paste0(fmt(pop["NORVÈGE"]), "   ", fmt(pop["SUÈDE"])),
    x=cx[3]-pw/6, y=0.755, hjust=0.5, vjust=1, fontfamily="dubois", size=16, color="black"
  ) +

  ## ── Row 2/3: Nederlanden + La Belgique (combined) | USA (black) | Suisse ───
  ## shapes
  draw_plot(p_nethbelg, x = cx[1] - pw/2, y = 0.310, width = pw, height = 0.265) +
  draw_plot(p_usa,      x = cx[2] - pw/3, y = 0.40, width = .8*pw, height = 0.225) +
  draw_plot(p_swiss,    x = cx[3]-pw/4, y = 0.485, width = .6*pw, height = 0.130) +
  ## labels
  draw_label("NEDERLANDEN",
             x=cx[1]+.03, y=0.49, hjust=0.5, vjust=1, fontfamily="dubois", size=19, color="black") +
  draw_label("LA BELGIQUE",
             x=cx[1], y=0.40, hjust=0.5, vjust=1, fontfamily="dubois", size=19, color="black") +
  draw_label(paste0(fmt(pop["NEDERLANDEN"])),
             x=cx[1]+.03, y=0.475, hjust=0.5, vjust=1, fontfamily="dubois", size=16, color="black") +
  draw_label(paste0(fmt(pop["LA BELGIQUE"])),
             x=cx[1], y=0.385, hjust=0.5, vjust=1, fontfamily="dubois", size=16, color="black") +
  ## USA text drawn inside the black silhouette
  draw_label(
    "U.S.A.\nBLACK POPULATION\nPOPULATION NOIRE\n7,500,000",
    x=cx[2], y=0.42, hjust=0.5, vjust=0.5,
    fontfamily="dubois", size=18, lineheight=1.35, color="black"
  ) +
  draw_label("SUISSE",              x=cx[3], y=0.56, hjust=0.5, vjust=1, fontfamily="dubois", size=19, color="black") +
  draw_label(fmt(pop["SUISSE"]),    x=cx[3], y=0.545, hjust=0.5, vjust=1, fontfamily="dubois", size=16, color="black") +

  ## ── Row 4: Hongrie | Bayern | England ────────────────────────────────────
  ## shapes
  draw_plot(p_hungary, x = cx[1] - pw/2, y = 0.075, width = pw, height = 0.175) +
  draw_plot(p_bavaria, x = cx[2] - pw/2, y = 0.085, width = pw, height = 0.160) +
  draw_plot(p_england, x = cx[3] - pw/2, y = 0.075, width = 1.5*pw, height = 0.25) +
  ## labels
  draw_label("HONGRIE",             x=cx[1], y=0.180, hjust=0.5, vjust=1, fontfamily="dubois", size=19, color="black") +
  draw_label(fmt(pop["HONGRIE"]),   x=cx[1], y=0.165, hjust=0.5, vjust=1, fontfamily="dubois", size=16, color="black") +
  draw_label("BAYERN",              x=cx[2], y=0.180, hjust=0.5, vjust=1, fontfamily="dubois", size=19, color="black") +
  draw_label(fmt(pop["BAYERN"]),    x=cx[2], y=0.165, hjust=0.5, vjust=1, fontfamily="dubois", size=16, color="black") +
  draw_label("ENGLAND",             x=cx[3]+.12, y=0.180, hjust=0.5, vjust=1, fontfamily="dubois", size=19, color="black") +
  draw_label(fmt(pop["ENGLAND"]),   x=cx[3]+.12, y=0.165, hjust=0.5, vjust=1, fontfamily="dubois", size=16, color="black")

ggsave(
  filename = "2026/final/challenge09.png",
  plot     = final,
  width    = 22, height = 28, units = "in", dpi = 600,
  bg       = background
)
