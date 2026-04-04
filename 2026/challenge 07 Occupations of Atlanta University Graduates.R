library(tidyverse)
library(showtext)
library(ggview)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

font_add_google("Play", family = "dubois")
showtext_auto()
showtext_opts(dpi = 600)

## DuBois color set
brown <- "#654321"
tan <- "#d2b48c"
crimson <- "#dc143c"
gold <- "#ffd700"
green <- "#006400"
lightblue <- "#add8e6"
blue <- "#000C7B"
pink <- "#ffc0cb"
lighttan <- "#D2C3AF"
background <- "#E6D4C3"

## load the data
data <- read_csv("2026/data/challenge07.csv")

## occupation order and colors matching the plate legend (top to bottom)
occ_colors <- c(
  "Teachers"          = crimson,
  "Ministers"         = pink,
  "Government Service"= gold,
  "Business"          = brown,
  "Other Professions" = lightblue,
  "House Wives"       = lighttan
)

data <- data |>
  mutate(Occupation = factor(Occupation, levels = names(occ_colors)))

## ── pie chart ──────────────────────────────────────────────────────────────────
## coord_polar starts at 12 o'clock; direction=-1 goes clockwise like the original
pie <- data |>
  ggplot(aes(x = "", y = Percentage, fill = Occupation)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.4) +
  coord_polar(theta = "y", start = 0, direction = -1) +
  scale_fill_manual(values = occ_colors) +
  geom_text(
    aes(label = ifelse(Percentage >= 4, paste0(Percentage, "%"), "")),
    position = position_stack(vjust = 0.5),
    family = "dubois", fontface = "bold", size = 22, color = "white"
  ) +
  ## label the smaller slices outside — tweak nudges as needed
  geom_text(
    aes(label = ifelse(Percentage < 4, paste0(Percentage, "%"), "")),
    position = position_stack(vjust = 0.5),
    family = "dubois", size = 18, color = "black"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA)
  )

## ── small US map with Georgia highlighted ──────────────────────────────────────
us_states <- ne_states(country = "United States of America", returnclass = "sf") |>
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico")) |>
  mutate(fill_color = ifelse(name == "Georgia", "black", background))

usmap <- ggplot() +
  geom_sf(data = us_states, aes(fill = fill_color), color = "black", linewidth = 0.3) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = NA, color = NA))

## ── legend items (drawn directly in cowplot, no panel clipping) ───────────────
## y positions in cowplot canvas coords (0-1), evenly spaced between 0.62 and 0.30
legend_y  <- seq(0.62, 0.30, length.out = length(occ_colors))
legend_x_dot  <- 0.04   # x for the coloured dot
legend_x_text <- 0.08   # x for the label

## ── assemble with cowplot ──────────────────────────────────────────────────────
final <- ggdraw(xlim = c(0, 1), ylim = c(0, 1)) +
  theme(plot.background = element_rect(fill = background, color = background)) +

  ## series title
  draw_label(
    "A SERIES OF STATISTICAL CHARTS ILLUSTRA-\nTING THE CONDITION OF THE DESCENDANTS OF FOR-\nMER AFRICAN SLAVES NOW RESIDENT IN THE\nUNITED STATES OF AMERICA.",
    x = 0.5, y = 0.965, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", fontface = "bold", size = 30, lineheight = 1.25,
    color = "black"
  ) +

  ## prepared by block (left)
  draw_label(
    "PREPARED AND EXECUTED BY\nNEGRO STUDENTS UNDER THE\nDIRECTION OF\nATLANTA UNIVERSITY,\nATLANTA, GA.,\nUNITED STATES OF AMERICA.",
    x = 0.03, y = 0.79, hjust = 0, vjust = 1,
    fontfamily = "dubois", size = 16, lineheight = 1.4, color = "black"
  ) +

  ## small US map (centred between the two text blocks)
  draw_plot(usmap, x = 0.35, y = 0.70, width = 0.30, height = 0.11) +

  ## university description
  draw_label(
    "THE UNIVERSITY WAS FOUNDED IN 1867. IT HAS INSTRUCTED 6000 NEGRO STUDENTS.\nIT HAS GRADUATED 330 NEGROES AMONG WHOM ARE:",
    x = 0.5, y = 0.665, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 18, lineheight = 1.3, color = "black"
  ) +

  ## legend dots
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[1], color=occ_colors[1], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[2], color=occ_colors[2], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[3], color=occ_colors[3], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[4], color=occ_colors[4], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[5], color=occ_colors[5], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[6], color=occ_colors[6], size=28) +
  ## legend labels
  draw_label("TEACHERS",           x=legend_x_text, y=legend_y[1], hjust=0, fontfamily="dubois", size=22, color="black") +
  draw_label("MINISTERS",          x=legend_x_text, y=legend_y[2], hjust=0, fontfamily="dubois", size=22, color="black") +
  draw_label("GOVERNMENT SERVICE", x=legend_x_text, y=legend_y[3], hjust=0, fontfamily="dubois", size=22, color="black") +
  draw_label("BUSINESS",           x=legend_x_text, y=legend_y[4], hjust=0, fontfamily="dubois", size=22, color="black") +
  draw_label("OTHER PROFESSIONS",  x=legend_x_text, y=legend_y[5], hjust=0, fontfamily="dubois", size=22, color="black") +
  draw_label("HOUSE WIVES",        x=legend_x_text, y=legend_y[6], hjust=0, fontfamily="dubois", size=22, color="black") +

  ## pie chart — right half of the page
  draw_plot(pie, x = 0.44, y = 0.15, width = 0.54, height = 0.52) +

  ## bottom descriptive text
  draw_label(
    paste0(
      "THE UNIVERSITY HAS 20 PROFESSORS AND INSTRUCTORS AND 250 STUDENTS AT PRESENT.\n",
      "IT HAS FIVE BUILDINGS, 60 ACRES OF CAMPUS, AND A LIBRARY OF 10,000 VOLUMES. IT AIMS TO RAISE\n",
      "AND CIVILIZE THE SONS OF THE FREEDMEN BY TRAINING THEM MORE CAPABLE MEMBERS IN THE LIBER-\n",
      "AL ARTS ACCORDING TO THE STANDARDS OF THE BEST.\n",
      "THE PROPER ACCOMPLISHMENT OF THIS WORK DEMANDS AN ENDOWMENT FUND OF $500,000."
    ),
    x = 0.5, y = 0.155, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 15, lineheight = 1.35, color = "black"
  )

final + canvas(width = 22, height = 28)

ggsave(
  filename = "2026/final/challenge07.png",
  plot     = final,
  width    = 22, height = 28, units = "in",
  bg       = background
)
