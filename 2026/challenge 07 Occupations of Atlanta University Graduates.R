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
pink <- "#ffc0cb"
lighttan <- "#D2C3AF"
background <- "#E6D4C3"

## load the data
data <- read_csv("2026/data/challenge07.csv")

## occupation order and colors matching the plate legend (top to bottom)
occ_colors <- c(
  "Teachers"          = crimson,
  "Ministers"         = lightblue,
  "Government Service"= pink,
  "Business"          = tan,
  "Other Professions" = green,
  "House Wives"       = gold
)

data <- data |>
  mutate(Occupation = factor(Occupation, levels = names(occ_colors)))

## ── pie chart ──────────────────────────────────────────────────────────────────
## coord_polar starts at 12 o'clock; direction=-1 goes clockwise like the original
pie <- data |>
  ggplot(aes(x = "", y = Percentage/100*2*pi, fill = Occupation)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.4) +
  coord_radial(theta = "y", start = 3*pi/2, reverse = "theta", expand=FALSE) +
  scale_fill_manual(values = occ_colors) +
  geom_text(
    aes(label = ifelse(Percentage >= 20, paste0(Percentage, "%"), "")),
    position = position_stack(vjust = 0.5),
    family = "dubois", fontface = "bold", size = 6, color = "black"
  ) +
  ## label the smaller slices outside — tweak nudges as needed
  geom_text(
    aes(x=1.4,label = ifelse(Percentage < 20, paste0(Percentage, "%"), "")),
    position = position_stack(vjust = 0.5),
    family = "dubois", size = 6, color = "black"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA)
  )

pie

## ── small US map with Georgia highlighted ──────────────────────────────────────
us_states <- ne_states(country = "United States of America", returnclass = "sf") |>
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico")) |>
  mutate(fill_color = ifelse(name == "Georgia", "black", background))

usmap <- ggplot() +
  geom_sf(data = us_states, aes(fill = fill_color), color = "black", linewidth = 0.3) +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = NA, color = NA))

usmap

## ── legend items (drawn directly in cowplot, no panel clipping) ───────────────
## y positions in cowplot canvas coords (0-1), evenly spaced between 0.62 and 0.30
legend_y  <- seq(0.55, 0.3, length.out = length(occ_colors))
legend_x_dot  <- 0.08   # x for the coloured dot
legend_x_dot2 <- 0.96
legend_x_text <- 0.1   # x for the label
legend_x_text2 <- 0.94

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
  draw_label(
    "UNE SERIE DE CARTES ET DIAGRAMMES STATISTIQUES MONTRANT LA\n CONDITION PRESENTE DES DESCENDANTS DES ANCIENS ESCLAVES AFRI-\nCAINS ACTUELLMENT ESTABLIS DANS LES ETATS UNIS D'AMERIQUE.",
    x = 0.5, y=0.86, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", fontface = "bold", size = 22, lineheight = 1.25,
    color = "red"
  ) +

  ## prepared by block (left)
  draw_label(
    "PREPARED AND EXECUTED BY\n BLACK STUDENTS UNDER THE\nDIRECTION OF\nATLANTA UNIVERSITY,\nATLANTA, GA.,\nUNITED STATES OF AMERICA.",
    x = 0.2, y = 0.79, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 16, lineheight = 1.4, color = "black"
  ) +
  ## prepared by block (right)
  draw_label(
    "PREPAREES ET EXECUTEES PAR\nDES ETUDIANTS NEGRES SOUS\nLA DIRECTION DE L'UNIVERSITE\nD'ATLANTA.\nETAT DE GEORGIE.\nETATS UNIS D'AMERIQUE.",
    x =0.8, y = 0.79, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 16, lineheight = 1.4, color = "red"
  ) +

  ## small US map (centred between the two text blocks)
  draw_plot(usmap, x = 0.35, y = 0.68, width = 0.30, height = 0.11) +

  ## university description
  draw_label(
    "THE UNIVERSITY WAS FOUNDED IN 1867. IT HAS INSTRUCTED 6000 BLACK STUDENTS.",
    x = 0.5, y = 0.665, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 18, lineheight = 1.3, color = "black"
  ) +
  draw_label(
    "L'UNIVERSITE A ETE FONDEE EN 1867. ELLE A DONNE L'INSTRUCTION A 6000 ETUDIANTS NEGRES.",
    x = 0.5, y = 0.648, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 16, lineheight = 1.3, color = "red"
  ) +
  draw_label(
    "IT HAS GRADUATED 330 BLACK STUDENTS AMONG WHOM ARE :",
    x = .5, y = .633, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 18, lineheight = 1.3, color = "black"
  ) +
  draw_label(
    "ELLE A DELIVRE DES DIPLOMES A 330 NEGRES DONT :",
    x = .5, y = .615, hjust = 0.5, vjust = 1,
    fontfamily = "dubois", size = 16, lineheight = 1.3, color = "red"
  ) +

  ## legend dots
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[1], color=occ_colors[1], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[2], color=occ_colors[2], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[3], color=occ_colors[3], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[4], color=occ_colors[4], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[5], color=occ_colors[5], size=28) +
  draw_label("\u25CF", x=legend_x_dot, y=legend_y[6], color=occ_colors[6], size=28) +
  draw_label("\u25CF", x=legend_x_dot2, y=legend_y[1], color=occ_colors[1], size=28) +
  draw_label("\u25CF", x=legend_x_dot2, y=legend_y[2], color=occ_colors[2], size=28) +
  draw_label("\u25CF", x=legend_x_dot2, y=legend_y[3], color=occ_colors[3], size=28) +
  draw_label("\u25CF", x=legend_x_dot2, y=legend_y[4], color=occ_colors[4], size=28) +
  draw_label("\u25CF", x=legend_x_dot2, y=legend_y[5], color=occ_colors[5], size=28) +
  draw_label("\u25CF", x=legend_x_dot2, y=legend_y[6], color=occ_colors[6], size=28) +
  ## legend labels
  draw_label("TEACHERS",           x=legend_x_text, y=legend_y[1], hjust=0, fontfamily="dubois", size=15, color="black") +
  draw_label("MINISTERS",          x=legend_x_text, y=legend_y[2], hjust=0, fontfamily="dubois", size=15, color="black") +
  draw_label("GOVERNMENT SERVICE", x=legend_x_text, y=legend_y[3], hjust=0, fontfamily="dubois", size=15, color="black") +
  draw_label("BUSINESS",           x=legend_x_text, y=legend_y[4], hjust=0, fontfamily="dubois", size=15, color="black") +
  draw_label("OTHER PROFESSIONS",  x=legend_x_text, y=legend_y[5], hjust=0, fontfamily="dubois", size=15, color="black") +
  draw_label("HOUSE WIVES",        x=legend_x_text, y=legend_y[6], hjust=0, fontfamily="dubois", size=15, color="black") +
  draw_label("PROFESSEURS ET INSTITUTERS", x=legend_x_text2, y=legend_y[1], hjust=1, fontfamily="dubois", size=13, color="red") +
  draw_label("MINISTRES DE L'EVANGILE", x=legend_x_text2, y=legend_y[2], hjust=1, fontfamily="dubois", size=13, color="red") +
  draw_label("EMPLOYES DU GOUVERNEMENT", x=legend_x_text2, y=legend_y[3], hjust=1, fontfamily="dubois", size=13, color="red") +
  draw_label("MARCHANDS", x=legend_x_text2, y=legend_y[4], hjust=1, fontfamily="dubois", size=13, color="red") +
  draw_label("MEDICINS, ADVOCATS, ET ETUDIANTS", x=legend_x_text2, y=legend_y[5], hjust=1, fontfamily="dubois", size=13, color="red") +
  draw_label("MERES DE FAMILLE", x=legend_x_text2, y=legend_y[6], hjust=1, fontfamily="dubois", size=13, color="red") +

  ## pie chart — middle of the page
  draw_plot(pie, x = 0.28, y = 0.22, width = 0.45, height = 0.45) +

  ## bottom descriptive text
  draw_label(
    paste0(
      "THE UNIVERSITY HAS 20 PROFESSORS AND INSTRUCTORS AND 250 STUDENTS AT PRESENT.\n",
      "IT HAS FIVE BUILDINGS, 60 ACRES OF CAMPUS, AND A LIBRARY OF 11,000 VOLUMES. IT AIMS TO RAISE\n",
      "AND CIVILIZE THE SONS OF THE FREEDMEN BY TRAINING THEIR MORE CAPABLE MEMBERS IN THE LIBER-\n",
      "AL ARTS ACCORDING TO THE BEST STANDARDS OF THE DAY.\n",
      "THE PROPER ACCOMPLISHMENT OF THIS WORK DEMANDS AN ENDOWMENT FUND OF $500,000."
    ),
    x = 0.05, y = 0.22, hjust = 0.0, vjust = 1,
    fontfamily = "dubois", size = 15, lineheight = 1.7, color = "black"
  ) +
  draw_label(
    paste0(
      "L'UNIVERSITE A ACTUELLEMENT 20 PROFESSEURS ET INSTRUCTEURS ET 250 ETUDIANTS.\n",
       "ELLE EST COMPOSEE DES CINQ BATIMENTS. 60 ACRES (ENVIRON 26 HECTARES) DE TERRAIN SERVANT DE\n",
       "COUR ET DE CHAMP DE RECREATION, ET DUNE BIBLIOTHEQUE CONTENANT 11,000 VOLUMES.\n",
       "SON BUT EST D'ELEVER ET DE CIVILISER LES FILS DES NEGRES AFFRANCHIS EN DONNANT AUX MIEUX\n",
       "DOUES UNE EDUCATION DANS LES ARTS LIBERAUX EN ACCORD AVEC LES IDEES LES LES PLUG PROGRESS -\n",
       "SISTES DE L'EPOGUE.\n",
       "L'ACCOMPLISSEMENT DE CETTE OEUVRE DEMANDE UNE DOTATION DE 500,000 DOLLARS."
    ),
    x = 0.05, y = 0.12, hjust = 0, vjust = 1,
    fontfamily = "dubois", size = 13, lineheight = 1.7, color = "red"
  )

final + canvas(width = 22, height = 28)

ggsave(
  filename = "2026/final/challenge07.png",
  plot     = final,
  width    = 22, height = 28, units = "in",
  bg       = background
)
