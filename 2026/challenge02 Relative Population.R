library(tidyverse)
library(sf)
library(ggpattern)
library(cowplot)
library(grid)
library(showtext)

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

font_add_google("Play", family = "dubois")
showtext_auto()

population <- read_csv("2026/data/challenge02.csv")
map <- read_sf("2026/data/challenge02 shapes/")

duboismap <- map |> 
  inner_join(population |> select(State, Population = PopulationCorrected), by=join_by(STUSPS==State)) |> 
  mutate(type=case_when(Population=="600,000 - 750,000" ~ "cross-hatch",
                         Population=="500,000 - 600,000" ~ "diagonal",
                        .default="color"))

final <- ggplot() +
  geom_sf_pattern(data=duboismap |> filter(type=="cross-hatch"),
                    pattern="crosshatch", fill=background, pattern_angle=0,
                    pattern_fill="black", pattern_density=.1, pattern_size=.1,
                    pattern_spacing=.01) +
  geom_sf_pattern(data=duboismap |> filter(type=="diagonal"),
                    pattern="stripe", fill=background, pattern_angle=-45,
                    pattern_fill="black", pattern_density=.1, pattern_size=.1,
                    pattern_spacing=.01) +
  geom_sf(data=duboismap |> filter(type=="color"),
          aes(fill=Population)) +
  scale_fill_manual(values=c(gold, tan, blue,pink, brown, crimson, "black", lighttan)) +
  theme_void() + theme(legend.position="none", 
                        plot.background=element_rect(fill=background, color = background)) +
  annotate("rect", xmin=-118, xmax=-114, ymin=22, ymax=25, fill="black") +
  annotate(
    geom = ggpattern::GeomRectPattern,
    xmin = -118, xmax = -114,  # Adjust coordinates to your map
    ymin = 16, ymax = 19,
    pattern = "crosshatch",
    pattern_density = 0.1,
    pattern_spacing=0.01,
    pattern_size=.1,
    pattern_angle=0,
    fill=background,
    pattern_fill = "black", color="black"
  ) +
  annotate(
    geom = ggpattern::GeomRectPattern,
    xmin=-118, xmax=-114,
    ymin=10, ymax=13,
    pattern="stripe", fill=background,
    pattern_angle=-45, pattern_fill="black",
    pattern_density=.1, pattern_size=.1, pattern_spacing=.01, color="black") +
  annotate(
    "rect", xmin=-118, xmax=-114,
    ymin=4, ymax=7,
    fill=brown, color="black"
  ) +
  annotate(
    "rect", xmin=-118, xmax=-114,
    ymin=-2, ymax=1, fill=blue, color="black"
  ) +
  annotate(
    "rect", xmin=-93, xmax=-89,
    ymin=22, ymax=25, fill=lighttan, color="black"
  ) +
  annotate(
    "rect", xmin=-93, xmax=-89,
    ymin=16, ymax=19, fill=crimson, color="black"
  ) +
  annotate(
    "rect", xmin=-93, xmax=-89,
    ymin=10, ymax=13, fill=pink, color="black"
  ) +
  annotate(
    "rect", xmin=-93, xmax=-89,
    ymin = 4, ymax=7, fill=gold, color="black"
  ) +
  annotate(
    "rect", xmin=-93, xmax=-89,
    ymin=-2, ymax=1, fill=tan, color="black") +
  annotate(
    "text", y=23.5, x=-113, hjust=0, label="750,000 NEGROES AND OVER", 
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=17.5, x=-113, hjust=0, label="600,000 - 750,000",
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=11.5, x=-113, hjust=0, label="500,000 - 600,000",
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=5.5, x=-113, hjust=0, label="300,000 - 500,000",
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=-0.5, x=-113, hjust=0, label="200,000 - 300,000",
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=23.5, x=-88, hjust=0, label="100,000 - 200,000",
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=17.5, x=-88, hjust=0, label="50,000 - 100,000",
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=11.5, x=-88, hjust=0, label="25,000 - 50,000",
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=5.5, x=-88, hjust=0, label="10,000 - 25,000",
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=-0.5, x=-88, hjust=0, label="UNDER - 10,000",
    size=34, family="dubois"
  ) +
  annotate(
    "text", y=70, x=-95, hjust=0.5, lineheight=.5, fontface=2,
    label = "RELATIVE NEGRO POPULATION OF THE STATES OF THE \n UNITED STATES .",
    size=50, family="dubois"
  )

final + canvas(width=22, height=28, bg=background)
