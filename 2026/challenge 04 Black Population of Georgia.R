library(tidyverse)
library(showtext)
library(sf)
library(ggview)

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

pop1870 <- read_csv("2026/data/challenge04 1870.csv", col_names=c("County","Color1870")) |> 
  mutate(County=str_to_upper(County))
pop1880 <- read_csv("2026/data/challenge04 1880.csv", col_names=c("County","Color1880")) |> 
  mutate(County=str_to_upper(County))
## 139 rows

map <- read_sf("2026/data/challenge03 shapes") |> 
  mutate(NHGISNAM=paste0(str_to_upper(NHGISNAM),"01")) |> 
  left_join(pop1870, by=c("NHGISNAM"="County")) |> 
  left_join(pop1880, by=c("NHGISNAM"="County"))

map1 <- map |> 
  ggplot(aes(fill=Color1870)) +
  geom_sf(color="black") +
  coord_sf(crs=sf::st_crs(4326)) +
  scale_fill_manual(values=c(blue, brown, crimson, gold, green, pink, tan, "white")) +
  theme_void() + theme(legend.position="none") +
  scale_x_continuous(expand = expansion(mult=.3)) +
  labs(title="1870") +
  annotate("point", x=-80.5, y=34.4, size=33, color=blue) +
  annotate("point", x=-80.5, y=33.6, size=33, color=brown) +
  annotate("point", x=-80.5, y=32.8, size=33, color=tan) +
  annotate("text", x=-79.7, y=34.4, label="BETWEEN 20,000 AND 30,000", family="dubois", size=33, hjust=0) +
  annotate("text", x=-79.7, y=33.6, label="15,000 TO 20,000", family="dubois", size=33, hjust=0) +
  annotate("text", x=-79.7, y=32.8, label="10,000 TO 15,000", family="dubois", size=33, hjust=0) +
  scale_x_continuous(expand = expansion(mult=c(0.2,.8))) +
  theme(plot.title=element_text(family="dubois", size=120, hjust=.2, vjust=-.75),
        plot.background=element_rect(fill=background, color=background)) 

map2 <- map |> 
  ggplot(aes(fill=Color1880)) +
  geom_sf(color="black") +
  coord_sf(crs=sf::st_crs(4326)) +
  scale_fill_manual(values=c(blue, brown, crimson, gold, green, pink, tan, "white")) +
  theme_void() + theme(legend.position="none") +
  scale_x_continuous(expand = expansion(mult=c(.2, .1))) +
  labs(title="1880") +
  annotate("point", x=-90.5, y=34.4, size=33, color=crimson) +
  annotate("point", x=-90.5, y=33.4, size=33, color=pink) +
  annotate("point", x=-90.5, y=32.4, size=33, color=gold) +
  annotate("point", x=-90.5, y=31.4, size=33, color=green) +
  annotate("text", x=-89.7, y=34.4, label="5,000 TO 10,000", family="dubois", size=33, hjust=0) +
  annotate("text", x=-89.7, y=33.4, label="2,500 TO 5,000", family="dubois", size=33, hjust=0) +
  annotate("text", x=-89.7, y=32.4, label="1,000 TO 2,500", family="dubois", size=33, hjust=0) +
  annotate("text", x=-89.7, y=31.4, label="UNDER 1,000", family="dubois", size=33, hjust=0) +
  theme(plot.title=element_text(family="dubois", size=120, hjust=.65, vjust=-.75),
        plot.background=element_rect(fill=background, color=background))

library(patchwork)
p <- (map1 + map2) + plot_layout(ncol = 1)
p <- p & theme(plot.background = element_rect(fill = background, color = background))

final <- p + plot_annotation(
  title = "BLACK POPULATION OF GEORGIA BY COUNTIES.",
  theme = theme(plot.title = element_text(family = "dubois", size = 160, hjust = .5, vjust = 5))
)

final + canvas(width=22, height=28, units="in", bg=background)
