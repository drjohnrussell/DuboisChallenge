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

counties <- read_csv("2026/data/challenge03.csv") |> 
  mutate(County=str_to_upper(County))
## 139 rows
map <- read_sf("2026/data/challenge03 shapes") |> 
  mutate(NHGISNAM=str_to_upper(NHGISNAM))
## 137 rows
pop <- read_csv("2026/data/challenge03 pop.csv", col_names=c("County","Color"))
## 200 rows??

## the names look to be inconsistent, let's see
counties |> 
  filter_out(County %in% map$NHGISNAM)
## 4 counties without a match (BIBB02, MARION03, McDUFFIE, McINTOSH)
## 2 after mutate above, look to be BIBB02 and MARION03

map |> 
  filter_out(NHGISNAM %in% counties$County)
## 2 counties without a match (MCDUFFIE, MCINTOSH)
## can make a complete set from the map with an str_upper mutate (above)
## complete set here with mutate

mapfinal <- map |> 
  left_join(counties, by=join_by(NHGISNAM==County))

image <- mapfinal |> 
  ggplot(aes(fill=Population)) +
  geom_sf(color="black") +
  coord_sf(crs=sf::st_crs(4326)) +
  scale_fill_manual(values=c(green, "black", gold, tan, brown,
                             blue, pink, crimson)) +
  theme_void() + theme(legend.position="none") +
  scale_x_continuous(expand = expansion(mult=.3)) +
  annotate("point", x=-86, y=29.8, size=38, shape=21, fill="black", color="black") +
  annotate("point", x=-86, y=29.1, size=38, shape=21, fill=blue, color="black") +
  annotate("point", x=-86, y=28.4, size=38, shape=21, fill=brown, color="black") +
  annotate("point", x=-86, y=27.7, size=38, shape=21, fill=tan, color="black") +
  annotate("point", x=-82, y=29.8, size=38, shape=21, fill=crimson, color="black") +
  annotate("point", x=-82, y=29.1, size=38, shape=21, fill=pink, color="black") +
  annotate("point", x=-82, y=28.4, size=38, shape=21, fill=gold, color="black") +
  annotate("point", x=-82, y=27.7, size=38, shape=21, fill=green, color="black") +
  annotate("text", x=-85.5, y=29.8, size=35, hjust=0, family="dubois", label="OVER 30,000 NEGROES") +
  annotate("text", x=-85.5, y=29.1, size=35, hjust=0, family="dubois", label="BETWEEN 20,000 AND 30,000") +
  annotate("text", x=-85.5, y=28.4, size=35, hjust=0, family="dubois", label="15,000 TO 20,000") +
  annotate("text", x=-85.5, y=27.7, size=35, hjust=0, family="dubois", label="10,000 TO 15,000") +
  annotate("text", x=-81.5, y=29.8, size=35, hjust=0, family="dubois", label="5,000 TO 10,000") +
  annotate("text", x=-81.5, y=29.1, size=35, hjust=0, family="dubois", label="2,500 TO 5,000") +
  annotate("text", x=-81.5, y=28.4, size=35, hjust=0, family="dubois", label="1,000 TO 2,500") +
  annotate("text", x=-81.5, y=27.7, size=35, hjust=0, family="dubois", label="UNDER 1,000") +
  annotate("text", x=-83, y=37.5, size=50, family="dubois", fontface=2, lineheight=.5, 
            label="NEGRO POPULATION OF GEORGIA BY COUNTIES \n 1890.")


image + canvas(width=22, height=28, bg=background)
  
