library(tidyverse)
library(sf)
library(rnaturalearth)
library(showtext)
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

# load us map data, by state, and filter to the contiguous US
us_states <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico"))

## load the data
presentlocation <- read_csv("2026/data/challenge05 present.csv") |> 
  rename(color=...3)
birthlocation <- read_csv("2026/data/challenge05 birthplace.csv") |> 
  rename(color=...3)

## double check that the data fits through postal
presentlocation |> 
  filter_out(State %in% us_states$postal)
## looks good, AK is only one missing and we don't want to graph that

### we need to create a set of lines that go from the centroid of georgia to the centroid of each state
## we have geometry in the us_states data, so we can get the centroids
us_states <- us_states |> 
  mutate(centroid = st_centroid(geometry))

## we can then create a data frame that has the centroids of georgia and the centroids of each state, and then we can use that to create the lines
# Get Georgia centroid as a single point
georgia_centroid <- us_states$centroid[us_states$name == "Georgia"]

lines <- us_states |>
  filter(name != "Georgia") |>
  select(name, postal, centroid) |>
  mutate(
    line = st_sfc(
      mapply(
        function(a, b) st_linestring(matrix(c(st_coordinates(a), st_coordinates(b)), nrow = 2, byrow = TRUE)),
        centroid,
        rep(georgia_centroid, n()),
        SIMPLIFY = FALSE
      ),
      crs = st_crs(us_states)
    )
  ) |>
  st_set_geometry("line") |> 
  ## shorten the linestring to only that part that is within the state
  rowwise() |>
  mutate(line = st_intersection(geometry, line)) |>
  ungroup()

textpresent <- presentlocation |> 
  left_join(us_states, by = c("State" = "postal")) |> 
  st_as_sf() |> 
  st_set_geometry("centroid")
mappresent <- us_states |> 
  left_join(presentlocation, by = c("postal" = "State"))
mapbirth <- us_states |> 
  left_join(birthlocation, by = c("postal" = "State"))

textbirth <- birthlocation |>
  filter(!is.na(Birthplace)) |> 
  left_join(us_states, by = c("State" = "postal")) |> 
  st_as_sf() |> 
  st_set_geometry("centroid")


## now we can plot the map
topmap <- ggplot() +
  geom_sf(data = mappresent, aes(fill = color), color = "black", show.legend=FALSE, alpha=.8) +
  scale_fill_manual(values = c(background, "black", lightblue, brown, crimson, gold, green, pink, tan)) +
  geom_sf(data = lines, color = "darkgrey", size = 2, alpha=.7, arrow = arrow(length = unit(0.8, "cm"), ends="first", type = "open")) +
  geom_sf_text(data = textpresent, aes(label = `Present Location`, color=ifelse(name == "Georgia", "white", "black")), size = 25, family = "dubois", vjust= 1.5, show.legend=FALSE) +
  scale_color_manual(values = c("black", "white")) +
  ## set to orthographic projection centered on missouri
  coord_sf(crs = sf::st_crs("+proj=ortho +lat_0=39 +lon_0=-98")) +
  theme_void() +
  labs(caption = "PRESENT DWELLING PLACE OF BLACK PEOPLE BORN IN GEORGIA") +
  theme(panel.background = element_rect(fill = background, color = background),
        plot.background = element_rect(fill = background, color = background),
## place title underneat plot
        plot.caption = element_text(family = "dubois", size = 150, hjust = 0.5, vjust = 0.5),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20))

bottommap <- ggplot() +
  geom_sf(data = mapbirth, aes(fill = color), color = "black", show.legend=FALSE, alpha=.8) +
  scale_fill_manual(values = c(background, "black", lightblue, brown, crimson, gold, green, pink, tan)) +
  geom_sf_text(data = textbirth, aes(label = `Birthplace`, color=ifelse(name == "Georgia", "white", "black")), size = 25, family = "dubois", vjust= 2, show.legend=FALSE) +
  scale_color_manual(values = c("black", "white")) +
  geom_sf(data = lines |> filter(name %in% textbirth$name), color = "darkgrey", size = 2, alpha=.7, arrow = arrow(length = unit(0.8, "cm"), ends="last", type = "open")) +
  ## set to orthographic projection centered on missouri
  coord_sf(crs = sf::st_crs("+proj=ortho +lat_0=39 +lon_0=-98")) +
  theme_void() +
  labs(caption = "BIRTH PLACE OF BLACK PEOPLE NOW RESIDENT IN GEORGIA") +
  theme(panel.background = element_rect(fill = background, color = background),
        plot.background = element_rect(fill = background, color = background),
        plot.caption = element_text(family = "dubois", size = 150, hjust = 0.5, vjust = 0.5),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20))

library(patchwork)

final <- topmap / bottommap + plot_annotation(title = "MIGRATION OF BLACK PEOPLE. \n 1890", 
                                              theme = theme(plot.title = element_text(family = "dubois", size = 200, hjust = 0.5, vjust = 0, lineheight=.25),
                                                            plot.background = element_rect(fill = background, color = background)))

final + canvas(width=22, height=28, units="in", bg=background)

ggsave("2026/final/challenge05.png", final, width = 22, height = 28, units = "in", bg = background)
