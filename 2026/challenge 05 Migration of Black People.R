library(tidyverse)
library(sf)
library(rnaturalearth)

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
georgia_centroid <- us_states |>
  filter(name == "Georgia") |>
  st_centroid() |>
  st_coordinates() |>
  as_tibble() |>
  rename(georgia_lon = X, georgia_lat = Y)

# Get all state centroids as one row per state
state_centroids <- us_states |>
  st_centroid() |>
  st_coordinates() |>
  as_tibble() |>
  rename(state_lon = X, state_lat = Y)

## create a line going from the centroid of georgia to the 
## state centroids, but trimmed to the geometry of the state
lines <- state_centroids |>
  bind_cols(georgia_centroid) |>
  rowwise() |>
  mutate(line = st_sfc(st_linestring(matrix(c(georgia_lon, georgia_lat, state_lon, state_lat), ncol = 2)))) |>
  ungroup() |>
  st_as_sf(crs = st_crs(us_states))

## now we can trim the lines to the geometry of the states
trimmed_lines <- st_intersection(lines, us_states)
### this did not work, there are 0 rows
## we can try to do this manually by creating a line from the centroid of georgia to the centroid of each state, and then trimming it to the geometry of the state
trimmed_lines_manual <- lines |>
  rowwise() |>
  mutate(trimmed_line = st_intersection(line, us_states$geometry[which(us_states$postal == state_centroids$postal)])) |>
  ungroup() |>
  st_as_sf(crs = st_crs(us_states))
