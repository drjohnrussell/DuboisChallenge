library(tidyverse)
library(showtext)
library(sf)

font_add_google("Play", family = "dubois")
showtext_auto()

data <- read_csv("2025/data/challenge03.csv")
map <- read_sf("2025/data/georgia-1880-county-shapefile") |> 
  mutate(NHGISNAM=str_to_title(NHGISNAM)) |> 
  left_join(data,
            by=c("NHGISNAM"="County1890")) |> 
  arrange(NHGISNAM)

cities <- data.frame(city=c("Atlanta", "Columbus", "Macon", "Augusta", "Savannah"),
                     lat=c(33.7501275, 32.4609764, 32.8406946, 33.4687314, 32.0808989),
                     lon=c(-84.3885209, -84.9877094, -83.6324022, -82.0283188, -81.091203),
                     hjust=c(1,0,1,1,1),
                     lonadjust=c(-.02,.02,-.02,-.02,-.02),
                     latadjust=c(-0.02,0,0,-0.02,0))

## First map to figure out colors

map |> 
  ggplot() +
  geom_sf(mapping=aes(geometry=geometry)) +
  geom_sf_text(mapping=aes(label=NHGISNAM)) +
  coord_sf(crs=sf::st_crs(4326))

## Now map with colors

brown <- "#d2b48c"
crimson <- "#dc143c"
gold <- "#ffd700"
green <- "#006400"
lightblue <- "#add8e6"
pink <- "#ffc0cb"
tan <- "#EFEACE"
background <- "#E6D4C3"
  
p <- map |> 
  ggplot() +
  geom_sf(mapping=aes(geometry=geometry,fill=color),show.legend=FALSE) +
  geom_sf_text(mapping=aes(label=`Acres 1899`),size=20,family="dubois") +
  geom_point(data=cities,aes(x=lon,y=lat),color="black",size=4,shape=21, fill=NA,alpha=.6) +
  geom_point(data=cities,aes(x=lon,y=lat),color="black",size=3,alpha=.6) +
  geom_text(data=cities,aes(x=lon+lonadjust,y=lat+latadjust,label=city,hjust=hjust),size=22,family="dubois") +
  coord_sf(crs=sf::st_crs(4326)) +
  scale_fill_manual(values=c(brown,crimson,gold,green,lightblue,pink,tan)) +
  annotate("text",x=-81.5,y=34.5,
           label=str_to_upper("The figures indicate the number of \n acres owned in each county in 1899"),
           family="dubois",size=30,lineheight=.5) +
  theme_void() +
  theme(text = element_text(family = "dubois", size = 80, lineheight = .5),
        axis.text=element_blank(),
        panel.background = element_rect(fill = NA, color = NA),
        panel.ontop = TRUE,
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 150, hjust=.5,lineheight=.8,margin=margin(0,0,3,0,"cm")),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#E6D4C3", color = NA),
        plot.margin = margin(0.2, 1, 0.1, 1, "cm")) +
  labs(title=str_to_upper("land owned by black americans in georgia, u.s.a  1870-1900."))

library(ggview)

p + canvas(width=22, height=28, units="in", bg=background)

ggsave("2026/final/challenge06.png",p,
       width=22, height=28, units="in", bg=background)

## now do it as a gradient instead

### first find the area of each geometry

map <- map |> 
  mutate(area=as.numeric(st_area(geometry))) |> 
  ## convert square meters to acres
  mutate(area=area*0.000247105) |> 
  ## now make a new variable that is the number of acres owned by black americans divided by the total area of the county
  mutate(share=`Acres 1899`/area * 100)

## this still looks off - I think the projection area is not in square meters, but it's not too far off

### let's make a continuous palette that goes from the background color to the crimson color
gradient <- colorRampPalette(c(background, crimson))

## now a new map, same as before but with the fill color determined by the share variable, and using the gradient palette
p2 <- map |> 
  ggplot() +
  geom_sf(mapping=aes(geometry=geometry,fill=share),show.legend=FALSE) +
  geom_point(data=cities,aes(x=lon,y=lat),color="black",size=4,shape=21, fill=NA,alpha=.6) +
  geom_point(data=cities,aes(x=lon,y=lat),color="black",size=3,alpha=.6) +
  geom_text(data=cities,aes(x=lon+lonadjust,y=lat+latadjust,label=city,hjust=hjust),size=22,family="dubois") +
  coord_sf(crs=sf::st_crs(4326)) +
  scale_fill_gradientn(colors = gradient(100)) +
  annotate("text",x=-81.5,y=34.5,
           label=str_to_upper("The color indicates the percentage \nof land owned by Black Americans in\n each county in 1899"),
           family="dubois",size=30,lineheight=.5) +
  theme_void() +
  theme(text = element_text(family = "dubois", size = 80, lineheight = .5),
        axis.text=element_blank(),
        panel.background = element_rect(fill = NA, color = NA),
        panel.ontop = TRUE,
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 150, hjust=.5,lineheight=.8,margin=margin(0,0,3,0,"cm")),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#E6D4C3", color = NA),
        plot.margin = margin(0.2, 1, 0.1, 1, "cm")) +
  labs(title=str_to_upper("land owned by black americans in georgia, u.s.a  1870-1900."))

p2 + canvas(width=22, height=28, units="in", bg=background)

ggsave("2026/final/challenge06_gradient.png",p2,
       width=22, height=28, units="in", bg=background)
