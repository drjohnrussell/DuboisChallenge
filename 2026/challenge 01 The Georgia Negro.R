library(tidyverse)
library(showtext)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggforce)
library(cowplot)
library(rnaturalearthhires)

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




## load the data from csv and hiresearth
routepairs <- read_csv("2026/data/challenge01 route-pairs.csv")
routes <- read_csv("2026/data/challenge01 routes.csv")

world <- ne_countries(scale = "small", returnclass = "sf")

usa <- ne_states(iso_a2 = 'us', returnclass = "sf")
brazil <- ne_states(country = 'brazil', returnclass = "sf")
mexico <- ne_states(country='mexico', returnclass = "sf")

amer <- world |> 
  filter(region_un=="Americas") |> 
  mutate(color=case_when(name %in% c("Cuba", "Bahamas", "Haiti", "Dominican Rep.", "Jamaica") ~ "black",
                         name %in% c("Guyana", "Suriname", "Venezuela", "Puerto Rico", "Belize",
                          "Honduras", "Nicaragua","Costa Rica","Panama","Colombia") ~ "brown",
                         .default = "tan"))

usa <- usa |> 
  mutate(color=case_when(name %in% c("Florida", "Georgia", "Alabama", "Mississippi", "South Carolina","Louisiana") ~ "black",
                         name %in% c("Tennessee", "North Carolina", "Arkansas", 
                                      "Virginia", "Kentucky", "Texas","Oklahoma") ~ "brown",
                         .default="tan"))

brazil <- brazil |> 
  mutate(color=case_when(name %in% c("Ceará", "Bahia", "Paraíba", "Piauí", 
                                    "Rio Grande do Norte", "Pernambuco", "Sergipe", "Alagoas", 
                                    "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "Goiás", "São Paulo") ~ "black",
                         name %in% c("Maranhão", "Tocantins", "Paraná", "Mato Grosso do Sul",
                                      "Distrito Federal", "Pará", "Amapá", "Mato Grosso") ~ "brown",
                         .default="tan"))

mexico <- mexico |> 
  mutate(color=case_when(name %in% c("Coahuila", "Nuevo León", "Tamaulipas", "San Luis Potosí",
                                     "Veracruz","Puebla","Tlaxcala","Hidalgo","Tabasco","Campeche",
                                     "Yucatán", "Quintana Roo") ~ "brown",
                          .default="tan"))



westmap <- ggplot() +
  geom_circle(aes(x0 = 1821000, y0 =3910000, r = 8160900), color="black", fill=background) + 
  geom_sf(data = amer, aes(fill=color), linewidth=NA) +
  geom_sf(data = brazil, aes(fill=color), linewidth=NA) +
  geom_sf(data = usa, aes(fill=color), linewidth=NA) +
  geom_sf(data=mexico, aes(fill=color), linewidth=NA) +
  geom_point(data=data.frame(x = 2121000, y =5580000), aes(x=x, y=y), pch=19, size=1, col="white") +
  coord_sf(crs = "+proj=laea +lat_0=10 +lon_0=-60 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ",
           xlim=c(-2339981, 12803058)-3400000, ylim=c(-5095286, 9970916)+1500000) +
  scale_fill_manual(values=c("black", brown, tan)) +
  theme_void() +
  theme(legend.position="none", 
        panel.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = lighttan, color = NA))

westmap

afr_eur <- world |> 
  filter_out(name %in% c("Indonesia", "Australia", "Canada", 
                                      "Greenland", "United States", "Cuba",
                                      "Papua New Guinea", "Fr. S. Antarctic Lands") |
             region_un %in% c("Americas", "Antarctica")) |> 
  mutate(color=case_when(name %in% c("Lesotho", "South Africa", "Algeria", "Tunisia", "Libya", 
                                       "Egypt", "Morocco", "Western Sahara", "W. Sahara", "Mauritania", 
                                       "Mali", "Niger", "Chad", "Sudan") ~ "brown",
                          name == "Madagascar" ~ "tan",
                          region_un=="Africa" ~ "black",
                         .default="tan"))
  

eastmap <- ggplot() +
  geom_circle(aes(x0 = 5221000, y0 =2510000, r = 8160900), colour="black", fill=background) +
  geom_sf(data = afr_eur, aes(fill=color)) +
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=45 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ",
           xlim=c(-2339981, 12803058), ylim=c(-5095286, 9970916)) +
  scale_fill_manual(values=c("black", brown, tan)) +
  theme_void() +
  theme(legend.position="none", 
        panel.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = lighttan, color = NA))


p <- grid.arrange(westmap, eastmap, nrow = 1, ncol=2)
p


library(ggview)
final <- ggdraw(p, xlim=c(-.05,1.05)) +
  theme(plot.background = element_rect(fill=lighttan, color = lighttan)) +
  draw_label("THE GEORGIA NEGRO .", x = 0.5, y = 0.92, hjust = 0.5, vjust = 0.5, 
             fontfamily = "dubois", fontface="bold", color = "black", size = 40) +
  draw_label("A SOCIAL STUDY\nBY\nW.E.BURGHARDT DU BOIS.", x = 0.5, y = 0.86, hjust = 0.5, vjust = 0.5, 
           fontfamily = "dubois", color = "black", size = 22, lineheight=1.5) +
  draw_label("ROUTES OF THE AFRICAN SLAVE TRADE", x = 0.3, y = 0.25, hjust = 0, vjust = 0.5, 
             fontfamily = "dubois", color = "black", size = 22) +
  draw_label("__", x = 0.24, y = 0.26, hjust = 0, vjust = 0.5, 
             fontfamily = "dubois", color = "black", size = 22) +
  draw_label("THE STATE OF GEORGIA", x = 0.3, y = 0.22, hjust = 0, vjust = 0.5, 
             fontfamily = "dubois", color = "black", size = 22) +
  draw_label("THIS CASE IS DEVOTED TO A SERIES OF CHARTS., MAPS AND OTHER DEVI-\nCES DESIGNED TO ILLUSTRATE THE DEVELOPMENT OF THE AMERICAN NEGRO IN A\nSINGLE TYPICAL STATE OF THE UNITED STATES.\n\nTHE PROBLEM OF THE 20TH CENTURY IS THE PROBLEM OF THE\nCOLOR LINE.", 
             x = 0.5, y = 0.1, hjust = 0.5, vjust = 0.5, fontfamily = "dubois", color = "black", alpha=.7, size = 20, lineheight=1.5) +
  draw_label("DISTRIBUTION OF", x=0.425, y=0.63, angle=-45, fontfamily="dubois", color="black", size=16)+
  draw_label("THE NEGRO RACE", x=0.575, y=0.63, angle=45, color="black", fontfamily="dubois", size=16) +
  draw_line(x=c(0.39, 0.62), y=c(0.43, 0.38), color="black", size=0.5) +
  draw_line(x=c(0.26, 0.62), y=c(0.53, 0.45), color="black", size=0.5) +
  draw_line(x=c(0.27, 0.62), y=c(0.51, 0.45), color="black", size=0.5) +
  draw_line(x=c(0.27, 0.62), y=c(0.54, 0.45), color="black", size=0.5)

final + canvas(width=22, height=28)

ggsave(filename="2026/final/challenge01.png",plot=final, width=22, height=28, units="in", bg=lighttan)
