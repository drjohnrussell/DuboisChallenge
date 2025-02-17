library(tidyverse)
library(ggforce)
library(showtext)

# load data
data <- read_csv("https://github.com/ajstarks/dubois-data-portraits/raw/refs/heads/master/challenge/2025/challenge05/data.csv") |>
  distinct() |> 
  mutate(label = paste0("$", format(`Valuation (Dollars)`,big.mark=",", trim=TRUE)))

# load fonts
font_add_google(name = "Play", family = "dubois")
showtext_auto()

# prep data
circles <- tibble(x = rep(0, 6), 
                  y = rep(0, 6), 
                  r = c(20, 19, 18, 11, 7, 6), 
                  fill = as.factor(1:6))

year_labels <- tibble(x = rep(0, 6), 
                      y = -1*c(20, 19, 18, 11, 7, 6)+0.5, 
                      label = rev(data$Year), 
                      colour = c(rep("black", 5), "white"))

# plot
p <- ggplot() +
  # circles
  geom_circle(data = circles, 
              mapping = aes(x0 = x, y0 = y, r = r, fill = fill), 
              colour = "black",linewidth=.1,show.legend=FALSE) +
  scale_fill_manual(values = c("#c11332", "#dbc8b0", "#efad00", 
                               "#3a5288", "#b28e73", "#131313")) +
  coord_fixed() +
  # cutout 1
  geom_arc_bar(aes(x0 = -1.6, y0 = 1.6, r0 = 3.9, r = 0, start = 5.33, end = 5.95), 
               fill = "#b28e73", colour = "#b28e73") +
  geom_arc_bar(aes(x0 = -1.7, y0 = 1.7, r0 = 4.7, r = 0, start = 5.38, end = 5.92), 
               fill = "#3a5288", colour = "#3a5288") +
  geom_arc_bar(aes(x0 = -1.8, y0 = 1.8, r0 = 8.7, r = 0, start = 5.43, end = 5.87), 
               fill = "#efad00", colour = "#efad00") +
  geom_arc_bar(aes(x0 = -1.9, y0 = 1.9, r0 = 15.7, r = 0, start = 5.47, end = 5.83), 
               fill = "#dbc8b0", colour = "#dbc8b0") +
  geom_arc_bar(aes(x0 = -2, y0 = 2, r0 = 16.6, r = 0, start = 5.5, end = 5.8), 
               fill = "#c11332", colour = "#c11332") +
  # cutout 2
  geom_arc_bar(aes(x0 = 1.7, y0 = 1.7, r0 = 3.9, r = 0, start = 0.36, end = 0.94), 
               fill = "#b28e73", colour = "#b28e73") +
  geom_arc_bar(aes(x0 = 1.8, y0 = 1.8, r0 = 4.7, r = 0, start = 0.41, end = 0.89), 
               fill = "#3a5288", colour = "#3a5288") +
  geom_arc_bar(aes(x0 = 1.9, y0 = 1.9, r0 = 8.7, r = 0, start = 0.46, end = 0.84), 
               fill = "#efad00", colour = "#efad00") +
  geom_arc_bar(aes(x0 = 2, y0 = 2, r0 = 15.7, r = 0, start = 0.5, end = 0.8), 
               fill = "#dbc8b0", colour = "#dbc8b0") +
  # cutout 3
  geom_arc_bar(aes(x0 = 2.7, y0 = 0, r0 = 3.9, r = 0, start = 1.32, end = 1.78), 
               fill = "#b28e73", colour = "#b28e73") +
  geom_arc_bar(aes(x0 = 2.85, y0 = 0, r0 = 4.7, r = 0, start = 1.36, end = 1.74), 
               fill = "#3a5288", colour = "#3a5288") +
  geom_arc_bar(aes(x0 = 3, y0 = 0, r0 = 13.7, r = 0, start = 1.4, end = 1.7), 
               fill = "#efad00", colour = "#efad00") +
  # cutout 4
  geom_arc_bar(aes(x0 = 1.9, y0 = -1.9, r0 = 3.9, r = 0, start = 2.07, end = 2.73), 
               fill = "#b28e73", colour = "#b28e73") +
  geom_arc_bar(aes(x0 = 2, y0 = -2, r0 = 7.7, r = 0, start = 2.1, end = 2.7), 
               fill = "#3a5288", colour = "#3a5288") +
  # cutout 5
  geom_arc_bar(aes(x0 = -2, y0 = -2, r0 = 3.3, r = 0, start = 3.8, end = 4.4), 
               fill = "#b28e73", colour = "#b28e73") +
  # year text
  geom_text(data = year_labels, 
            mapping = aes(x = x, y = y, label = label, colour = I(colour)),
            family = "dubois") +
  # valuation text
  annotate("text", x=0,y=0, label="$5,393,885",colour="white",family="dubois",size=2) +
  annotate("text",x=-4.1,y=-3.4,label="$5,764,293",colour="black",family="dubois",angle=30,size=2) +
  annotate("text",x=5,y=-5.4,label="$8,153,390",colour="white",family="dubois",angle=-47,size=2) +
  annotate("text",x=10,y=0,label="$12,322,003",colour="black",family="dubois",angle=0,size=2) +
  annotate("text",x=8,y=10,label="$12,941,230",colour="black",family="dubois",angle=52,size=2) +
  annotate("text",x=-8,y=10,label="$13,447,423",colour="black",family="dubois",angle=-55,size=2) +
  labs(title = "ASSESSED VALUATION OF ALL TAXABLE PROPERTY\nOWNED BY GEORGIA NEGROES. ") +
  theme(plot.background = element_rect(fill = "#dac8b8", colour="#dac8b8"),
        panel.background = element_rect(fill = "#dac8b8", colour="#dac8b8"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none",
        plot.title = element_text(family = "space", face = "bold", 
                                  hjust = 0.5, size = 24, 
                                  lineheight = 0.3, 
                                  margin = margin(b = 30)),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.1, 0.8, 1.3, 0.8), "cm")) 
p

ggsave(p, filename = "2025/challenge_05.pdf", width=22,height=28,units="in",dpi=600,bg="#E6D4C3")
