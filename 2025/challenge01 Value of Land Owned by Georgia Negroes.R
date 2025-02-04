library(tidyverse)
library(showtext)
library(ggimage)

font_add_google("Play", family = "dubois")
showtext_auto()

data <- read_csv("https://github.com/ajstarks/dubois-data-portraits/raw/refs/heads/master/challenge/2025/challenge01/data.csv") |> 
  mutate(image="2025/images/rawpixel.svg",
         image2="2025/images/rawpixel.jpg",
         position=`Land Value (Dollars)`^(1/3))

library(ggimg)
p <- ggplot(data, mapping=aes(img=image2)) +
  geom_rect_img(mapping=aes(xmin=0-position/5,
                            xmax=0+position/5,
                            ymin=0,
                            ymax=200)) +
  geom_text(mapping=aes(label=paste0("$",`Land Value (Dollars)`)),y=75,x=0,size=3,family="dubois") +
  scale_y_continuous(limits=c(0,200)) +
  scale_x_continuous(limits=c(-100,100)) +
  theme_void() +
  labs(title=str_to_upper("value of land owned by georgia negroes")) +
  theme(text = element_text(family = "dubois", size = 20, lineheight = .5),
        panel.background = element_rect(fill = NA, color = NA),
        panel.ontop = TRUE,
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.justification = "top",
        legend.text = element_text(size = 0),
        legend.key.size = unit(0, "cm"),
        plot.title = element_text(size = 15, hjust=.5),
        plot.background = element_rect(fill = "#E6D4C3", color = NA),
        plot.margin = margin(0.2, .3, 0.1, .3, "cm"),
        plot.caption = element_text(size = 10), 
        plot.caption.position = "plot",
        strip.text = element_text(size = 15, hjust = 0.5)) +
  facet_wrap(~Year,ncol=1,scales="free",strip.position="bottom")

ggsave(plot=p,filename="2025/challenge01.pdf",width=5.5,height=8,units="in",dpi=600,bg="#E6D4C3")
