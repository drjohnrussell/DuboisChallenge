library(tidyverse)
library(showtext)
library(ggimage)

font_add_google("Play", family = "dubois")
showtext_auto()

data <- read_csv("https://github.com/ajstarks/dubois-data-portraits/raw/refs/heads/master/challenge/2025/challenge02/data.csv") |> 
  mutate(Date=as.factor(Date),
         color=case_when(Date %in% c("1874","1899") ~ "black",
                         .default="transparent"))

p <- ggplot(data,mapping=aes(x=fct_rev(Date))) +
  geom_col(mapping=aes(y=Land),fill="#cf334e",width=.7) +
  geom_text(mapping=aes(label=Land,y=Land,color=color),position=position_stack(vjust=.5),size=5,show.legend=FALSE) +
  scale_color_manual(values=c("black","#cf334e"))+
  coord_flip() +
  theme_void() +
  theme(text = element_text(family = "dubois", size = 20, lineheight = .5),
        axis.text.y=element_text(size=15,color="#636363"),
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
  labs(title=str_to_upper("acres of land owned by negroes \n in georgia."))
