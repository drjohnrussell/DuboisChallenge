library(tidyverse)
library(showtext)
library(ggbrace)
options(scipen = 999)

font_add_google("Play", family = "dubois")
showtext_auto()
showtext_opts(dpi = 600)

data <- read_csv("https://github.com/ajstarks/dubois-data-portraits/raw/refs/heads/master/challenge/2025/challenge08/data.csv") |> 
  mutate(Occupation=str_wrap(Occupation, width=15,exdent=4),
         Occupation=fct_inorder(Occupation))

length1 <- data |> 
  filter(Occupation=="AGRICULTURAL\n    LABORERS") |> 
  pull(Count) - 63012

length2 <- data |> 
  filter(as.numeric(Occupation) > 7) |> 
  summarise(sum=sum(Count)) |> 
  pull(sum)

bracket1 <- data.frame(x=c(.6, 15.5), y=c(1500, 4500))

p <- data |> 
  mutate(Count2=ifelse(Occupation=="AGRICULTURAL\n    LABORERS", 63012, Count)) |>
  ggplot(aes(x=fct_rev(Occupation), y=Count2)) +
  geom_col(width=.3,fill="#dc143c") +
  geom_rect(aes(xmin=21.4,xmax=21.7,ymin=length1,ymax=63012),fill="#dc143c") +
  annotate("curve",x=21.55,y=62820,xend=22,yend=62820,curvature=1,linewidth=12.4,color="#dc143c") +
  annotate("rect",ymin=5000,ymax=5000+length2,xmin=6.7,xmax=7,fill="#dc143c") +
  stat_brace(data=bracket1,aes(x=x,y=y),outside=FALSE,rotate=0,bending=0.3,mid=.42) +
  annotate("text",x=11,y=30000,label="1890.",size=16,family="dubois") +
  geom_text(aes(x=fct_rev(Occupation),hjust=1,y=-300,label=format(Count,big.mark=",")),size=10,family="dubois") +
  scale_y_continuous(expand=expansion(c(.08,.03))) +
  scale_x_discrete(expand=expansion(c(0.01,.06))) +
  coord_flip() +
  theme_void() +
  annotate("text",x=23,y=30000,label="MALES OVER 10",size=11) +
  labs(title=str_to_upper("Occupations of Georgia Negroes.")) +
  theme(text = element_text(family = "dubois", size = 20, lineheight = .5),
        axis.text.y=element_text(size=24,family="dubois",hjust=0,lineheight = 1),
        panel.background = element_rect(fill = NA, color = NA),
        panel.ontop = TRUE,
        plot.title = element_text(size = 45, hjust=.5,lineheight=.8,margin=margin(0.2,0,0,0,"cm")),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#E6D4C3", color = NA),
        plot.margin = margin(0, 1, 0.1, 1, "cm"))

ggsave(plot=p,filename="2025/final/challenge08.pdf",width=22,height=28,units="in",dpi=600,bg="#E6D4C3")

library(magick)
ggsave(plot=p,filename="2025/final/challenge08.png",width=22, height=28, units="in",dpi=600,bg="#E6D4C3")
image <- image_read("2025/final/challenge08.png")
image2 <- image_resize(image,"812x1024")
image_write(image2, path = "2025/final/challenge08.png", format = "png")

  