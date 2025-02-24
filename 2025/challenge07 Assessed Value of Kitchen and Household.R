library(tidyverse)
library(showtext)
library(ggborderline)
options(scipen = 999)

font_add_google("Play", family = "dubois")
showtext_auto()

data <- read_csv("https://github.com/ajstarks/dubois-data-portraits/raw/refs/heads/master/challenge/2025/challenge07/data.csv") |> 
  mutate(Year = as.factor(Year))

datastart <- data |> 
  mutate(
    rstart=4-.1*as.numeric(Year)
  )

dataend <- data |> 
  mutate(
    x=1.875*(2*pi)*`Houshold Value (Dollars)`/1434975,
    rend=4-.1*as.numeric(Year)-x/(2*pi)
  )

## slope is -1/(2pi)

data2 <- left_join(datastart,dataend)

data1875 <- data.frame(theta=seq(0,0.1739345,.01)) |> 
  mutate(r=3.9-(1/(2*pi)*theta),
         x=r*cos(pi/2-theta),
         y=r*sin(pi/2-theta),
         Year=1875)

data1880 <- data.frame(theta=seq(0,2.8928879,.01)) |> 
  mutate(r=3.8-(1/(2*pi)*theta),
         x=r*cos(pi/2-theta),
         y=r*sin(pi/2-theta),
         Year=1880)

data1885 <- data.frame(theta=seq(0,4.0438673,.01)) |> 
  mutate(r=3.7-(1/(2*pi)*theta),
         x=r*cos(pi/2-theta),
         y=r*sin(pi/2-theta),
         Year=1885)

data1890 <- data.frame(theta=seq(0,7.5353121,.01)) |> 
  mutate(r=3.6-(1/(2*pi)*theta),
         x=r*cos(pi/2-theta),
         y=r*sin(pi/2-theta),
         Year=1890)

data1895 <- data.frame(theta=seq(0,9.6353121,.01)) |>
  mutate(r=3.5-(1/(2*pi)*theta),
         x=r*cos(pi/2-theta),
         y=r*sin(pi/2-theta),
         Year=1895)

data1900 <- data.frame(theta=seq(0,11.7809725,.01)) |>
  mutate(r=3.4-(1/(2*pi)*theta),
         x=r*cos(pi/2-theta),
         y=r*sin(pi/2-theta),
         Year=1900)

data3 <- bind_rows(data1875,data1880,data1885,data1890,data1895,data1900)

p <- data3 |> 
  mutate(Year=as.factor(Year)) |> 
  ggplot() +
  geom_borderpath(mapping=aes(x=x,y=y,group=Year,color=Year),
                  linewidth=10,bordercolour="black",borderwidth=.1,
                  show.legend=FALSE) +
  annotate("text",x=-.05,y=3.9,label="1875 -----   $21,186",hjust=1,family="dubois",size=10) +
  annotate("text",x=-.05,y=3.8,label="1880 ---   $ 498,532",hjust=1,family="dubois",size=10) +
  annotate("text",x=-.05,y=3.7,label='1885 ---   "  736,170',hjust=1,family="dubois",size=10) +
  annotate("text",x=-.05,y=3.6,label='1890 ---" 1,173,624',hjust=1,family="dubois",size=10) +
  annotate("text",x=-.05,y=3.5,label='1895 ---" 1,322,694',hjust=1,family="dubois",size=10) +
  annotate("text",x=-.05,y=3.4,label='1900 ---" 1,434,975',hjust=1,family="dubois",size=10) +
  theme_void() +
  labs(title=str_to_upper("assessed value of household and kitchen furniture\n owned by georgia negroes.")) +
  coord_fixed() +
  scale_color_manual(values=c("#ffc0cb","#4682b4","#654321","#ffd700","#d2b48c","#dc143c"))+
  theme(legend.position="none",
        text = element_text(family = "dubois", size = 20, lineheight = .5),
        panel.background = element_rect(fill = NA, color = NA),
        panel.ontop = TRUE,
        plot.title = element_text(size = 45, hjust=.5,lineheight=.8,margin=margin(0.2,0,1,0,"cm")),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#E6D4C3", color = NA),
        plot.margin = margin(1, 1, 0.1, 1, "cm"))


ggsave(plot=p,filename="2025/challenge07.pdf",width=22,height=28,units="in",dpi=600,bg="#E6D4C3")

