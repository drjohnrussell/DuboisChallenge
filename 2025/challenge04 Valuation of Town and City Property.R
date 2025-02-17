library(tidyverse)
library(showtext)
options(scipen = 999)

font_add_google("Play", family = "dubois")
showtext_auto()

data <- read_csv("https://github.com/ajstarks/dubois-data-portraits/raw/refs/heads/master/challenge/2025/challenge04/data.csv")

data_spline <- as.data.frame(spline(data$Year, data$`Property Valuation`,n=200)) |> 
  rename(Year=x,`Property Valuation`=y)


p <- data_spline |> 
  ggplot(aes(x = Year, 
             y = `Property Valuation`, 
             color = (Year > 1874 & Year < 1899))) +
  geom_rect(aes(xmin=1865,xmax=1870,ymin=0,ymax=4800000),fill="#E6D4C3",color=NA) +
  geom_rect(aes(xmin=1860,xmax=1900,ymin=4800000,ymax=5000000),fill="#E6D4C3",color="#E6D4C3") +
  geom_rect(aes(xmin=1860,xmax=1865,ymin=0,ymax=4800000),fill=NA,color="black") +
  geom_rect(aes(xmin=1870,xmax=1900,ymin=0,ymax=4800000),fill=NA,color="black") +
  annotate("segment",x=1865,xend=1900,y=1000000,yend=1000000,color="#dc143c",linewidth=0.005) +
  annotate("segment",x=1865,xend=1900,y=2000000,yend=2000000,color="#dc143c",linewidth=0.005) +
  annotate("segment",x=1865,xend=1900,y=3000000,yend=3000000,color="#dc143c",linewidth=0.005) +
  annotate("segment",x=1865,xend=1900,y=4000000,yend=4000000,color="#dc143c",linewidth=0.005) +
  annotate("text",x=1862,y=400000,label="$",size=11) +
  annotate("text",x=1862,y=600000,label="$",size=11) +
  annotate("text",x=1862,y=1400000,label="$",size=11) +
  annotate("text",x=1862,y=1600000,label="$",size=11) +
  annotate("text",x=1862,y=2400000,label="$",size=11) +
  annotate("text",x=1862,y=2600000,label="$",size=11) +
  annotate("text",x=1862,y=3400000,label="$",size=11) +
  annotate("text",x=1862,y=3600000,label="$",size=11) +
  annotate("text",x=1862,y=4300000,label="$",size=11) +
  annotate("text",x=1862,y=4500000,label="$",size=11) +
  annotate("text",x=1862.5,y=4750000,label="DOLLARS",size=11) +
  annotate("text",x=1872,y=500000,angle=90,label="KU-KLUXISM",size=11) +
  annotate("text",x=1875,y=2300000,label="POLITICAL\n      UNREST",lineheight=.8,size=11,hjust=0) +
  annotate("text",x=1892.5,y=1550000,label="LYNCHING",size=11) +
  annotate("text",x=1894,y=500000,label="FINANCIAL PANIC",angle=90,size=11) +
  annotate("text",x=1894,y=2340000,label="DISFRANCHISEMENT \n  AND \n  PROSCRIPTIVE \n  LAWS",lineheight=.8,size=11,hjust=0.5) +
  annotate("text",x=1881.5,y=4050000,label="RISE OF \n    THE NEW \n       INDUSTRIALISM",lineheight=.8,size=11,hjust=0) +
  geom_line(color="black",size=3.5) +
  geom_line(aes(group=1),size=3,show.legend=FALSE) +
  labs(title=str_to_upper("valuation of town and city property owned \n by georgia negroes.")) +
  scale_color_manual(values=c("white", "black")) +
  scale_x_continuous(breaks = seq(1870, 1900, by = 5),
                     limits=c(1860,1900),
                     minor_breaks = seq(1870, 1899, by = 1),
                     expand=expansion(mult=c(0,0))) +
  scale_y_continuous(breaks = seq(1000000, 4000000, by = 1000000),
                     limits=c(0,5000000),
                     minor_breaks = seq(100000, 4800000, by = 100000),
                     expand=c(0,0)) +
  theme(text = element_text(family = "dubois", size = 20, lineheight = .5),
        axis.text.y = element_text(hjust = 0,
                                   margin = margin(0, -5, 0, 0, 'cm'),
                                   color="black",
                                   size=25),
        axis.text.x= element_text(size=30),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.title = element_text(size = 40, hjust=.5,lineheight=.8),
        panel.grid=element_line(color="#dc143c",linewidth=0.005),
        panel.grid.major.x = element_line(color="#dc143c",size=0.005),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "#E6D4C3", color = NA),
        panel.background = element_rect(fill = "#E6D4C3"),
        plot.margin = margin(1, 1.5, 1, 1.5, "in"))

ggsave(plot=p,filename="2025/challenge04.pdf",width=22,height=28,units="in",dpi=600,bg="#E6D4C3")

        