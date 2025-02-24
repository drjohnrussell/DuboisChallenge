library(tidyverse)
library(showtext)
library(ggbrace)
?options(scipen = 999)

font_add_google("Play", family = "dubois")
showtext_auto()

data <- read_csv("https://github.com/ajstarks/dubois-data-portraits/raw/refs/heads/master/challenge/2025/challenge06/data.csv") |> 
  mutate(Year=case_when(Year==1898 ~ 1899,
         .default=Year))

datahor <- data |> 
  select(City,Year,Owners) |> 
  mutate(xmin=1879,
         xmax=(Owners/2157)*22.2+1879,
         ymin=case_when(City=="Savannah" ~ Year-.6,
                        .default=Year),
         ymax=case_when(City=="Savannah" ~ Year,
                        .default=Year+.6),
         label=paste0(format(Owners,big.mark=",")),
         labelx=(xmin+xmax)/2,
         labely=(ymin+ymax)/2,
         angle=0)

dataver <- data |> 
  select(City,Year,`Property Value (Dollars)`) |> 
  mutate(xmin=case_when(City=="Atlanta" ~ Year,
                        .default=Year-.6),
         xmax=case_when(City=="Atlanta" ~ Year+.6,
                        .default=Year),
         ymax=1903,
         ymin=1902.5-((`Property Value (Dollars)`/1308995)*21),
         label=paste0("$",format(`Property Value (Dollars)`,big.mark=",")),
         labelx=(xmin+xmax)/2,
         labely=case_when(City=="Savannah" & Year==1890 ~ (ymin+ymax)/2-1.3,
                          .default=(ymin+ymax)/2),
         angle=90)

datayears <- bind_rows(
  data |> 
    select(Year) |> 
    mutate(label=paste0(Year),
           labelx=Year,
           labely=1904,
           hjust=0.5),
  data |> 
    select(Year) |> 
    mutate(label=paste0(Year),
           labely=Year,
           labelx=1878.5,
           hjust=1))
  

data2 <- bind_rows(datahor |> 
                     filter(Year!=1899),
                   dataver |> 
                     filter(Year!=1880),
                   datahor |> 
                     filter(Year==1899),
                   dataver |> 
                     filter(Year==1880))

bracket1 <- data.frame(x=c(1878.5, 1879.1), y=c(1879.2, 1880.8))
bracket2 <- data.frame(x=c(1878.5, 1879.1),y=c(1889.2,1890.8))
bracket3 <- data.frame(x=c(1878.5, 1879.1),y=c(1898.2,1899.8))
bracket4 <- data.frame(x=c(1876.7, 1877.2),y=c(1899.5,1879.5))
bracket5 <- data.frame(x=c(1879.3,1899.7),y=c(1904.2,1905.1))

p <- data2 |> 
  mutate(City=str_to_upper(City),
         City=fct_relevel(City,"SAVANNAH")) |>
  ggplot() +
  geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=City),color="black") +
  geom_text(aes(x=labelx,y=labely,label=label,angle=angle),family="dubois",size=11) +
  geom_text(data=datayears,aes(x=labelx,y=labely,label=label,hjust=hjust),family="dubois", size=11) +
  scale_y_reverse() +
  annotate("text",x=1890,y=1905.5,label="PROPERTY",family="dubois", size=12) +
  annotate("text",y=1890,x=1875.3,label="OWNERS",family="dubois", size=12) +
  theme_void() +
  scale_fill_manual(values=c("ATLANTA"="#add8e6","SAVANNAH"="#ffd700")) +
  stat_brace(data=bracket1,aes(x=x,y=y),outside=FALSE,rotate=270) +
  stat_brace(data=bracket2,aes(x=x,y=y),outside=FALSE,rotate=270) +
  stat_brace(data=bracket3,aes(x=x,y=y),outside=FALSE,rotate=270) +
  stat_brace(data=bracket4,aes(x=x,y=y),outside=FALSE,rotate=270,mid=.47) +
  stat_brace(data=bracket5,aes(x=x,y=y),outside=FALSE,rotate=180,mid=.53) +
  # hand make a legend
  annotate("rect",xmin=1879.8,xmax=1881.8,ymin=1906,ymax=1907,fill="#ffd700",color="black") +
  annotate("text",x=1881.9,y=1906.5,hjust=0,label="=SAVANNAH",family="dubois",size=12) +
  annotate("rect",xmin=1897.5,xmax=1899.5,ymin=1906,ymax=1907,fill="#add8e6",color="black") +
  annotate("text",x=1897.3,y=1906.5,hjust=1,label="ATLANTA=",family="dubois",size=12)+
  labs(title=str_to_upper("Negro Property in Two Cities\n  of Georgia.")) +
  theme(legend.position="none",
        text = element_text(family = "dubois", size = 20, lineheight = .5),
        panel.background = element_rect(fill = NA, color = NA),
        panel.ontop = TRUE,
        plot.title = element_text(size = 45, hjust=.5,lineheight=.8,margin=margin(0.2,0,1,0,"cm")),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#E6D4C3", color = NA),
        plot.margin = margin(1, 1, 0.1, 1, "cm"))

ggsave(plot=p,filename="2025/challenge06.pdf",width=22,height=28,units="in",dpi=600,bg="#E6D4C3")

