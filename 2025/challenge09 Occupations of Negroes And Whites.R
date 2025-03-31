library(tidyverse)
library(showtext)
library(ggforce)
options(scipen = 999)

font_add_google("Play", family = "dubois")
showtext_auto()
showtext_opts(dpi = 600)

data <- read_csv("https://github.com/ajstarks/dubois-data-portraits/raw/refs/heads/master/challenge/2025/challenge09/data.csv") |> 
  mutate(Occupation=factor(Occupation, levels=c("Agriculture, Fisheries and Mining", 
                                                "Domestic and Personal Service", 
                                                "Manufacturing and Mechanical Industries",
                                                "Trade and Transportation",
                                                "Professions")))

circles <- data.frame(Occupation=levels(data$Occupation),
                      x=c(-.7,.7,-.7,.7,.7),
                      y=c(.15,.25,-.15,-.25,0))

text <- data.frame(Occupation=levels(data$Occupation),
                   x=c(-.63,.63,-.63,.63,.63),
                   y=c(.15,.25,-.15,-.25,0),
                   hjust=c(0,1,0,1,1))


data2 <- data |> 
  arrange(Occupation) |> 
  group_by(Group) |> 
  mutate(CumPercentage=cumsum(Percentage),
    start=(lag(CumPercentage, default=0)-50),
    end=CumPercentage-50
  ) |> 
  ungroup() |> 
  mutate(
    across(start:end, ~case_when(Group=="Whites" ~ .+180,
                                 .default=.))
  ) |> 
  mutate(across(start:end,~./360*2*pi),
         textlocation=(start+end)/2,
         textx=.9*cos(pi/2-textlocation),
         texty=.9*sin(pi/2-textlocation))

p <- data2 |> 
  ggplot() +
  geom_arc_bar(aes(x0=0, y0=0, r0=0, r=1, start=start, end=end, fill=Occupation), color="black",linewidth=.1) +
  geom_text(aes(x=textx, y=texty, label=paste0(Percentage,"%")), family="dubois", size=10) +
  geom_mark_circle(data=circles,aes(x=x, y=y, fill=Occupation), expand=unit(1.5,"cm"),alpha=1, color="black") +
  geom_text(data=text,aes(x=x, y=y, label=str_wrap(Occupation,20), hjust=hjust),vjust=0.5,lineheight=.8, family="dubois", size=11) +
  theme_void() +
  coord_fixed() +
  scale_fill_manual(values=c("#dc143c", "#ffd700", "#4682b4", "#d2b48c", "#654321")) +
  guides(fill="none") +
  annotate("text", x=0, y=1.05, label="NEGROES.",alpha=.8,family="dubois",size=14) +
  annotate("text", x=0, y=-1.05, label="WHITES", alpha=.8,family="dubois",size=14) +
  labs(title=str_to_upper("occupations of negroes and whites in georgia.")) +
  theme(legend.position="none",
        text = element_text(family = "dubois", size = 20, lineheight = .5),
        panel.background = element_rect(fill = NA, color = NA),
        panel.ontop = TRUE,
        plot.title = element_text(size = 48, hjust=.5,lineheight=.8,margin=margin(0.2,0,1,0,"cm")),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#E6D4C3", color = NA),
        plot.margin = margin(1, 1, 0.1, 1, "cm"))

ggsave(p, filename = "2025/final/challenge09.pdf", width=22,height=28,units="in",dpi=600,bg="#E6D4C3")

library(magick)
ggsave(plot=p,filename="2025/final/challenge09.png",width=22, height=28, units="in",dpi=600,bg="#E6D4C3")
image <- image_read("2025/final/challenge09.png")
image2 <- image_resize(image,"812x1024")
image_write(image2, path = "2025/final/challenge09.png", format = "png")

