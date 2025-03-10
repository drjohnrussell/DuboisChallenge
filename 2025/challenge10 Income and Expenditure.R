library(tidyverse)
library(gt)
library(magick)
library(showtext)
library(ggbrace)

font_add_google("Play", family = "dubois")
showtext_auto()

data <- read_csv("https://github.com/ajstarks/dubois-data-portraits/raw/refs/heads/master/challenge/2025/challenge10/data.csv") |> 
  mutate(Other=case_when(Class=="$100-200" ~ 9.9,
                          .default=Other),
         Tax=case_when(Class=="$100-200" ~ 0.1,
                       .default=Tax))

## Table at the top
directtaxes <- data.frame(table1=c(
  '1880 -',
  '1885 -',
  '1890 -',
  '1895 ',
  '1889 '),
  table2=c(
    "$3.50",
    "$3.50",
    "$3.96",
    "$4.56",
    "$5.36"),
  table3=c('PER',
           '"',
           '"',
           '"',
           '"'),
  table4=c('$1000',
           '"',
           '"',
           '"',
           '"'))

innertable <- directtaxes |> 
  gt() |> 
  tab_header(title="THE STATE TAX RATE IS:") |> 
  tab_source_note(
    source_note=html("STATE AND COUNTY TAXES <br> RAISE THIS TO <br> $21 PER $1000 <br> IN ATLANTA")
  ) |> 
  tab_style(style = cell_text(align = "center"),locations = cells_source_notes()) |> 
  tab_options(column_labels.hidden = TRUE,
              table_body.hlines.style = "hidden",
              table.border.top.style = "hidden", 
              table.border.bottom.style = "hidden", 
              table.border.left.style = "hidden", 
              table.border.right.style = "hidden",
              heading.border.bottom.style="hidden",
              source_notes.border.bottom.style = "hidden",
              stub.border.style= "hidden",
              row_group.border.bottom.style = "hidden",
              footnotes.border.bottom.style = "hidden",
              table.background.color="#E6D4C3") |> 
  opt_table_font(
    font = list(
      google_font(name = "Play"),
      "Play")) |> 
  cols_align(align = "center", columns= everything()) |> 
  cols_align(align= "left", columns=table1)

innertable |> 
  gtsave("2025/images/tab_1.png", expand = 0)

## Pulling together a table
table <- tibble(RENT= c("2025/images/rent.PNG",""),
                FOOD= c("2025/images/food.PNG",""),
                CLOTHES= c("2025/images/clothes.PNG",""),
                `DIRECT TAXES`= c("2025/images/tab_1.png",""),
                `OTHER EXPENSES AND SAVINGS`=c("THE HIGHER LIFE <br> &nbsp;  RELIGION <br> ART <br> EDUCATION <br> SICKNESS <br> SAVINGS <br> AMUSEMENTS <br> BOOKS AND PAPERS <br> TRAVEL",""))

legendtable <- table |> 
  gt() |> 
  tab_header(title="ANNUAL EXPENDITURE FOR") |> 
  text_transform(
    locations=cells_body(columns=c(RENT:`DIRECT TAXES`),rows=1),
    fn = function(x) {
      local_image(filename=x, height=190)
      }
  ) |> 
  data_color(columns=RENT,rows=2,colors=c("black")) |>
  data_color(columns=FOOD,rows=2,colors=c("#7e6583")) |>
  data_color(columns=CLOTHES,rows=2,colors=c("#ffc0cb")) |>
  data_color(columns=`DIRECT TAXES`,rows=2,colors=c("grey")) |>
  data_color(columns=`OTHER EXPENSES AND SAVINGS`,rows=2,colors=c("#d2b48c")) |>
  fmt_markdown(columns=`OTHER EXPENSES AND SAVINGS`) |> 
  opt_table_font(
    font = list(
      google_font(name = "Play"),
      "Play")) |> 
  tab_options(table.background.color="#E6D4C3",
              table_body.border.top.color = "black",
              table_body.border.bottom.color = "black",
              table_body.vlines.style = "solid",
              table_body.hlines.color = "black",
              table_body.hlines.style = "solid",
              table_body.vlines.color = "black",
              heading.border.bottom.color = "black",
              table.border.top.color = "black",
              table.border.left.color="black",
              column_labels.vlines.color = "black",
              column_labels.vlines.style = "solid",
              column_labels.border.bottom.color="black") |> 
  opt_table_outline(style = "solid", width = px(1), color = "black") |> 
  cols_width(everything() ~ px(185))

legendtable |> 
  gtsave("2025/images/legendtable.png", vwidth = 2000, vheight = 1000)

### Trimming the image
legendtable <- image_ggplot(image_trim(image_read("2025/images/legendtable.PNG")))
shield <- image_ggplot(image_read("2025/images/income.PNG"))
library(patchwork)

## rest of the plot

data <- data |> 
  mutate(Class=case_when(Class=="Over $1000" ~ "1,000 \n AND OVER",
                         .default=Class),
    Class=fct_inorder(Class))

graph <- data |> 
  select(-`Actual Average`) |> 
  pivot_longer(cols=c(Rent:Other), names_to="Category", values_to="Amount") |> 
  mutate(Category=fct_inorder(Category)) |> 
  group_by(Class) |> 
  mutate(start=cumsum(lag(Amount, default=0)), end=cumsum(Amount),
         textlocation=(start+end)/2) |> 
  ungroup()

segments <- graph |> 
  group_by(Category) |>
  mutate(x=as.numeric(Class)+.2,
         xend=as.numeric(Class)+.8,
         y=end,
         yend=lead(end, default=0)) |> 
  filter(y!=yend,
         Class!="1,000 \n AND OVER")

bracket1 <- data.frame(x=c(.6, 2.4), y=c(98,104))
bracket2 <- data.frame(x=c(2.6, 4.4), y=c(98,104))
bracket3 <- data.frame(x=c(4.6, 6.4), y=c(98,104))
bracket4 <- data.frame(x=c(6.6, 7.4), y=c(98,104))

p <- graph |> 
  ggplot() +
  geom_rect(aes(xmin=as.numeric(Class)-.2,
                xmax=as.numeric(Class)+.2,
                ymin=start,
                ymax=end,
                fill=Category), color="black") +
  coord_flip() + scale_x_reverse(limits=c(7.7,-3)) +
  geom_segment(data=segments,
               aes(x=x,xend=xend,y=y,yend=yend),color="black") +
  geom_text(data=graph |> 
              filter(!Amount %in% c(0,0.1)),
            aes(x=as.numeric(Class), y=textlocation, label=paste0(Amount,"%"),color=Category),size=11) +
  scale_fill_manual(values=c("black","#7e6583","#ffc0cb","grey","#d2b48c")) +
  scale_color_manual(values=c("white","black","black","black","black")) +
  geom_text(data=data,
            aes(x=as.numeric(Class), y=-25, label=Class),color="black",lineheight=.8,size=11) +
  geom_text(data=data,
            aes(x=as.numeric(Class), y=-10, label=paste0("$",`Actual Average`)),color="black",size=11) +
  scale_y_continuous(limits=c(-35,108)) +
  theme_void() +
  guides(fill="none",
         color="none") +
  annotate("rect",ymin=-33,ymax=-2,xmin=.2,xmax=7.5,fill=NA,color="black") +
  annotate("segment",x=.2,xend=7.5,y=-17,yend=-17,color="black") +
  annotate("segment",y=-33,yend=5,x=0.5,xend=0.5,arrow=arrow(type="open",length=unit(0.1,"cm")),color="black") +
  annotate("segment",y=-33,yend=5,x=1.5,xend=1.5,arrow=arrow(type="open",length=unit(0.1,"cm")),color="black") +
  annotate("segment",y=-33,yend=5,x=2.5,xend=2.5,arrow=arrow(type="open",length=unit(0.1,"cm")),color="black") +
  annotate("segment",y=-33,yend=5,x=3.5,xend=3.5,arrow=arrow(type="open",length=unit(0.1,"cm")),color="black") +
  annotate("segment",y=-33,yend=5,x=4.5,xend=4.5,arrow=arrow(type="open",length=unit(0.1,"cm")),color="black") +
  annotate("segment",y=-33,yend=5,x=5.5,xend=5.5,arrow=arrow(type="open",length=unit(0.1,"cm")),color="black") +
  annotate("segment",y=-33,yend=5,x=6.5,xend=6.5,arrow=arrow(type="open",length=unit(0.1,"cm")),color="black") +
  annotate("text",x=0.35,y=-25,label="CLASS",size=6.5) +
  annotate("text",x=0.35,y=-10,label="ACTUAL AVERAGE",size=6.5) +
  annotate("text",x=1.5,y=106,label="POOR",angle=90,size=8) +
  annotate("text",x=3.5,y=106,label="FAIR",angle=90,size=8) +
  annotate("text",x=5.5,y=106,label="COMFORTABLE",angle=90,size=8) +
  annotate("text",x=7,y=106,label="WELL-TO DO",angle=90,size=8) +
  labs(title="INCOME AND EXPENDITURE of 150 NEGRO FAMILIES IN ATLANTA,GA.,USA.") +
  stat_brace(data=bracket1,
             aes(x=x,y=y),outside=FALSE,rotate=-180) +
  stat_brace(data=bracket2,
             aes(x=x,y=y),outside=FALSE,rotate=-180) +
  stat_brace(data=bracket3,
             aes(x=x,y=y),outside=FALSE,rotate=-180) +
  stat_brace(data=bracket4,
             aes(x=x,y=y),outside=FALSE,rotate=-180) +
  annotate("text",y=50,x=7.5,label="FOR FURTHER STATISTICS RAISE THIS FRAME.",size=11) +
  annotate("segment",y=19,yend=21.5,x=.8,xend=0) +
  annotate("segment",y=62,yend=42,x=.8,xend=0) +
  annotate("segment",y=90,yend=62.5,x=.8,xend=0) +
  annotate("segment",y=90.1,yend=83,x=.8,xend=0) +
  theme(text = element_text(family = "dubois", size = 20, lineheight = .5),
        panel.background = element_rect(fill = NA, color = NA),
        panel.ontop = TRUE,
        plot.background = element_rect(fill = "#E6D4C3", color = NA),
        plot.title = element_text(size = 55, hjust=.5,lineheight=.8,margin=margin(1,0,1,0,"cm")))

q <- p + inset_element(shield,left=0,bottom=.7,right=.3,top=1) +
  inset_element(legendtable,left=.2,bottom=.7,right=1,top=1)


ggsave(plot=q,filename="2025/challenge10.pdf",width=28,height=22,units="in",dpi=600,bg="#E6D4C3")

