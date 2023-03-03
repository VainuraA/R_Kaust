library(tidyverse)
noodid= read.csv("https://www.tlu.ee/~jaagup/andmed/muu/muusika/regiviisid.txt")
library(ggplot2)
library(gifski)
library(gganimate)
library(png)

noodid%>% select(P1:P16)
noodid%>%filter(P1=="a")%>%
  group_by(P16)%>%summarise(kogus=n())%>%
  arrange(-kogus)%>%head(4)
noodid%>% unite("viimased", c(P13:P16))%>%
  group_by(viimased)%>% summarise(kogus=n())%>%
  arrange(-kogus)

