library(tidyverse)
noodid= read.csv("https://www.tlu.ee/~jaagup/andmed/muu/muusika/regiviisid.txt")
library(ggplot2)
library(gifski)
library(gganimate)
library(png)

noodid%>% select(P1:P16)
noodid%>%
  group_by(P16)%>%summarise(kogus=n())%>%
  arrange(-kogus)%>%head(6)
noodid%>%
  group_by(P1)%>% summarise(kogus=n())%>%
  arrange(-kogus)%>%head(6)

tavalised=c("g", "a", "h", "2c", "2d")
pildid = noodid%>% filter(P1 %in% tavalised, P16 %in% tavalised)%>%
  group_by(P1, P16)%>% summarise(kogus=n())%>%
  ggplot(aes(P16, kogus))+geom_col()+transition_states(P1)
  pildid%>%animate(renderer=gifski_renderer())
