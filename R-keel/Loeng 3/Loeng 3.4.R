library(tidyverse)
andmed= read.csv("https://www.tlu.ee/~jaagup/andmed/keel/korpus/dokkoik.txt")
library(ggplot2)
library(gifski)
library(gganimate)
library(png)

pildid = andmed%>%filter(S>0, keeletase %in% c("A1", "A2", "B1", "B2", "C1", "C2"))%>%
  group_by(keeletase)%>%
  summarise(keskS=mean(S), keskV=mean(V))%>%
  ggplot(aes(keskS, keskV))+
  geom_point()+transition_states(keeletase)+
  ggtitle("{closest_state}")
pildid%>% animate(renderer=gifski_renderer())

