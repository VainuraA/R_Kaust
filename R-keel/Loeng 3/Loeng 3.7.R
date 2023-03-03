library(tidyverse)
andmed= read.csv("https://www.tlu.ee/~jaagup/andmed/keel/korpus/dokkoik.txt")
library(ggplot2)
library(gifski)
library(gganimate)
library(png)

pildid = andmed%>%na.omit()%>%
  filter(S>0,tahti<1000, keeletase %in% c("A1", "A2", "B1", "B2", "C1", "C2"))%>%
  mutate(pikkusklass=round(tahti/50))%>%
  group_by(keeletase, pikkusklass)%>%
  summarise(keskS=mean(S), keskV=mean(V))%>%
  ggplot(aes(keskS, keskV,label=keeletase))+
  geom_text()+transition_states(pikkusklass)+
  ggtitle("{closest_state}")
pildid%>% animate(renderer=gifski_renderer())