library(tidyverse)
sonad=read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haali
kud.txt")
library(gganimate)
library(gifski)
library(png)

pildid <- sonad %>% filter(taishaalikuid<6) %>%
  ggplot(aes(taishaalikuid, sulghaalikuid))+geom_point()+
  transition_time(sonapikkus)
pildid %>% animate(renderer=gifski_renderer())
