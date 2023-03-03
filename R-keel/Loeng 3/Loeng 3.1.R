library(tidyverse)
sonad=read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haali
kud.txt")
library(gganimate)
library(gifski)
library(png)


pildid <-sonad%>%filter(taishaalikuid<6)%>%
  group_by(taishaalikuid)%>%
  summarise(thkogus=sum(taishaalikuid), sh=sum(sulghaalikuid))%>%
  gather(haalikuid, kogus, -taishaalikuid)%>%
  ggplot(aes(haalikuid, kogus,))+ geom_col()+facet_wrap(.~taishaalikuid)

pildid%>%animate(renderer=gifski_renderer())
