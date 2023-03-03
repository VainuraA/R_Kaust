library(tidyverse)
sonad=read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
library(gganimate)
library(gifski)
library(png)
library(glue)


pildid <- sonad %>% filter(taishaalikuid<6) %>%
  group_by(taishaalikuid) %>%
  summarise(thkogus=sum(taishaalikuid), sh=sum(sulghaalikuid)) %>%
  gather(haalikud, kogus, -taishaalikuid) %>%
  ggplot(aes(haalikud, kogus)) + geom_col() +
  transition_states(taishaalikuid) + ggtitle("Täishaalikuid {closest_state} {frame}")



pildid %>% animate(renderer=gifski_renderer())
