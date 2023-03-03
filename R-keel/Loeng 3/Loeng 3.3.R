library(tidyverse)
sonad = read.csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")

tabel=sonad%>% group_by(lugu)%>% summarise(thosa=sum(taishaalikuid)/sum(sonapikkus), shosa=sum(sulghaalikuid)/sum(sonapikkus))
tabel%>%gather(haalikud, osakaal, -lugu)

pildid=tabel%>% gather(haalikud, osakaal, -lugu)%>%
  ggplot(aes(haalikud, osakaal))+ geom_col()+ transition_states(lugu)+
  ggtitle("{closest_state}")

pildid%>% animate(renderer=gifski_renderer())
