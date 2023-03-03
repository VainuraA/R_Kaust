library(tidyverse)
sonad = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
sonad%>% filter(lugu=="lambipirn")%>%sample_n(100)%>%
  mutate(pikk=sonapikkus<5)%>%group_by(pikk)%>%summarise(kogus=n())

