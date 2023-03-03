library(tidyverse)
sonad = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
t.test(sonad%>%filter(lugu=="kungla")%>%.$taishaalikuid,sonad%>%filter(lugu=="lambipirn")%>%.$taishaalikuid)
