library(tidyverse)
sonad = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
kungla_th <- (sonad%>%filter(lugu=="kungla")%>%.$taishaalikuid)/(sonad%>%filter(lugu=="kungla")%>%.$sonapikkus)
lambipirn_th <- (sonad%>%filter(lugu=="lambipirn")%>%.$taishaalikuid)/(sonad%>%filter(lugu=="lambipirn")%>%.$sonapikkus)
t.test(kungla_th, lambipirn_th)

