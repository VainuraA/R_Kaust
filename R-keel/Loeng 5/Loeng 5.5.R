library(tidyverse)
sonad = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
lambipirn_kesk<- sapply(1:1000, function(x){sonad%>% filter(lugu=="lambipirn")%>%sample_n(40)%>%
    summarise(k=mean(sonapikkus))%>% .$k})
kungla_kesk<- sapply(1:1000, function(x){sonad%>% filter(lugu=="kungla")%>%sample_n(40)%>%
    summarise(k=mean(sonapikkus))%>% .$k})
koik <- tibble(kungla=kungla_kesk, labipirn= lambipirn_kesk)
hist(lambipirn_kesk)
hist(kungla_kesk)
