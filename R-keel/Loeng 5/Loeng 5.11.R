library(tidyverse)
sonad = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
t.test(sonad %>% filter(lugu=="kungla")%>%sample_n(50) %>% .$taishaalikuid, 
       sonad %>%filter(lugu=="kungla")%>%sample_n(50) %>% .$sulghaalikuid)
t.test(sonad %>% filter(lugu=="kungla")%>%sample_n(50) %>% .$taishaalikuid, 
       sonad %>% filter(lugu=="kungla")%>%sample_n(50) %>% .$sulghaalikuid, paired = TRUE)
