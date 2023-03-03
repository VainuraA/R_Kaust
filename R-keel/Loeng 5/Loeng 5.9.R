library(tidyverse)
sonad = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
sonad2 <- sonad %>% mutate(tosakaal=taishaalikuid/sonapikkus)
t.test(sonad2 %>% filter(lugu=="kungla") %>% .$tosakaal, 
       sonad2 %>% filter(lugu=="lambipirn") %>% .$tosakaal)

