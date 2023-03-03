library(tidyverse)
sonad=read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")

#Kuvage korrelatsiooni 95% usaldusvahemik Lambipirni jutu sõnade sõnapikkuse ja täishäälikute arvu vahel
lambi = sonad%>%filter(lugu == "lambipirn")
cor.test(lambi$taishaalikuid, lambi$sonapikkus)


