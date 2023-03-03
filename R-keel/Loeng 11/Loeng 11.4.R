library(tidyverse)
andmed = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")

mudel = lm(sonapikkus~taishaalikuid, data=andmed %>% filter(lugu=="kungla"))
predict(mudel, tibble(taishaalikuid=c(2,3,4,5)))

mudel1 = lm(sonapikkus~taishaalikuid, data=andmed %>% filter(lugu=="lambipirn"))
predict(mudel1, tibble(taishaalikuid=c(2,3,4,5)))


mudel2 = lm(formula = sonapikkus~taishaalikuid+sulghaalikuid, data = andmed%>%filter(lugu=="kungla"))
uuritav = tibble(taishaalikuid = c(2,2,5,5), sulghaalikuid = c(1,4,1,4))
uuritav$sonapikkus=predict(mudel2, uuritav)
andmed$e_pikkus = predict(mudel2, andmed)


summary(lm(taishaalikuid~sonapikkus+sulghaalikuid+lugu, andmed))

lm(sonapikkus~lugu, data=andmed)
