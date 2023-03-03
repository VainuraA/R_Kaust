library(tidyverse)
andmed = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_hinnad_pikkused_haalikud.txt")


andmed%>%ggplot(aes(lugu, sonapikkus))+ geom_boxplot()
andmed%>%sample_n(10)
aov(sonapikkus~lugu, data = andmed)                  
summary(aov(sonapikkus~lugu, data = andmed))
TukeyHSD(aov(sonapikkus~lugu, data = andmed))


aov(sulghaalikuid~lugu, data = andmed)
aov((taishaalikuid/sonapikkus)~lugu, data = andmed)

summary(aov(sulghaalikuid~lugu, data = andmed))
summary(aov((taishaalikuid/sonapikkus)~lugu, data = andmed))

TukeyHSD(aov(sulghaalikuid~lugu, data = andmed))
TukeyHSD(aov((taishaalikuid/sonapikkus)~lugu, data = andmed))
