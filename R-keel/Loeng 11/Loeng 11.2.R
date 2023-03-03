library(tidyverse)
ilm = read_csv("http://www.tlu.ee/~jaagup/andmed/ilm/harkutund.txt")
jaanuar = ilm%>%filter(Kuu==1)
juuni = ilm%>%filter(Kuu == 6)
juuli = ilm %>%filter(Kuu == 7)
august = ilm %>%filter(Kuu == 8)
oktoober = ilm %>%filter(Kuu == 10)

summary(aov(TA1H~Kell, data = jaanuar))
summary(aov(TA1H~Kell, data = juuli))

TukeyHSD(aov(TA1H~factor(Kell), data = juuli))
#mida väiksem on "p adj", seda suurem on erinevus

summary(aov(PR1H~Kell, data = jaanuar))
summary(aov(PR1H~Kell, data = juuli))
summary(aov(PR1H~Kell, data = oktoober))

TukeyHSD(aov(PR1H~factor(Kell), data = jaanuar))
TukeyHSD(aov(PR1H~factor(Kell), data = oktoober))

summary(aov(TA1H~Kuu, data = ilm%>%filter(Kuu%in% c(6,7,8))))
TukeyHSD(aov(TA1H~factor(Kuu), data = ilm%>%filter(Kuu %in% c(6,7,8))))
