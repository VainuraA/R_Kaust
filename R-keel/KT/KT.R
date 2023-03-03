library(tidyverse)
hinnad = read_csv("https://www.tlu.ee/~jaagup/andmed/elekter/hinnad_2014_eur.txt")
hinnad1 = hinnad%>%select(-Date, -Hours)
hinnad3 = hinnad%>%select(-Date)

miinimum = apply(hinnad1, 2, min, na.rm = TRUE)
maksimum = apply(hinnad1, 2, max, na.rm = TRUE)
keskmine = apply(hinnad1, 2, mean, na.rm = TRUE)
piirkond1 = c(colnames(tulemused1[2:18]))

tulemused<- rbind(miinimum, maksimum, keskmine)%>%t()%>%as_tibble()
tulemused1 = tulemused%>%mutate(piirkond = piirkond1)
tulemused1%>%ggplot(aes(piirkond))+
  geom_point(aes(y=miinimum, colour = "min"))+
  geom_point(aes(y= maksimum, colour = "max"))+
  geom_point(aes(y=keskmine, colour = "keskm"))

hinnad2= t(hinnad1)
colnames(hinnad2)= t(hinnad[,2])


aeg_kesk = function(nr){
  aeg = hinnad3%>%filter(Hours == as.character(hinnad3[nr,1]))%>%select(-Hours)
  kesk = apply(aeg, 2, mean, na.rm= TRUE)
  tabel1= c(kesk)
}
tabel <- sapply(1:24, function(arv){aeg_kesk(arv)})%>%as_tibble()
Tabel1 = tabel%>%t()%>%as_tibble()%>%mutate(tunnid =colnames(tabel))
colnames(Tabel1)=piirkond1

Tabel1%>%ggplot(aes(tunnid))+
  geom_point(aes(y=SYS, colour="SYS"))+
  geom_point(aes(y=SE1, colour="SE1"))+
  geom_point(aes(y=SE2, colour="SE2"))+
  geom_point(aes(y=SE3, colour="SE3"))+
  geom_point(aes(y=SE4, colour="SE4"))+
  geom_point(aes(y=FI, colour = "FI"))+
  geom_point(aes(y=DK1, colour="DK1"))+
  geom_point(aes(y=DK2, colour = "DK2"))+
  geom_point(aes(y=Oslo, colour = "Oslo"))+
  geom_point(aes(y=Kr.sand, colour = "Kr.sand"))+
  geom_point(aes(y=Bergen, colour = "Bergen"))+
  geom_point(aes(y=Molde, colour = "Molde"))+
  geom_point(aes(y=Tr.heim, colour = "Tr.heim"))+
  geom_point(aes(y=Tromso, colour = "Tromso"))+
  geom_point(aes(y=EE, colour = "EE"))+
  geom_point(aes(y=LV, colour = "LV"))+
  geom_point(aes(y=LT, colour="LT"))


sääst = (max(tabel[1,])-tabel[1,(which(tabel[1,]==max(tabel[1,]))+1)])/1000+
  (tabel[1,(which(tabel[1,]==min(tabel[1,]))-1)]-min(tabel[1,]))/1000

