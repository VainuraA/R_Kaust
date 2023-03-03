library(tidyverse)
kohad = read_csv("https://minitorn.tlu.ee/~jaagup/kool/java/kursused/22/andmeanalyysi_lahendused_r_keeles/naited/1021/andmed/kohad.txt")
kohad%>%ggplot(aes(pikkuskraad, laiuskraad, label= kohanimi))+geom_text()

kohad_tekstid = read_csv("https://minitorn.tlu.ee/~jaagup/kool/java/kursused/22/andmeanalyysi_lahendused_r_keeles/naited/1021/andmed/kohad_tekstid.txt", c("koht","tekst"))
kohad_tekstid2 = kohad_tekstid%>%inner_join(kohad, by= c("koht"="kohanimi"))
tkesk = kohad_tekstid2%>% group_by(tekst)%>%summarise(pk=mean(pikkuskraad), lk = mean(laiuskraad))
kohad3= kohad%>%inner_join(tekstsuurus, by = c("kohanimi"="koht"))
kohad4= tkesk%>%inner_join(tuupsuurus, by = c("tekst"="tekst"))
  
kohad_tekstid2%>%ggplot(aes(pikkuskraad, laiuskraad, label= tekst))+geom_text()
kohad_tekstid2%>%mutate(laiuskraad2 = laiuskraad+runif(n()))%>%
  ggplot(aes(pikkuskraad, laiuskraad2, label = tekst))+geom_text()

kohad_tekstid2%>% group_by(tekst)%>%summarise(pk=mean(pikkuskraad), lk = mean(laiuskraad))%>%
  ggplot(aes(pk, lk, label = tekst))+geom_text()+geom_text(data=kohad, aes(pikkuskraad, laiuskraad, label = kohanimi), color="pink")

kohad%>%ggplot(aes(pikkuskraad, laiuskraad, label = kohanimi))+ geom_text(color = "pink", size = 3)+
  geom_text(data = tkesk, aes( pk, lk, label = tekst) )

tekstid = kohad_tekstid2%>% inner_join(tkesk, by ="tekst")

kohad%>%ggplot(aes(pikkuskraad, laiuskraad))+ geom_text(aes(label = kohanimi), color = "pink", size = 3)+
  geom_text(data = tkesk, aes( pk, lk, label = tekst))+
  geom_segment(data=tekstid, aes(pk, lk, xend = pikkuskraad, yend = laiuskraad), color = "blue")

kohad%>%ggplot(aes(pikkuskraad, laiuskraad))+
  geom_segment(data=tekstid, aes(pk, lk, xend = pikkuskraad, yend = laiuskraad), color = "gray")+
  geom_text(aes(label = kohanimi), color = "pink", size = 3)+
  geom_text(data = tkesk, aes( pk, lk, label = tekst))

kohad%>%ggplot(aes(pikkuskraad, laiuskraad))+
  geom_segment(data=tekstid%>%filter(tekst!="tl1"), aes(pk, lk, xend = pikkuskraad, yend = laiuskraad), color = "gray")+
  geom_text(aes(label = kohanimi), color = "pink", size = 3)+
  geom_text(data = tkesk, aes( pk, lk, label = tekst))

tekstsuurus= kohad_tekstid2%>%group_by(koht)%>% na.omit()%>% summarise(suurus = length(tekst))
tuupsuurus = kohad_tekstid2%>%group_by(tekst)%>% na.omit()%>% summarise(kogus = length(koht))

kohad3%>%ggplot(aes(pikkuskraad, laiuskraad))+
  geom_text(aes(label = kohanimi, size = suurus), color = "red" )

kohad3%>%ggplot(aes(pikkuskraad, laiuskraad))+
  geom_text(aes(label = kohanimi, size = suurus), color = "red" )+
  geom_text(data = kohad4, aes(pk, lk, label = tekst, size = kogus))+
  annotate("text", 25, 59, label = " ", size =1)+
  ggtitle("Tekstide kujuteldavad asukohad")


