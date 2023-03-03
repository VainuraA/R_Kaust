library(tidyverse)
linnad = read_csv("http://minitorn.tlu.ee/~jaagup/kool/java/kursused/22/andmeanalyysi_lahendused_r_keeles/naited/1202/asukohad.txt")
paigutus1 = read_csv("http://minitorn.tlu.ee/~jaagup/kool/java/kursused/22/andmeanalyysi_lahendused_r_keeles/naited/1202/paigutamine.txt")


reanr = 1
uuritavx = as.numeric(paigutus1[reanr, "x"])
uuritavy = as.numeric(paigutus1[reanr, "y"])

paigutus2 = paigutus1%>%mutate(dx = x-uuritavx, dy = y-uuritavy, joonisekaugus=sqrt(dx**2+dy**2))

uuritavA = as.numeric(linnad[reanr, "tallinn"])
uuritavB = as.numeric(linnad[reanr, "tartu"])
uuritavC = as.numeric(linnad[reanr, "pärnu"])

Andmekaugus = linnad%>%mutate(A=tallinn-uuritavA, B = tartu-uuritavB, C = pärnu-uuritavC, andmekaugus = sqrt(A**2+B**2+C**2))
paigutus2$andmetekaugused = Andmekaugus$andmekaugus
paigutus3 = paigutus2%>%mutate(kaugustevahe = andmetekaugu-joonisekaugus, skalaarnihe = kaugustevahe*0.1, 
                  ühikvektorx = -dx/joonisekaugus, ühikvektory = -dy/joonisekaugus, hüppevx = ühikvektorx*skalaarnihe,
                  hüppevy = ühikvektory*skalaarnihe)
paigutus3[is.na(paigutus3)]=0
uusx = uuritavx + sum(paigutus3$hüppevx)
uusy = uuritavy + sum(paigutus3$hüppevy)


sapply(1:5, function(arv){c(a=arv, b= arv*2)})%>%t()%>%as_tibble()

uusAsukoht = function(reanr){
  uuritavx = as.numeric(paigutus1[reanr, "x"])
  uuritavy = as.numeric(paigutus1[reanr, "y"])
  
  paigutus2 = paigutus1%>%mutate(dx = x-uuritavx, dy = y-uuritavy, joonisekaugus=sqrt(dx**2+dy**2))
  
  uuritavA = as.numeric(linnad[reanr, "tallinn"])
  uuritavB = as.numeric(linnad[reanr, "tartu"])
  uuritavC = as.numeric(linnad[reanr, "pärnu"])
  
  Andmekaugus = linnad%>%mutate(A=tallinn-uuritavA, B = tartu-uuritavB, C = pärnu-uuritavC, andmekaugus = sqrt(A**2+B**2+C**2))
  paigutus2$andmetekaugused = Andmekaugus$andmekaugus
  paigutus3 = paigutus2%>%mutate(kaugustevahe = andmetekaugused-joonisekaugus, skalaarnihe = kaugustevahe*0.1, 
                                 ühikvektorx = -dx/joonisekaugus, ühikvektory = -dy/joonisekaugus, hüppevx = ühikvektorx*skalaarnihe,
                                 hüppevy = ühikvektory*skalaarnihe)
  paigutus3[is.na(paigutus3)]=0
  uusx = uuritavx + sum(paigutus3$hüppevx)
  uusy = uuritavy + sum(paigutus3$hüppevy)
  c(kood = paigutus3$kood[[reanr]], x=as.numeric(uusx), y=as.numeric(uusy))
}
uusAsukoht(1)
paigutus=paigutus1
nrow(paigutus)
p1=sapply(1:nrow(paigutus), function(arv){uusAsukoht(arv)})%>%t()%>%as_tibble()
p1$x=as.numeric(p1$x)
p1$y=as.numeric(p1$y)


p1%>%ggplot(aes(x,y, label = kood))+geom_text() 
paigutus1%>%ggplot(aes(x,y,label = kood))+geom_text() 
paigutus1=p1
