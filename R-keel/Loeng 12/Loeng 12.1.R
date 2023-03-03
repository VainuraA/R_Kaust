andmevahe = sqrt((3-1)**2+(0-0)**2+(6-7)**2)
joonisvahe = sqrt((0.5-2.28)**2+(0.866-(-0.48))**2)
vahe = andmevahe-joonisvahe
nihe = vahe *0.1
vt3t1x = 2.28-0.5
vt3t1y = -0.48-0.866
vt3t1pikk = sqrt(vt3t1x**2+vt3t1y**2)
ühikx = vt3t1x/joonisvahe
ühiky = vt3t1y/joonisvahe
t1x = 2.28+ühikx*nihe
t1y = -0.48+ühiky*nihe
print(t1x)
print(t1y)





colnames(tabel)=t(hinnad[,2])
Tabel1 = t(tabel)
tunnid <- rownames(Tabel1)
rownames(Tabel1)<-NULL
Tabel2<- cbind(tunnid, Tabel1)
Tabel2%>%ggplot(aes(,tunnid))+
  geom_point(aes(x=col(Tabel1)))
