library(tidyverse)
andmed = read_csv("https://www.tlu.ee/~jaagup/andmed/keel/korpus/dokkoik.txt")
t.test(andmed%>% filter(keeletase=="A")%>%.$tahti,
       andmed%>% filter(keeletase=="B")%>%.$tahti)

t.test(andmed%>% filter(keeletase=="A")%>%sample_n(50)%>%.$tahti,
       andmed%>% filter(keeletase=="B")%>%sample_n(50)%>%.$tahti)

t.test(andmed%>% filter(keeletase=="B")%>%.$tahti,
       andmed%>% filter(keeletase=="C")%>%.$tahti)

t.test(andmed%>% filter(keeletase=="B")%>%sample_n(50)%>%.$tahti,
       andmed%>% filter(keeletase=="C")%>%sample_n(50)%>%.$tahti)

t.test(andmed%>% filter(keeletase=="B")%>%.$sonu,
       andmed%>% filter(keeletase=="C")%>%.$sonu)

andmed%>%filter(keeletase=="A")%>%summarise(kA=mean(A), kD=mean(D))

#A on omadussõna, D on määrsõnad
t.test(andmed%>%filter(keeletase=="A")%>%.$A,
       andmed%>%filter(keeletase=="A")%>%.$D, paired = TRUE)