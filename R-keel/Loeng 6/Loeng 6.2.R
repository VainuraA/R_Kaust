library(tidyverse)
andmed = read_csv("https://www.tlu.ee/~jaagup/andmed/keel/korpus/dokkoik.txt")
andmed%>% filter(keeletase=="A")%>% .$tahti
t.test(andmed%>% filter(keeletase=="A")%>%.$tahti)

t.test(andmed%>% filter(keeletase=="A")%>%.$tahti, conf.level = 0.999)

t.test(andmed%>% filter(keeletase=="A")%>%.$tahti, mu=1000)

t.test(andmed%>% filter(keeletase=="A")%>%sample_n(100)%>%.$tahti, mu=1000)


