library(tidyverse)
andmed = read_csv("https://www.tlu.ee/~jaagup/andmed/keel/korpus/dokkoik.txt")
andmed2=andmed%>%sample_n(50)

t.test(andmed2%>%filter(keeletase=="A")%>%.$A,
       andmed2%>%filter(keeletase=="A")%>%.$D, paired = TRUE)

t.test(andmed%>%filter(keeletase=="A")%>%.$tahti,
       andmed%>%filter(keeletase=="A")%>%.$sonu, alternative = "greater")
