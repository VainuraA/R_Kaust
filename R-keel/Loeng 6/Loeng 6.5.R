library(tidyverse)
andmed = read_csv("https://www.tlu.ee/~jaagup/andmed/keel/korpus/dokkoik.txt")
andmed2 <- andmed%>%filter(S>0, V>0)%>% mutate(nosakaal = S/sonu, tosakaal = V/sonu, svsuhe = S/V)

t.test(andmed%>%filter(emakeel=="vene")%>%.$S,
       andmed%>%filter(emakeel=="soome")%>%.$S)

t.test(andmed2%>%filter(emakeel=="vene")%>%.$nosakaal,
       andmed2%>%filter(emakeel=="soome")%>%.$nosakaal)

t.test(andmed2%>%filter(emakeel=="vene")%>%.$nosakaal,
       andmed2%>%filter(emakeel=="vene")%>%.$tosakaal, paired = TRUE)

t.test(andmed2%>%filter(emakeel=="soome")%>%.$nosakaal,
       andmed2%>%filter(emakeel=="soome")%>%.$tosakaal, paired = TRUE)



t.test(andmed2%>%filter(emakeel=="vene")%>%.$svsuhe,
       andmed2%>%filter(emakeel=="soome")%>%.$svsuhe)
