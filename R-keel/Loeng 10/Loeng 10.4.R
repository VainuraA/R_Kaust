library(tidyverse)
dokmeta = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/korpus/dokmeta.txt")
sonaliigid = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/korpus/doksonaliigid.txt")

korr =sonaliigid %>% select(where(is.numeric)) %>% cor()

kõik = (korr["V", ])%>%as_tibble()%>%mutate(sonaliik = colnames(korr))
kõik1 = korr["V", ]%>%as_tibble()%>%mutate(sonaliik = colnames(korr))%>%arrange(-value)%>%
  mutate(reanr = row_number())
                                                 
koos = dokmeta%>%inner_join(sonaliigid, by= c("kood"="kood"))
eestik = koos%>%filter(tekstikeel == "eesti")

korrel = eestik%>%select(where(is.numeric))%>%cor()

eesti_keelsed =korrel["V", ]%>% as_tibble()%>%mutate(sonaliik = colnames(korrel))
eest_keelsed1 = korrel["V", ]%>% as_tibble()%>%mutate(sonaliik = colnames(korrel))%>%arrange(-value)%>%
  mutate(reanr = row_number())

üld = eesti_keelsed%>%inner_join(kõik, by = c("sonaliik"="sonaliik"))
üld1= eest_keelsed1 %>% inner_join(kõik1, by = c("sonaliik"="sonaliik"))%>% mutate(kohavahe = reanr.y-reanr.x, osakaaluvahe = value.y-value.x)

