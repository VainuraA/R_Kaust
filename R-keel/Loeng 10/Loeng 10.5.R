library(tidyverse)
dokmeta = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/korpus/dokmeta.txt")
sonaliigid = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/korpus/doksonaliigid.txt")
koos = dokmeta%>%inner_join(sonaliigid, by= c("kood"="kood"))

korr =sonaliigid %>% select(where(is.numeric)) %>% cor()
korr["S", ]%>%as_tibble()%>%rename(S=value)%>%arrange(-S)

b1 = koos%>%filter(tekstikeel == "eesti", keeletase =="B1")%>%
  select(where(is.numeric))%>%cor()
b2 = koos%>%filter(tekstikeel == "eesti", keeletase =="B2")%>%
  select(where(is.numeric))%>%cor()


alg = b1["S", ]%>%as_tibble()%>%rename(S=value)%>%mutate(sonaliik = colnames(korr))%>%
  arrange(-S)%>%mutate(reanr = row_number())
kõrg = b2["S", ]%>%as_tibble()%>%rename(S=value)%>%mutate(sonaliik = colnames(korr))%>%
  arrange(-S)%>%mutate(reanr=row_number())

b1_b2_võrd = alg%>%inner_join(kõrg, by = c("sonaliik" = "sonaliik"))%>% 
  mutate(kohavahe = reanr.y-reanr.x)
