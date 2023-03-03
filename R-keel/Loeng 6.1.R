library(tidyverse)
andmed = read_csv("https://www.tlu.ee/~jaagup/andmed/keel/korpus/dokkoik.txt")
count(andmed)
andmed$keeletase
andmed%>%group_by(keeletase)%>%summarise(kogus=n())%>%
  arrange(-kogus)
andmed%>%filter(keeletase %in% c("A", "B"))%>% group_by(keeletase)%>%
  summarise(keskpikkus=mean(tahti))

andmed%>% filter(keeletase%in% c("A","B"))%>%
  ggplot(aes(tahti))+geom_histogram()

andmed%>% filter(keeletase%in% c("A","B"), tahti <= 10000)%>%
  ggplot(aes(keeletase, tahti))+geom_boxplot()

andmed%>% filter(keeletase%in% c("A","B"), tahti <= 5000)%>%
  ggplot(aes(tahti, fill=keeletase))+geom_histogram()

andmed%>% filter(keeletase%in% c("A","B"), tahti <= 5000)%>%
  ggplot(aes(tahti, fill=keeletase))+geom_boxplot()
