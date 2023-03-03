library(tidyverse)
andmed = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
kunglaandmed = andmed%>%filter(lugu == "kungla")
lambiandmed = andmed%>%filter(lugu == "lambipirn")


rühmad = kmeans(kunglaandmed%>%select(taishaalikuid, sulghaalikuid), centers=3)
kunglaandmed$rühm = rühmad$cluster
ggplot() +
  geom_point(aes(taishaalikuid, jitter(sulghaalikuid), color=factor(rühm)), data=kunglaandmed)+
  geom_point(aes(taishaalikuid, sulghaalikuid), data=as_tibble(rühmad$centers) %>% mutate(nr=paste("rühm ", row_number(), sep=""))) 


rühmad2 = kmeans(lambiandmed%>%select(taishaalikuid, sulghaalikuid), centers=3)
lambiandmed$rühm2 = rühmad2$cluster
ggplot() +
  geom_text(aes(taishaalikuid, jitter(sulghaalikuid), color=factor(rühm), label=sona), data=lambiandmed)+
  geom_text(aes(taishaalikuid, sulghaalikuid, label=nr), data=as_tibble(rühmad$centers) %>% mutate(nr=paste("rühm ", row_number(), sep=""))) 

install.packages("fdm2id")
library(fdm2id)

lamp = predict(rühmad, andmed%>%filter(lugu=="lambipirn")%>%
          select(taishaalikuid, sulghaalikuid))
lambiandmed$lampi = lamp

lambiandmed%>%ggplot(aes(taishaalikuid, sulghaalikuid, color = factor(lampi)))+
  geom_jitter()


lambiandmed%>%add_row(kunglaandmed)