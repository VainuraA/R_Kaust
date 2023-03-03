library(tidyverse)
sonad = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
sonad %>% filter(lugu=="kungla") %>% sample_n(100, replace=TRUE) %>% bind_rows(
  sonad %>% filter(lugu=="lambipirn") %>% sample_n(100)) %>%
  ggplot(aes(sonapikkus, fill=lugu)) + 
  geom_histogram(binwidth=2, position="identity", alpha=0.5)
sonad%>%filter(lugu=="kungla")%>%head()
sonad%>%filter(lugu=="lambipirn")%>%head()


t.test(c(3,6,6,7,3,4),c(3,5,108,13,4))

t.test(sonad%>%filter(lugu=="kungla")%>%.$sonapikkus,sonad%>%filter(lugu=="lambipirn")%>%.$sonapikkus)

