library(tidyverse)
andmed = read_csv("https://www.tlu.ee/~jaagup/andmed/keel/korpus/dokkoik.txt")
andmed2 <- andmed%>%filter(S>0, V>0)%>%mutate(noskaal=S/sonu)

andmed2%>%group_by(keeletase)%>%summarise(no=mean(noskaal))

andmed2$noskaal%>%hist()
andmed2%>% ggplot(aes(noskaal, kokku)) + geom_point()

andmed%>%group_by(keeletase)%>%summarise(asum=sum(A), ssum=sum(S), vsum=sum(V))%>%
  select(-keeletase)%>%chisq.test()
