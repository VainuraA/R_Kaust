library(tidyverse)
suunad = runif(10, 0, 360)
a = 10*cos(suunad)
b = 10*sin(suunad)
tibble(a,b)%>%ggplot(aes(x = a, y = b))+geom_point()+
  geom_segment(aes(x=0, y=0, xend=a, yend=b))

c(0,a)
c(0,b)
tibble(cumsum(c(0,a)), cumsum(c(0,b)))

tibble(cumsum(c(0,a)), cumsum(c(0,b)))%>%ggplot(aes(x = cumsum(c(0,a)), y = cumsum(c(0,b))))+
  geom_point()+geom_path()+geom_vline(xintercept = 100)

kaugus_teest = tibble(cumsum(c(0,a))-100)

sammude_arv = which(kaugus_teest>=0)[1]
loetelu = c()
for (i in 1:100){
  suunad = runif(100, 0, 360)
  a = 10*cos(suunad)
  c(0,a)
  kaugus_teest = tibble(cumsum(c(0,a))-100)
  sammude_arv = which(kaugus_teest>=0)[1]
  if (!is.na(sammude_arv)){
  loetelu = c(sammude_arv, loetelu)
  }
}
 print(loetelu)
keskmiselt_samme = sum(loetelu)/length(loetelu)
tõenäosus_jõuda = length(loetelu)
