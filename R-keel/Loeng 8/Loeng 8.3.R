library(tidyverse)
tibble(xd=seq(125, 215, length=100)) %>%
  ggplot(aes(xd))+stat_function(fun=dnorm, args=c(mean=170, sd=10)) + 
  geom_vline(xintercept=160)+geom_vline(xintercept=180)


tibble(xd=seq(125, 215, length=100)) %>%
  mutate(yd=dnorm(xd, mean=170, sd=10)) %>%
  ggplot(aes(xd, yd))+geom_line() + geom_area(aes(y=ifelse(xd>=160 & xd<=180, yd, 0))) 
  