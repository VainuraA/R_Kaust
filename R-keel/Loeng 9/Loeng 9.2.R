#kippari lahendus
library(tidyverse)
andmed <- read_csv("Sales.csv")
andmed%>%select(W0:W51)
andmed%>%select(W0:W51)%>% summarise_all(sum)
nsummad = andmed%>% select(W0:W51)%>%colSums()
max(nsummad)
nsummad[nsummad==max(nsummad)]
which(nsummad == max(nsummad))
arvud = unname(nsummad)
which(arvud == max(arvud))
colnames(andmed)[which(arvud == max(arvud))+1]


andmed[1, 2:52]
tabel = tibble(müük= unname(andmed[1,2:52]), tulemus = as.numeric(andmed[1, 2:52]))
tabel%>%tail()

tibble(nadal = names(andmed[1, 2:52]), tulemus = as.numeric(andmed[1, 2:52]))%>% 
         ggplot(aes(1:51, tulemus))+geom_line()