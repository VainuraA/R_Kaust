#minu lahendused
library(tidyverse)
andmed <- read_csv("Sales.csv")
näd = andmed%>%select(-Product_Code)
sum_näd = näd%>%summarise_all(c(sum = sum))
sum_näd1 = sum_näd[-c(53:106)]
maks = max(sum_näd1)
names(näd)[match(maks,sum_näd1)]


talv = andmed[c(1:10, 50:53)]
suvi = andmed[c(1, 23:35)]
talv_müük = talv%>% rowwise()%>%
  mutate(kõik = sum(c_across(2:13)))
suvi_müük = suvi%>% rowwise()%>%
  mutate(kõik = sum(c_across(2:13)))
s_vs_t = suvi_müük$kõik>talv_müük$kõik
which(s_vs_t == TRUE)
üldarv = andmed%>%select(Product_Code)%>%
  mutate(suve_m = suvi_müük$kõik, talve_m = talv_müük$kõik, suhe = suvi_müük$kõik-talv_müük$kõik)
(üldarv%>%filter(suhe > 0))$Product_Code    

