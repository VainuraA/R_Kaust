library(tidyverse)
sonad=read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")

sonad%>%select(taishaalikuid, sulghaalikuid, sonapikkus)%>%
  transmute(täish_osak = sonad$taishaalikuid/sonad$sonapikkus,sulgh_osak = sonad$sulghaalikuid/sonad$sonapikkus )%>%
  prcomp()%>%biplot(xlabs=sonad$sona)


