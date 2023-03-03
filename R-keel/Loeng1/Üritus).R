library(tidyverse)
sonad=read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haali
kud.txt")
head(sonad, 3)
max(sonad$sonapikkus)
filter(sonad, sonapikkus==3, taishaalikuid==3)

