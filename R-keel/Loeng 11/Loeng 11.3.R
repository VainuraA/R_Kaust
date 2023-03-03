library(tidyverse)
ilm = read_csv("http://www.tlu.ee/~jaagup/andmed/ilm/harkupaev_toorandmed.txt")

aov(DTA08~Kuu, data = ilm)
