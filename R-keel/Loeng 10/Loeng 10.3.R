library(tidyverse)
andmed = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/korpus/doksonaliigid.txt")

korr =andmed %>% select_if(is.numeric) %>% cor()

min_väärtus = sort(korr)[1]
max_väärtus = sort(korr)[length(sort(korr))-17]
match(min_väärtus, korr)
match(max_väärtus, korr)
