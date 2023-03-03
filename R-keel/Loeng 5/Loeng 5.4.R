library(tidyverse)
tekst1= read_file("http://www.tlu.ee/~jaagup/andmed/keel/ajalehetekstid/Johnson_OL.txt")
tekst1 = str_to_lower(tekst)
tekst1=gsub("[^a-zõäöü0-9]", " ", tekst)[[1]]
tsonad=str_split(tekst, " ")[[1]]
head(tsonad)
str_length(tsonad)
tsonad=tsonad[str_length(tsonad)>0]
head(tsonad)
sagedused1=tibble(sona=tsonad) %>% group_by(sona) %>% summarise(kogus=n()) %>%
  arrange(desc(kogus))

tekst2=read_file("http://www.tlu.ee/~jaagup/andmed/keel/ajalehetekstid/Johnson_PM.txt")
tekst2=str_to_lower(tekst)
tekst2=gsub("[^a-zõäöü0-9]", " ", tekst)[[1]]
tsonad=str_split(tekst, " ")[[1]]
head(tsonad)
str_length(tsonad)
tsonad=tsonad[str_length(tsonad)>0]
head(tsonad)
sagedused2=tibble(sona=tsonad) %>% group_by(sona) %>% summarise(kogus=n()) %>%
  arrange(desc(kogus))
head(sagedused2)
