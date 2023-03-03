library(tidyverse)
sonad = read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
t.test(sonad %>% sample_n(50) %>% .$taishaalikuid, 
         sonad %>% sample_n(40) %>% .$sulghaalikuid)
t.test(sonad $taishaalikuid, sonad$sulghaalikuid, paired = TRUE)
