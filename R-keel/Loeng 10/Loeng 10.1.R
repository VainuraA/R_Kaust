library(tidyverse)
sonad=read_csv("http://www.tlu.ee/~jaagup/andmed/keel/kunglarahvas_lambipirn_pikkused_haalikud.txt")
head(sonad)

cor(sonad$sonapikkus, sonad$taishaalikuid)

sonad%>%ggplot(aes(sonapikkus, taishaalikuid)) + geom_point()
sonad%>%ggplot(aes(sonapikkus, taishaalikuid)) + geom_point(position = "jitter")
sonad%>%ggplot(aes(sonapikkus, sulghaalikuid)) + geom_point(position = "jitter")

#Leidke korrelatsioon täishäälikute ja sulghäälikute arvu vahel ja tehke joonis
cor(sonad$taishaalikuid, sonad$sulghaalikuid)
sonad%>%ggplot(aes(taishaalikuid, sulghaalikuid)) + geom_point(position = "jitter")
sonad%>%ggplot(aes(taishaalikuid, sulghaalikuid)) + geom_point()

#Leidke korrelatsioon sõnas täishäälikute osakaalu ja sulghäälikute osakaalu vahel, lisage joonis
cor((sonad$taishaalikuid/sonad$sonapikkus), (sonad$sulghaalikuid/sonad$sonapikkus))
täisosak = (sonad$taishaalikuid/sonad$sonapikkus)
sulgosak = (sonad$sulghaalikuid/sonad$sonapikkus)
sonad%>%ggplot(aes(täisosak,sulgosak))+
                 geom_point()
               
               