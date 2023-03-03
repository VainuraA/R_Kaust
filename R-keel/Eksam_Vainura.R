library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(shiny)
library(ggpubr)
library(plotly)
filmi_andmed <- read_csv("marvel.csv")
#Kuna tabelis ei ole World wide tulpa lisatud ka avamis nädalavahetus summa, siis lisasin eraldi tulba, 
#kus on kõik rahad kokku arvestatud
filmi_andmed = filmi_andmed%>%mutate(All_over = Worldwide + `Opening weekend(North America)`)


#20 63st, et palju neist on walt disney omad ja mida see võiks öelda tuleviku kohta

prop.test(20,63)

#Alustuseks uuriks mõningaid n-ö üldisi asju

#Mitu filmi iga stuudio väljastas ja millised kõige rohkem/vähem

stuudiod = filmi_andmed %>% count(`Distributor(s)`)
stuudiod[which.max(stuudiod$n),1]
stuudiod[which.min(stuudiod$n),1]
plot_ly(stuudiod, labels =stuudiod$`Distributor(s)` , values =stuudiod$n, type = 'pie',
        textposition = 'none',
        hoverinfo = 'text',
        text = stuudiod$`Distributor(s)`)

stuudiod2 = filmi_andmed%>%group_by(`Distributor(s)`)%>%summarise(budget= sum(Budget), open_wknd = sum(`Opening weekend(North America)`), North_america = sum(`North America`),
                                                                  other_ter = sum(`Other territories`), worldw=sum(Worldwide),
                                                                  allover = sum(All_over))%>%mutate(profit = allover- budget, n=stuudiod$n)


fox_20th = järj_kp%>%filter(`Distributor(s)`=="20th Century Fox")
walt_disney = järj_kp%>%filter(`Distributor(s)`=="Walt Disney Studios Motion Pictures")
sony = järj_kp%>%filter(`Distributor(s)`=="Sony Pictures")

#Mis filmidel oli suurim/ vähim eelarve

eelarve = filmi_andmed%>%select(Title, Budget, `Distributor(s)`)%>%mutate(Budget = Budget/1000000)%>%
  rename(Budget_millions=Budget)
plot1 <- head(eelarve %>% arrange(Budget_millions)) %>% ggplot(aes(x=Title, y= Budget_millions, fill = Title))+ geom_col()+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
plot2 <- tail(eelarve %>% arrange(Budget_millions)) %>% ggplot(aes(x=Title, y= Budget_millions, fill = Title))+ geom_col()
ggarrange(plot1, plot2, ncol = 2)


#Mis filmidel oli suurim/ vähim avamis nädalavahetuse summa

opening_wknd = filmi_andmed %>% select(Title, `Opening weekend(North America)`)%>%
  rename(open_wknd = `Opening weekend(North America)` )
head(opening_wknd %>% arrange(open_wknd)) %>% ggplot(aes(x=Title, y= open_wknd, fill = Title))+ geom_col()
tail(opening_wknd %>% arrange(open_wknd)) %>% ggplot(aes(x=Title, y= open_wknd, fill = Title))+ geom_col()

#Mis filmidel on suurim/vähim üld teenistus Põhja-Ameerikas

north_America = filmi_andmed %>% select(Title, `North America`)
head(north_America %>% arrange(`North America`)) %>% ggplot(aes(x=Title, y= `North America`, fill = Title))+ geom_col()
tail(north_America %>% arrange(`North America`)) %>% ggplot(aes(x=Title, y= `North America`, fill = Title))+ geom_col()

#Mis filmidel on suurim/ vähim üld teenistus muudel aladel

muu_ala = filmi_andmed %>% select(Title, `Other territories`)
head(muu_ala %>% arrange(`Other territories`)) %>% ggplot(aes(x=Title, y= `Other territories`, fill = Title))+ geom_col()
tail(muu_ala %>% arrange(`Other territories`)) %>% ggplot(aes(x=Title, y= `Other territories`, fill = Title))+ geom_col()

#Mis filmidel on suurim/ vähim kogu teenistus üle maailma

üle_maailma = filmi_andmed %>% select(Title, Worldwide)
head(üle_maailma %>% arrange(Worldwide)) %>% ggplot(aes(x=Title, y= Worldwide, fill = Title))+ geom_col()
tail(üle_maailma %>% arrange(Worldwide)) %>% ggplot(aes(x=Title, y= Worldwide, fill = Title))+ geom_col()

#Mis filmidel on suurim/ vähim teenistus kogu arvestuses

kogu = filmi_andmed %>% select(Title, All_over)
head(kogu %>% arrange(All_over)) %>% ggplot(aes(x=Title, y= All_over, fill = Title))+ geom_col()
tail(kogu %>% arrange(All_over)) %>% ggplot(aes(x=Title, y= All_over, fill = Title))+ geom_col()


#Järgmiseks uurin millistel filmidel olid suurimad/väikseimad kasumid 

#budgeti ja kasumi graafik
filmi1 =filmi_andmed%>%mutate(Profit= All_over-Budget)
filmi1%>%ggplot(aes(Budget, Profit))+geom_point()
#winter soldier on see mis pekki viib 

#avamis nädalavahetuse suhtes

tulu_avanv = filmi_andmed%>% select(Title, Budget, `Opening weekend(North America)`)%>% 
  mutate(Profit= `Opening weekend(North America)`-Budget)
head(tulu_avanv %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()
tail(tulu_avanv %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()

#üldiselt Põhja-Ameerika suhtes

tulu_pA = filmi_andmed%>% select(Title, Budget, `North America`)%>% 
  mutate(Profit= `North America`-Budget)
head(tulu_pA %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()
tail(tulu_pA %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()

# Muude territooriumite suhtes

tulu_mA = filmi_andmed%>% select(Title, Budget, `Other territories`)%>% 
  mutate(Profit= `Other territories`-Budget)
head(tulu_mA %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()
tail(tulu_mA %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()

# Kogu maailma suhtes

tulu_Ww = filmi_andmed%>% select(Title, Budget, Worldwide)%>% 
  mutate(Profit= Worldwide-Budget)
head(tulu_Ww %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()
tail(tulu_Ww %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()

# Kõige suhtes

tulu_Ao = filmi_andmed%>% select(Title, Budget, All_over)%>% 
  mutate(Profit= All_over-Budget)
head(tulu_Ww %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()
tail(tulu_Ww %>% arrange(Profit)) %>% ggplot(aes(x=Title, y= Profit, fill = Title))+ geom_col()

# Järgmiseks uuriks, kas eelarve suuruse ja kogu tulu vahel on mingi seos
#Siin saan kasutada eelmises leitud tabelit uuesti

cor(tulu_Ao$Budget, tulu_Ao$Profit) 
# korrelatsioon on negatiivne, seega ei saa öelda, et eelarve suuruse ja tulu suuruse vahel oleks seos
tulu_Ao %>% ggplot(aes(Budget,Profit))+ geom_point()

#Leian kõik filmi seeriad, milles on vähemalt kaks filmi, õnneks on andmeid suhteliselt
#vähe ning saan iga ühe neist nime järgi üles leida

Iron_Man = filmi_andmed%>% filter(str_detect(Title, "Iron"))
Avengers = filmi_andmed%>% filter(str_detect(Title, "Avengers"))
Captain_America = filmi_andmed%>% filter(str_detect(Title, "America"))
Venom = filmi_andmed%>% filter(str_detect(Title, "Venom"))
X_Men = filmi_andmed%>% filter(str_detect(Title, "X-Men"))
Spider_Man = filmi_andmed%>% filter(str_detect(Title, "Spider-Man"))
Deadpool = filmi_andmed%>% filter(str_detect(Title, "Deadpool"))
Thor = filmi_andmed%>% filter(str_detect(Title, "Thor"))
Guardians = filmi_andmed%>% filter(str_detect(Title, "Guardians"))
Hulk = filmi_andmed%>% filter(str_detect(Title, "Hulk"))
Fantastic4 = filmi_andmed%>% filter(str_detect(Title, "Fantastic Four"))
Blade = filmi_andmed%>% filter(str_detect(Title, "Blade"))
Ghost_Rider = filmi_andmed%>% filter(str_detect(Title, "Ghost"))
Ant_Man = filmi_andmed%>% filter(str_detect(Title, "Ant-Man"))
Punisher = filmi_andmed%>% filter(str_detect(Title, "Punisher"))

#Siin eraldan originaal andmestikust kõik need filmid, mis on kuskil seerias,
#et saada kätte need filmid, mis on n-ö üksikud

f1 = filmi_andmed[!grepl("Iron", filmi_andmed$Title),]
f2 = f1[!grepl("Avengers", f1$Title),]
f3 = f2[!grepl("America", f2$Title),]                        
f4 = f3[!grepl("Venom", f3$Title),]
f5 = f4[!grepl("X-Men", f4$Title),]
f6 = f5[!grepl("Spider-Man", f5$Title),]
f7 = f6[!grepl("Deadpool", f6$Title),]
f8 = f7[!grepl("Thor", f7$Title),]
f9 = f8[!grepl("Guardians", f8$Title),]
f10 = f9[!grepl("Hulk", f9$Title),]
f11 = f10[!grepl("Fantastic Four", f10$Title),]
f12 = f11[!grepl("Blade", f11$Title),]
f13 = f12[!grepl("Ghost", f12$Title),]
f14 = f13[!grepl("Ant-Man", f13$Title),]
üksikud_filmid = f14[!grepl("Punisher", f14$Title),]


#Teeks mõned graafikud kuupäevade järgi ka

järj_kp = filmi_andmed[order(as.Date(filmi_andmed$`Release date(United States)`, format="%d.%m.%Y")),]%>%
  mutate(`Release date(United States)`= as.Date(`Release date(United States)`, "%d.%m.%Y"))%>%
  mutate(Profit= All_over-Budget)

aasta_järj_kp = järj_kp%>%group_by(year=format(`Release date(United States)`, "%Y"))%>%
  summarise(budget = sum(Budget), all_o = sum(All_over))%>% mutate(profit=all_o-budget)



plot(aasta_järj_kp$year, aasta_järj_kp$budget)
lines(aasta_järj_kp$year, aasta_järj_kp$budget)

aasta1 = aasta_järj_kp%>%mutate(year=as.numeric(year))
aasta1%>%ggplot(aes(x=year, y=budget))+ geom_line()
#Tegin graafikud eelarve kohta võrdluses
aasta_järj_kp%>%ggplot(aes(x = year, y = budget, fill = year))+ geom_col()
aasta_järj_kp%>%ggplot(aes(x = all_o, y = budget))+geom_point()+geom_line()+geom_text(aes(label = year), size =3, vjust = -0.5)
aasta_järj_kp%>%ggplot(aes(x = Profit, y = budget))+geom_point()+geom_line()+geom_text(aes(label = year), size =3, vjust = -0.5)



#shiny joonis, mis näitab vastavalt aastale välja lastud filmi, koos selle nime ja selle kasumiga/kahjumiga
ui <- fluidPage(
  titlePanel("Filmide numbrid aastate lõikes"),
  sliderInput("aasta", "Aasta:", min = 1986,  max = 2021, value = 2020, step = 1),
  plotOutput("distPlot")
)

server <- function(input, output) {
  data_filtered <- reactive({
    järj_kp %>% filter(year(`Release date(United States)`)==input$aasta)
  })
  output$distPlot <- renderPlot({
    ggplot(data_filtered(), aes(x = `Release date(United States)`, y = Profit)) +
      geom_point()+geom_text(aes(label = Title), size = 3, vjust = -0.5)
  })
}

shinyApp(ui = ui, server = server)