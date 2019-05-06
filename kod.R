library(readr)
setwd("C:/naUczelnie/aipd/4sem/warsztaty_epsk_analiza/repo_nasze/eksploracja-danych")
getwd()
# wczytujemy dane i je wstêpnie ogladamy
data_train <- read_table2("NEW-DATA-1.T15.txt")
data_test <- read_table2("NEW-DATA-2.T15.txt")
# sprawdzamy wartosci, jakie przyjmuje dana zmienna 
unique(data_train[,19])
unique(data_train[,20])
unique(data_train[,21])
unique(data_test[,19])
unique(data_test[,20])
unique(data_test[,21])
# same zera zatem nie ma sensu patrzec na te zmienne
# kolumna nr 5 zawiera prognoze temperatury
unique(data_train[,1])
# 30 dni w danych trenujacych
unique(data_test[,1])
# 15 dni w danych testowych
# do ogolnych analiz (przed modelowaniem) patrzymy na wszystkie dane
data_all <- rbind(data_train, data_test)
#data_all <- data_all[,-c(19,20,21)]
View(summary(data_all))

library(ggplot2)
install.packages("chron")
library(chron)
# laczymy date z godzina
Date_time = chron(dates=data_all[,1],times=data_all[,2],format=c('d/m/y','h:m:s'))
data_all <- as.data.frame(data_all)
data_all[,25] <- Date_time
#robimy wykres; dodajemy szum losowy by punkty sie nie nakladaly
ggplot(data = data_all) +
  geom_point(mapping = aes(x = `1:Date`, y = `3:Temperature_Comedor_Sensor`), position = 'jitter')
#robimy wykres: porównujemy temperature jadalni z pokojem
plot(data_all[,25],data_all[,4], col = 'red', xlab = "Data", ylab = 'Stopnie Celsjusza', main = 'Temperatura')
points(data_all[,25],data_all[,3], col = 'blue')
legend('topleft', legend=c("Pokoj", "Jadalnia"), col = c('red','blue'), pch = 21)

#robimy wykres: porównujemy temperature jadalni z pokojem i z temp na dworze
plot(data_all[,25],data_all[,22], col = 'green', xlab = "Data", ylab = 'Stopnie Celsjusza', main = 'Temperatura')
points(data_all[,25],data_all[,3], col = 'blue')
points(data_all[,25],data_all[,4], col = 'red')
legend('topleft', legend=c("Pokoj", "Jadalnia", "Poza domem"), col = c('red','blue', 'green'), pch = 21, cex=0.45)



