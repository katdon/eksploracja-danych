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
data_all <- as.data.frame(data_all)
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

#robimy wykres: porównujemy zawartosc CO2 w jadalni i pokoju
plot(data_all[,25],data_all[,7], col = 'red', xlab = "Data", ylab = 'ppm', main = 'Zawartoœæ CO2')
points(data_all[,25],data_all[,6], col = 'blue')
legend('topleft', legend=c("Pokój", "Jadalnia"), col = c('red','blue'), pch = 21, cex=0.6)

#robimy wykres: sprawdzamy kiedy dokladnie byly skoki zawartosci
plot(data_all[1240:1300,25],data_all[1240:1300,7], col = 'red', xlab = "Data", ylab = 'ppm', main = 'Zawartoœæ CO2')
points(data_all[1240:1300,25],data_all[1240:1300,6], col = 'blue')
legend('topleft', legend=c("Pokój", "Jadalnia"), col = c('red','blue'), pch = 21, cex=0.6)
plot(data_all[80:120,25],data_all[80:120,7], col = 'red', xlab = "Data", ylab = 'ppm', main = 'Zawartoœæ CO2')
points(data_all[80:120,25],data_all[80:120,6], col = 'blue')
legend('topleft', legend=c("Pokój", "Jadalnia"), col = c('red','blue'), pch = 21, cex=0.6)
plot(data_all[750:800,25],data_all[750:800,7], col = 'red', xlab = "Data", ylab = 'ppm', main = 'Zawartoœæ CO2')
points(data_all[750:800,25],data_all[750:800,6], col = 'blue')
legend('topleft', legend=c("Pokój", "Jadalnia"), col = c('red','blue'), pch = 21, cex=0.6)

#robimy wykres: porównujemy wilgotnoœæ powietrza w pokoju i jadalni
plot(data_all[,25],data_all[,9], col = 'red', xlab = "Data", ylab = '%', main = 'Wilgotnoœæ')
points(data_all[,25],data_all[,8], col = 'blue')
legend('topleft', legend=c("Pokój", "Jadalnia"), col = c('red','blue'), pch = 21, cex=0.6)

#robimy wykres: porównujemy wilgotnoœæ powietrza w pokoju, jadalni na zewnatrz
plot(data_all[,25],data_all[,23], col = 'green', xlab = "Data", ylab = '%', main = 'Wilgotnoœæ')
points(data_all[,25],data_all[,8], col = 'blue')
points(data_all[,25],data_all[,9], col = 'red')
legend('topleft', legend=c("Pokój", "Jadalnia", 'Poza domem'), col = c('red','blue', 'green'), pch = 21, cex=0.6)

# boxploty temperatur w pokoju, jadalni oraz poza domem 
boxplot(data_all[,3], data_all[,4], data_all[,22], col = c('blue', 'red', 'green'), 
        names = c('Jadalnia','Pokój','Poza domem'), ylab = 'Stopnie Celsjusza', main = 'Temperatura')

# boxploty CO2 w pokoju, jadalni
boxplot(data_all[,6], data_all[,7], col = c('blue', 'red'), 
        names = c('Jadalnia','Pokój'), ylab = 'ppm', main = 'Zawartoœæ CO2')

# boxploty wilgotnoœci w pokoju, jadalni oraz poza domem 
boxplot(data_all[,8], data_all[,9], data_all[,23], col = c('blue', 'red', 'green'), 
        names = c('Jadalnia','Pokój','Poza domem'), ylab = '%', main = 'Wilgotnoœæ')



#robimy wykres: porównujemy natezenie oswietlenia w pokoju i jadalni
plot(data_all[1:1000,25],data_all[1:1000,11], col = 'red', ylab = 'Lux', xlab='Data', main = 'Natê¿enie oœwietlenia')
points(data_all[1:1000,25],data_all[1:1000,10], col = 'blue')
legend('topleft', legend=c("Pokój", "Jadalnia"), col = c('red','blue'), pch = 21, cex=0.6)

# boxploty natezenia swiatla w pokoju, jadalni
boxplot(data_all[,10], data_all[,11], col = c('blue', 'red'), 
        names = c('Jadalnia','Pokój'), ylab = 'Lux', main = 'Natê¿enie')


#wiatr
plot(data_all[1:1000,25],data_all[1:1000,14], col = 'green', xlab = "Data", ylab = 'm/s', main = 'Wiatr')
hist(data_all[,14], xlab = "m/s", ylab = 'Czêstoœæ', main = 'Wiatr', col = "green")
