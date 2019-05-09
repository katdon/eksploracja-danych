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

#Macierze korelacji
data <- data_train[,c(3:18,22:24)]
View(round(cor(data),2))
data_jadalnia <- data_train[,c(3,4,6,7,8,9,10,11,12,14,15,16,17,18,22,23,24)]
View(round(cor(data_jadalnia),2))

#Tworzymy model wielorakiej regresji liniowej dla jadalni
model <- lm(`3:Temperature_Comedor_Sensor`~., data = data_jadalnia)
summary(model)
model_2 <- lm(`3:Temperature_Comedor_Sensor`~ . - `12:Precipitacion`, data = data_jadalnia)
summary(model_2)
model_3 <- lm(`3:Temperature_Comedor_Sensor`~ . - `12:Precipitacion` - `17:Meteo_Exterior_Sol_Sud`, data = data_jadalnia)
summary(model_3)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(model_3)
# prognoza
install.packages('Metrics')
library(Metrics)
x <- data.frame(predict(model_3,data_test),data_test)
rmse(x[,4], x[,1]) # 0.2094367
# Sprawdzenie prognozy
data_test_2 <- data_test[,c(3,4,6,7,8,9,10,11,12,14,15,16,17,18,22,23,24)]
y <- data.frame(data_test_2,predict(model_3,data_test_2))
rmse(y[,1], y[,18])

#Tworzymy model wielorakiej regresji liniowej dla pokoju
data_pokoj <- data_train[,c(3,4,6,7,8,9,10,11,12,14,15,16,17,18,22,23,24)]
View(round(cor(data_pokoj),2))
model_p <- lm(`4:Temperature_Habitacion_Sensor`~., data = data_pokoj)
summary(model_p)
model_p_1 <- lm(`4:Temperature_Habitacion_Sensor`~ . - `17:Meteo_Exterior_Sol_Sud`, data = data_pokoj)
summary(model_p_1)
model_p_2 <- lm(`4:Temperature_Habitacion_Sensor` ~ . - `17:Meteo_Exterior_Sol_Sud` - `18:Meteo_Exterior_Piranometro`, data = data_pokoj)
summary(model_p_2)
model_p_3 <- lm(`4:Temperature_Habitacion_Sensor` ~ . - `17:Meteo_Exterior_Sol_Sud` - `18:Meteo_Exterior_Piranometro` - `10:Lighting_Comedor_Sensor`, data = data_pokoj)
summary(model_p_3)
# prognoza
install.packages('Metrics')
library(Metrics)
data_test_3 <- data_test[,c(3,4,6,7,8,9,10,11,12,14,15,16,17,18,22,23,24)]
y <- data.frame(data_test_3,predict(model_p_3,data_test_3))
rmse(y[,2], y[,18]) # 0.1706739
step(model_p) # wyszlo to samo

#Tworzymy model wielorakiej regresji liniowej dla dworu
data_dwor <- data_train[,c(3,4,6,7,8,9,10,11,12,14,15,16,17,18,22,23,24)]
model_d <- lm(`22:Temperature_Exterior_Sensor`~., data = data_dwor)
summary(model_d)
model_d_1 <- lm(`22:Temperature_Exterior_Sensor`~ . - `24:Day_Of_Week`, data = data_dwor)
summary(model_d_1)
model_d_2 <- lm(`22:Temperature_Exterior_Sensor` ~ . - `17:Meteo_Exterior_Sol_Sud` - `24:Day_Of_Week`, data = data_dwor)
summary(model_d_2)
# prognoza i sprawdzenie z prognoza z danych
data_test_4 <- data_test[,c(3,4,6,7,8,9,10,11,12,14,15,16,17,18,22,23,24)]
y <- data.frame(data_test_4,predict(model_d_2,data_test_4),data_test[,5])
data_test[,5] <- as.data.frame(data_test[,5])
rmse(y[,19], y[,18])
rmse(y[,15],y[,18])
rmse(y[,15],y[,19])

#Obcinamy dane
#Obserwacje odstaj¹ce zawartosci CO2 dla jadalni i pokoju
data_train <- as.data.frame(data_train)
boxplot(data_train[,6], data_train[,7], col = c('blue', 'red'), 
        names = c('Jadalnia','Pokój'), ylab = 'ppm', main = 'Zawartoœæ CO2')
summary(data_train[,6:7])
odst_co2_j <- (211.2 - 200.9) * 1.5 + 211.2
data_train_1 <- data_train[data_train[,6] < odst_co2_j,]
boxplot(data_train_1[,6], data_train_1[,7], col = c('blue', 'red'), 
        names = c('Jadalnia','Pokój'), ylab = 'ppm', main = 'Zawartoœæ CO2')
summary(data_train_1[,7])
odst_co2_p <- (212.8 - 202.4) * 1.5 + 212.8
data_train_1 <- data_train_1[data_train_1[,7] < odst_co2_p,]
boxplot(data_train_1[,6], data_train_1[,7], col = c('blue', 'red'), 
        names = c('Jadalnia','Pokój'), ylab = 'ppm', main = 'Zawartoœæ CO2 bez obserwacji odstaj¹cych')

#Obserwacje odstaj¹ce natê¿enia oœwietlenia dla jadalni i pokoju
summary(data_train_1[,10:11])
boxplot(data_train_1[,10], data_train_1[,11], col = c('blue', 'red'), 
        names = c('Jadalnia','Pokój'), ylab = 'ppm', main = 'Natê¿enie oœwietlenia')
odst_lux_j <- (26.69 - 11.52) * 1.5 + 26.69
data_train_1 <- data_train_1[data_train_1[,10] < odst_lux_j,]
boxplot(data_train_1[,10], data_train_1[,11], col = c('blue', 'red'), 
        names = c('Jadalnia','Pokój'), ylab = 'ppm', main = 'Natê¿enie oœwietlenia')
summary(data_train_1[,11])
odst_lux_p <- (23.42 - 13.18) * 1.5 + 23.42
data_train_1 <- data_train_1[data_train_1[,11] < odst_lux_p,]
boxplot(data_train_1[,10], data_train_1[,11], col = c('blue', 'red'), 
        names = c('Jadalnia','Pokój'), ylab = 'ppm', main = 'Natê¿enie oœwietlenia bez obserwacji odstaj¹cych')

#Sprawdzamy ile zosta³o obserwacji odstaj¹cych
summary(data_train_1[,10:11])
dim(data_train_1[data_train_1[,10] >= (17.17 - 11.52) * 1.5 + 17.17,]) # 25 obserwacji odstaj¹cych
dim(data_train_1[data_train_1[,11] >= (22.60 - 13.10) * 1.5 + 22.60,]) # 11 obserwacji odstaj¹cych
dim(data_train_1[,10])
summary(data_train[,10:11])
dim(data_train[data_train[,10] >= (31.22 - 11.59) * 1.5 + 31.22,]) # 351 obserwacji odstaj¹cych
dim(data_train[data_train[,11] >= (52.06 - 13.27) * 1.5 + 52.06,]) # 415 obserwacji odstaj¹cych

# robimy nowe modele dla danych bez obserwacji odstajacych
#Macierze korelacji
data_jadalnia_1 <- data_train_1[,c(3,4,6,7,8,9,10,11,12,14,15,16,17,18,22,23,24)]
View(round(cor(data_jadalnia_1),2))

#Tworzymy model wielorakiej regresji liniowej dla jadalni
model_ob_1 <- lm(`3:Temperature_Comedor_Sensor`~., data = data_jadalnia_1)
summary(model_ob_1)
model_ob_2 <- lm(`3:Temperature_Comedor_Sensor`~ . - `10:Lighting_Comedor_Sensor`, data = data_jadalnia_1)
summary(model_ob_2)
model_ob_3 <- lm(`3:Temperature_Comedor_Sensor`~ . - `10:Lighting_Comedor_Sensor` - `15:Meteo_Exterior_Sol_Oest`, data = data_jadalnia_1)
summary(model_ob_3)
model_ob_4 <- lm(`3:Temperature_Comedor_Sensor`~ . - `10:Lighting_Comedor_Sensor` - `15:Meteo_Exterior_Sol_Oest` - `12:Precipitacion`, data = data_jadalnia_1)
summary(model_ob_4)
model_ob_5 <- lm(`3:Temperature_Comedor_Sensor`~ . - `10:Lighting_Comedor_Sensor` - `15:Meteo_Exterior_Sol_Oest` - `12:Precipitacion` - `16:Meteo_Exterior_Sol_Est`, data = data_jadalnia_1)
summary(model_ob_5)#ten w miare dobry
model_ob_6 <- lm(`3:Temperature_Comedor_Sensor`~ . - `10:Lighting_Comedor_Sensor` - `15:Meteo_Exterior_Sol_Oest` - `12:Precipitacion` - `16:Meteo_Exterior_Sol_Est` - `17:Meteo_Exterior_Sol_Sud`, data = data_jadalnia_1)
summary(model_ob_6)
model_ob_7 <- lm(`3:Temperature_Comedor_Sensor`~ . - `10:Lighting_Comedor_Sensor` - `15:Meteo_Exterior_Sol_Oest` - `12:Precipitacion` - `16:Meteo_Exterior_Sol_Est` - `17:Meteo_Exterior_Sol_Sud` - `18:Meteo_Exterior_Piranometro`, data = data_jadalnia_1)
summary(model_ob_7) #ok
# prognoza
install.packages('Metrics')
library(Metrics)
x <- data.frame(predict(model_ob_7,data_test),data_test)
rmse(x[,4], x[,1]) # 0.238235
y <- data.frame(predict(model_ob_5,data_test),data_test)
rmse(y[,4], y[,1]) # 0.2377279
BIC(model_3, model_ob_5, model_ob_7)
AIC(model_3, model_ob_5, model_ob_7)
#Wybieramy model_ob_7 - ma niskie kryterium Akaike i niskie rsme

#Tworzymy model wielorakiej regresji liniowej dla pokoju
data_pokoj_1 <- data_train_1[,c(3,4,6,7,8,9,10,11,12,14,15,16,17,18,22,23,24)]
View(round(cor(data_pokoj_1),2))
model_p_ob_1 <- lm(`4:Temperature_Habitacion_Sensor`~ ., data = data_pokoj_1)
summary(model_p_ob_1)
model_p_ob_2 <- lm(`4:Temperature_Habitacion_Sensor`~ . - `16:Meteo_Exterior_Sol_Est`, data = data_pokoj_1)
summary(model_p_ob_2)
model_p_ob_3 <- lm(`4:Temperature_Habitacion_Sensor`~ . - `16:Meteo_Exterior_Sol_Est` - `11:Lighting_Habitacion_Sensor`, data = data_pokoj_1)
summary(model_p_ob_3)
model_p_ob_4 <- lm(`4:Temperature_Habitacion_Sensor`~ . - `16:Meteo_Exterior_Sol_Est` - `11:Lighting_Habitacion_Sensor` - `15:Meteo_Exterior_Sol_Oest`, data = data_pokoj_1)
summary(model_p_ob_4)
# prognoza
z <- data.frame(predict(model_p_ob_4,data_test),data_test)
rmse(z[,4], z[,1]) # 0.5654549
BIC(model_p_3, model_p_ob_4)
AIC(model_p_3, model_p_ob_4)
# wybieramy model_p_ob_4; kryteria BIC i AIC s¹ duzo lepsze niz dla modelu z obserwacjami odstajacymi

# tabelki z podsumowaniem modeli pod prezentacjê
install.packages("snakecase")
library(snakecase)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(model_3)
tab_model(model_ob_7)
tab_model(model_p_3)
tab_model(model_p_ob_4)
#tab_model(model_d_2)

#PCA
model_pca <- prcomp(data_all[,c(3:18,22:23)], scale = T)
summary(model_pca)
data_all[,24] <- ifelse(data_all[,24]<2,1,
                        ifelse(2<=data_all[,24] & data_all[,24]<3,2,
                               ifelse(3<=data_all[,24] & data_all[,24]<4,3,
                                      ifelse(4<=data_all[,24] & data_all[,24]<5,4,
                                             ifelse(5<=data_all[,24] & data_all[,24]<6,5,
                                                    ifelse(6<=data_all[,24] & data_all[,24]<7,6,7))))))
as.data.frame(summary(model_pca)$importance)
model_pca$rotation # pokazuje w jakiej mierze poszczegolne cechy wyjasniaja skladowe glowne
plot(model_pca$x, xlab = paste('PC1 (', 100* summary(model_pca)$importance[2],'%)'), ylab = paste('PC2 (', 100 *summary(model_pca)$importance[5], '%)'), col = data_all[,24], main = "PCA")
table(data_all[,24])


