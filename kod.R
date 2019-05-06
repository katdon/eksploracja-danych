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
summary(data_all)
