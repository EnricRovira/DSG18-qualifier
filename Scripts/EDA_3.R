setwd("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0")

library(data.table)
library(sqldf)
library(lubridate)
library(anytime)
library(tidyverse)
library(dplyr)

#Challenge <- fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/Challenge_20180423.csv")
#Trade <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/Trade.csv"))
Trade_valido <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/Trade_valido.csv"))
Trade_Generado <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/Train_Generado.csv"))
test <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3//test.csv"))

Trade$TradeStatus <- as.factor(Trade$TradeStatus)
Trade$BuySell <- as.factor(Trade$BuySell)
Trade$CustomerInterest <- as.factor(Trade$CustomerInterest)

#6.762.021
Trade = Trade[!duplicated(Trade), ]
#6.691.753
str(Trade)
prop.table(table(Trade$CustomerInterest))


Trade$fecha <- ymd(Trade$TradeDateKey)
Trade$mes <- as.factor(month(Trade$fecha))
Trade$año <- as.factor(year(Trade$fecha))
Trade$dia <- as.factor(weekdays(Trade$fecha))
Trade$num_dia <- NA
Trade$num_dia <- ifelse(Trade$dia=='lunes',1,Trade$num_dia)
Trade$num_dia <- ifelse(Trade$dia=='martes',2,Trade$num_dia)
Trade$num_dia <- ifelse(Trade$dia=='miércoles',3,Trade$num_dia)
Trade$num_dia <- ifelse(Trade$dia=='jueves',4,Trade$num_dia)
Trade$num_dia <- ifelse(Trade$dia=='viernes',5,Trade$num_dia)
Trade$num_dia <- ifelse(Trade$dia=='sábado',6,Trade$num_dia)
Trade$num_dia <- ifelse(Trade$dia=='domingo',7,Trade$num_dia)

prop.table(table(Trade$CustomerInterest))
################################

Challenge$Cust_Bond <- paste(Challenge$CustomerIdx, Challenge$IsinIdx, sep = "-")
Trade$Cust_Bond <- paste(Trade$CustomerIdx, Trade$IsinIdx, sep = "-")
Trade$valido <- ifelse(Trade$Cust_Bond %in% Challenge$Cust_Bond, TRUE, FALSE)

Trade_valido <- Trade %>% filter (valido==TRUE)
prop.table(table(Trade_valido$CustomerInterest))
Trade_valido$valido <- NULL

#-- Eliminamos los anteriores al 20160104 para hacer semanas de Lunes a Domingo y no dejar ninguna semana partida
Trade_valido <- filter (Trade_valido, TradeDateKey >= 20160104)
prop.table(table(Trade_valido$CustomerInterest))

# Vamos a buscar las parejas Clientes/bonos que aparecen en challenge

aux <- Challenge %>% group_by(CustomerIdx, IsinIdx) %>% summarise()

#-- Creamos las 120 semanas entre la fecha min(20160104) y la fecha max(20180422)
#-- Creamos variables de tiempo en el intervalo de tiempo que tenemos en train. (mes, dia, ultima_semana)
#-- Creamos una copia para trabajar sobre ella y ahorrarnos el parseo de TradeDateKey que es muy molesto
copia <- Trade_valido

DateBeginWeek <- seq(as.Date("2016-01-04"), length=120, by="weeks")
DateEndLastWeek <- seq(as.Date("2016-02-01"), length=28, by="months")-1
DateDays <- seq(as.Date("2016-01-04"), length=838, by="days")
DateBeginMonth <- seq(as.Date("2016-01-01"), length=28, by="months")

for (i in 1:28){
  copia$resul1 <- between(copia$fecha, DateBeginMonth[i], DateEndLastWeek[i])
  copia$mes_intervalo[copia$resul1==TRUE]<-i
}
for (i in 1:28){
  copia$resul2 <- between(copia$fecha, DateEndLastWeek[i] - days(7), DateEndLastWeek[i])
  copia$ultima_semana[copia$resul2==TRUE]<-1
}

copia$ultima_semana <- ifelse(is.na(copia$ultima_semana), 0, copia$ultima_semana) 

for (i in 1:120){
  copia$resul3 <- between(copia$fecha, DateBeginWeek[i], DateBeginWeek[i] + days(7))
  copia$semana[copia$resul3==TRUE]<-i
}

for (i in 1:838){
  copia$dia_intervalo[copia$fecha %in% DateDays[i]] <- i
}

#--Asignamos las columnas auxiliares y borramos la copia para liberar espacio

Trade_valido$mes_intervalo <- copia$mes_intervalo
Trade_valido$semana <- copia$semana
Trade_valido$ultima_semana <- copia$ultima_semana
Trade_valido$dia_intervalo <- copia$dia_intervalo

summary(Trade_valido)
rm(copia)

#Añadimos nuevas variables que nos ayudaran a entende mejor el problema y crear nuevas sub_variables
Trade_valido$Holding <- ifelse(Trade_valido$TradeStatus=='Holding', 1, 0)
Trade_valido$Done <- ifelse(Trade_valido$TradeStatus=='Done', 1, 0)

ggplot(Trade_valido, aes(x = mes_intervalo, fill=CustomerInterest)) + 
  geom_bar()
ggplot(Trade_valido, aes(x = semana, fill=CustomerInterest)) + 
  geom_bar()
ggplot(Trade_valido, aes(x = mes, fill=CustomerInterest)) + 
  geom_bar()
ggplot(Trade_valido, aes(x = a?o, fill=CustomerInterest)) + 
  geom_bar()
ggplot(Trade_valido, aes(x = BuySell, fill=CustomerInterest)) + 
  geom_bar()
ggplot(Trade_valido, aes(x = TradeStatus, fill=CustomerInterest)) + 
  geom_bar()

#################################################
#-- Vamos a buscar parejas de (Cliente-Bono) que hayan mostrado interes en los 6 meses anteriores a la fecha actual
rango_seis_meses <- Trade_valido %>% 
  group_by(CustomerIdx, IsinIdx) %>% 
  summarise() %>% 
  arrange(CustomerIdx, IsinIdx)

# Seleccionamos las variables que necesitamos y ordenamos
copia_ci <- data.table(Trade_valido [, c("CustomerIdx", "IsinIdx", "CustomerInterest", "semana")])
copia_ci <- copia_ci %>% arrange (CustomerIdx, IsinIdx, semana)
Trade_valido <- Trade_valido %>% arrange (CustomerIdx, IsinIdx, semana)

copia_ci$CustomerInterest <- as.numeric(copia_ci$CustomerInterest)-1
table(copia_ci$CustomerInterest)
str(copia_ci)

cliente <- rango_seis_meses$CustomerIdx
bono <- rango_seis_meses$IsinIdx
copia_ci <- data.table(copia_ci)
setkey(copia_ci, CustomerIdx, IsinIdx)

v1 = numeric(nrow(copia_ci))
v2 = numeric(nrow(copia_ci))
v3 = numeric(nrow(copia_ci))
v4 = numeric(nrow(copia_ci))
v5 = numeric(nrow(copia_ci))
v6 = numeric(nrow(copia_ci))
v12 = numeric(nrow(copia_ci))
v27 = numeric(nrow(copia_ci))

ind <- 0
#12min
system.time({
  for (i in 1:nrow(rango_seis_meses)){
    aaa <- copia_ci[.(cliente[i], bono[i])]
    for (j in 1:nrow(aaa)){
      actual <- aaa$semana[j]
      v1[ind+j] <- sum(aaa$CustomerInterest[aaa$semana >= actual - 1 & aaa$semana < actual])
      v2[ind+j] <- sum(aaa$CustomerInterest[aaa$semana >= actual - 2 & aaa$semana < actual])
      v3[ind+j] <- sum(aaa$CustomerInterest[aaa$semana >= actual - 3 & aaa$semana < actual])
      v4[ind+j] <- sum(aaa$CustomerInterest[aaa$semana >= actual - 4 & aaa$semana < actual])
      v5[ind+j] <- sum(aaa$CustomerInterest[aaa$semana >= actual - 5 & aaa$semana < actual])
      v6[ind+j] <- sum(aaa$CustomerInterest[aaa$semana >= actual - 6 & aaa$semana < actual])
      v12[ind+j] <- sum(aaa$CustomerInterest[aaa$semana >= actual - 12 & aaa$semana < actual])
      v27[ind+j] <- sum(aaa$CustomerInterest[aaa$semana >= actual - 26 & aaa$semana < actual])
    }
    ind <- nrow(aaa) + ind
  }
})

summary(v27)
Trade_valido$num_interest1 <- v1
Trade_valido$num_interest2 <- v2
Trade_valido$num_interest3 <- v3
Trade_valido$num_interest4 <- v4
Trade_valido$num_interest5 <- v5
Trade_valido$num_interest6 <- v6
Trade_valido$num_interest12 <- v12
Trade_valido$num_interest27 <- v27
summary(Trade_valido)


write.csv(Trade_valido,"C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/Trade_valido.csv")
rm(copia_ci, rango_seis_meses, cliente, bono, v1, v2, v3, v4, v5, v6, v12, v27, j, ind, actual, aaa)

##-- Hacemos lo mismo para test(Challenge)
# Vamos a buscar las parejas Clientes/bonos que aparecen en challenge

aux <- Challenge %>% 
  group_by(CustomerIdx, IsinIdx) %>% 
  summarise() %>% 
  arrange(CustomerIdx, IsinIdx)

Challenge$semana <- max(Trade_valido$semana) + 1
Challenge$dia_intervalo <- max(Trade_valido$dia_intervalo) + 3
copia_test <- data.table(Challenge)

copia_grupos <- data.table(Trade_valido[, c("CustomerIdx", "IsinIdx", "CustomerInterest", "semana")])
copia_grupos <- data.table(copia_grupos %>% arrange(CustomerIdx, IsinIdx))

copia_grupos$CustomerIdx <- as.numeric(copia_grupos$CustomerIdx)
copia_grupos$IsinIdx <- as.numeric(copia_grupos$IsinIdx)

setkey(copia_test, CustomerIdx, IsinIdx)
clientes_test <- aux$CustomerIdx
bonos_test <- aux$IsinIdx

# Defino actual como constante porque siempre vale lo mismo en test
semana_test <- 121
#i <- 19
copia_grupos$CustomerInterest <- as.numeric(copia_grupos$CustomerInterest)
table(copia_grupos$CustomerInterest)
setkey(copia_grupos, CustomerIdx, IsinIdx)

v1 <- numeric(nrow(aux))
v2 <- numeric(nrow(aux))
v3 <- numeric(nrow(aux))
v4 <- numeric(nrow(aux))
v5 <- numeric(nrow(aux))
v6 <- numeric(nrow(aux))
v12 <- numeric(nrow(aux))
v27 <- numeric(nrow(aux))
#7min
system.time({
  for (i in 1:nrow(aux)){
    bbb <- copia_grupos[.(clientes_test[i], bonos_test[i])]
    v1[i] <- sum(bbb$CustomerInterest[bbb$semana >= semana_test - 1 & bbb$semana < semana_test])
    v2[i] <- sum(bbb$CustomerInterest[bbb$semana >= semana_test - 2 & bbb$semana < semana_test])
    v3[i] <- sum(bbb$CustomerInterest[bbb$semana >= semana_test - 3 & bbb$semana < semana_test])
    v4[i] <- sum(bbb$CustomerInterest[bbb$semana >= semana_test - 4 & bbb$semana < semana_test])
    v5[i] <- sum(bbb$CustomerInterest[bbb$semana >= semana_test - 5 & bbb$semana < semana_test])
    v6[i] <- sum(bbb$CustomerInterest[bbb$semana >= semana_test - 6 & bbb$semana < semana_test])
    v12[i] <- sum(bbb$CustomerInterest[bbb$semana >= semana_test - 12 & bbb$semana < semana_test])
    v27[i] <- sum(bbb$CustomerInterest[bbb$semana >= semana_test - 26 & bbb$semana < semana_test])
  }
}) 

summary(v27)
aux$num_interest1 <- v1
aux$num_interest2 <- v2
aux$num_interest3 <- v3
aux$num_interest4 <- v4
aux$num_interest5 <- v5
aux$num_interest6 <- v6
aux$num_interest12 <- v12
aux$num_interest27 <- v27
summary(aux)

test <- sqldf ("SELECT c.*, a.num_interest1, a.num_interest2, a.num_interest3, a.num_interest4,
               a.num_interest5, a.num_interest6, a.num_interest12, a.num_interest27
               FROM Challenge c LEFT JOIN aux a ON c.CustomerIdx = a.CustomerIdx AND c.IsinIdx = a.IsinIdx
               ORDER BY CustomerIdx, IsinIdx")

test$semana <- 121
test$dia_intervalo <- 841
summary(test)
write.csv(test,"C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/test.csv")
rm(copia_grupos, copia_test, aux, clientes_test, bonos_test, v1, v2, v3, v4, v5, v6, v12, v27, dia_test, bbb)

#################################################
copia_Trade_valido <- Trade_valido[,c("CustomerIdx", "IsinIdx", "BuySell", "CustomerInterest", "semana",
                                    "fecha", "num_interest27", "TradeStatus", "TradeDateKey", "Price")]
#Bucle principal:
#Clave (semana, Customeridx, IsinIdx, BuySell)

ggplot(Trade_valido, aes(x = mes_intervalo, fill=CustomerInterest)) + 
  geom_bar()
ggplot(Trade_valido, aes(x = semana, fill=CustomerInterest)) + 
  geom_bar()

#--Vamos a usar fechas unicamente de 2018 para reducir el tamaño de la muestra. Usaremos a partir de la ultima semana de Enero
#asi entrenamos con la primera ultima semana de mes y permitimos que la muestra se parezca mas a test
semanas_validas <- unique(Trade_valido$semana)
semanas_validas <- sort(semanas_validas[(semanas_validas >= (max(Trade_valido$semana) - 11))])

clave_unica <- test %>% group_by(CustomerIdx, IsinIdx, BuySell) %>% summarise()
unicos <- test %>% group_by(CustomerIdx, IsinIdx) %>% summarise()
clientes <- unicos$CustomerIdx
bonos <- unicos$IsinIdx

Trade_valido <- data.table(Trade_valido)
Trade_valido$CustomerInterest <- as.numeric(Trade_valido$CustomerInterest)
table(Trade_valido$CustomerInterest)
setkey(Trade_valido, CustomerIdx, IsinIdx)

v1 <- numeric(nrow(unicos))
v2 <- numeric(nrow(unicos))
v3 <- numeric(nrow(unicos))
v4 <- numeric(nrow(unicos))
v5 <- numeric(nrow(unicos))
v6 <- numeric(nrow(unicos))
v12 <- numeric(nrow(unicos))
v27 <- numeric(nrow(unicos))

#1:45h
system.time(
  for (s in semanas_validas){
    print(paste("Semana: ", s))
    esta_semana <- copia_Trade_valido[copia_Trade_valido$semana==s,] %>% filter(num_interest27>=1) %>%
                     arrange (CustomerIdx, IsinIdx, BuySell)

    esta_semana_grup <- sqldf ("SELECT *, COUNT(*) AS interacciones
                                FROM esta_semana
                                GROUP BY CustomerIdx, IsinIdx, BuySell
                                ORDER BY CustomerIdx, IsinIdx, semana")
    
    esta_semana_completo <- clave_unica %>% left_join(esta_semana_grup, by=c("CustomerIdx", "IsinIdx", "BuySell"))
    
    ##- Feature generation?
    for (i in 1:nrow(unicos)){
      bbb <- Trade_valido[.(clientes[i], bonos[i])]
      v1[i] <- sum(bbb$CustomerInterest[bbb$semana >= s - 1 & bbb$semana < s])
      v2[i] <- sum(bbb$CustomerInterest[bbb$semana >= s - 2 & bbb$semana < s])
      v3[i] <- sum(bbb$CustomerInterest[bbb$semana >= s - 3 & bbb$semana < s])
      v4[i] <- sum(bbb$CustomerInterest[bbb$semana >= s - 4 & bbb$semana < s])
      v5[i] <- sum(bbb$CustomerInterest[bbb$semana >= s - 5 & bbb$semana < s])
      v6[i] <- sum(bbb$CustomerInterest[bbb$semana >= s - 6 & bbb$semana < s])
      v12[i] <- sum(bbb$CustomerInterest[bbb$semana >= s - 12 & bbb$semana < s])
      v27[i] <- sum(bbb$CustomerInterest[bbb$semana >= s - 27 & bbb$semana < s])
    }
    
    unicos$num_interest1 <- v1
    unicos$num_interest2 <- v2
    unicos$num_interest3 <- v3
    unicos$num_interest4 <- v4
    unicos$num_interest5 <- v5
    unicos$num_interest6 <- v6
    unicos$num_interest12 <- v12
    unicos$num_interest27 <- v27
    #summary(unicos)
    #ceros <- sqldf ("SELECT * FROM unicos WHERE num_interest27==0")
    
    trozo <- esta_semana_completo %>% arrange(CustomerIdx, IsinIdx)
    esta_semana_completo <- sqldf ("SELECT t.*, a.num_interest1, a.num_interest2, a.num_interest3, a.num_interest4,
                                              a.num_interest5, a.num_interest6, a.num_interest12, a.num_interest27
                                    FROM trozo t LEFT JOIN unicos a ON t.CustomerIdx = a.CustomerIdx AND t.IsinIdx = a.IsinIdx
                                    ORDER BY CustomerIdx, IsinIdx")
    print(paste("*Variables generadas*"))
    ###
    
    esta_semana_completo$CustomerInterest <- ifelse(is.na(esta_semana_completo$CustomerInterest), 0, esta_semana_completo$CustomerInterest)
    esta_semana_completo$semana <- ifelse(is.na(esta_semana_completo$semana), s, esta_semana_completo$semana)
    esta_semana_completo$TradeDateKey <- ifelse(is.na(esta_semana_completo$TradeDateKey), min(esta_semana_grup$TradeDateKey), esta_semana_completo$TradeDateKey)
    esta_semana_completo$fecha <- ifelse(is.na(esta_semana_completo$fecha), min(esta_semana_grup$fecha), esta_semana_completo$fecha)
    esta_semana_completo$interacciones <- ifelse(is.na(esta_semana_completo$interacciones), 1, esta_semana_completo$interacciones)
    esta_semana_completo$TradeStatus <- ifelse(is.na(esta_semana_completo$TradeStatus), 'Holding', esta_semana_completo$TradeStatus)
  
    if (s==semanas_validas[1]){
      resul <- esta_semana_completo
    } else {
      resul <- rbind(resul, esta_semana_completo)
      print(paste("Filas anadidas: ", nrow(esta_semana_completo)))
    }
    print(paste("Tamano total: ", nrow(resul)))
  }
  gc()
)

resul <- resul %>% arrange(semana, CustomerIdx, IsinIdx, BuySell)
rm(antes, antes_grup, esta_semana, esta_semana_grup, antes_completo, esta_semana_completo, bbb, trozo)
prop.table(table(resul$CustomerInterest))
summary(resul)
mean(as.numeric(resul$CustomerInterest))
table(resul$BuySell)
min(resul$num_interest27[resul$semana==120])

ggplot(resul, aes(x = semana, fill=as.factor(CustomerInterest))) + 
  geom_bar()
ggplot(resul, aes(x = BuySell, fill=as.factor(CustomerInterest))) + 
  geom_bar()
cor(as.numeric(resul$BuySell), as.numeric(resul$CustomerInterest))
ggplot(resul, aes(x = num_interest27, fill=as.factor(CustomerInterest))) + 
  geom_bar() + xlim(0,15)
cor(resul$num_interest27, as.numeric(resul$CustomerInterest))
ggplot(resul, aes(x = num_interest12, fill=as.factor(CustomerInterest))) + 
  geom_bar() + xlim(0,15)
cor(resul$num_interest12, as.numeric(resul$CustomerInterest))

write.csv(resul,"C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/Train_Generado2.csv")

#######@@@-- Pruebas sobre la muestra --###

prueba <- resul %>% filter (num_interest27 >= 1)

prop.table(table(prueba$CustomerInterest))

ggplot(prueba, aes(x = semana, fill=as.factor(CustomerInterest))) + 
  geom_bar()








