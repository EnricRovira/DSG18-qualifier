setwd("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0")

library(data.table)
library(Metrics)
library(sqldf)
library(lubridate)
library(anytime)
library(ROCR)
library(corrplot)
library(ggplot2)
library(dplyr)

test <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/test.csv"))
Customer <- read.csv("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/Customer.csv")
Isin <- read.csv("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/isin.csv")
TradeGenerado <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/Train_Generado2.csv"))

TradeGenerado[,c(1)] <- NULL
test[,c(1)] <- NULL

summary(test)
summary(TradeGenerado)
#1.68 %
prop.table(table(TradeGenerado$CustomerInterest))

TradeGenerado$fecha <- ymd(TradeGenerado$fecha)
TradeGenerado$mes <- as.factor(month(TradeGenerado$fecha))
TradeGenerado$año <- as.factor(year(TradeGenerado$fecha))
TradeGenerado$dia <- as.factor(weekdays(TradeGenerado$fecha))
TradeGenerado$dia_mes <- as.factor(mday(TradeGenerado$fecha))

#Exploracion de las nuevas variables de tiempo

ggplot(TradeGenerado, aes(x = mes, fill=CustomerInterest)) + 
  geom_bar()
ggplot(TradeGenerado, aes(x = año, fill=CustomerInterest)) + 
  geom_bar()
ggplot(TradeGenerado, aes(x = dia, fill=CustomerInterest)) + 
  geom_bar()
ggplot(TradeGenerado, aes(x = dia_mes, fill=CustomerInterest)) + 
  geom_bar()

#--

#Mean encodings y sumas acumuladas
#Esta mal! habria que havcerlo en el bucle principal para cada semana!! 
#importante ver que valor usar para rellenar las NA generadas en los CustIn=0
#Tambien se pueden sacar minimos maximos y std

TradeGenerado$CustomerInterest <- as.numeric(TradeGenerado$CustomerInterest)
table(TradeGenerado$CustomerInterest)

grupos1 <- TradeGenerado %>% group_by(CustomerIdx, IsinIdx, BuySell) %>% summarise(totalCIB = n(), mediaCIB = mean(CustomerInterest))
grupos2 <- TradeGenerado %>% group_by(CustomerIdx, IsinIdx) %>% summarise(totalCI = n(), mediaCI = mean(CustomerInterest))
grupos3 <- TradeGenerado %>% group_by(CustomerIdx) %>% summarise(totalC = n(), mediaC = mean(CustomerInterest))
grupos4 <- TradeGenerado %>% group_by(IsinIdx) %>% summarise(totalI = n(), mediaI = mean(CustomerInterest))


a <- left_join(TradeGenerado, grupos1, by=c("CustomerIdx", "IsinIdx", "BuySell"))
b <- left_join(a, grupos2, by=c("CustomerIdx", "IsinIdx"))
c <- left_join(b, grupos3, by=c("CustomerIdx"))
d <- left_join(c, grupos4, by=c("IsinIdx"))

TradeGenerado <- d
rm(a,b,c,d)

# ->Ahora lo llevamos a Test
a <- left_join(test, grupos1, by=c("CustomerIdx", "IsinIdx", "BuySell"))
b <- left_join(a, grupos2, by=c("CustomerIdx", "IsinIdx"))
c <- left_join(b, grupos3, by=c("CustomerIdx"))
d <- left_join(c, grupos4, by=c("IsinIdx"))

test <- d
rm(a,b,c,d,grupos1,grupos2,grupos3,grupos4)
#--
### Ahora sacamos variables estadisticas derivadas de los ID y num_interest
trozo_train <- TradeGenerado %>% select(CustomerIdx, IsinIdx, BuySell, num_interest27, num_interest12, num_interest6)
trozo_test <- test %>% select(CustomerIdx, IsinIdx, BuySell, num_interest27, num_interest12, num_interest6)
total <- rbind (trozo_train, trozo_test)

statCIB <- total %>% 
  group_by(CustomerIdx, IsinIdx, BuySell) %>% 
  summarise(maxCIB_n27 = max(num_interest27), minCIB_n27 = min(num_interest27), mediaCIB_n27 = mean(num_interest27), stdCIB_n27 = sd(num_interest27), 
            maxCIB_n12 = max(num_interest12), minCIB_n12 = min(num_interest12), mediaCIB_n12 = mean(num_interest12), stdCIB_n12 = sd(num_interest12),
            maxCIB_n6 = max(num_interest6), minCIB_n6 = min(num_interest6), mediaCIB_n6 = mean(num_interest6), stdCIB_n6 = sd(num_interest6))

statCI <- total %>% 
  group_by(CustomerIdx, IsinIdx) %>% 
  summarise(maxCI_n27 = max(num_interest27), minCI_n27 = min(num_interest27), mediaCI_n27 = mean(num_interest27), stdCI_n27 = sd(num_interest27), 
            maxCI_n12 = max(num_interest12), minCI_n12 = min(num_interest12), mediaCI_n12 = mean(num_interest12), stdCI_n12 = sd(num_interest12),
            maxCI_n6 = max(num_interest6), minCI_n6 = min(num_interest6), mediaCI_n6 = mean(num_interest6), stdCI_n6 = sd(num_interest6))

statC <- total %>% 
  group_by(CustomerIdx) %>% 
  summarise(maxC_n27 = max(num_interest27), minC_n27 = min(num_interest27), mediaC_n27 = mean(num_interest27), stdC_n27 = sd(num_interest27), 
            maxC_n12 = max(num_interest12), minC_n12 = min(num_interest12), mediaC_n12 = mean(num_interest12), stdC_n12 = sd(num_interest12),
            maxC_n6 = max(num_interest6), minC_n6 = min(num_interest6), mediaC_n6 = mean(num_interest6), stdC_n6 = sd(num_interest6))

statI <- total %>% 
  group_by(IsinIdx) %>% 
  summarise(maxI_n27 = max(num_interest27), minI_n27 = min(num_interest27), mediaI_n27 = mean(num_interest27), stdI_n27 = sd(num_interest27), 
            maxI_n12 = max(num_interest12), minI_n12 = min(num_interest12), mediaI_n12 = mean(num_interest12), stdI_n12 = sd(num_interest12),
            maxI_n6 = max(num_interest6), minI_n6 = min(num_interest6), mediaI_n6 = mean(num_interest6), stdI_n6 = sd(num_interest6))

a <- left_join(TradeGenerado, statCIB, by=c("CustomerIdx", "IsinIdx", "BuySell"))
b <- left_join(a, statCI, by=c("CustomerIdx", "IsinIdx"))
c <- left_join(b, statC, by=c("CustomerIdx"))
d <- left_join(c, statI, by=c("IsinIdx"))

#Ahora la parte de test
at <- left_join(test, statCIB, by=c("CustomerIdx", "IsinIdx", "BuySell"))
bt <- left_join(at, statCI, by=c("CustomerIdx", "IsinIdx"))
ct <- left_join(bt, statC, by=c("CustomerIdx"))
dt <- left_join(ct, statI, by=c("IsinIdx"))

TradeGenerado <- d
test <- dt
rm(a,b,c,d,at,bt,ct,dt,statC, statI,statCI,statCIB,trozo_test,trozo_train,total)
#--

###Datos del cliente y del bono
clientes_unicos <- test %>% group_by(CustomerIdx) %>% summarise()
bonos_unicos <- test %>% group_by(IsinIdx) %>% summarise()

clientes <- Customer[Customer$CustomerIdx %in% clientes_unicos$CustomerIdx, c("CustomerIdx", "Sector", "Subsector", "Region")]
bonos <- Isin[Isin$IsinIdx %in% bonos_unicos$IsinIdx, c("IsinIdx", "ActualMaturityDateKey", "IssueDateKey", "CompositeRating",
                                                         "Region", "Activity", "Currency", "MarketIssue")]

#Unimos las dos tablas de Cliente y de Bonos manteniendo las columnas y añadiendo las nuevas
a <-  inner_join(TradeGenerado, clientes, by=c("CustomerIdx"))
b <-  inner_join(a, bonos, by=c("IsinIdx"))

c <- inner_join(test, clientes, by=c("CustomerIdx"))
d <- inner_join(c, bonos, by=c("IsinIdx"))

TradeGenerado <- b
test <- d
rm(a,b,c,d)
names(TradeGenerado)[names(TradeGenerado)=="Region.x"] <- c("RegionCliente")
names(TradeGenerado)[names(TradeGenerado)=="Region.y"] <- c("RegionBono")
names(test)[names(test)=="Region.x"] <- c("RegionCliente")
names(test)[names(test)=="Region.y"] <- c("RegionBono")

#Exploracion de las nuevas variables de Cliente y de Bono

ggplot(TradeGenerado, aes(x = Sector, fill=as.factor(CustomerInterest))) + 
  geom_bar()
cor(as.numeric(TradeGenerado$Sector), TradeGenerado$CustomerInterest)

ggplot(TradeGenerado, aes(x = Subsector, fill=as.factor(CustomerInterest))) + 
  geom_bar()
cor(as.numeric(TradeGenerado$Subsector), TradeGenerado$CustomerInterest)

ggplot(TradeGenerado, aes(x = RegionCliente, fill=as.factor(CustomerInterest))) + 
  geom_bar()
cor(as.numeric(TradeGenerado$RegionCliente), TradeGenerado$CustomerInterest)

ggplot(TradeGenerado, aes(x = CompositeRating, fill=as.factor(CustomerInterest))) + 
  geom_bar()
cor(as.numeric(TradeGenerado$CompositeRating), TradeGenerado$CustomerInterest)

ggplot(TradeGenerado, aes(x = RegionBono, fill=as.factor(CustomerInterest))) + 
  geom_bar()
cor(as.numeric(TradeGenerado$RegionBono), TradeGenerado$CustomerInterest)

ggplot(TradeGenerado, aes(x = MarketIssue, fill=as.factor(CustomerInterest))) + 
  geom_bar()
cor(as.numeric(TradeGenerado$MarketIssue), TradeGenerado$CustomerInterest)

#--

TradeGenerado$Subsector <- droplevels(TradeGenerado$Subsector)
TradeGenerado$CompositeRating <- droplevels(TradeGenerado$CompositeRating)
TradeGenerado$MarketIssue <- droplevels(TradeGenerado$MarketIssue)

test$Subsector <- droplevels(test$Subsector)
test$CompositeRating <- droplevels(test$CompositeRating)
test$MarketIssue <- droplevels(test$MarketIssue)


table(TradeGenerado$RegionCliente, TradeGenerado$CustomerInterest)

#########Creamos variables a raiz de las nuevas extraidas

TradeGenerado$Vencimiento <- ymd(TradeGenerado$ActualMaturityDateKey)
TradeGenerado$duracion_bono_dias <- as.numeric(TradeGenerado$Vencimiento - TradeGenerado$fecha)
TradeGenerado$duracion_bono_semanas <- ceiling(TradeGenerado$duracion_bono_dias/7)
TradeGenerado$edad_bono <- ymd(TradeGenerado$IssueDateKey)
TradeGenerado$edad_bono_dias <- as.numeric(TradeGenerado$fecha - TradeGenerado$edad_bono)
TradeGenerado$edad_bono_dias <- ifelse (TradeGenerado$edad_bono_dias <= 0 , 1, TradeGenerado$edad_bono_dias)
TradeGenerado$edad_bono_semanas <- ceiling(TradeGenerado$edad_bono_dias/7)

TradeGenerado$calificacion[TradeGenerado$CompositeRating == 'NR'] <- "no_rating"
TradeGenerado$calificacion[TradeGenerado$CompositeRating %in% list('DDD', 'DDD+', 'DD+', 'D')] <- "suspenso"
TradeGenerado$calificacion[TradeGenerado$CompositeRating %in% list('CCC+', 'CC', 'CCC', 'CCC-', 'CC-', 'CC+', 'C+', 'C')] <- "aprobado"
TradeGenerado$calificacion[TradeGenerado$CompositeRating %in% list('BB-', 'BB', 'B-', 'BB+', 'B', 'B+','BBB-', 'BBB', 'BBB+')] <- "notable"
TradeGenerado$calificacion[TradeGenerado$CompositeRating %in% list('AA-', 'A', 'A-', 'A+', 'AA+', 'AA', 'AAA')] <- "sobresaliente"

ggplot(TradeGenerado, aes(x = duracion_bono_dias, fill=as.factor(CustomerInterest))) + 
  geom_bar()
ggplot(TradeGenerado, aes(x = calificacion, fill=as.factor(CustomerInterest))) + 
  geom_bar()

#Toca hacer el mismo proceso para test:

test$fecha <- ymd(test$DateKey)
test$Vencimiento <- ymd(test$ActualMaturityDateKey)
test$duracion_bono_dias <- as.numeric(test$Vencimiento - test$fecha)
test$duracion_bono_semanas <- ceiling(test$duracion_bono_dias/7)
test$edad_bono <- ymd(test$IssueDateKey)
test$edad_bono_dias <- as.numeric(test$fecha - test$edad_bono)
test$edad_bono_semanas <- ceiling(test$edad_bono_dias/7)

test$calificacion[test$CompositeRating == 'NR'] <- "no_rating"
test$calificacion[test$CompositeRating %in% list('DDD', 'DDD+', 'DD+', 'D')] <- "suspenso"
test$calificacion[test$CompositeRating %in% list('CCC+', 'CC', 'CCC', 'CCC-', 'CC-', 'CC+', 'C+', 'C')] <- "aprobado"
test$calificacion[test$CompositeRating %in% list('BB-', 'BB', 'B-', 'BB+', 'B', 'B+','BBB-', 'BBB', 'BBB+')] <- "notable"
test$calificacion[test$CompositeRating %in% list('AA-', 'A', 'A-', 'A+', 'AA+', 'AA', 'AAA')] <- "sobresaliente"

summary(test)
#########

trozo_train <- TradeGenerado %>% select(CustomerIdx, IsinIdx, BuySell, duracion_bono_dias, edad_bono_dias)
trozo_test <- test %>% select(CustomerIdx, IsinIdx, BuySell, duracion_bono_dias, edad_bono_dias )
total <- rbind (trozo_train, trozo_test)

statC <- total %>% 
  group_by(CustomerIdx) %>% 
  summarise(maxC_duracion = max(duracion_bono_dias), minC_duracion = min(duracion_bono_dias), mediaC_duracion = mean(duracion_bono_dias), 
            stdC_duracion = sd(duracion_bono_dias), maxC_edad = max(edad_bono_dias), minC_edad = min(edad_bono_dias), 
            mediaC_edad = mean(edad_bono_dias), stdC_edad = sd(edad_bono_dias))

a <- left_join(TradeGenerado, statC, by=c("CustomerIdx"))
at <- left_join(test, statC, by=c("CustomerIdx"))

TradeGenerado <- a
test <- at

rm(statC, a, at, trozo_test, trozo_train, total)
################

write.csv(TradeGenerado, "C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/Train_final2.csv")
write.csv(test, "C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/test_final2.csv")

########################






