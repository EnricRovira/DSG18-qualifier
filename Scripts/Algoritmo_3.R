setwd("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0")

library(xgboost)
library(dplyr)
library(pROC) #for AUC calculation
library(caret)
library(data.table)

test <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/test_final3.csv"))
train_prim <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/Train_final3.csv"))

train_prim[,c(1)] <- NULL
test[,c(1)] <- NULL

str(train_prim)

prop.table(table(train_prim$CustomerInterest))

train_prim$CustomerInterest <- as.factor(train_prim$CustomerInterest)
#Reducimos el tamaño del DF para mejorar la eficiencia la RAM de nuestro PC
train <- train_prim[,c("CustomerInterest", "RegionCliente", "Sector", "Subsector", "duracion_bono_dias", "semana",
                  "totalCIB","totalCI", "totalC", "totalI", "calificacion", "edad_bono_dias", 
                  "BuySell","RegionBono", "Activity", "edad_bono_semanas", "duracion_bono_semanas","num_interest1",
                  "num_interest2", "num_interest3", "num_interest4", "num_interest5", "num_interest6",
                  "num_interest12", "num_interest27", "MarketIssue", "maxC_n27", "minC_n27",
                  "mediaC_n27", "stdC_n27", "mediaC_duracion", "intereses_anteriores",
                  "diferencia_lastcomprado_lastvendido", "ultima_semana", "lastaction", "mediaC", "diferencia_comprados_vendidos",
                  "mediaI", "mediaCI", "mediaCIB", "dias_primerinteres")]

test <- test[,c("PredictionIdx", "CustomerInterest", "RegionCliente", "Sector", "Subsector", "duracion_bono_dias", "semana",
                 "totalCIB","totalCI", "totalC", "totalI", "calificacion", "edad_bono_dias", 
                 "BuySell","RegionBono", "Activity", "edad_bono_semanas", "duracion_bono_semanas","num_interest1",
                 "num_interest2", "num_interest3", "num_interest4", "num_interest5", "num_interest6",
                 "num_interest12", "num_interest27", "MarketIssue", "maxC_n27", "minC_n27",
                 "mediaC_n27", "stdC_n27", "mediaC_duracion", "intereses_anteriores",
                 "diferencia_lastcomprado_lastvendido", "ultima_semana", "lastaction", "mediaC", "diferencia_comprados_vendidos",
                 "mediaI", "mediaCI", "mediaCIB", "dias_primerinteres")]

#Pasar las variables necesarias a factores
indx <- c(2,3,4,11,13,14,15,26,35,38)
for (i in indx){
  train[,i] <- as.factor(train[,i])
}
#Hacemos lo mismo con test
indx_test <- c(3,4,5,12,14,15,16,27,36,39)
for (i in indx_test){
  test[,i] <- as.factor(test[,i])
}
#str(train)

train_red <- train %>% filter (semana < 120)
train_val <- train %>% filter (semana == 120)

prop.table(table(train_red$CustomerInterest))
prop.table(table(train_val$CustomerInterest))

###########-- XGBOOST ################
##-- @@- XGBOOST
#train_total <- train[,c("RegionCliente", "Sector", "duracion_bono_dias", "calificacion", "edad_bono_dias",
#                        "BuySell", "num_interest27", "num_interest12", "num_interest6", "totalCI", "totalC", "totalI", "totalCIB",
#                        "num_interest5", "num_interest4", "num_interest3", "num_interest2", "num_interest1", "MarketIssue",
#                        "maxC_n27", "minC_n27", "mediaC_n27", "stdC_n27", "mediaC_duracion", "intereses_anteriores",
#                        "Subsector", "ultima_semana", "diferencia_comprados_vendidos", "lastaction", "mediaC", "Activity",
#                        "duracion_bono_semanas")]

train_xgb <- train_red[,c("RegionCliente", "Sector", "duracion_bono_dias", "calificacion", "edad_bono_dias",
                          "BuySell", "num_interest27", "num_interest12", "num_interest6", "totalCI", "totalC", "totalI", "totalCIB",
                          "num_interest5", "num_interest4", "num_interest3", "num_interest2", "num_interest1", "MarketIssue",
                          "maxC_n27", "minC_n27", "mediaC_n27", "stdC_n27", "mediaC_duracion", "intereses_anteriores",
                          "Subsector", "ultima_semana", "diferencia_comprados_vendidos", "lastaction", "mediaC", "Activity",
                          "duracion_bono_semanas")]

val_xgb <- train_val[,c("RegionCliente", "Sector", "duracion_bono_dias", "calificacion", "edad_bono_dias", 
                        "BuySell", "num_interest27", "num_interest12", "num_interest6", "totalCI", "totalC", "totalI", "totalCIB",
                        "num_interest5", "num_interest4", "num_interest3","num_interest2", "num_interest1", "MarketIssue",
                        "maxC_n27", "minC_n27", "mediaC_n27", "stdC_n27", "mediaC_duracion", "intereses_anteriores",
                        "Subsector", "ultima_semana", "diferencia_comprados_vendidos", "lastaction", "mediaC", "Activity",
                        "duracion_bono_semanas")]

test_xgb <- test[,c("RegionCliente", "Sector", "duracion_bono_dias", "calificacion", "edad_bono_dias", "BuySell", 
                    "num_interest27", "num_interest12", "num_interest6", "totalCI", "totalC", "totalI", "totalCIB",
                    "num_interest5", "num_interest4", "num_interest3", "num_interest2", "num_interest1", "MarketIssue",
                    "maxC_n27", "minC_n27", "mediaC_n27", "stdC_n27", "mediaC_duracion", "intereses_anteriores",
                    "Subsector", "ultima_semana", "diferencia_comprados_vendidos", "lastaction", "mediaC", "Activity",
                    "duracion_bono_semanas")]

#for(j in 1:ncol(train_total)) train_total[,j]=as.numeric(train_total[,j])
for(j in 1:ncol(train_xgb)) train_xgb[,j]=as.numeric(train_xgb[,j])
for(j in 1:ncol(val_xgb)) val_xgb[,j]=as.numeric(val_xgb[,j])
for(j in 1:ncol(test_xgb)) test_xgb[,j]=as.numeric(test_xgb[,j])

labels = as.numeric(train_red$CustomerInterest)-1
labels_val = as.numeric(train_val$CustomerInterest)-1
#labelstt = as.numeric(train$CustomerInterest)-1

dtrain <- xgb.DMatrix(data = data.matrix(train_xgb), label = labels)
dtrain_val <- xgb.DMatrix(data = data.matrix(val_xgb), label = labels_val)
#dtrain_tt <- xgb.DMatrix(data = data.matrix(train_total), label = labelstt)


watchlist <- list(train=dtrain, val=dtrain_val)

xgb_params = list( eta = 0.025, #0.025, #0.06
                   max.depth = 15, #14, #10
                   colsample_bytree = 0.8, #0.27, #0.5
                   
                   subsample = 0.75, #0.7
                   min_child_weight = 1, #0, #1
                   gamma=1, #20, #1
                   alpha=1, #20, #1
                   lambda=2, #20, #1
                   
                   seed=12,
                   nthread = 6,
                   eval_metric = "auc",
                   objective = "binary:logistic",
                   booster = "gbtree")
#memory.size()
#gc()

system.time(
  xgb_model <- xgb.train(data = dtrain, params = xgb_params, nrounds = 451, print_every_n = 50, watchlist = watchlist,
                         early_stopping_rounds = 50, maximize = T)
)
#nrounds=451
#time=45
#round=451<-0.805611
#0.78870

pred_xgb <- predict(xgb_model, dtrain_val)
pred_xgb_bin <- ifelse(pred_xgb < 0.5,0,1)

confusionMatrix(pred_xgb_bin, train_val$CustomerInterest)
table(pred_xgb_bin, train_val$CustomerInterest)
prop.table(table(train_val$CustomerInterest))

mat <- xgb.importance (feature_names = colnames(train_xgb), model = xgb_model)
mat


#####################################
#-- Guardamos el modelo
#xgb.save(xgb_model, "xgboost.model_0.78870")
#xgb_model <- xgb.load("xgboost.model_0.78812")

####### Prediccion XGBOOST #######
str(test_xgb)
prediccionXGB <- predict(xgb_model,data.matrix(test_xgb))
test$CustomerInterest<-prediccionXGB

summary(test$CustomerInterest)
escritura <- test [, c(1, 2)]
write.csv(escritura,"C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/PrediccionesV3/testXGB4.csv", row.names=F)

################################


#######-- Modelo 2 --##########
#0.79071
system.time(
  xgb_model_tt <- xgb.train(data = dtrain_tt, params = xgb_params, nrounds = 451, print_every_n = 50, maximize = T)
)
#xgb.save(xgb_model_tt, "xgboost.model_tt_0.79070")
prediccionXGB_tt <- predict(xgb_model_tt,data.matrix(test_xgb))
test$CustomerInterest<-prediccionXGB_tt

summary(test$CustomerInterest)
escritura <- test [, c(1, 2)]
write.csv(escritura,"C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/PrediccionesV3/testXGB1.csv", row.names=F)
##############################

#######-- Modelo 3  --##########
xgb_params2 = list( eta = 0.025, #0.025, #0.06
                   max.depth = 16, #14, #10
                   colsample_bytree = 0.8, #0.27, #0.5
                   
                   subsample = 0.75, #0.7
                   min_child_weight = 1, #0, #1
                   gamma=1, #20, #1
                   alpha=1, #20, #1
                   lambda=2, #20, #1
                   
                   seed=21,
                   nthread = 6,
                   eval_metric = "auc",
                   objective = "binary:logistic",
                   booster = "gbtree")
#memory.size()
#gc()
system.time(
  xgb_model2 <- xgb.train(data = dtrain, params = xgb_params2, nrounds = 451, print_every_n = 50, watchlist = watchlist,
                         early_stopping_rounds = 50, maximize = T)
)
#xgb.save(xgb_model2, "xgboost.model2_2")
prediccionXGB2 <- predict(xgb_model2,data.matrix(test_xgb))
#0.7869
##############################


######-- Ensembling --########
xgb_model <- xgb.load("xgboost.model_0.78870")
xgb_model2 <- xgb.load("xgboost.model2_2")
xgb_model_tt <- xgb.load("xgboost.model_tt_0.79070")

prediccionXGB <- predict(xgb_model,data.matrix(test_xgb))
prediccionXGB_tt <- predict(xgb_model_tt,data.matrix(test_xgb))
prediccionXGB2 <- predict(xgb_model2,data.matrix(test_xgb))

ensembled <- data.frame(test$PredictionIdx, prediccionXGB, prediccionXGB_tt, prediccionXGB2)
#ensembled$CustomerInterest <- ensembled$prediccionXGB * 0.12 + ensembled$prediccionXGB_tt * 0.8 + ensembled$prediccionXGB2 * 0.08
ensembled$CustomerInterest <- ensembled$prediccionXGB * 0.2 + ensembled$prediccionXGB_tt * 0.8
#ensembled$CustomerInterest <- ensembled$prediccionXGB * 0.7 + ensembled$prediccionXGB2 * 0.3
names(ensembled)[1]<-paste("PredictionIdx")
escritura <- ensembled [, c(1, 5)]
write.csv(escritura,"C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/PrediccionesV3/testENS7.csv", row.names=F)

#0.79090






