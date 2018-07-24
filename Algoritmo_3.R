setwd("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0")

library(xgboost)
library(data.table)
library(Metrics)
library(randomForest)
library(dplyr)
library(pROC) #for AUC calculation
library(caret)

test <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/test_final2.csv"))
train_prim <- data.frame(fread("C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/Data/V3/Train_final2.csv"))

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
                  "mediaC_n27", "stdC_n27", "mediaC", "mediaC_duracion", "mediaC_edad", "stdC_duracion")]

#Pasar las variables necesarias a factores
indx <- c(2,3,4,11,13,14,15,26)
for (i in indx){
  train[,i] <- as.factor(train[,i])
}
#Hacemos lo mismo con test
indx_test <- c(5,74,75,76,80,81,82,83,91)
for (i in indx_test){
  test[,i] <- as.factor(test[,i])
}
#str(train)

train_red <- train %>% filter (semana < 120)
test_train <- train %>% filter (semana == 120)

prop.table(table(train_red$CustomerInterest))
prop.table(table(test_train$CustomerInterest))

###########-- XGBOOST ################
##-- @@- XGBOOST

train_xgb <- train_red[,c("RegionCliente", "Sector", "duracion_bono_dias", "calificacion", "edad_bono_dias",
                            "BuySell", "num_interest27", "num_interest12", "num_interest6", "totalCI", "totalC", "totalI", "totalCIB",
                            "num_interest5", "num_interest4", "num_interest3", "num_interest2", "num_interest1", "MarketIssue",
                          "maxC_n27", "minC_n27", "mediaC_n27", "stdC_n27", "mediaC_duracion")]

train_test <- test_train[,c("RegionCliente", "Sector", "duracion_bono_dias", "calificacion", "edad_bono_dias", 
                            "BuySell", "num_interest27", "num_interest12", "num_interest6", "totalCI", "totalC", "totalI", "totalCIB",
                            "num_interest5", "num_interest4", "num_interest3","num_interest2", "num_interest1", "MarketIssue",
                            "maxC_n27", "minC_n27", "mediaC_n27", "stdC_n27", "mediaC_duracion")]

test_xgb <- test[,c("RegionCliente", "Sector", "duracion_bono_dias", "calificacion", "edad_bono_dias", "BuySell", 
                    "num_interest27", "num_interest12", "num_interest6", "totalCI", "totalC", "totalI", "totalCIB",
                    "num_interest5", "num_interest4", "num_interest3", "num_interest2", "num_interest1", "MarketIssue",
                    "maxC_n27", "minC_n27", "mediaC_n27", "stdC_n27", "mediaC_duracion")]

for(j in 1:ncol(train_xgb)) train_xgb[,j]=as.numeric(train_xgb[,j])
for(j in 1:ncol(train_test)) train_test[,j]=as.numeric(train_test[,j])
for(j in 1:ncol(test_xgb)) test_xgb[,j]=as.numeric(test_xgb[,j])

labels = as.numeric(train_red$CustomerInterest)-1
labels_tt = as.numeric(test_train$CustomerInterest)-1

dtrain <- xgb.DMatrix(data = data.matrix(train_xgb), label = labels)
dtrain_test <- xgb.DMatrix(data = data.matrix(train_test), label = labels_tt)


watchlist <- list(train=dtrain, val=dtrain_test)

xgb_params = list( eta = 0.025, #0.025, #0.06
                   max.depth = 14, #14, #10
                   colsample_bytree = 0.8, #0.27, #0.5
                   
                   subsample = 0.75, #0.7
                   min_child_weight = 1, #0, #1
                   gamma=1, #20, #1
                   alpha=1, #20, #1
                   lambda=2, #20, #1
                   
                   nthread = 6,
                   eval_metric = "auc",
                   objective = "binary:logistic",
                   booster = "gbtree")
#memory.size()
#gc()
xgb_cv <- xgb.cv(params = xgb_params, data = dtrain, 
                 showsd = TRUE, stratified = TRUE, watchlist = watchlist,
                 nrounds = 2000, nfold = 3, print_every_n = 50, early_stopping_rounds = 25, maximize = T)
system.time(
  xgb_model <- xgb.train(data = dtrain, params = xgb_params, nrounds = 351, print_every_n = 50, watchlist = watchlist,
                         early_stopping_rounds = 50, maximize = T)
)
#nrounds=401
#time=39
#round=351<-0.79333

pred_xgb <- predict(xgb_model, dtrain_test)
pred_xgb_bin <- ifelse(pred_xgb < 0.5,0,1)

xgb_test_train_auc <- roc(test_train$CustomerInterest, pred_xgb)
xgb_test_train_auc

confusionMatrix(pred_xgb_bin, test_train$CustomerInterest)
prop.table(table(test_train$CustomerInterest))

mat <- xgb.importance (feature_names = colnames(train_xgb), model = xgb_model)
mat


#####################################


####### Prediccion XGBOOST #######
str(test_xgb)
prediccionXGB <- predict(xgb_model,data.matrix(test_xgb))
test$CustomerInterest<-prediccionXGB

summary(test$CustomerInterest)
escritura <- test [, c(1, 6)]
write.csv(escritura,"C:/Users/Enric/Google Drive/Competiciones_MachineLearning/DSG_2018_2.0/PrediccionesV3/testXGB13.csv", row.names=F)

################################


#-- Guardamos el modelo
#xgb.save(xgb_model, "xgboost.model_0.77315")
#xgb_model <- xgb.load("xgboost.model_0.77315")


#######-- Stacking --##########
val <- data.frame(pred_xgb, labels_tt)
stack_test <- data.frame(prediccionXGB)


##############################






