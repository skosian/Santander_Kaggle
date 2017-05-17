library(pROC)
library(doParallel)		# parallel processing
library(caret)
library(xgboost)
library(dplyr)
library(plyr)
library(pastecs)
library(pryr)
library(tidyr)
library(reshape)

source("C:/Users/skosian/Desktop/Sample Code/calibration function_mean.R")

# load("C:/Santander/Data/train100K_with_lags_dummies.RData")
# load("C:/Santander/Data/test_with_lags_dummies.RData")

product_columns=c("ind_ahor_fin_ult1","ind_aval_fin_ult1","ind_cco_fin_ult1","ind_cder_fin_ult1","ind_cno_fin_ult1","ind_ctju_fin_ult1",
                  "ind_ctma_fin_ult1","ind_ctop_fin_ult1","ind_ctpp_fin_ult1","ind_deco_fin_ult1","ind_deme_fin_ult1","ind_dela_fin_ult1",
                  "ind_ecue_fin_ult1","ind_fond_fin_ult1","ind_hip_fin_ult1","ind_plan_fin_ult1","ind_pres_fin_ult1","ind_reca_fin_ult1",
                  "ind_tjcr_fin_ult1","ind_valo_fin_ult1","ind_viv_fin_ult1","ind_nomina_ult1","ind_nom_pens_ult1","ind_recibo_ult1")

top_products=c("ind_recibo_ult1_growth","ind_nom_pens_ult1_growth","ind_nomina_ult1_growth","ind_tjcr_fin_ult1_growth","ind_cco_fin_ult1_growth",
               "ind_cno_fin_ult1_growth","ind_ecue_fin_ult1_growth","ind_dela_fin_ult1_growth","ind_reca_fin_ult1_growth","ind_ctma_fin_ult1_growth",
               "ind_valo_fin_ult1_growth","ind_fond_fin_ult1_growth","ind_ctop_fin_ult1_growth","ind_ctpp_fin_ult1_growth","ind_deco_fin_ult1_growth",
               "ind_ctju_fin_ult1_growth","ind_plan_fin_ult1_growth","ind_pres_fin_ult1_growth","ind_deme_fin_ult1_growth","ind_cder_fin_ult1_growth",
               "ind_hip_fin_ult1_growth","ind_viv_fin_ult1_growth","ind_ahor_fin_ult1_growth","ind_aval_fin_ult1_growth")

top_products <- top_products[1:21]

product_columns_growth <- paste0(product_columns,"_growth")

load("C:/Santander/Data/train300K_9lags_v4.RData")
load("C:/Santander/Data/test_9lags_v4.RData")

#drop constant and redundant variables
train300K_9lags_v4$tipodom = train300K_9lags_v4$nomprov = train300K_9lags_v4$ult_fec_cli_1t = train300K_9lags_v4$fecha_alta = NULL

####################################################################################################################################
### MISSING VALUE IMPUTATION: Impute missing values with medians ###################################################################
####################################################################################################################################
numeric_columns_train300K <- names(train300K_9lags_v4[,sapply(train300K_9lags_v4,is.numeric)])
numeric_columns_test <- names(test_9lags_v4[,sapply(test_9lags_v4,is.numeric)])

# all_columns <- names(train300K_9lags_v4)
stats.train300K_9lags_v4 <- stat.desc(train300K_9lags_v4[,numeric_columns_train300K])

write.csv(stats.train300K_9lags_v4,file="C:/Santander/EDA/stats.train300K_9lags_v4_Dec18.csv")
save(stats.train300K_9lags_v4,file="C:/Santander/EDA/stats.train300K_9lags_v4.RData")
load("C:/Santander/EDA/stats.train300K_9lags_v4.RData")


#exclude response variables from imputation lists
numeric_columns_train300K <- numeric_columns_train300K[!(numeric_columns_train300K %in% c(top_products,"num_products","num_new_products","delta_products"))]
numeric_columns_test <- numeric_columns_test[!(numeric_columns_test %in% c(top_products,"num_products","num_new_products","delta_products"))]

# names(train300K_9lags_v4)[order(names(train300K_9lags_v4))]

train300K_9lags_v4_imputed <- train300K_9lags_v4

# imputation with median
for (i in 1:length(numeric_columns_train300K)) {
  train300K_9lags_v4_imputed[,numeric_columns_train300K[i]][is.na(train300K_9lags_v4_imputed[,numeric_columns_train300K[i]])] <- stats.train300K_9lags_v4[8,numeric_columns_train300K[i]]  
}


test_9lags_v4_imputed <- test_9lags_v4

for (i in 1:length(numeric_columns_test)) {
  test_9lags_v4_imputed[,numeric_columns_test[i]][is.na(test_9lags_v4_imputed[,numeric_columns_test[i]])] <- stats.train300K_9lags_v4[8,numeric_columns_test[i]]  
}

save(test_9lags_v4_imputed ,file="C:/Santander/Data/test_9lags_v4_imputed.RData")
save(train300K_9lags_v4_imputed ,file="C:/Santander/Data/train300K_9lags_v4_imputed.RData")

load("C:/Santander/Data/test_9lags_v4_imputed.RData")
load("C:/Santander/Data/train300K_9lags_v4_imputed.RData")

##############################################################################################################################################

exclude_columns=c("fecha_dato","ncodpers","fecha_alta","ult_fec_cli_1t","tipodom","nomprov","num_products",
                  "num_new_products","num_new_products_factor","delta_products")

# Set the response variables as factor
for (i in 1:length(top_products)) {
train300K_9lags_v4_imputed[,top_products[i]] = as.factor(train300K_9lags_v4_imputed[,top_products[i]])
}

train300K_9lags_v4_imputed$num_new_products_factor=as.factor(train300K_9lags_v4_imputed$num_new_products)

## Drop any records where target variable is missing (XGboost does not like it)

for (i in 1:length(top_products)) {
  eq <-  paste0("train300K_9lags_v4_imputed <- dplyr::filter(train300K_9lags_v4_imputed,!is.na(",top_products[i],"))")
  eval(parse(text=eq))
}


train100K_9lags_v4_imputed=dplyr::inner_join(train300K_9lags_v4_imputed,unique100K,by="ncodpers")

# save(train100K_9lags_v4_imputed ,file="C:/Santander/Data/train100K_9lags_v4_imputed.RData")

## Split into modeling and valiation sets by month

# mod=dplyr::filter(train100K_9lags_v4_imputed,fecha_dato >'2015-11-28' & fecha_dato <= '2016-03-28')
# val=dplyr::filter(train100K_9lags_v4_imputed,fecha_dato > '2016-03-28')

mod <- dplyr::filter(train300K_9lags_v4_imputed, fecha_dato == '2016-04-28')
val <- dplyr::filter(train300K_9lags_v4_imputed,fecha_dato == '2016-05-28')

# mod_nonmissing <- na.omit(mod)
####################################################################################################################################################

### randomly split the modeling set mod

mod_train=dplyr::sample_frac(mod,size=0.6)
mod_valid=dplyr::anti_join(mod,mod_train[,c("ncodpers","fecha_dato")])

# mod_train_sample <- dplyr::sample_n(mod_train,150000)

product_columns_lag2=paste0(product_columns,"_lag2")
product_columns_lag4=paste0(product_columns,"_lag4")
product_columns_lag5=paste0(product_columns,"_lag5")
product_columns_lag7=paste0(product_columns,"_lag7")
product_columns_lag8=paste0(product_columns,"_lag8")

biz_columns <-c("income_by_product","age_by_product","account_age_by_product","engagement","financial_maturity","banking_propensity")
new_cols <- c("avg_num_prod_last3","avg_num_prod_last6","max_num_prod_last3","max_num_prod_last6","min_num_prod_last3","min_num_prod_last6") #,"steady_6mon","steady_zero_products_6mon")
x <- names(mod)[!(names(mod) %in% c(product_columns,product_columns_growth,exclude_columns,biz_columns,drop_list2))]  #,drop_list2,product_columns_lag2,product_columns_lag4,product_columns_lag5,product_columns_lag7,product_columns_lag8

test_raw_ids = as.data.frame(test_raw$ncodpers)
names(test_raw_ids)=c("ncodpers")

  mod_train_short <- mod_train[,x]
  # mod_train_sample_short <- mod_train_sample[,x]
  mod_valid_short <- mod_valid[,x]
  test_short <- val[,x]
  scoring_short <- test_9lags_v4_imputed[,x]
  
  # colnames(train150K_9lags_v4_imputed)[order(colnames(train150K_9lags_v4_imputed))]
  ordered_column_names <- colnames(mod_train_short)[order(colnames(mod_train_short))]
  
  mod_train_short <- mod_train_short[,ordered_column_names]
  # mod_train_sample_short <- mod_train_sample_short[,ordered_column_names]
  mod_valid_short <- mod_valid_short[,ordered_column_names]
  test_short <- test_short[,ordered_column_names]
  scoring_short <- scoring_short[,ordered_column_names]
  
  # pca <- prcomp(mod_train_short,center = TRUE,scale. = TRUE) 
  # summary(pca)
  # 
  # mod_train_short.pca <- as.data.frame(predict(pca,newdata=mod_train_short))[,1:303]
  # mod_valid_short.pca <- predict(pca,newdata=mod_valid_short)[,1:303]
  # test_short.pca <- predict(pca,newdata=test_short)[,1:303]
  # scoring_short.pca <- predict(pca,newdata=scoring_short)[,1:303]
  
  scoring.xgb <- xgb.DMatrix(data = as.matrix(scoring_short), missing=NaN)
  
  # cor.mod_train_short <- cor(cbind(mod_train_short,train_y))
  # write.csv(cor.mod_train_short,file="C:/Santander/EDA/cor.mod_train_short.csv")
  
  # stats.mod_train_short <- stat.desc(mod_train_short)
  # stats.scoring_short <- stat.desc(scoring_short)
  # 
  # write.csv(stats.mod_train_short,file="C:/Santander/EDA/stats.9lags_v4.mod_train_short.csv")
  # write.csv(stats.scoring_short,file="C:/Santander/EDA/stats.9lags_v4.scoring_short.csv") 
# table(train100K_9lags_v4_imputed$fecha_dato)
  
model_date <- "Dec17"  
  
for (i in 1:20) #length(top_products)) 
 {
  print(paste("Starting with model",i,":",top_products[i]))
 
  train_y <- as.numeric(as.character(mod_train[,top_products[i]]))
  valid_y <- as.numeric(as.character(mod_valid[,top_products[i]]))
  test_y <- as.numeric(as.character(val[,top_products[i]]))
  
  train_y_factor <- as.factor(paste0("A",as.character(mod_train[,top_products[i]])))
  valid_y_factor <- as.factor(paste0("A",as.character(mod_valid[,top_products[i]])))
  test_y_factor <- as.factor(paste0("A",as.character(val[,top_products[i]])))
  
mod_train.xgb <- xgb.DMatrix(data = as.matrix(mod_train_short), label = train_y, missing=NaN)
mod_valid.xgb <- xgb.DMatrix(data = as.matrix(mod_valid_short), label = valid_y, missing=NaN)
mod_test.xgb <- xgb.DMatrix(data = as.matrix(test_short), label = test_y, missing=NaN)


# watchlist <- list(eval=)
watchlist <- list(eval=mod_valid.xgb, mod_train.xgb)

registerDoParallel(4,cores=4)
getDoParWorkers()
# stopImplicitCluster()

eta=0.05
seed=20161217

xgbGrid <- expand.grid(nrounds = 200, #the maximum number of iterations
                       eta = c(eta), # shrinkage
                       max_depth = c(4,5,6,7,8,10,12),
                       gamma=c(1),
                       colsample_bytree=c(0.2,0.6,0.8),
                       min_child_weight=c(10),
                       subsample=c(0.4,0.6,0.8,1)
)

xgbTrControl <- trainControl(method = "cv",   # 10fold cross validation
                             number = 2,							# do 5 repititions of cv
                             summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                             classProbs=TRUE,
                             allowParallel = TRUE)

print(paste("Start time:",Sys.time()))

xgbGrid <- train(
  x = mod_train_short,
  y = train_y_factor,
  method="xgbTree",
  metric="ROC",
  trControl = xgbTrControl,
  tuneGrid = xgbGrid,
  # early.stop.round = 10,
  # watchlist = watchlist,
  # maximize = TRUE,
  # lambda=0.01,
  missing = NaN
  # nthreads=8
  # print.every.n=1
)

# xgbGrid$bestTune

assign(paste0("Grid_results",i),dplyr::arrange(as.data.frame(xgbGrid$results),desc(ROC)))

# Save grid search results
write.csv(get(paste0("Grid_results",i)),file=paste0("C:/Santander/Saved Models/",model_date," XGboost/Grids/Grid_results_eta",eta,"_",i,".csv"))

# Retrieve previously saved grid search results
# assign(paste0("Grid_results",i),read.csv(paste0("C:/Santander/Saved Models/Grids/",model_date," XGboost/Grid_results_eta",eta,"_",i,".csv")))

best_params <- get(paste0("Grid_results",i))[1,]
print(best_params)

parama <- list(
  "eta" = best_params$eta
  ,"objective" = "binary:logistic"
  ,"min_child_weight" = best_params$min_child_weight
  ,"subsample" = best_params$subsample
  ,"max_depth" = best_params$max_depth
  , "eval.metric" = "auc"
  ,"colsample_bytree" = best_params$colsample_bytree
  ,"gamma" = best_params$gamma
  # ,"scale_pos_weight" = PAR$scale_pos_weight
  #            ,"alpha" = PAR$alpha
  # ,"nthreads" = 4
)

set.seed(seed)

xgbEarlyStop <- xgb.train(
  params = parama,
  data =  mod_train.xgb,
  nrounds = 2000,
  early.stop.round = 50,
  watchlist = watchlist,
  eval_metric = "auc",
  method = "xgbTree",
  maximize = TRUE,
  print.every.n=20,
  missing = NaN,
  nthread=4
)

assign(paste0("best_iter_",i),xgbEarlyStop$bestInd)
# assign(paste0("best_iter_",i),nrow(xgbEarlyStop.cv)-50)

set.seed(seed)

xgbFull <- xgb.train(
  params = parama,
  data =  mod_train.xgb,
  nrounds = get(paste0("best_iter_",i)),
  # early.stop.round = 50,
  watchlist = watchlist,
  eval_metric = "auc",
  method = "xgbTree",
  maximize = T,
  print.every.n=100,
  missing = NaN,
  nthread=4
)



# Save the best model on disk for easy retrieval
xgb.save(xgbFull, paste0("C:/Santander/Saved Models/",model_date," XGboost/xgbFull_eta",eta,"_",i))

# Load a previously saved model

# xgbFull <- xgb.load(paste0("C:/Santander/Saved Models/",model_date," XGboost/xgbFull_eta",eta,"_",i))
# assign(paste0("best_iter_",i),999999)
# Score all datasets

importance = xgb.importance(feature_names = ordered_column_names,model = xgbFull)

write.csv(importance,file=paste0("C:/Santander/Saved Models/",model_date," XGboost/Importance/variable_importance_model_",i,"_",model_date,"_eta",eta,".csv"))

predicted_train=as.data.frame(predict(xgbFull,mod_train.xgb))
predicted_valid=as.data.frame(predict(xgbFull,mod_valid.xgb))
predicted_test=as.data.frame(predict(xgbFull,mod_test.xgb))
predicted_scoring=as.data.frame(predict(xgbFull,scoring.xgb))

names(predicted_train) = names(predicted_valid) = names(predicted_test) <- names(predicted_scoring) <- c("pred")

# Compute and print AUCs
auc_train <- auc(train_y, predicted_train$pred)[1]
auc_valid <- auc(valid_y, predicted_valid$pred)[1]
auc_test <- auc(test_y, predicted_test$pred)[1]

print(paste("AUC training set: ",round(auc_train,4)))
print(paste("AUC validation set: ",round(auc_valid,4)))
print(paste("AUC test set: ",round(auc_test,4)))

# Get the mean valus of predictions on all data sets
mean_predictions_temp <- data.frame(i,top_products[i],mean(predicted_train$pred),mean(predicted_valid$pred),mean(predicted_test$pred),mean(predicted_scoring$pred),
                                                           nrow(mod_train_short),nrow(mod_valid_short),nrow(test_short))
names(mean_predictions_temp)=c("Model_ID","Product_Name","Mean_Train_Prediction","Mean_Valid_Prediction","Mean_Test_Prediction","Mean_Scoring_Prediction","Train_Rows","Valid_Rows","Test_Rows")
assign(paste0("mean_predictions_temp_",i),mean_predictions_temp)


# Create a summary vector that has all model parameters for the best model and AUC values and mean predictions
perf_metric <- as.data.frame(cbind(auc_train,auc_valid,auc_test,get(paste0("best_iter_",i)),seed))
names(perf_metric) <- c("auc_train","auc_valid","auc_test","best_iteration","seed_number")

assign(paste0("best_params",i),cbind(perf_metric,best_params,get(paste0("mean_predictions_temp_",i))))

# Create scoring datasets
assign(paste0("train_scores_",i),cbind(mod_train[,c("ncodpers","fecha_dato")],predicted_train,train_y))
assign(paste0("valid_scores_",i),cbind(mod_valid[,c("ncodpers","fecha_dato")],predicted_valid,valid_y))
assign(paste0("test_scores_",i),cbind(val[,c("ncodpers","fecha_dato")],predicted_test,test_y))
assign(paste0("scores_",i),cbind(test_9lags_v4_imputed[,c("ncodpers","fecha_dato")],predicted_scoring))

eq <-  paste0("save(train_scores_",i,", valid_scores_",i,", test_scores_",i,", scores_",i,",file='C:/Santander/Saved Models/",model_date," XGboost/Scored Data/model_eta",eta,"_",i,"_scores.RData')")
eval(parse(text=eq))

# load(paste0("C:/Santander/Saved Models/Nov28 XGboost/Scored Data/model_",i,"_scores.RData"))

## Calibration 
# for (i in 1:12) {

# load(paste0("C:/Santander/Saved Models/",model_date," XGboost/Scored Data/Best/model_best_",i,"_scores.RData"))

# Calibration(
#   FileType = "rdf",
#   RDataFrameTrain = get(paste0("valid_scores_",i)),
#   ActualColumnNameTrain = "valid_y",
#   PredictedColumnNameTrain = "pred",
#   RDataFrameTest = get(paste0("test_scores_",i)),
#   ActualColumnNameTest = "test_y",
#   PredictedColumnNameTest = "pred",
#   PredictedVariableName ="Product Propensity",
#   Target = "binary",
#   Description = paste0("Model_2perc_best_",i,"_Valid_Test"),
#   outputDir = "C:/Santander/Saved Models/",model_date," XGboost/Calibration/")
# }

# Aggregate best model parameters, AUC values and mean values of predictions
# best_models=data_frame()

 
   best_models <- dplyr::bind_rows(best_models,get(paste0("best_params",i)))
 
   print(paste("End time:",Sys.time()))

assign(paste0("best_models_",model_date,"_eta",eta),best_models)
write.csv(get(paste0("best_models_",model_date,"_eta",eta)),file=paste0("C:/Santander/Stats/best_models_",model_date,"_eta",eta,".csv"))

}

  







best_models <-  data.frame()
# score_string <- paste(shQuote(paste0("score",c(1:9))), collapse=", ")  
# dput(as.character(get(paste0("scores_",1:1))))

# for (i in 1:12){


cl <- makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()

# stopCluster(cl)
## Verify that manually selected best models are the ones intended to pick by looking at the AUC values
foreach(i=1:12) %dopar% {
  
library(pROC)  
  load(paste0("C:/Santander/Saved Models/Nov30 XGboost/Scored Data/Best/model_best_",i,"_scores.RData"))
  
  print(paste("model i=",i))
  # auc_train <- auc(train_y, predicted_train$pred)[1]
  # auc_valid <- auc(valid_y, predicted_valid$pred)[1]
  # auc_test <- auc(get(paste0("test_scores_",i))[,"test_y"], get(paste0("test_scores_",i))[,"pred"])[1]
  
    # str(get(paste0("test_scores_",i))[,"pred"])
  
  # print(paste("AUC training set: ",round(auc_train,4)))
  # print(paste("AUC validation set: ",round(auc_valid,4)))
  # print(paste("AUC test set: ",round(auc_test,4)))
}

# Perform Isotonic calibration 

# foreach(i=1:4) %dopar% {
for (i in 2:12) {
  library(pROC) 
  load(paste0("C:/Santander/Saved Models/Nov30 XGboost/Scored Data/Best/model_best_",i,"_scores.RData"))
  
  top_products=c("ind_recibo_ult1_growth","ind_nom_pens_ult1_growth","ind_nomina_ult1_growth","ind_tjcr_fin_ult1_growth","ind_cco_fin_ult1_growth",
                 "ind_cno_fin_ult1_growth","ind_ecue_fin_ult1_growth","ind_dela_fin_ult1_growth","ind_reca_fin_ult1_growth","ind_ctma_fin_ult1_growth",
                 "ind_valo_fin_ult1_growth","ind_fond_fin_ult1_growth","ind_ctop_fin_ult1_growth","ind_ctpp_fin_ult1_growth","ind_deco_fin_ult1_growth",
                 "ind_ctju_fin_ult1_growth","ind_plan_fin_ult1_growth","ind_pres_fin_ult1_growth","ind_deme_fin_ult1_growth","ind_cder_fin_ult1_growth",
                 "ind_hip_fin_ult1_growth","ind_viv_fin_ult1_growth","ind_ahor_fin_ult1_growth","ind_aval_fin_ult1_growth")
  
  top_products <- top_products[1:21]
  
  # print(paste("model i=",i))
  eq_product <- paste0("scores_",i,"$product=gsub('_growth','',top_products[",i,"])")
  eval(parse(text=eq_product))

  # train_y <- as.numeric(as.character(mod_train[,top_products[i]]))
  valid_y <- as.numeric(as.character(mod_valid[,top_products[i]]))
  # test_y <- as.numeric(as.character(val[,top_products[i]]))

  pred.valid.ord <- dplyr::arrange(get(paste0("valid_scores_",i)), pred)
  iRFit= stats::isoreg(pred.valid.ord$pred,pred.valid.ord[,"valid_y"])

  eq_iso<- paste0("train_scores_",i," <- cbind(train_scores_",i,",data.frame(pred.cal = stats::as.stepfun(iRFit)(train_scores_",i,"$pred)))")
  eval(parse(text=eq_iso))
  eq_iso<- paste0("valid_scores_",i," <- cbind(valid_scores_",i,",data.frame(pred.cal = stats::as.stepfun(iRFit)(valid_scores_",i,"$pred)))")
  eval(parse(text=eq_iso))
  eq_iso<- paste0("test_scores_",i," <- cbind(test_scores_",i,",data.frame(pred.cal = stats::as.stepfun(iRFit)(test_scores_",i,"$pred)))")
  eval(parse(text=eq_iso))
  eq_iso<- paste0("scores_",i," <- cbind(scores_",i,",data.frame(pred.cal = stats::as.stepfun(iRFit)(scores_",i,"$pred)))")
  eval(parse(text=eq_iso))

  print(paste("model i=",i))
  auc_test.cal <- auc(get(paste0("test_scores_",i))[,"test_y"], get(paste0("test_scores_",i))[,"pred.cal"])[1]

  print(paste("Model i=",i,"Calibrated AUC test set: ",round(auc_test.cal,4)))

  eq <-  paste0("save(train_scores_",i,", valid_scores_",i,", test_scores_",i,", scores_",i,",file='C:/Santander/Saved Models/Nov30 XGboost/Scored Data/Calibrated Scores/model_",i,"_scores_isotonic.RData')")
  eval(parse(text=eq))
# for (i in 1:12) {
  if (i==1) {all_scores <- data.frame()}
  all_scores=rbind(all_scores,get(paste0("scores_",i)))
}  


options(max.print=100)


mod_actual_frequencies <- t(as.data.frame(sapply(mod[,top_products],table)))
test_actual_frequencies <- as.data.frame(t(sapply(val[,top_products],table)))


  
# for (i in 1:9) print(table(as.data.frame(train.hex)[,top_products[i]],useNA = "ifany"))

## Apply prediction modifiers in order to true up the predictions (modifier = actual event rate/mean prediction)
## Modifiers were computed in excel as simple ratios

# intercept_adjustment <- c(1.609437912,1.609437912,1.609437912,1.609437912,1.609437912,0.693147181,1.609437912,1.609437912,1.609437912)
prediction_modifier=c(1,1,1,1,1,1,1,1,1,1,1,1)
# prediction_modifier=c(0.36524409,0.38191785,0.38655770,0.35395721,0.38432317,0.35006457,0.26614673,0.24814717,0.23944503) # from test dataset actual/model mean ratios
#prediction_modifier=c( 0.8483,1.0277,1.0344,1.0202,0.9419,1.0175,0.8233,0.9897,0.7706) # from modeling dataset actual/model mean ratios

for (j in 1:9) #length(prediction_modifier))
{ 
  load(paste0("C:/Santander/Saved Models/Nov28 XGboost/Scored Data/Best/model_2perc_best_",j,"_scores.RData"))
  
  eq1 <- paste0("scores_",j,"$p0=NULL")
  eval(parse(text=eq1))
  eq2 <- paste0("scores_",j,"$prediction=scores_",j,"$pred*prediction_modifier[",j,"]" )
  #eq2 <- paste0("scores_20pct_adj_",j," = mutate(scores_20pct_",j,",prediction = exp(log(p1/(1-p1)) - intercept_adjustment[",j,"])/(1+exp(log(p1/(1-p1)) - intercept_adjustment[",j,"])))" )
  eval(parse(text=eq2))
  eq3 <- paste0("scores_",j,"$product=gsub('_growth','',top_products[",j,"])")
  eval(parse(text=eq3))
  
  if (j==1)
  {
    all_scores=get(paste0("scores_",j))
  } else {
    all_scores=rbind(all_scores,get(paste0("scores_",j)))
    }
}

#######################################################################################################################
### Perform calibration by Isotonic, Platt or Linear depending on visual inspection of what works better ##############
#######################################################################################################################

for (i in 1:12) {
  
  # load(paste0("C:/Santander/Saved Models/Nov28 XGboost/Scored Data/Best/model_2perc_best_",i,"_scores.RData"))
  load(paste0("C:/Santander/Saved Models/Nov24 XGboost/Scored Data/model_",i,"_scores.RData"))
  
  eq_product <- paste0("scores_",i,"$product=gsub('_growth','',top_products[",i,"])")
  eval(parse(text=eq_product))
  
  # if (i %in% c(1,2,3,6,7)) {
  if (i %in% 1:12) {  
    ## Isotonic calibration models
    print(paste("Processing Isotonic for i=",i))
    
    # load(paste0("C:/Santander/Saved Models/Nov28 XGboost/Calibration/iso.calib.model.rda.Model_2perc_best_",i,"_Valid_Test"))
    load(paste0("C:/Santander/Saved Models/Nov24 XGboost/Calibration/iso.calib.model.rda.Model_",i,"_Valid_Test"))
    eq_iso<- paste0("train_scores_",i," <- cbind(train_scores_",i,",data.frame(pred.cal = stats::as.stepfun(iRFit)(train_scores_",i,"$pred)))")
    eval(parse(text=eq_iso))
    eq_iso<- paste0("valid_scores_",i," <- cbind(valid_scores_",i,",data.frame(pred.cal = stats::as.stepfun(iRFit)(valid_scores_",i,"$pred)))")
    eval(parse(text=eq_iso))
    eq_iso<- paste0("test_scores_",i," <- cbind(test_scores_",i,",data.frame(pred.cal = stats::as.stepfun(iRFit)(test_scores_",i,"$pred)))")
    eval(parse(text=eq_iso))
    eq_iso<- paste0("scores_",i," <- cbind(scores_",i,",data.frame(pred.cal = stats::as.stepfun(iRFit)(scores_",i,"$pred)))")
    eval(parse(text=eq_iso))
    
    # eq <-  paste0("save(train_scores_",i,", valid_scores_",i,", test_scores_",i,", scores_",i,",file='C:/Santander/Saved Models/Nov28 XGboost/Scored Data/Calibrated Scores/model_",i,"_scores_isotonic.RData')")
    # eval(parse(text=eq))
  } else if (i %in% (100:900)) {
    ## Platt's calibration models 
    print(paste("Processing Platts for i=",i))
    
    load(paste0("C:/Santander/Saved Models/Nov28 XGboost/Calibration/Platt.calib.model.rda.Model_",i,"_Valid_Test"))
    
    eq_plat<- paste0("train_scores_",i," <- cbind(train_scores_",i,",data.frame(pred.cal = predict(Platt.calib.model,newdata= data.frame(PREDICTED = train_scores_",i,"$pred),type='response')))")
    eval(parse(text=eq_plat))
    eq_plat<- paste0("valid_scores_",i," <- cbind(valid_scores_",i,",data.frame(pred.cal = predict(Platt.calib.model,newdata= data.frame(PREDICTED = valid_scores_",i,"$pred),type='response')))")
    eval(parse(text=eq_plat))
    eq_plat<- paste0("test_scores_",i," <- cbind(test_scores_",i,",data.frame(pred.cal = predict(Platt.calib.model,newdata= data.frame(PREDICTED = test_scores_",i,"$pred),type='response')))")
    eval(parse(text=eq_plat))
    eq_plat<- paste0("scores_",i," <- cbind(scores_",i,",data.frame(pred.cal = predict(Platt.calib.model,newdata= data.frame(PREDICTED = scores_",i,"$pred),type='response')))")
    eval(parse(text=eq_plat))
    
    eq <-  paste0("save(train_scores_",i,", valid_scores_",i,", test_scores_",i,", scores_",i,",file='C:/Santander/Saved Models/Nov28 XGboost/Scored Data/Calibrated Scores/model_",i,"_scores_platt.RData')")
    eval(parse(text=eq))
    
  } else if (i %in% c(5,8,9)) {
    ## Linear calibration 
    print(paste("Processing Linear for i=",i))
    
    train_scores_5 <- mutate(train_scores_5,pred.cal = 0.46316732638944*pred - 0.0000106875039926765)
    valid_scores_5 <- mutate(valid_scores_5,pred.cal = 0.46316732638944*pred - 0.000010687503992676)
    test_scores_5 <- mutate(test_scores_5,pred.cal = 0.46316732638944*pred - 0.000010687503992676)
    scores_5 <- mutate(scores_5,pred.cal = 0.46316732638944*pred - 0.000010687503992676)
    
    train_scores_8 <- mutate(train_scores_8,pred.cal = 0.174638234144952*pred - 0.000285159490222494)
    valid_scores_8 <- mutate(valid_scores_8,pred.cal = 0.174638234144952*pred - 0.000285159490222494)
    test_scores_8 <- mutate(test_scores_8,pred.cal = 0.174638234144952*pred - 0.000285159490222494)
    scores_8 <- mutate(scores_8,pred.cal = 0.174638234144952*pred - 0.000285159490222494)
    
    train_scores_9 <- mutate(train_scores_9,pred.cal = 1.02597529571518*pred - 0.00010292658209756)
    valid_scores_9 <- mutate(valid_scores_9,pred.cal = 1.02597529571518*pred - 0.00010292658209756)
    test_scores_9 <- mutate(test_scores_9,pred.cal = 1.02597529571518*pred - 0.00010292658209756)
    scores_9 <- mutate(scores_9,pred.cal = 1.02597529571518*pred - 0.00010292658209756)
    
    
      
  } else if (i %in% c(4)) {
    ## No change needed
    print(paste("Processing No Change for i=",i))
   
    eq_nochange <- paste0("train_scores_",i,"$pred.cal <-train_scores_",i,"$pred")
    eval(parse(text=eq_nochange))
    eq_nochange <- paste0("valid_scores_",i,"$pred.cal <-valid_scores_",i,"$pred")
    eval(parse(text=eq_nochange))
    eq_nochange <- paste0("test_scores_",i,"$pred.cal <-test_scores_",i,"$pred")
    eval(parse(text=eq_nochange))
    eq_nochange <- paste0("scores_",i,"$pred.cal <-scores_",i,"$pred")
    eval(parse(text=eq_nochange))
      }
   if (i==1){    
    all_scores=get(paste0("scores_",i))
  } else {
    all_scores=rbind(all_scores,get(paste0("scores_",i)))
  }    
}

round(mean(scores_1$pred.cal),5)

# all_scores_6prod <- dplyr::filter(all_scores,product %in% gsub("_growth","",top_products[1:6]))

# all_scores_sorted  <- dplyr::arrange(all_scores,ncodpers,desc(prediction))
all_scores_sorted  <- dplyr::arrange(all_scores,ncodpers,desc(pred.cal))
# all_scores_sorted  <- dplyr::arrange( all_scores_6prod,ncodpers,desc(pred.cal))
all_scores_sorted$prediction <- all_scores_sorted$pred.cal
all_scores_sorted$pred=all_scores_sorted$pred.cal= NULL

all_scores_sorted <- all_scores_sorted[,c("ncodpers","prediction","product")]

# Identify and exclude the currently active (as of 05-28-2015) products from all_scores_sorted and that will give us the predicted new products
actual_current_products <- subset(train[,c("ncodpers","fecha_dato",product_columns)],fecha_dato == "2016-05-28")

actual_current_products_vertical <- reshape(actual_current_products,
                                            varying = product_columns,
                                            v.names = "score",
                                            timevar = "product",
                                            direction = "long",
                                            times = product_columns)

actual_current_products_active <- dplyr::filter(actual_current_products_vertical,score==1)
actual_current_products_active$score = actual_current_products_active$id = actual_current_products_active$fecha_dato = NULL
actual_current_products_active <- dplyr::arrange(actual_current_products_active,ncodpers,product)
actual_current_products_active$multiplier=0

all_scores_sorted1 <- dplyr::left_join(all_scores_sorted,actual_current_products_active)
all_scores_sorted1$multiplier[is.na(all_scores_sorted1$multiplier)] <- 1
all_scores_sorted1$new_score <- all_scores_sorted1$prediction*all_scores_sorted1$multiplier
all_scores_sorted2 <- arrange(all_scores_sorted1,ncodpers,desc(new_score))

# create a column that simply runs prod1, prod2, ... , prod9 and repeats for each incodpers. These column will be used for transposing the products
all_scores_sorted2$prod_num <- as.integer(rownames(all_scores_sorted2))  %% 12
all_scores_sorted2$prod_num[all_scores_sorted2$prod_num == 0] <- 12
all_scores_sorted2$prod_num <- paste0("prod",all_scores_sorted2$prod_num)

#Transpose the scores dataset from vertical to horizontal
all_scores_sorted2$prediction = all_scores_sorted2$multiplier = all_scores_sorted2$new_score=NULL
all_scores_transposed <- spread(all_scores_sorted2, prod_num,product)

table(all_scores_transposed$prod1)
all_scores_transposed %>% dplyr::group_by(prod1) %>% dplyr::summarise(count=n())

 # save(pred_num_products_scoring1,file ="C:/Santander/Data/pred_num_products_scoring1.Rdata")
#####################################################################################################################
# Combine propensity models with number of products model results 
# Merge the ordered products data frame with the predicted number of products data frame, and produce the output data
#######################################################################################################################
# all_scores_transposed <- all_scores_transposed_precal
 
# all_scores_combined=inner_join(all_scores_transposed,pred_num_products_scoring1)
# all_scores_combined$pred_num_products = as.numeric(as.character(all_scores_combined$pred_num_products))

# Create an output string from all individual prod1, prod2... variables up to the predicted number of products (pred_num_products)
all_scores_combined <- all_scores_transposed
all_scores_combined$output_string=NA

all_scores_combined$pred_num_products <- 7

for (i in 1:max(all_scores_combined$pred_num_products)) {
  assign(paste0("s",i),subset(all_scores_combined,pred_num_products==i))
  for (j in 1:i)  
  { if (j==1) prod_string=paste0("s",i,"$prod",j) 
  else prod_string <- paste0(prod_string,",s",i,"$prod",j)
  }
  eq=paste0("s",i,"$output_string <- paste(", prod_string, ")" )
  eval(parse(text=eq)) 
  if (i == 1)
    all_scores_combined_string <- get(paste0("s",i))
  else all_scores_combined_string <-rbind(all_scores_combined_string,get(paste0("s",i)))
}

all_scores_combined_string=dplyr::arrange(all_scores_combined_string[,c("ncodpers","pred_num_products","output_string")],ncodpers)


# save(all_scores_combined_string,file="C:/Santander/Data/all_scores_combined_XGboost_6lags_plus_isotonic_calibration_12products_Nov28.RData")


submission=all_scores_combined_string[,c("ncodpers","output_string")]
names(submission)=c("ncodpers","added_products")

submission$ncodpers <- as.integer(submission$ncodpers)
write.csv(submission[,1:2],file="C:/Santander/Submissions/submission31_XGboost_6lags_plus_v2_12prod_isotonic_calibration_Nov30.csv",row.names = FALSE)


# # Submission 2. Put just one product
# 
# submission2=all_scores_combined[,c("ncodpers","prod1")]
# names(submission2)=c("ncodpers","added_products")
# write.csv(submission2,file="C:/Santander/Submissions/submission2.csv",row.names = FALSE)
