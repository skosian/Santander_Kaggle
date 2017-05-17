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


product_columns=c("ind_ahor_fin_ult1","ind_aval_fin_ult1","ind_cco_fin_ult1","ind_cder_fin_ult1","ind_cno_fin_ult1","ind_ctju_fin_ult1",
                  "ind_ctma_fin_ult1","ind_ctop_fin_ult1","ind_ctpp_fin_ult1","ind_deco_fin_ult1","ind_deme_fin_ult1","ind_dela_fin_ult1",
                  "ind_ecue_fin_ult1","ind_fond_fin_ult1","ind_hip_fin_ult1","ind_plan_fin_ult1","ind_pres_fin_ult1","ind_reca_fin_ult1",
                  "ind_tjcr_fin_ult1","ind_valo_fin_ult1","ind_viv_fin_ult1","ind_nomina_ult1","ind_nom_pens_ult1","ind_recibo_ult1")

top_products=c("ind_recibo_ult1_growth","ind_nom_pens_ult1_growth","ind_nomina_ult1_growth","ind_tjcr_fin_ult1_growth","ind_cco_fin_ult1_growth",
               "ind_cno_fin_ult1_growth","ind_ecue_fin_ult1_growth","ind_dela_fin_ult1_growth","ind_reca_fin_ult1_growth","ind_ctma_fin_ult1_growth",
               "ind_valo_fin_ult1_growth","ind_fond_fin_ult1_growth")

#drop constant and redundant variables
train100K_9lags_v3$tipodom = train100K_9lags_v3$nomprov = train100K_9lags_v3$ult_fec_cli_1t = train100K_9lags_v3$fecha_alta = NULL

####################################################################################################################################
### MISSING VALUE IMPUTATION: Impute missing values with medians ###################################################################
####################################################################################################################################
all_columns <- names(train100K_9lags_v3)
stats.train100K_9lags_v3 <- stat.desc(train100K_9lags_v3[,all_columns])

write.csv(stats.train100K_9lags_v3,file="C:/Santander/EDA/stats.train100K_9lags_v3_Nov30.csv")
# load("C:/Santander/EDA/stats.train100K_9lags_v3_Nov30.csv")

numeric_columns_train100K <- names(train100K_9lags_v3[,sapply(train100K_9lags_v3,is.numeric)])
numeric_columns_test <- names(test_9lags_v3[,sapply(test_9lags_v3,is.numeric)])

#exclude response variables from imputation lists
numeric_columns_train100K <- numeric_columns_train100K[!(numeric_columns_train100K %in% c(top_products,"num_products","num_new_products","delta_products"))]
numeric_columns_test <- numeric_columns_test[!(numeric_columns_test %in% c(top_products,"num_products","num_new_products","delta_products"))]

names(train100K_9lags_v3)[order(names(train100K_9lags_v3))]


train100K_9lags_v3_imputed <- train100K_9lags_v3

# imputation with median
for (i in 1:length(numeric_columns_train100K)) {
  train100K_9lags_v3_imputed[,numeric_columns_train100K[i]][is.na(train100K_9lags_v3_imputed[,numeric_columns_train100K[i]])] <- stats.train100K_9lags_v3[8,numeric_columns_train100K[i]]  
}

test_9lags_v3_imputed <- test_9lags_v3

for (i in 1:length(numeric_columns_test)) {
  test_9lags_v3_imputed[,numeric_columns_test[i]][is.na(test_9lags_v3_imputed[,numeric_columns_test[i]])] <- stats.train100K_9lags_v3[8,numeric_columns_test[i]]  
}

save(test_9lags_v3_imputed ,file="C:/Santander/Data/test_9lags_v3_imputed.RData")
save(train100K_9lags_v3_imputed ,file="C:/Santander/Data/train100K_9lags_v3_imputed.RData")

load("C:/Santander/Data/test_9lags_v3_imputed.RData")
load("C:/Santander/Data/train100K_9lags_v3_imputed.RData")

##############################################################################################################################################

# Select only non-zer growth records, and create a column called new_product that identifies the new product name
for (i in 1:12) {
   assign(paste0("train_growth",i),subset(train100K_9lags_v3_imputed,get(top_products[i])==1))
   eq <- paste0("train_growth",i,"$new_product=gsub('_growth','',top_products[",i,"])")
  eval(parse(text=eq))
  if (i==1) train_growth_all <- get(paste0("train_growth",i))
  else train_growth_all <- rbind(train_growth_all,get(paste0("train_growth",i)))
}

train_growth_all$new_product <- as.factor(train_growth_all$new_product)
# write.csv(stat.desc(train300K_with_lags[,product_columns_growth])[7:9,],file="C:/Santander/Stats/growth_stats.csv")
# write.csv(stat.desc(train300K_with_lags)[7:9,],file="C:/Santander/Stats/train300K_with_lags_stats.csv")
# write.csv(stat.desc(test_with_lags)[7:9,],file="C:/Santander/Stats/test_with_lags_stats.csv")
table(train_growth_all$new_product)

exclude_columns=c("fecha_dato","ncodpers","fecha_alta","ult_fec_cli_1t","tipodom","nomprov","num_products","num_new_products",
                  "num_new_products_factor","new_product","delta_products")

## Set the esponse variables as factor
# for (i in 1:length(top_products)) {
# train300K_with_lags[,top_products[i]] = as.factor(train300K_with_lags[,top_products[i]])
# }


mod_multinomial <- dplyr::filter(train_growth_all,fecha_dato >'2015-09-28' & fecha_dato <= '2016-03-28')
val_multinomial <- dplyr::filter(train_growth_all,fecha_dato > '2016-03-28')

# source("C:/Santander/Code/drop_list.R")
####################################################################################################################################################
# randomly split the modeling set mod.hex

mod_train=dplyr::sample_frac(mod_multinomial,size=0.6)
mod_valid=dplyr::anti_join(mod_multinomial,mod_train[,c("ncodpers","fecha_dato")])


biz_columns <-c("income_by_product","age_by_product","account_age_by_product","engagement","financial_maturity","banking_propensity")
new_cols <- c("avg_num_prod_last3","avg_num_prod_last6","max_num_prod_last3","max_num_prod_last6","min_num_prod_last3","min_num_prod_last6") #,"steady_6mon","steady_zero_products_6mon")
x <- names(mod_multinomial)[!(names(mod_multinomial) %in% c(product_columns,product_columns_growth,exclude_columns,biz_columns))]

test_raw_ids = as.data.frame(test_raw$ncodpers)
names(test_raw_ids)=c("ncodpers")

mod_train_short <- mod_train[,x]
mod_valid_short <- mod_valid[,x]
test_short <- val_multinomial[,x]
scoring_short <- train_ids <- dplyr::arrange(data.frame(temp_id = rep(c(1:nrow(mod_train)),12), ncodpers=rep(t(mod_train[,c("ncodpers")]),12),fecha_dato=rep(t(mod_train[,c("fecha_dato")]),12)),temp_id)[,x]

ordered_column_names <- colnames(mod_train_short)[order(colnames(mod_train_short))]

mod_train_short <- mod_train_short[,ordered_column_names]
mod_valid_short <- mod_valid_short[,ordered_column_names]
test_short <- test_short[,ordered_column_names]
scoring_short <- scoring_short[,ordered_column_names]

scoring.xgb <- xgb.DMatrix(data = as.matrix(scoring_short), missing=NaN)

model_date = "Dec05"

y <- "new_product"

train_y_factor <- mod_train[,y]
valid_y_factor <- mod_valid[,y]
test_y_factor <- val_multinomial[,y]

train_y <- as.numeric(mod_train[,y])-1
valid_y <- as.numeric(mod_valid[,y])-1
test_y <- as.numeric(val_multinomial[,y])-1

mod_train.xgb <- xgb.DMatrix(data = as.matrix(mod_train_short), label = train_y, missing=NaN)
mod_valid.xgb <- xgb.DMatrix(data = as.matrix(mod_valid_short), label = valid_y, missing=NaN)
mod_test.xgb <- xgb.DMatrix(data = as.matrix(test_short), label = test_y, missing=NaN)


watchlist <- list(eval=mod_valid.xgb, mod_train.xgb)

registerDoParallel(4,cores=4)
getDoParWorkers()

# ImplicitCluster()

eta=0.05
seed=20161206

xgbGrid <- expand.grid(nrounds = 200, #the maximum number of iterations
                       eta = c(eta), # shrinkage
                       max_depth = c(5,7,8,10,12),
                       gamma=c(0.5,1),
                       colsample_bytree=c(0.2,0.6,0.8),
                       min_child_weight=c(50,10,5),
                       subsample=c(0.6,0.8)
)

xgbTrControl <- trainControl(method = "cv",   # 10fold cross validation
                             number = 5,							# do 5 repititions of cv
                             summaryFunction=defaultSummary,	# Use AUC to pick the best model
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

xgbGrid$results

assign(paste0("Grid_results"),dplyr::arrange(as.data.frame(xgbGrid$results),desc(Accuracy)))

# Save grid search results
write.csv(get(paste0("Grid_results")),file=paste0("C:/Santander/Saved Models/",model_date," Multinomial XGboost/Grids/Grid_results_eta",eta,".csv"))

# Retrieve previously saved grid search results
# assign(paste0("Grid_results",i),read.csv(paste0("C:/Santander/Saved Grids/",model_date," XGboost/Grid_results_eta",eta,"_",i,".csv")))

best_params <- get(paste0("Grid_results"))[1,]
print(best_params)

parama <- list(
  "eta" = best_params$eta
  ,"objective" = "multi:softprob"
  ,"min_child_weight" = best_params$min_child_weight
  ,"subsample" = best_params$subsample
  ,"max_depth" = best_params$max_depth
  , "eval.metric" = "merror"
  ,"colsample_bytree" = best_params$colsample_bytree
  ,"gamma" = best_params$gamma
  # ,"scale_pos_weight" = PAR$scale_pos_weight
  #            ,"alpha" = PAR$alpha
  # ,"nthreads" = 4
)

set.seed(seed)

# xgbEarlyStop <- xgb.train(
xgbEarlyStop <- xgb.cv(
  nfold = 5,
  params = parama,
  data =  mod_train.xgb,
  nrounds = 2000,
  early.stop.round = 50,
  watchlist = watchlist,
  eval_metric = "merror",
  method = "xgbTree",
  maximize = FALSE,
  print.every.n=20,
  missing = NaN,
  num_class = 12,
  nthread=4
)

xgbFull <- xgbEarlyStop

assign(paste0("best_iter"),xgbEarlyStop$bestInd)
# assign(paste0("best_iter_",i),nrow(xgbEarlyStop.cv)-50)

set.seed(seed)

xgbFull <- xgb.train(
  params = parama,
  data =  mod_train.xgb,
  nrounds = get(paste0("best_iter")),
  # early.stop.round = 50,
  watchlist = watchlist,
  eval_metric = "merror",
  method = "xgbTree",
  maximize = FALSE,
  print.every.n=100,
  missing = NaN,
  num_class = 12,
  nthread=4
)



# Save the best model on disk for easy retrieval
xgb.save(xgbFull, paste0("C:/Santander/Saved Models/",model_date," Multinomial XGboost/xgbFull_eta",eta))

# Load a previously saved model

# xgbFull <- xgb.load(paste0("C:/Santander/Saved Models/",model_date," XGboost/xgbFull_eta",eta,"_",i))
# assign(paste0("best_iter_",i),999999)
# Score all datasets

importance = xgb.importance(feature_names = ordered_column_names,model = xgbFull)

write.csv(importance,file=paste0("C:/Santander/Saved Models/",model_date," Multinomial XGboost/Importance/variable_importance_model_",model_date,"_eta",eta,".csv"))

predicted_train=as.data.frame(predict(xgbFull,mod_train.xgb))
predicted_valid=as.data.frame(predict(xgbFull,mod_valid.xgb))
predicted_test=as.data.frame(predict(xgbFull,mod_test.xgb))
predicted_scoring=as.data.frame(predict(xgbFull,scoring.xgb))

names(predicted_train) = names(predicted_valid) = names(predicted_test) <- names(predicted_scoring) <- c("pred")

# probs_train <- as.data.frame(t(matrix(predicted_train, nrow=12, ncol=nrow(predicted_train)/12)))




# pred <- matrix(predicted_train, ncol = 12, byrow = TRUE)
train_ids <- dplyr::arrange(data.frame(temp_id = rep(c(1:nrow(mod_train)),12), ncodpers=rep(t(mod_train[,c("ncodpers")]),12),fecha_dato=rep(t(mod_train[,c("fecha_dato")]),12)),temp_id)
valid_ids <- dplyr::arrange(data.frame(temp_id = rep(c(1:nrow(mod_valid)),12), ncodpers=rep(t(mod_valid[,c("ncodpers")]),12),fecha_dato=rep(t(mod_valid[,c("fecha_dato")]),12)),temp_id)
test_ids <- dplyr::arrange(data.frame(temp_id = rep(c(1:nrow(val_multinomial)),12), ncodpers=rep(t(val_multinomial[,c("ncodpers")]),12),fecha_dato=rep(t(val_multinomial[,c("fecha_dato")]),12)),temp_id)
scoring_ids <- dplyr::arrange(data.frame(temp_id = rep(c(1:nrow(test_9lags_v3_imputed)),12), ncodpers=rep(t(test_9lags_v3_imputed[,c("ncodpers")]),12),fecha_dato=rep(t(test_9lags_v3_imputed[,c("fecha_dato")]),12)),temp_id)


pred_products_train_vertical <- data.frame(train_ids,pred = predicted_train,product = rep(top_products[order(top_products)],nrow(mod_train)),prod_num = rep(c(1:12),nrow(mod_train)))
predicted_train.product <- pred_products_train_vertical %>%   dplyr::group_by(ncodpers, fecha_dato) %>%  filter(pred == max(pred)) 

pred_products_valid_vertical <- data.frame(valid_ids,pred = predicted_valid,product = rep(top_products[order(top_products)],nrow(mod_valid)),prod_num = rep(c(1:12),nrow(mod_valid)))
predicted_valid.product <- pred_products_valid_vertical %>%   dplyr::group_by(ncodpers, fecha_dato) %>%  filter(pred == max(pred)) 

pred_products_test_vertical <- data.frame(test_ids,pred = predicted_test,product = rep(top_products[order(top_products)],nrow(test_short)),prod_num = rep(c(1:12),nrow(test_short)))
predicted_test.product <- pred_products_test_vertical %>%   dplyr::group_by(ncodpers, fecha_dato) %>%  filter(pred == max(pred)) 

pred_products_scoring_vertical <- data.frame(scoring_ids,pred = predicted_scoring,product = rep(top_products[order(top_products)],nrow(scoring_short)),prod_num = rep(c(1:12),nrow(scoring_short)))

# Compute and print AUCs
accuracy_train <- mean(gsub("_growth","",predicted_train.product$product)==mod_train$new_product)
accuracy_valid <- mean(gsub("_growth","",predicted_valid.product$product)==mod_valid$new_product)
accuracy_test <- mean(gsub("_growth","",predicted_test.product$product)==val_multinomial$new_product)


print(paste("Accuracy training set: ",round(accuracy_train,4)))
print(paste("Accuracy validation set: ",round(accuracy_valid,4)))
print(paste("Accuracy test set: ",round(accuracy_test,4)))


table(predicted_train.product$product,mod_train$new_product)
table(predicted_valid.product$product,mod_valid$new_product)

all_scores_sorted  <- dplyr::arrange(pred_products_scoring_vertical,ncodpers,desc(pred))
# all_scores_sorted  <- dplyr::arrange( all_scores_6prod,ncodpers,desc(pred.cal))
all_scores_sorted$prediction <- all_scores_sorted$pred
# all_scores_sorted$pred=all_scores_sorted$pred.cal= NULL

all_scores_sorted <- all_scores_sorted[,c("ncodpers","prediction","product")]
all_scores_sorted$product <- gsub("_growth","",all_scores_sorted$product)


load("C:/Santander/data/actual_current_products_active.RData")

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
write.csv(submission[,1:2],file="C:/Santander/Submissions/submission36_Multinomial_XGboost_6lags_v3_eta0.05_Dec05_logloss.csv",row.names = FALSE)


























# pred_products_scoring_vertical$predict = pred_products_scoring_vertical$score = pred_products_scoring_vertical$id = NULL

#Transpose the scores dataset from vertical to horizontal
pred_products_train_transposed <- spread(pred_products_train_vertical, prod_num, product)


confusionMatrix(predicted_train$pred,train_y)
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
assign(paste0("scores_",i),cbind(test_with_lags_dummies_imputed[,c("ncodpers","fecha_dato")],predicted_scoring))

eq <-  paste0("save(train_scores_",i,", valid_scores_",i,", test_scores_",i,", scores_",i,",file='C:/Santander/Saved Models/",model_date," XGboost/Scored Data/model_eta",eta,"_",i,"_scores.RData')")
eval(parse(text=eq))




best_models <- dplyr::bind_rows(best_models,get(paste0("best_params",i)))

print(paste("End time:",Sys.time()))

assign(paste0("best_models_",model_date,"_eta",eta),best_models)
write.csv(get(paste0("best_models_",model_date,"_eta",eta)),file=paste0("C:/Santander/Stats/best_models_",model_date,"_eta",eta,".csv"))



# select the top model with the highest validation AUC as the best model from each grid
best_model_from_multinomial_grid <- h2o.getModel(sortedGrid_mulinomial@model_ids[[1]])

h2o.saveModel(best_model_from_multinomial_grid,path="C:/Santander/Saved Models/Nov11_6lag_plus/")

# record the best model parameters (default values remain as NA)
model_size <- as.data.frame(cbind(nrow(train_multinomial.hex),nrow(valid_multinomial.hex),nrow(test_multinomial.hex),length(x)))
names(model_size) <- c("Train_Rows","Validation_Rows","Test_Rows","Num_Model_Columns")


#sample_rate_per_class <- as.data.frame(toString(as.character(best_model_from_multinomial_grid@parameters$sample_rate_per_class)))
#names(sample_rate_per_class) <- c("sample_rate_per_class")
best_model_parms_temp <- cbind(model_size,as.data.frame(best_model_from_multinomial_grid@parameters[!(names(best_model_from_multinomial_grid@parameters) %in% c("x","y","sample_rate_per_class"))]))

print("Mulinomial GBM Grid: 9 products:")
print(top_products[1:9])

pred_products_train = as.data.frame(h2o.predict(best_model_from_multinomial_grid,newdata = train_multinomial.hex))
pred_products_valid = as.data.frame(h2o.predict(best_model_from_multinomial_grid,newdata = valid_multinomial.hex))
pred_products_test = as.data.frame(h2o.predict(best_model_from_multinomial_grid,newdata = test_multinomial.hex))
pred_products_scoring = as.data.frame(h2o.predict(best_model_from_multinomial_grid,newdata = scoring.hex))

print(h2o.confusionMatrix(best_model_from_multinomial_grid,train_multinomial.hex))
print(h2o.confusionMatrix(best_model_from_multinomial_grid,valid_multinomial.hex))
print(h2o.confusionMatrix(best_model_from_multinomial_grid,test_multinomial.hex))

pred_products_scoring=cbind(test_raw$ncodpers,as.data.frame(pred_products_scoring))

# best_model <-h2o.getModel(sortedGrid@model_ids[[1]])


# auc_report_6lags_Nov8_part3 <- auc_report
# mean_predictions_6lags_Nov8_part3 <- mean_predictions
# 
# write.csv(auc_report_6lags_Nov8_part2,file="C:/Santander/Stats/auc_report_6lags_tuned_GBM_Nov8_part2.csv")
# write.csv(mean_predictions_6lags_Nov8_part2,file="C:/Santander/Stats/mean_predictions_6lags_tuned_GBM_Nov8_part2.csv")



###################################################################################################################
## Use the best model hyperparameters and create an n-fold cross-validation model
###################################################################################################################
# best_models <- read.csv("C:/Santander/Stats/auc_report_tuned_GBM_Nov8_part2.csv")
# best_models$nbins[is.na(best_models$nbins)] <- 20
# best_models$max_depth[is.na(best_models$max_depth)] <- 5
# 
# for (i in 1:9)  {
#   
# model_id <- paste0("xval_model",i)
# #best_model <- get(paste0("best_model_from_grid",i))
# best_mod <- best_models[i=i,]
# 
# y <- top_products[i]
# 
# gbm<-h2o.gbm(x = x,
#              y = y,
#              training_frame = train.hex,
#              validation_frame = valid.hex,
#              nfolds = 5,
#              model_id = model_id,
#              score_tree_interval = 10,
#              ntrees = 2000,
#              max_depth = as.list(best_mod$max_depth)[[1]],
#              min_rows =  as.list(best_mod$min_rows)[[1]],
#              nbins_cats = as.list(best_mod$nbins_cats)[[1]],   
#              stopping_rounds = 5,
#              stopping_metric = "AUC",
#              stopping_tolerance = 1e-4,
#              learn_rate = 0.05,
#              learn_rate_annealing=0.99,
#              distribution="bernoulli",  
#              sample_rate = as.list(best_mod$sample_rate)[[1]],
#              col_sample_rate = as.list(best_mod$col_sample_rate)[[1]],
#              histogram_type = as.character(as.list(best_mod$histogram_type)[[1]]),
#              # sample_rate_per_class = c(1,1),
#              nbins = as.list(best_mod$nbins)[[1]],
#              seed=1234)
# 
#   model_size <- as.data.frame(cbind(nrow(train.hex),nrow(valid.hex),nrow(test.hex),length(x)))
#   names(model_size) <- c("Train_Rows","Validation_Rows","Test_Rows","Num_Model_Columns")
#   
#   sample_rate_per_class <- as.data.frame(toString(as.character(gbm@parameters$sample_rate_per_class)))
#   names(sample_rate_per_class) <- c("sample_rate_per_class")
#   best_model_parms_temp <- cbind(model_size,as.data.frame(gbm@parameters[!(names(gbm@parameters) %in% c("x","y","sample_rate_per_class"))]),sample_rate_per_class)
#   
#   print(paste("GBM xvald Model:",i," ",top_products[i]))
#   
#   auc_train=h2o.auc(h2o.performance(gbm, newdata = train.hex))
#   auc_valid=h2o.auc(h2o.performance(gbm, newdata = valid.hex))
#   auc_test=h2o.auc(h2o.performance(gbm, newdata = test.hex))
#   auc_temp= cbind(data.frame(i,top_products[i],auc_train,auc_valid,auc_test))
#   names(auc_temp)=c("Model_ID","Product_Name","AUC_Train","AUC_Valid","AUC_Test")
#   auc_comb=cbind(auc_temp,best_model_parms_temp)
#   
#    print(paste("AUC training set: ",round(auc_train,4)))
#    print(paste("AUC validation set: ",round(auc_valid,4)))
#    print(paste("AUC test set: ",round(auc_test,4)))
#   # Score all datasets
#    assign(paste0("fit.train_",i),h2o.predict(gbm,newdata = train.hex))
#    assign(paste0("fit.valid_",i),h2o.predict(gbm,newdata = valid.hex))
#    assign(paste0("fit.test_",i),h2o.predict(gbm,newdata = test.hex))
#    assign(paste0("fit.scoring_",i),h2o.predict(gbm,newdata = scoring.hex))
#    
#   # Confusion matrices for all datasets
#    # print(h2o.confusionMatrix(get(paste0("best_model_from_grid",i)),train.hex))
#    # print(h2o.confusionMatrix(get(paste0("best_model_from_grid",i)),valid.hex))
#    # print(h2o.confusionMatrix(get(paste0("best_model_from_grid",i)),test.hex))
# 
#    print(summary(get(paste0("fit.train_",i))))
#    print(summary(get(paste0("fit.valid_",i))))
#    print(summary(get(paste0("fit.test_",i))))
#    print(summary(get(paste0("fit.scoring_",i))))
# 
#   assign(paste0("scores_",i),cbind(test_raw_ids,as.data.frame(get(paste0("fit.scoring_",i)))))
#   
#   # Get the mean valus of predictions on all data sets
#    mean_predictions_temp=data.frame(i,top_products[i],h2o.mean(get(paste0("fit.train_",i))$p1),h2o.mean(get(paste0("fit.valid_",i))$p1),h2o.mean(get(paste0("fit.test_",i))$p1),h2o.mean(get(paste0("fit.scoring_",i))$p1))
#    names(mean_predictions_temp)=c("Model_ID","Product_Name","Mean_Train_Prediction","Mean_Valid_Prediction","Mean_Test_Prediction","Mean_Scoring_Prediction")
#   
#   # Aggregate best model parameters, AUC values and mean values of predictions
#   
#    if (i==1) {
#      mean_predictions <- mean_predictions_temp
#      auc_report <- auc_comb
#    }
#    else {
#      mean_predictions <- dplyr::bind_rows(mean_predictions,mean_predictions_temp)
#      auc_report <- dplyr::bind_rows(auc_report,auc_comb)
#    }
#    
#    # Save the best model on disk for easy retrieval
#    h2o.saveModel(gbm,path = "C:/Santander/Saved Models/best_6lag_model/",force = T)
# 
# }
# 
# mean_predictions_Nov8_part2_xval <- mean_predictions
# auc_report_Nov8_part2_xval <- auc_report
# 
# 
# write.csv(mean_predictions_Nov8_part2_xval,file="C:/Santander/Stats/mean_predictions_tuned_GBM_Nov8_part2_xval.csv")
# write.csv(auc_report_Nov8_part2_xval,file="C:/Santander/Stats/auc_report_tuned_GBM_Nov8_part2_xval.csv")

# Compute the means of response variables (event rates) by modeling/validation split
# train100K_with_lags_numeric=train100K_with_lags
# for (i in 1:length(top_products)) {
#   train100K_with_lags_numeric[,top_products[i]] = as.numeric(as.character(train100K_with_lags[,top_products[i]]))
# }
# 
# mod_numeric=dplyr::filter(train100K_with_lags_numeric,fecha_dato >'2015-03-28' & fecha_dato <= '2016-03-28')
# val_numeric=dplyr::filter(train100K_with_lags_numeric,fecha_dato > '2016-03-28')
# 
# write.csv(stat.desc(mod_numeric[,top_products])[7:9,],file="C:/Santander/Stats/growth_stats_mod.csv")
# write.csv(stat.desc(val_numeric[,top_products])[7:9,],file="C:/Santander/Stats/growth_stats_val.csv")


## Apply prediction modifiers in order to true up the predictions (modifier = actual event rate/mean prediction)
## Modifiers were computed in excel as simple ratios

# prediction_modifier=c(1,1,1,1,1,1,1,1,1,1,1,1)
#prediction_modifier=c( 0.8017,0.9283,0.9797,0.9173,0.7617,0.8918,1.1027,0.0715,0.7897) # from test dataset actual/model mean ratios
#prediction_modifier=c( 0.8483,1.0277,1.0344,1.0202,0.9419,1.0175,0.8233,0.9897,0.7706) # from modeling dataset actual/model mean ratios

# for (j in 1:9) #length(prediction_modifier))
# { 
#   eq1 <- paste0("scores_",j,"$p0=NULL")
#   eval(parse(text=eq1))
#   eq2 <- paste0("scores_",j,"$prediction=scores_",j,"$p1*prediction_modifier[",j,"]" )
#   eval(parse(text=eq2))
#   eq3 <- paste0("scores_",j,"$product=gsub('_growth','',top_products[",j,"])")
#   eval(parse(text=eq3))
#   
#   if (j==1)
#   {
#     all_scores=get(paste0("scores_",j))
#   } else {
#     all_scores=rbind(all_scores,get(paste0("scores_",j)))
#     }
# }

# Reshape the test output data from horizontal to vertical, so that we can reshape it back in a more usable horizontal form

pred_products_scoring_vertical <- reshape(pred_products_scoring,
                                          varying = gsub("_growth","",top_products[1:9]),
                                          v.names = "score",
                                          timevar = "product",
                                          direction = "long",
                                          times = gsub("_growth","",top_products[1:9]))
names(pred_products_scoring_vertical) <- c("ncodpers","predict","product","score","id")
pred_products_scoring_vertical <- arrange(pred_products_scoring_vertical,ncodpers,desc(score))

# create a column that simply runs prod1, prod2, ... , prod9 and repeats for each incodpers. These column will be used for transposing the products
pred_products_scoring_vertical$prod_num <- as.integer(rownames(pred_products_scoring_vertical))  %% 9
pred_products_scoring_vertical$prod_num[pred_products_scoring_vertical$prod_num == 0] <- 9
pred_products_scoring_vertical$prod_num <- paste0("prod",pred_products_scoring_vertical$prod_num)

pred_products_scoring_vertical$predict = pred_products_scoring_vertical$score = pred_products_scoring_vertical$id = NULL

#Transpose the scores dataset from vertical to horizontal
pred_products_scoring_transposed <- spread(pred_products_scoring_vertical, prod_num,product)

# all_scores_sorted  <- dplyr::arrange(all_scores,ncodpers,desc(prediction))
# all_scores_sorted$p1=all_scores_sorted$predict=all_scores_sorted$prediction = NULL
# 
# # create a column that simply runs prod1, prod2, ... , prod9 and repeats for each incodpers. These column will be used for transposing the products
# all_scores_sorted$prod_num <- as.integer(rownames(all_scores_sorted))  %% 9
# all_scores_sorted$prod_num[all_scores_sorted$prod_num == 0] <- 9
# all_scores_sorted$prod_num <- paste0("prod",all_scores_sorted$prod_num)

#Transpose the scores dataset from vertical to horizontal
# all_scores_transposed <- spread(all_scores_sorted, prod_num,product)

#####################################################################################################################
# Combine propensity models with number of products model results 
# Merge the ordered products data frame with the predicted number of products data frame, and produce the output data
#######################################################################################################################
 
#all_scores_combined=cbind(all_scores_transposed,pred_num_products_scoring$predict)
all_scores_combined=cbind(pred_products_scoring_transposed,pred_num_products_scoring$predict)

names(all_scores_combined$`pred_num_products_scoring$predict`) <- c("pred_number_of_products")

all_scores_combined$pred_num_products = as.numeric(as.character(all_scores_combined$`pred_num_products_scoring$predict`))
all_scores_combined$`pred_num_products_scoring$predict`=NULL

# Create an output string from all individual prod1, prod2... variables up to the predicted number of products (pred_num_products)

all_scores_combined$output_string=NA

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


save(all_scores_combined_string,file="C:/Santander/Data/all_scores_combined_multinomial_6lags.RData")


submission=all_scores_combined_string[,c("ncodpers","output_string")]
names(submission)=c("ncodpers","added_products")

write.csv(submission[,1:2],file="C:/Santander/Submissions/submission10_Nov11_multinomial_6lags_plus.csv",row.names = FALSE)

