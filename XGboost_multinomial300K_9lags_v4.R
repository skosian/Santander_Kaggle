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
library(Hmisc)


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

write.csv(stats.train300K_9lags_v4,file="C:/Santander/EDA/stats.train300K_9lags_v4_Dec16.csv")
load("C:/Santander/EDA/stats.train300K_9lags_v4_Dec16.csv")

#exclude response variables from imputation lists
numeric_columns_train300K <- numeric_columns_train300K[!(numeric_columns_train300K %in% c(top_products,"num_products","num_new_products","delta_products"))]
numeric_columns_test <- numeric_columns_test[!(numeric_columns_test %in% c(top_products,"num_products","num_new_products","delta_products"))]

names(train300K_9lags_v4)[order(names(train300K_9lags_v4))]

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

# Select only non-zero growth records, and create a column called new_product that identifies the new product name
for (i in 1:21) {
   assign(paste0("train_growth",i),subset(train300K_9lags_v4_imputed,get(top_products[i])==1))
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


mod_multinomial <- dplyr::filter(train_growth_all,fecha_dato >'2015-09-28' & fecha_dato <= '2016-03-28')
val_multinomial <- dplyr::filter(train_growth_all,fecha_dato > '2016-03-28')

source("C:/Santander/Code/drop_list_v4.R")
####################################################################################################################################################
# randomly split the modeling set mod.hex

mod_train=dplyr::sample_frac(mod_multinomial,size=0.6)
mod_valid=dplyr::anti_join(mod_multinomial,mod_train[,c("ncodpers","fecha_dato")])


biz_columns <-c("income_by_product","age_by_product","account_age_by_product","engagement","financial_maturity","banking_propensity")
new_cols <- c("avg_num_prod_last3","avg_num_prod_last6","max_num_prod_last3","max_num_prod_last6","min_num_prod_last3","min_num_prod_last6") #,"steady_6mon","steady_zero_products_6mon")
x <- names(mod_multinomial)[!(names(mod_multinomial) %in% c(product_columns,product_columns_growth,exclude_columns,biz_columns,drop_constants))]

# source("C:/Santander/Code/multinomial_keep_lists.R")
# keep_list2=keep_list1[1:157]
# x <- keep_list2

test_raw_ids = as.data.frame(test_raw$ncodpers)
names(test_raw_ids)=c("ncodpers")

mod_train_short <- mod_train[,x]
mod_valid_short <- mod_valid[,x]
test_short <- val_multinomial[,x]
scoring_short <- test_9lags_v4_imputed[,x]

ordered_column_names <- colnames(mod_train_short)[order(colnames(mod_train_short))]

mod_train_short <- mod_train_short[,ordered_column_names]
mod_valid_short <- mod_valid_short[,ordered_column_names]
test_short <- test_short[,ordered_column_names]
scoring_short <- scoring_short[,ordered_column_names]


# Variable clustering
# mod_train_short.matrix <- data.matrix(mod_train_short)
# cl1 <- varclus(mod_train_short.matrix,similarity = "hoeffding")
# print(round(cl1$sim,2))
# 
# plot(cl1)

# stats.mod_train_short <- stat.desc(mod_train_short)
# stats.scoring_short <- stat.desc(scoring_short)
# 
# write.csv(stats.mod_train_short,file="C:/Santander/EDA/stats.mod_train_short.csv")
# write.csv(stats.scoring_short,file="C:/Santander/EDA/stats.scoring_short.csv")

scoring.xgb <- xgb.DMatrix(data = as.matrix(scoring_short), missing=NaN)

model_date = "Dec18"

y <- "new_product"

train_y_factor <- mod_train[,y]
valid_y_factor <- mod_valid[,y]
test_y_factor <- val_multinomial[,y]


train_y <- as.numeric(mod_train[,y])-1
valid_y <- as.numeric(mod_valid[,y])-1
test_y <- as.numeric(val_multinomial[,y])-1

mod_y <- rbind(train_y,valid_y)
mod_short <- rbind(mod_train,mod_valid)

mod_train.xgb <- xgb.DMatrix(data = as.matrix(mod_train_short), label = train_y, missing=NaN)
mod_valid.xgb <- xgb.DMatrix(data = as.matrix(mod_valid_short), label = valid_y, missing=NaN)
mod_test.xgb <- xgb.DMatrix(data = as.matrix(test_short), label = test_y, missing=NaN)


watchlist <- list(eval=mod_valid.xgb, mod_train.xgb)

registerDoParallel(4,cores=4)
getDoParWorkers()

# ImplicitCluster()

eta=0.05
seed=20161216
version=2

xgbGrid <- expand.grid(nrounds = 200, #the maximum number of iterations
                       eta = c(eta), # shrinkage
                       max_depth = c(3,4,5,7,8),
                       gamma=c(0.5,1),
                       colsample_bytree=c(0.2,0.6,0.8),
                       min_child_weight=c(10),
                       subsample=c(0.8,1)
)

xgbTrControl <- trainControl(method = "cv",   # 10fold cross validation
                             number = 2,							# do 5 repititions of cv
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
write.csv(get(paste0("Grid_results")),file=paste0("C:/Santander/Saved Models/",model_date," Multinomial XGboost/Grids/Grid_results_eta",eta,"_version",version,".csv"))

# Retrieve previously saved grid search results
assign(paste0("Grid_results"),read.csv(paste0("C:/Santander/Saved Models/",model_date," Multinomial XGboost/Grids/Grid_results_eta",eta,"_version",version,".csv")))

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

xgbEarlyStop <- xgb.train(
# xgbEarlyStop <- xgb.cv(
#   nfold = 5,
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
  num_class = 21,
  nthread=4
)

# xgbFull <- xgbEarlyStop

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
  print.every.n=10,
  missing = NaN,
  num_class = 21,
  nthread=4
)


# Save the best model on disk for easy retrieval
xgb.save(xgbFull, paste0("C:/Santander/Saved Models/",model_date," Multinomial XGboost/xgbFull_eta",eta,"_version",version))

# Load a previously saved model

# xgbFull <- xgb.load(paste0("C:/Santander/Saved Models/",model_date," Multinomial XGboost/xgbFull_eta",eta,"_version",version))
# assign(paste0("best_iter_",i),999999)
# Score all datasets

importance = xgb.importance(feature_names = ordered_column_names,model = xgbFull)

write.csv(importance,file=paste0("C:/Santander/Saved Models/",model_date," Multinomial XGboost/Importance/variable_importance_model_",model_date,"_eta",eta,"_version",version,".csv"))

predicted_train=as.data.frame(predict(xgbFull,mod_train.xgb))
predicted_valid=as.data.frame(predict(xgbFull,mod_valid.xgb))
predicted_test=as.data.frame(predict(xgbFull,mod_test.xgb))
predicted_scoring=as.data.frame(predict(xgbFull,scoring.xgb))

names(predicted_train) = names(predicted_valid) = names(predicted_test) <- names(predicted_scoring) <- c("pred")

# probs_train <- as.data.frame(t(matrix(predicted_train, nrow=12, ncol=nrow(predicted_train)/12)))

# pred <- matrix(predicted_train, ncol = 12, byrow = TRUE)
train_ids <- dplyr::arrange(data.frame(temp_id = rep(c(1:nrow(mod_train)),21), ncodpers=rep(t(mod_train[,c("ncodpers")]),21),fecha_dato=rep(t(mod_train[,c("fecha_dato")]),21)),temp_id)
valid_ids <- dplyr::arrange(data.frame(temp_id = rep(c(1:nrow(mod_valid)),21), ncodpers=rep(t(mod_valid[,c("ncodpers")]),21),fecha_dato=rep(t(mod_valid[,c("fecha_dato")]),21)),temp_id)
test_ids <- dplyr::arrange(data.frame(temp_id = rep(c(1:nrow(val_multinomial)),21), ncodpers=rep(t(val_multinomial[,c("ncodpers")]),21),fecha_dato=rep(t(val_multinomial[,c("fecha_dato")]),21)),temp_id)
scoring_ids <- dplyr::arrange(data.frame(temp_id = rep(c(1:nrow(test_9lags_v4_imputed)),21), ncodpers=rep(t(test_9lags_v4_imputed[,c("ncodpers")]),21),fecha_dato=rep(t(test_9lags_v4_imputed[,c("fecha_dato")]),21)),temp_id)


pred_products_train_vertical <- data.frame(train_ids,pred = predicted_train,product = rep(top_products[order(top_products)],nrow(mod_train)),prod_num = rep(c(1:21),nrow(mod_train)))
predicted_train.product <- pred_products_train_vertical %>%   dplyr::group_by(ncodpers, fecha_dato) %>%  filter(pred == max(pred)) 

pred_products_valid_vertical <- data.frame(valid_ids,pred = predicted_valid,product = rep(top_products[order(top_products)],nrow(mod_valid)),prod_num = rep(c(1:21),nrow(mod_valid)))
predicted_valid.product <- pred_products_valid_vertical %>%   dplyr::group_by(ncodpers, fecha_dato) %>%  filter(pred == max(pred)) 

pred_products_test_vertical <- data.frame(test_ids,pred = predicted_test,product = rep(top_products[order(top_products)],nrow(test_short)),prod_num = rep(c(1:21),nrow(test_short)))
predicted_test.product <- pred_products_test_vertical %>%   dplyr::group_by(ncodpers, fecha_dato) %>%  filter(pred == max(pred)) 

pred_products_scoring_vertical <- data.frame(scoring_ids,pred = predicted_scoring,product = rep(top_products[order(top_products)],nrow(scoring_short)),prod_num = rep(c(1:21),nrow(scoring_short)))
predicted_scoring.product <- pred_products_scoring_vertical %>%   dplyr::group_by(ncodpers, fecha_dato) %>%  filter(pred == max(pred)) 

# Compute and print AUCs
accuracy_train <- mean(gsub("_growth","",predicted_train.product$product)==mod_train$new_product)
accuracy_valid <- mean(gsub("_growth","",predicted_valid.product$product)==mod_valid$new_product)
accuracy_test <- mean(gsub("_growth","",predicted_test.product$product)==val_multinomial$new_product)


print(paste("Accuracy training set: ",round(accuracy_train,4)))
print(paste("Accuracy validation set: ",round(accuracy_valid,4)))
print(paste("Accuracy test set: ",round(accuracy_test,4)))

table(predicted_train.product$product,mod_train$new_product)
# table(predicted_valid.product$product,mod_valid$new_product)

predicted_train.product %>% dplyr::group_by(product) %>% dplyr::summarise(count=n())
predicted_valid.product %>% dplyr::group_by(product) %>% dplyr::summarise(count=n())
predicted_test.product %>% dplyr::group_by(product) %>% dplyr::summarise(count=n())
predicted_scoring.product %>% dplyr::group_by(product) %>% dplyr::summarise(count=n())

# Get the mean valus of predictions on all data sets
data_info <- data.frame(nrow(mod_train_short),nrow(mod_valid_short),nrow(test_short),length(x))
names(data_info)=c("Train_Rows","Valid_Rows","Test_Rows","Number_of_columns")


# Create a summary vector that has all model parameters for the best model and AUC values and mean predictions
perf_metric <- as.data.frame(cbind(accuracy_train,accuracy_valid,accuracy_test,get(paste0("best_iter")),seed))
names(perf_metric) <- c("accuracy_train","accuracy_valid","accuracy_test","best_iteration","seed_number")

best_multinomial_model <- cbind(perf_metric,best_params,data_info)

eq <-  paste0("save(pred_products_train_vertical, pred_products_valid_vertical, pred_products_test_vertical, pred_products_scoring_vertical,file='C:/Santander/Saved Models/",model_date," Multinomial XGboost/Scored Data/model_eta",eta,"_version",version,"_scores.RData')")
eval(parse(text=eq))

print(paste("End time:",Sys.time()))

write.csv(best_multinomial_model,file=paste0("C:/Santander/Stats/best_multinomial_model_",model_date,"_eta",eta,"_version",version,".csv"))





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
all_scores_sorted2$prod_num <- as.integer(rownames(all_scores_sorted2))  %% 21
all_scores_sorted2$prod_num[all_scores_sorted2$prod_num == 0] <- 21
all_scores_sorted2$prod_num <- paste0("prod",all_scores_sorted2$prod_num)

#Transpose the scores dataset from vertical to horizontal
all_scores_sorted2$prediction = all_scores_sorted2$multiplier = all_scores_sorted2$new_score=NULL
all_scores_transposed <- spread(all_scores_sorted2, prod_num,product)

# table(all_scores_transposed$prod1)
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
write.csv(submission[,1:2],file="C:/Santander/Submissions/submission48_Multinomial_XGboost_9lags_v4_eta0.05_Dec21.csv",row.names = FALSE)
