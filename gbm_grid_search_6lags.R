library(dplyr)
library(pastecs)
library(pryr)
library(h2o)
library(tidyr)
library(reshape)
h2o.init(nthreads = 4)


product_columns=c("ind_ahor_fin_ult1","ind_aval_fin_ult1","ind_cco_fin_ult1","ind_cder_fin_ult1","ind_cno_fin_ult1","ind_ctju_fin_ult1",
                  "ind_ctma_fin_ult1","ind_ctop_fin_ult1","ind_ctpp_fin_ult1","ind_deco_fin_ult1","ind_deme_fin_ult1","ind_dela_fin_ult1",
                  "ind_ecue_fin_ult1","ind_fond_fin_ult1","ind_hip_fin_ult1","ind_plan_fin_ult1","ind_pres_fin_ult1","ind_reca_fin_ult1",
                  "ind_tjcr_fin_ult1","ind_valo_fin_ult1","ind_viv_fin_ult1","ind_nomina_ult1","ind_nom_pens_ult1","ind_recibo_ult1")

top_products=c("ind_recibo_ult1_growth","ind_nom_pens_ult1_growth","ind_nomina_ult1_growth","ind_tjcr_fin_ult1_growth","ind_cco_fin_ult1_growth",
               "ind_cno_fin_ult1_growth","ind_ecue_fin_ult1_growth","ind_dela_fin_ult1_growth","ind_reca_fin_ult1_growth","ind_ctma_fin_ult1_growth",
               "ind_valo_fin_ult1_growth","ind_fond_fin_ult1_growth")

#drop constant and redundant variables
train100K_with_lags$tipodom = train100K_with_lags$nomprov = train100K_with_lags$ult_fec_cli_1t = train100K_with_lags$fecha_alta = NULL

# write.csv(stat.desc(train100K_with_lags[,product_columns_growth])[7:9,],file="C:/Santander/Stats/growth_stats.csv")

exclude_columns=c("fecha_dato","ncodpers","fecha_alta","ult_fec_cli_1t","tipodom","nomprov","num_products","num_new_products","num_new_products_factor")

## Set the esponse variables as factor
for (i in 1:length(top_products)) {
train100K_with_lags[,top_products[i]] = as.factor(train100K_with_lags[,top_products[i]])
}

train100K_with_lags$num_new_products_factor=as.factor(train100K_with_lags$num_new_products)

mod=dplyr::filter(train100K_with_lags,fecha_dato >'2015-06-28' & fecha_dato <= '2016-03-28')
val=dplyr::filter(train100K_with_lags,fecha_dato > '2016-03-28')

mod.hex=as.h2o(mod)
test.hex = as.h2o(val)
scoring.hex=as.h2o(test_with_lags)


####################################################################################################################################################

# randomly split the modeling set mod.hex
splits <- h2o.splitFrame(data = mod.hex, ratios = c(0.7), destination_frames = c("train.hex", "valid.hex"), seed = 1234)

train.hex <- splits[[1]]
valid.hex <- splits[[2]]

x <- names(mod.hex)[!(names(mod.hex) %in% c(product_columns,product_columns_growth,exclude_columns))]

test_raw_ids = as.data.frame(test_raw$ncodpers)
names(test_raw_ids)=c("ncodpers")


for (i in 1:9) #length(top_products)) 
  {

y <- top_products[i]

grid_id = paste0("grid",i)

hyper_params = list( max_depth = c(4,5,6,7,8,10,12),
                     sample_rate=c(0.1,0.6,0.8),
                     col_sample_rate=c(0.1,0.2,0.4,0.6,0.8),
                     min_rows=c(25,50,100),
                     nbins=c(20,50,100),
                     nbins_cats = c(10,20,50),
                     histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin"),
                     sample_rate_per_class=list(c(0.5,1),c(1,1),c(0.8,1))
                     )
search_criteria = list(strategy = "RandomDiscrete",
                       max_runtime_secs = 3600,
                       max_models = 60,
                       seed = 1234567,
                       stopping_rounds = 5,
                       stopping_metric = "AUC",
                       stopping_tolerance = 1e-3
                       )


grid<-h2o.grid(x = x,
               y = y,
               training_frame = train.hex,
               validation_frame = valid.hex,
               hyper_params = hyper_params,
               search_criteria = search_criteria,
               grid_id = grid_id,
               algorithm = "gbm",
               distribution="bernoulli",
               ntrees = 2000,
               learn_rate = 0.05,
               stopping_rounds = 5,
               stopping_tolerance = 1e-4,
               stopping_metric = "AUC",
               learn_rate_annealing=0.99,
               score_tree_interval = 10,
               seed=1234)

assign(paste0("sortedGrid",i),h2o.getGrid(grid_id, sort_by = "auc", decreasing = TRUE))

print(get(paste0("sortedGrid",i)))

# select the top model with the highest validation AUC as the best model from each grid
eq <- paste0("best_model_from_grid",i,"=h2o.getModel(sortedGrid",i,"@model_ids[[1]])")
eval(parse(text=eq))

# record the best model parameters (default values remain as NA)
model_size <- as.data.frame(cbind(nrow(train.hex),nrow(valid.hex),nrow(test.hex),length(x)))
names(model_size) <- c("Train_Rows","Validation_Rows","Test_Rows","Num_Model_Columns")

bst <- get(paste0("best_model_from_grid",i)) # best model for iteration i
sample_rate_per_class <- as.data.frame(toString(as.character(bst@parameters$sample_rate_per_class)))
names(sample_rate_per_class) <- c("sample_rate_per_class")
best_model_parms_temp <- cbind(model_size,as.data.frame(bst@parameters[!(names(bst@parameters) %in% c("x","y","sample_rate_per_class"))]),sample_rate_per_class)

print(paste("GBM Grid:",i," ",top_products[i]))

 auc_train=h2o.auc(h2o.performance(get(paste0("best_model_from_grid",i)), newdata = train.hex))
 auc_valid=h2o.auc(h2o.performance(get(paste0("best_model_from_grid",i)), newdata = valid.hex))
 auc_test=h2o.auc(h2o.performance(get(paste0("best_model_from_grid",i)), newdata = test.hex))
 auc_temp= cbind(data.frame(i,top_products[i],auc_train,auc_valid,auc_test))
 names(auc_temp)=c("Model_ID","Product_Name","AUC_Train","AUC_Valid","AUC_Test")
 auc_comb=cbind(auc_temp,best_model_parms_temp)
 print(paste("AUC training set: ",round(auc_train,4)))
 print(paste("AUC validation set: ",round(auc_valid,4)))
 print(paste("AUC test set: ",round(auc_test,4)))
# Score all datasets
 assign(paste0("fit.train_",i),h2o.predict(get(paste0("best_model_from_grid",i)),newdata = train.hex))
 assign(paste0("fit.valid_",i),h2o.predict(get(paste0("best_model_from_grid",i)),newdata = valid.hex))
 assign(paste0("fit.test_",i),h2o.predict(get(paste0("best_model_from_grid",i)),newdata = test.hex))
 assign(paste0("fit.scoring_",i),h2o.predict(get(paste0("best_model_from_grid",i)),newdata = scoring.hex))
# Confusion matrices for all datasets
 # print(h2o.confusionMatrix(get(paste0("best_model_from_grid",i)),train.hex))
 # print(h2o.confusionMatrix(get(paste0("best_model_from_grid",i)),valid.hex))
 # print(h2o.confusionMatrix(get(paste0("best_model_from_grid",i)),test.hex))
#  
 print(summary(get(paste0("fit.train_",i))))
 print(summary(get(paste0("fit.valid_",i))))
 print(summary(get(paste0("fit.test_",i))))
 print(summary(get(paste0("fit.scoring_",i))))

assign(paste0("scores_",i),cbind(test_raw_ids,as.data.frame(get(paste0("fit.scoring_",i)))))

# Get the mean valus of predictions on all data sets
 mean_predictions_temp=data.frame(i,top_products[i],h2o.mean(get(paste0("fit.train_",i))$p1),h2o.mean(get(paste0("fit.valid_",i))$p1),h2o.mean(get(paste0("fit.test_",i))$p1),h2o.mean(get(paste0("fit.scoring_",i))$p1))
 names(mean_predictions_temp)=c("Model_ID","Product_Name","Mean_Train_Prediction","Mean_Valid_Prediction","Mean_Test_Prediction","Mean_Scoring_Prediction")
 
 # Aggregate best model parameters, AUC values and mean values of predictions

 if (i==1) {
 mean_predictions <- mean_predictions_temp
 auc_report <- auc_comb
 }
 else {
  mean_predictions <- dplyr::bind_rows(mean_predictions,mean_predictions_temp)
  auc_report <- dplyr::bind_rows(auc_report,auc_comb)
 }
 # Save the best model on disk for easy retrieval
 h2o.saveModel(get(paste0("best_model_from_grid",i)),paste0("C:/Santander/Saved Models/best_6lag_model_from_grid",i))
}

# best_model <-h2o.getModel(sortedGrid@model_ids[[1]])


auc_report_6lags_Nov8_part3 <- auc_report
mean_predictions_6lags_Nov8_part3 <- mean_predictions

write.csv(auc_report_6lags_Nov8_part2,file="C:/Santander/Stats/auc_report_6lags_tuned_GBM_Nov8_part2.csv")
write.csv(mean_predictions_6lags_Nov8_part2,file="C:/Santander/Stats/mean_predictions_6lags_tuned_GBM_Nov8_part2.csv")



###################################################################################################################
## Use the best model hyperparameters and create an n-fold cross-validation model
###################################################################################################################
best_models <- read.csv("C:/Santander/Stats/auc_report_tuned_GBM_Nov8_part2.csv")
best_models$nbins[is.na(best_models$nbins)] <- 20
best_models$max_depth[is.na(best_models$max_depth)] <- 5

for (i in 1:9)  {
  
model_id <- paste0("xval_model",i)
#best_model <- get(paste0("best_model_from_grid",i))
best_mod <- best_models[i=i,]

y <- top_products[i]

gbm<-h2o.gbm(x = x,
             y = y,
             training_frame = train.hex,
             validation_frame = valid.hex,
             nfolds = 5,
             model_id = model_id,
             score_tree_interval = 10,
             ntrees = 2000,
             max_depth = as.list(best_mod$max_depth)[[1]],
             min_rows =  as.list(best_mod$min_rows)[[1]],
             nbins_cats = as.list(best_mod$nbins_cats)[[1]],   
             stopping_rounds = 5,
             stopping_metric = "AUC",
             stopping_tolerance = 1e-4,
             learn_rate = 0.05,
             learn_rate_annealing=0.99,
             distribution="bernoulli",  
             sample_rate = as.list(best_mod$sample_rate)[[1]],
             col_sample_rate = as.list(best_mod$col_sample_rate)[[1]],
             histogram_type = as.character(as.list(best_mod$histogram_type)[[1]]),
             # sample_rate_per_class = c(1,1),
             nbins = as.list(best_mod$nbins)[[1]],
             seed=1234)

  model_size <- as.data.frame(cbind(nrow(train.hex),nrow(valid.hex),nrow(test.hex),length(x)))
  names(model_size) <- c("Train_Rows","Validation_Rows","Test_Rows","Num_Model_Columns")
  
  sample_rate_per_class <- as.data.frame(toString(as.character(gbm@parameters$sample_rate_per_class)))
  names(sample_rate_per_class) <- c("sample_rate_per_class")
  best_model_parms_temp <- cbind(model_size,as.data.frame(gbm@parameters[!(names(gbm@parameters) %in% c("x","y","sample_rate_per_class"))]),sample_rate_per_class)
  
  print(paste("GBM xvald Model:",i," ",top_products[i]))
  
  auc_train=h2o.auc(h2o.performance(gbm, newdata = train.hex))
  auc_valid=h2o.auc(h2o.performance(gbm, newdata = valid.hex))
  auc_test=h2o.auc(h2o.performance(gbm, newdata = test.hex))
  auc_temp= cbind(data.frame(i,top_products[i],auc_train,auc_valid,auc_test))
  names(auc_temp)=c("Model_ID","Product_Name","AUC_Train","AUC_Valid","AUC_Test")
  auc_comb=cbind(auc_temp,best_model_parms_temp)
  
   print(paste("AUC training set: ",round(auc_train,4)))
   print(paste("AUC validation set: ",round(auc_valid,4)))
   print(paste("AUC test set: ",round(auc_test,4)))
  # Score all datasets
   assign(paste0("fit.train_",i),h2o.predict(gbm,newdata = train.hex))
   assign(paste0("fit.valid_",i),h2o.predict(gbm,newdata = valid.hex))
   assign(paste0("fit.test_",i),h2o.predict(gbm,newdata = test.hex))
   assign(paste0("fit.scoring_",i),h2o.predict(gbm,newdata = scoring.hex))
   
  # Confusion matrices for all datasets
   # print(h2o.confusionMatrix(get(paste0("best_model_from_grid",i)),train.hex))
   # print(h2o.confusionMatrix(get(paste0("best_model_from_grid",i)),valid.hex))
   # print(h2o.confusionMatrix(get(paste0("best_model_from_grid",i)),test.hex))

   print(summary(get(paste0("fit.train_",i))))
   print(summary(get(paste0("fit.valid_",i))))
   print(summary(get(paste0("fit.test_",i))))
   print(summary(get(paste0("fit.scoring_",i))))

  assign(paste0("scores_",i),cbind(test_raw_ids,as.data.frame(get(paste0("fit.scoring_",i)))))
  
  # Get the mean valus of predictions on all data sets
   mean_predictions_temp=data.frame(i,top_products[i],h2o.mean(get(paste0("fit.train_",i))$p1),h2o.mean(get(paste0("fit.valid_",i))$p1),h2o.mean(get(paste0("fit.test_",i))$p1),h2o.mean(get(paste0("fit.scoring_",i))$p1))
   names(mean_predictions_temp)=c("Model_ID","Product_Name","Mean_Train_Prediction","Mean_Valid_Prediction","Mean_Test_Prediction","Mean_Scoring_Prediction")
  
  # Aggregate best model parameters, AUC values and mean values of predictions
  
   if (i==1) {
     mean_predictions <- mean_predictions_temp
     auc_report <- auc_comb
   }
   else {
     mean_predictions <- dplyr::bind_rows(mean_predictions,mean_predictions_temp)
     auc_report <- dplyr::bind_rows(auc_report,auc_comb)
   }
   
   # Save the best model on disk for easy retrieval
   h2o.saveModel(gbm,path = "C:/Santander/Saved Models/best_6lag_model/",force = T)

}

mean_predictions_Nov8_part2_xval <- mean_predictions
auc_report_Nov8_part2_xval <- auc_report


write.csv(mean_predictions_Nov8_part2_xval,file="C:/Santander/Stats/mean_predictions_tuned_GBM_Nov8_part2_xval.csv")
write.csv(auc_report_Nov8_part2_xval,file="C:/Santander/Stats/auc_report_tuned_GBM_Nov8_part2_xval.csv")

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

prediction_modifier=c(1,1,1,1,1,1,1,1,1,1,1,1)
#prediction_modifier=c( 0.8017,0.9283,0.9797,0.9173,0.7617,0.8918,1.1027,0.0715,0.7897) # from test dataset actual/model mean ratios
#prediction_modifier=c( 0.8483,1.0277,1.0344,1.0202,0.9419,1.0175,0.8233,0.9897,0.7706) # from modeling dataset actual/model mean ratios

for (j in 1:9) #length(prediction_modifier))
{ 
  eq1 <- paste0("scores_",j,"$p0=NULL")
  eval(parse(text=eq1))
  eq2 <- paste0("scores_",j,"$prediction=scores_",j,"$p1*prediction_modifier[",j,"]" )
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


all_scores_sorted  <- dplyr::arrange(all_scores,ncodpers,desc(prediction))
all_scores_sorted$p1=all_scores_sorted$predict=all_scores_sorted$prediction = NULL

# create a column that simply runs prod1, prod2, ... , prod9 and repeats for each incodpers. These column will be used for transposing the products
all_scores_sorted$prod_num <- as.integer(rownames(all_scores_sorted))  %% 9
all_scores_sorted$prod_num[all_scores_sorted$prod_num == 0] <- 9
all_scores_sorted$prod_num <- paste0("prod",all_scores_sorted$prod_num)

#Transpose the scores dataset from vertical to horizontal
all_scores_transposed <- spread(all_scores_sorted, prod_num,product)

#########################################################################################################################################
## Build a number of new products model (multinomial response variable of 1,2,3,4,...)
# Since we don't care about mis-predicting the 0 product adds, we will exclude them in order to make non-zero predictiosn more accurate
########################################################################################################################################
train_temp=as.data.frame(train.hex)
train_nonzero=dplyr::filter(train_temp,num_new_products>0)
train_nonzero.hex=as.h2o(train_nonzero)


valid_temp=as.data.frame(valid.hex)
valid_nonzero=dplyr::filter(valid_temp,num_new_products>0)
valid_nonzero.hex=as.h2o(valid_nonzero)

hyper_params_nprod = list( max_depth = c(3,4,5,6,7,8,10,12),
                     sample_rate=c(0.1,0.6,0.8),
                     col_sample_rate=c(0.1,0.2,0.4,0.6,0.8),
                     min_rows=c(25,50,100),
                     nbins=c(20,50,100),
                     nbins_cats = c(10,20,50),
                     histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
                     )
search_criteria_nprod = list(strategy = "RandomDiscrete",
                       max_runtime_secs = 3600,
                       max_models = 60,
                       seed = 12345,
                       stopping_rounds = 5,
                       # stopping_metric = "misclassfication",
                       stopping_tolerance = 1e-3
                       )

grid <- h2o.grid(x = x,
               y = c("num_new_products_factor"),
               grid_id = "num_products_grid",
               hyper_params = hyper_params_nprod,
               search_criteria = search_criteria_nprod,
               training_frame = train_nonzero.hex,
               validation_frame = valid_nonzero.hex,
               algorithm = "gbm",
               distribution="multinomial",
               ntrees = 3000,
               stopping_rounds = 5,
               stopping_metric="misclassification",
               learn_rate = 0.05,
               learn_rate_annealing=0.99,
               # max_depth = 7,
               # sample_rate = 0.8,
               # col_sample_rate = 0.8,
               score_tree_interval = 10,
               seed=1234)

sorted_num_products_grid <- h2o.getGrid(grid_id = "num_products_grid", sort_by = "accuracy", decreasing = TRUE)

# Select the top model as the most accurate model
best_gbm_num_products <- h2o.getModel(sorted_num_products_grid@model_ids[[1]])
 
pred_num_products_train = h2o.predict(best_gbm_num_products,newdata = train.hex)
pred_num_products_valid = h2o.predict(best_gbm_num_products,newdata = valid.hex)
pred_num_products_test = h2o.predict(best_gbm_num_products,newdata = test.hex)
pred_num_products_scoring = h2o.predict(best_gbm_num_products,newdata = scoring.hex)

 print(h2o.confusionMatrix(best_gbm_num_products,train.hex))
 print(h2o.confusionMatrix(best_gbm_num_products,valid.hex))
 print(h2o.confusionMatrix(best_gbm_num_products,test.hex))

 pred_num_products_scoring=cbind(test_raw$ncodpers,as.data.frame(pred_num_products_scoring))
 
#####################################################################################################################
# Combine propensity models with number of products model results 
# Merge the ordered products data frame with the predicted number of products data frame, and produce the output data
#######################################################################################################################
 
all_scores_combined=cbind(all_scores_transposed,pred_num_products_scoring$predict)

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


save(all_scores_combined_string,file="C:/Santander/Data/all_scores_combined_calibrated_by_modeling_set_ratios_Nov8.RData")


submission=all_scores_combined_string[,c("ncodpers","output_string")]
names(submission)=c("ncodpers","added_products")

write.csv(submission[,1:2],file="C:/Santander/Submissions/submission7_Nov8_part2_replicate_xval.csv",row.names = FALSE)


# # Submission 2. Put just one product
# 
# submission2=all_scores_combined[,c("ncodpers","prod1")]
# names(submission2)=c("ncodpers","added_products")
# write.csv(submission2,file="C:/Santander/Submissions/submission2.csv",row.names = FALSE)
