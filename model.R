library(tidyr)
library(reshape)
library(h2o)
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

write.csv(stat.desc(train100K_with_lags[,product_columns_growth])[7:9,],file="C:/Santander/Stats/growth_stats.csv")

# How many products did customers add in any given month, of the top 12 products in consideration?

train100K_with_lags$num_new_products = 
  train100K_with_lags$ind_recibo_ult1_growth+
  train100K_with_lags$ind_nom_pens_ult1_growth+
  train100K_with_lags$ind_nomina_ult1_growth+
  train100K_with_lags$ind_tjcr_fin_ult1_growth+
  train100K_with_lags$ind_cco_fin_ult1_growth+
  train100K_with_lags$ind_cno_fin_ult1_growth+
  train100K_with_lags$ind_ecue_fin_ult1_growth+
  train100K_with_lags$ind_dela_fin_ult1_growth+
  train100K_with_lags$ind_reca_fin_ult1_growth+
  train100K_with_lags$ind_ctma_fin_ult1_growth+
  train100K_with_lags$ind_valo_fin_ult1_growth+
  train100K_with_lags$ind_fond_fin_ult1_growth

#table(train100K_with_lags_numeric$num_new_products)

exclude_columns=c("fecha_dato","ncodpers","fecha_alta","ult_fec_cli_1t","num_products","num_new_products","num_new_products_factor")

## Set the esponse variables as factor
for (i in 1:length(top_products)) {
train100K_with_lags[,top_products[i]] = as.factor(train100K_with_lags[,top_products[i]])
}

train100K_with_lags$num_new_products_factor=as.factor(train100K_with_lags$num_new_products)

mod=dplyr::filter(train100K_with_lags,fecha_dato >'2015-03-28' & fecha_dato <= '2016-03-28')
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


for (i in 1:1) #length(top_products)) 
{
  model_id = paste0("gbm_no_annealing_stratified",i)
  
y <- top_products[i]

gbm <- h2o.gbm(x = x,
               y = y,
               model_id = model_id,
               training_frame = train.hex,
               validation_frame = valid.hex,
               distribution="bernoulli",
               ntrees = 1000,
               stopping_rounds = 5,
               learn_rate = 0.05,
               #learn_rate_annealing=0.99,
               sample_rate = 0.8,
               col_sample_rate = 0.8,
               stopping_tolerance = 1e-4,
               stopping_metric = "AUC",
               score_tree_interval = 10,
               sample_rate_per_class=c(0.5,1),
               seed=1234)
print(paste("GBM Model:",i," ",top_products[i]))
gbm=h2o.getModel(model_id)
auc_train=h2o.auc(h2o.performance(gbm, newdata = train.hex))
auc_valid=h2o.auc(h2o.performance(gbm, newdata = valid.hex))
auc_test=0
#auc_test=h2o.auc(h2o.performance(gbm, newdata = test.hex))
auc_temp=data.frame(i,model_id,top_products[i],auc_train,auc_valid,auc_test)
names(auc_temp)=c("Model_Num","Model_ID","Product_Name","AUC_Train","AUC_Valid","AUC_Test")
print(paste("AUC training set: ",round(auc_train,4)))
print(paste("AUC validation set: ",round(auc_valid,4)))
#print(paste("AUC test set: ",round(auc_test,2)))
# 
#assign(paste0("fit.train_",i),h2o.predict(gbm,newdata = train.hex))
#assign(paste0("fit.valid_",i),h2o.predict(gbm,newdata = valid.hex))
#assign(paste0("fit.test_",i),h2o.predict(gbm,newdata = test.hex))
#assign(paste0("fit.scoring_",i),h2o.predict(gbm,newdata = scoring.hex))
# print(summary(fit.train))
# print(summary(fit.valid))
# print(summary(fit.test))
# print(summary(get(paste0("fit.train_",i))))
# print(summary(get(paste0("fit.valid_",i))))
# print(summary(get(paste0("fit.test_",i))))
# print(summary(get(paste0("fit.scoring_",i))))
# print(h2o.confusionMatrix(gbm,train.hex))
# print(h2o.confusionMatrix(gbm,valid.hex))
# print(h2o.confusionMatrix(gbm,test.hex))

# Get the mean valuds of predictions on all data sets and export to CSV
#mean_predictions_temp=data.frame(i,top_products[i],h2o.mean(get(paste0("fit.train_",i))$p1),h2o.mean(get(paste0("fit.valid_",i))$p1),h2o.mean(get(paste0("fit.test_",i))$p1),h2o.mean(get(paste0("fit.scoring_",i))$p1))
#names(mean_predictions_temp)=c("Model_ID","Product_Name","Mean_Train_Prediction","Mean_Valid_Prediction","Mean_Test_Prediction","Mean_Scoring_Prediction")

if (i==1) {
 # mean_predictions <- mean_predictions_temp
  auc_report <- auc_temp
  } 
else {
#  mean_predictions <- dplyr::bind_rows(mean_predictions,mean_predictions_temp)
  auc_report <- dplyr::bind_rows(auc_report,auc_temp)
}
assign(paste0("scores_",i),cbind(test_raw_ids,as.data.frame(get(paste0("fit.scoring_",i)))))

}

write.csv(mean_predictions,file="C:/Santander/Data/mean_predictions.csv")


# Compute the means of response variables (event rates) by modeling/validation split
train100K_with_lags_numeric=train100K_with_lags
for (i in 1:length(top_products)) {
  train100K_with_lags_numeric[,top_products[i]] = as.numeric(as.character(train100K_with_lags[,top_products[i]]))
}

mod_numeric=dplyr::filter(train100K_with_lags_numeric,fecha_dato >'2015-03-28' & fecha_dato <= '2016-03-28')
val_numeric=dplyr::filter(train100K_with_lags_numeric,fecha_dato > '2016-03-28')

write.csv(stat.desc(mod_numeric[,top_products])[7:9,],file="C:/Santander/Stats/growth_stats_mod.csv")
write.csv(stat.desc(val_numeric[,top_products])[7:9,],file="C:/Santander/Stats/growth_stats_val.csv")


## Apply prediction modifiers in order to true up the predictions (modifier = actual event rate/mean prediction)
## Modifiers were computed in excel as simple ratios

prediction_modifier=c(1.01,1.02,1.02,1.01,0.58,1.02,0.90,1.06,0.99,0.08,0.58,0.67)

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
# all_scores_sorted0=dplyr::slice(all_scores_sorted,1:10000)
# all_scores_sorted0$p1=all_scores_sorted0$prediction=all_scores_sorted0$predict=NULL


all_scores_sorted$p1=all_scores_sorted$predict=all_scores_sorted$prediction = NULL

# create a column that simply runs prod1, prod2, ... , prod9 and repeats for each incodpers. These column will be used for transposing the products
all_scores_sorted$prod_num <- as.integer(rownames(all_scores_sorted))  %% 9
all_scores_sorted$prod_num[all_scores_sorted$prod_num == 0] <- 9
all_scores_sorted$prod_num <- paste0("prod",all_scores_sorted$prod_num)


# md <- melt(all_scores_sorted0,id="ncodpers")
# all_scores_transposed <- cast(md, ncodpers ~ variable+value ,fun.aggregate = NULL)
# all_scores_transposed <- reshape(md, idvar = c("ncodpers"),timevar = c("value"),direction = "wide")
all_scores_transposed <- spread(all_scores_sorted, prod_num,product)



## Build a number of new products model
# Since we don't care about mis-predicting the 0 product adds, we will exclude them in order to make non-zero predictiosn more accurate

train_temp=as.data.frame(train.hex)
train_nonzero=dplyr::filter(train_temp,num_new_products>0)
train_nonzero.hex=as.h2o(train_nonzero)

valid_temp=as.data.frame(valid.hex)
valid_nonzero=dplyr::filter(valid_temp,num_new_products>0)
valid_nonzero.hex=as.h2o(valid_nonzero)

gbm <- h2o.gbm(x = x,
               y = c("num_new_products_factor"),
               model_id = "gbm_new_products_8",
               training_frame = train_nonzero.hex,
               validation_frame = valid_nonzero.hex,
               distribution="multinomial",
               ntrees = 3000,
               stopping_rounds = 5,
               stopping_metric="misclassification",
               learn_rate = 0.05,
               max_depth = 7,
               #learn_rate_annealing=0.99,
               sample_rate = 0.8,
               col_sample_rate = 0.8,
               seed=1234)


 
pred_num_products_train = h2o.predict(gbm,newdata = train.hex)
pred_num_products_valid = h2o.predict(gbm,newdata = valid.hex)
pred_num_products_test = h2o.predict(gbm,newdata = test.hex)
pred_num_products_scoring = h2o.predict(gbm,newdata = scoring.hex)

 print(h2o.confusionMatrix(gbm,train.hex))
 print(h2o.confusionMatrix(gbm,valid.hex))
 print(h2o.confusionMatrix(gbm,test.hex))

 pred_num_products_scoring=cbind(test_raw$ncodpers,as.data.frame(pred_num_products_scoring))
 
 
#Merge the ordered products data frame with the predicted number of products data frame, and produc the output data
 
all_scores_combined=cbind(all_scores_transposed,pred_num_products_scoring$predict)

names(all_scores_combined$`pred_num_products_scoring$predict`) <- c("pred_number_of_products")

all_scores_combined$pred_num_products = as.numeric(as.character(all_scores_combined$`pred_num_products_scoring$predict`))
all_scores_combined$`pred_num_products_scoring$predict`=NULL

for (i in (all_scores_combined$pred_num_products+1):9) {
  assign(paste0("all_scores_combined.prod",j),NULL)
}


for (m in 1:nrow(all_scores_combined)) {
  for (p in 1:all_scores_combined[m,"pred_num_products"]) {
    if (p ==1) {all_scores_combined[m,"output_string"] = all_scores_combined[m,paste0("prod",p)]}
         else {all_scores_combined[m,"output_string"] = paste(all_scores_combined[m,"output_string"],all_scores_combined[m,paste0("prod",p)],sep=" ") }
   }
}

all_scores_combined$output_list=all_scores_combined$list=NULL

save(all_scores_combined,file="C:/Santander/Data/all_scores_combined.RData")


submission=all_scores_combined[,c("ncodpers","output_string")]
names(submission)=c("ncodpers","added_products")

write.csv(submission[,1:2],file="C:/Santander/Submissions/submission1.csv",row.names = FALSE)


# Submission 2. Put just one product

submission2=all_scores_combined[,c("ncodpers","prod1")]
names(submission2)=c("ncodpers","added_products")
write.csv(submission2,file="C:/Santander/Submissions/submission2.csv",row.names = FALSE)
