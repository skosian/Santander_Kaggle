install.packages("sqldf")
install.packages("pastecs")
install.packages("pryr")
library(dplyr)
library(pastecs)
library(sqldf)
library(pryr)

test_raw = read.csv("C:/Santander/Data/test_ver2.csv")
train_raw = read.csv("C:/Santander/Data/train_ver2.csv")

write.csv(capture.output(str(train_raw)),file="C:/Santander/Stats/train_raw_str.csv")
write.csv(capture.output(str(test_raw)),file="C:/Santander/Stats/test_raw_str.csv")


write.csv(as.data.frame(table(train_raw$fecha_dato)),file="C:/Santander/Stats/date_frequency.csv")
write.csv(as.data.frame(table(train_raw$ind_empleado)),file="C:/Santander/Stats/ind_empleado_frequency.csv")
write.csv(as.data.frame(table(train_raw$pais_residencia)),file="C:/Santander/Stats/pais_residencia_frequency.csv")
write.csv(as.data.frame(table(train_raw$sexo)),file="C:/Santander/Stats/sexo_frequency.csv")
write.csv(as.data.frame(table(train_raw$age,useNA = c("ifany"))),file="C:/Santander/Stats/age_frequency.csv")
write.csv(as.data.frame(table(train_raw$fecha_alta)),file="C:/Santander/Stats/fecha_alta_frequency.csv")
write.csv(as.data.frame(table(train_raw$ind_nuevo,useNA = c("ifany"))),file="C:/Santander/Stats/ind_nuevo_frequency.csv")
write.csv(as.data.frame(table(train_raw$antiguedad,useNA = c("ifany"))),file="C:/Santander/Stats/antiguedad_frequency.csv")
write.csv(as.data.frame(table(train_raw$indrel,useNA = c("ifany"))),file="C:/Santander/Stats/indrel_frequency.csv")
write.csv(as.data.frame(table(train_raw$ult_fec_cli_1t)),file="C:/Santander/Stats/ult_fec_cli_1t_frequency.csv")
write.csv(as.data.frame(table(train_raw$indrel_1mes)),file="C:/Santander/Stats/indrel_1mes_frequency.csv")
write.csv(as.data.frame(table(train_raw$tiprel_1mes)),file="C:/Santander/Stats/tiprel_1mes_frequency.csv")
write.csv(as.data.frame(table(train_raw$indresi)),file="C:/Santander/Stats/indresi_frequency.csv")
write.csv(as.data.frame(table(train_raw$indext)),file="C:/Santander/Stats/indext_frequency.csv")
write.csv(as.data.frame(table(train_raw$conyuemp)),file="C:/Santander/Stats/conyuemp_frequency.csv")
write.csv(as.data.frame(table(train_raw$canal_entrada)),file="C:/Santander/Stats/canal_entrada_frequency.csv")
write.csv(as.data.frame(table(train_raw$indfall)),file="C:/Santander/Stats/indfall_frequency.csv")
write.csv(as.data.frame(table(train_raw$tipodom)),file="C:/Santander/Stats/tipodom_frequency.csv")
write.csv(as.data.frame(table(train_raw$cod_prov,useNA = c("ifany"))),file="C:/Santander/Stats/cod_prov_frequency.csv")
write.csv(as.data.frame(table(train_raw$nomprov)),file="C:/Santander/Stats/nomprov_frequency.csv")
write.csv(as.data.frame(table(train_raw$ind_actividad_cliente,useNA = c("ifany"))),file="C:/Santander/Stats/ind_actividad_cliente_frequency.csv")
write.csv(as.data.frame(table(train_raw$segmento)),file="C:/Santander/Stats/segmento_frequency.csv")

train<- dplyr::mutate(train_raw,num_products=ind_ahor_fin_ult1+ind_aval_fin_ult1+ind_cco_fin_ult1+ind_cder_fin_ult1+ind_cno_fin_ult1+ind_ctju_fin_ult1+
                     ind_ctma_fin_ult1+ind_ctop_fin_ult1+ind_ctpp_fin_ult1+ind_deco_fin_ult1+ind_deme_fin_ult1+ind_dela_fin_ult1+ind_ecue_fin_ult1+
                     ind_fond_fin_ult1+ind_hip_fin_ult1+ind_plan_fin_ult1+ind_pres_fin_ult1+ind_reca_fin_ult1+ind_tjcr_fin_ult1+ind_valo_fin_ult1+
                     ind_viv_fin_ult1+ind_nomina_ult1+ind_nom_pens_ult1+ind_recibo_ult1)
test=test_raw

### Correct wrong variable types

train$age=as.integer(as.character(train$age))
train$ind_nuevo=as.factor(train$ind_nuevo)
train$antiguedad[train$antiguedad == "-999999"] <-NA
train$antiguedad=as.integer(as.character(train$antiguedad))
train$indrel=as.factor(train$indrel)
train$indrel_1mes[train$indrel_1mes == "1.0"] <-"1"
train$indrel_1mes[train$indrel_1mes == "2.0"] <-"2"
train$indrel_1mes[train$indrel_1mes == "3.0"] <-"3"
train$indrel_1mes[train$indrel_1mes == "4.0"] <-"4"

train$cod_prov=as.factor(train$cod_prov)
train$ind_actividad_cliente=as.factor(train$ind_actividad_cliente)

write.csv(as.data.frame(table(train$ind_actividad_cliente,useNA = c("ifany"))),file="C:/Santander/Stats/ind_actividad_cliente_frequency_converted.csv")
write.csv(as.data.frame(table(train$indrel_1mes,useNA = c("ifany"))),file="C:/Santander/Stats/indrel_1mes_frequency_converted.csv")

test$ind_nuevo=as.factor(test$ind_nuevo)
test$antiguedad[test$antiguedad == -999999] <-NA
test$indrel=as.factor(test$indrel)
test$indrel_1mes=as.factor(test$indrel_1mes)
test$cod_prov=as.factor(test$cod_prov)
test$ind_actividad_cliente=as.factor(test$ind_actividad_cliente)
test$renta=as.numeric(as.character(test$renta))

## Create sample files for trial runs
train_sample <- train[sample(1:nrow(train), 50000,replace=FALSE),]
train_sample_1M <- train[sample(1:nrow(train), 1000000,replace=FALSE),]



write.csv(as.data.frame(table(train$num_products)),file="C:/Santander/Stats/num_products_frequency.csv")


write.csv(capture.output(str(train)),file="C:/Santander/Stats/train_str.csv")
write.csv(capture.output(str(test)),file="C:/Santander/Stats/test_str.csv")

means_train=stat.desc(train) 
means_test=stat.desc(test)

write.csv(t(means_train),file="C:/Santander/Stats/means_train.csv")
write.csv(t(means_test),file="C:/Santander/Stats/means_test.csv")

# save pre-processed train and test datasets for easy access

save(train,test,file="C:/Santander/Data/train_and_test.RData")

## Create an account age variable

# train_sample$account_age_days=as.numeric(as.Date(train_sample$fecha_dato) - as.Date(train_sample$fecha_alta))
# train_sample$account_age_months=train_sample$account_age_days/30.417
# train_sample$account_age_years=train_sample$account_age_days/365
# 
# hist(train_sample$antiguedad,50)
# hist(train_sample$account_age_months)
# summary(train_sample$antiguedad)
# summary(round(train_sample$account_age_months))

### load the presaved data
load("C:/Santander/Data/train_and_test.RData")
train$account_age=(as.numeric(as.Date(train$fecha_dato) - as.Date(train$fecha_alta)))/30.417
test$account_age=(as.numeric(as.Date(test$fecha_dato) - as.Date(test$fecha_alta)))/30.417

unique_train_rows=dplyr::distinct(data.frame(train$ncodpers))
unique_test_rows=dplyr::distinct(data.frame(test$ncodpers))

# table(train$indfall,train$fecha_dato)
# table(test$indfall)

# Select a random subset by unique ID
# # random 10K
# unique10K <- dplyr::sample_n(unique_test_rows,10000,replace=FALSE)
# names(unique10K)=c("ncodpers")
# 
# train10K <- dplyr::inner_join(train,unique10K,by="ncodpers")
# test10K <- dplyr::inner_join(test,unique10K,by="ncodpers")
# 
# summary_train10K <- train10K %>% group_by(ncodpers) %>% 
#      summarise(min_products=min(num_products), max_products=max(num_products),first_products=first(num_products), last_products=last(num_products)) 
# train10K <- ungroup(train10K)
# summary_train10K$growth_product=summary_train10K$last_products - summary_train10K$first_products
# hist(summary_train10K$growth_product)

# random 100K
unique100K <- dplyr::sample_n(unique_test_rows,100000,replace=FALSE)
names(unique100K)=c("ncodpers")

train100K <- dplyr::inner_join(train,unique100K,by="ncodpers")
test100K <- dplyr::inner_join(test,unique100K,by="ncodpers")

summary_train100K <- train100K %>% group_by(ncodpers) %>% 
  summarise(min_products=min(num_products), max_products=max(num_products),first_products=first(num_products), last_products=last(num_products)) 
train100K <- ungroup(train100K)
summary_train100K$growth_product=summary_train100K$last_products - summary_train100K$first_products
# hist(summary_train100K$growth_product)
# table(summary_train100K$growth_product)


# train10K_sorted=dplyr::arrange(train10K,ncodpers,fecha_dato)
# write.csv(train10K_sorted,file="C:/Santander/Data/train10K.csv")

product_columns=c("ind_ahor_fin_ult1","ind_aval_fin_ult1","ind_cco_fin_ult1","ind_cder_fin_ult1","ind_cno_fin_ult1","ind_ctju_fin_ult1",
                  "ind_ctma_fin_ult1","ind_ctop_fin_ult1","ind_ctpp_fin_ult1","ind_deco_fin_ult1","ind_deme_fin_ult1","ind_dela_fin_ult1",
                  "ind_ecue_fin_ult1","ind_fond_fin_ult1","ind_hip_fin_ult1","ind_plan_fin_ult1","ind_pres_fin_ult1","ind_reca_fin_ult1",
                  "ind_tjcr_fin_ult1","ind_valo_fin_ult1","ind_viv_fin_ult1","ind_nomina_ult1","ind_nom_pens_ult1","ind_recibo_ult1")

# Compute the year over year growth for each of the 24 products on 100K sample, compute last 3 years of lags of products

temp_train100K=train100K[,c("ncodpers","fecha_dato",product_columns,"num_products")]
temp_test100K=test100K[,c("ncodpers","fecha_dato")]

temp = dplyr::bind_rows(temp_train100K,temp_test100K)

temp=dplyr::arrange(temp,ncodpers,fecha_dato)

product_columns_lag1=product_columns
product_columns_lag2=product_columns
product_columns_lag3=product_columns
product_columns_lag4=product_columns
product_columns_lag5=product_columns
product_columns_lag6=product_columns
product_columns_lag7=product_columns
product_columns_lag8=product_columns
product_columns_lag9=product_columns

product_columns_delta=product_columns
product_columns_delta_lag1=product_columns
product_columns_delta_lag2=product_columns
product_columns_delta_lag3=product_columns
product_columns_delta_lag4=product_columns
product_columns_delta_lag5=product_columns
product_columns_delta_lag6=product_columns
product_columns_delta_lag7=product_columns
product_columns_delta_lag8=product_columns
product_columns_delta_lag9=product_columns

product_columns_growth=product_columns
product_columns_growth_lag1=product_columns
product_columns_growth_lag2=product_columns
product_columns_growth_lag3=product_columns
product_columns_growth_lag4=product_columns
product_columns_growth_lag5=product_columns
product_columns_growth_lag6=product_columns
product_columns_growth_lag7=product_columns
product_columns_growth_lag8=product_columns
product_columns_growth_lag9=product_columns


for (i in 1:length(product_columns))
 { product_columns_lag1[i]=paste0(product_columns[i],"_lag1")
   product_columns_lag2[i]=paste0(product_columns[i],"_lag2")
   product_columns_lag3[i]=paste0(product_columns[i],"_lag3")
   product_columns_lag4[i]=paste0(product_columns[i],"_lag4")
   product_columns_lag5[i]=paste0(product_columns[i],"_lag5")
   product_columns_lag6[i]=paste0(product_columns[i],"_lag6")
   product_columns_lag7[i]=paste0(product_columns[i],"_lag7")
   product_columns_lag8[i]=paste0(product_columns[i],"_lag8")
   product_columns_lag9[i]=paste0(product_columns[i],"_lag9")
   
   product_columns_delta[i]=paste0(product_columns[i],"_delta")
   product_columns_delta_lag1[i]=paste0(product_columns[i],"_delta_lag1")
   product_columns_delta_lag2[i]=paste0(product_columns[i],"_delta_lag2")
   product_columns_delta_lag3[i]=paste0(product_columns[i],"_delta_lag3")
   product_columns_delta_lag4[i]=paste0(product_columns[i],"_delta_lag4")
   product_columns_delta_lag5[i]=paste0(product_columns[i],"_delta_lag5")
   product_columns_delta_lag6[i]=paste0(product_columns[i],"_delta_lag6")
   product_columns_delta_lag7[i]=paste0(product_columns[i],"_delta_lag7")
   product_columns_delta_lag8[i]=paste0(product_columns[i],"_delta_lag8")
   product_columns_delta_lag9[i]=paste0(product_columns[i],"_delta_lag9")
   
   product_columns_growth[i]=paste0(product_columns[i],"_growth")
   product_columns_growth_lag1[i]=paste0(product_columns[i],"_growth_lag1")
   product_columns_growth_lag2[i]=paste0(product_columns[i],"_growth_lag2")
   product_columns_growth_lag3[i]=paste0(product_columns[i],"_growth_lag3")
   product_columns_growth_lag4[i]=paste0(product_columns[i],"_growth_lag4")
   product_columns_growth_lag5[i]=paste0(product_columns[i],"_growth_lag5")
   product_columns_growth_lag6[i]=paste0(product_columns[i],"_growth_lag6")
   product_columns_growth_lag7[i]=paste0(product_columns[i],"_growth_lag7")
   product_columns_growth_lag8[i]=paste0(product_columns[i],"_growth_lag8")
   product_columns_growth_lag9[i]=paste0(product_columns[i],"_growth_lag9")
   
   temp[,product_columns_lag1[i]] = lag(temp[,product_columns[i]])
   temp[,product_columns_lag2[i]] = lag(temp[,product_columns[i]],2)
   temp[,product_columns_lag3[i]] = lag(temp[,product_columns[i]],3)
   temp[,product_columns_lag4[i]] = lag(temp[,product_columns[i]],4)
   temp[,product_columns_lag5[i]] = lag(temp[,product_columns[i]],5)
   temp[,product_columns_lag6[i]] = lag(temp[,product_columns[i]],6)
   temp[,product_columns_lag7[i]] = lag(temp[,product_columns[i]],7)
   temp[,product_columns_lag8[i]] = lag(temp[,product_columns[i]],8)
   temp[,product_columns_lag9[i]] = lag(temp[,product_columns[i]],9)
   
   temp[,product_columns_delta[i]] = temp[,product_columns[i]] -temp[,product_columns_lag1[i]]
   temp[,product_columns_delta_lag1[i]] = lag(temp[,product_columns_delta[i]],1)
   temp[,product_columns_delta_lag2[i]] = lag(temp[,product_columns_delta[i]],2)
   temp[,product_columns_delta_lag3[i]] = lag(temp[,product_columns_delta[i]],3)
   temp[,product_columns_delta_lag4[i]] = lag(temp[,product_columns_delta[i]],4)
   temp[,product_columns_delta_lag5[i]] = lag(temp[,product_columns_delta[i]],5)
   temp[,product_columns_delta_lag6[i]] = lag(temp[,product_columns_delta[i]],6)
   temp[,product_columns_delta_lag7[i]] = lag(temp[,product_columns_delta[i]],7)
   temp[,product_columns_delta_lag8[i]] = lag(temp[,product_columns_delta[i]],8)
   temp[,product_columns_delta_lag9[i]] = lag(temp[,product_columns_delta[i]],9)
   
   temp[,product_columns_growth[i]] =  as.integer(temp[,product_columns_delta[i]]>0)
   temp[,product_columns_growth_lag1[i]] = lag(temp[,product_columns_growth[i]],1)
   temp[,product_columns_growth_lag2[i]] = lag(temp[,product_columns_growth[i]],2)
   temp[,product_columns_growth_lag3[i]] = lag(temp[,product_columns_growth[i]],3)
   temp[,product_columns_growth_lag4[i]] = lag(temp[,product_columns_growth[i]],4)
   temp[,product_columns_growth_lag5[i]] = lag(temp[,product_columns_growth[i]],5)
   temp[,product_columns_growth_lag6[i]] = lag(temp[,product_columns_growth[i]],6)
   temp[,product_columns_growth_lag7[i]] = lag(temp[,product_columns_growth[i]],7)
   temp[,product_columns_growth_lag8[i]] = lag(temp[,product_columns_growth[i]],8)
   temp[,product_columns_growth_lag9[i]] = lag(temp[,product_columns_growth[i]],9)
}

temp$num_products_lag1=lag(temp$num_products,1)
temp$num_products_lag2=lag(temp$num_products,2)
temp$num_products_lag3=lag(temp$num_products,3)
temp$num_products_lag4=lag(temp$num_products,4)
temp$num_products_lag5=lag(temp$num_products,5)
temp$num_products_lag6=lag(temp$num_products,6)

# merge lag and delta variables with original 

train100K_with_lags=dplyr::left_join(train100K,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,"num_products"))],by=c("ncodpers","fecha_dato"))
test100K_with_lags=dplyr::left_join(test100K,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,product_columns_growth,"num_products"))],by=c("ncodpers","fecha_dato"))

train100K_with_lags=dplyr::arrange(train100K_with_lags,ncodpers,fecha_dato)

# get stats of delta variables

# table(train100K_with_lags[,product_columns_growth[5]])

for (i in 1:length(product_columns))
{ print(product_columns[i])
  #print("Delta:")
  #print(table(train100K_with_lags[,product_columns_delta[i]]))
  #print("Growth:")
  print(table(train100K_with_lags[,product_columns_growth[i]]))
}

### Compute the year over year growth for each of the 24 products on full data, compute last 3 years of lags of products

temp_train=train[,c("ncodpers","fecha_dato",product_columns,"num_products")]
temp_test=test[,c("ncodpers","fecha_dato")]

temp = dplyr::bind_rows(temp_train,temp_test)

temp=dplyr::arrange(temp,ncodpers,fecha_dato)

product_columns_lag1=product_columns
product_columns_lag2=product_columns
product_columns_lag3=product_columns
product_columns_lag4=product_columns
product_columns_lag5=product_columns
product_columns_lag6=product_columns
product_columns_delta=product_columns
product_columns_growth=product_columns

for (i in 1:length(product_columns))
{ product_columns_lag1[i]=paste0(product_columns[i],"_lag1")
product_columns_lag2[i]=paste0(product_columns[i],"_lag2")
product_columns_lag3[i]=paste0(product_columns[i],"_lag3")
product_columns_lag4[i]=paste0(product_columns[i],"_lag4")
product_columns_lag5[i]=paste0(product_columns[i],"_lag5")
product_columns_lag6[i]=paste0(product_columns[i],"_lag6")
product_columns_delta[i]=paste0(product_columns[i],"_delta")
product_columns_growth[i]=paste0(product_columns[i],"_growth")
temp[,product_columns_lag1[i]] = lag(temp[,product_columns[i]])
temp[,product_columns_lag2[i]] = lag(temp[,product_columns[i]],2)
temp[,product_columns_lag3[i]] = lag(temp[,product_columns[i]],3)
temp[,product_columns_lag4[i]] = lag(temp[,product_columns[i]],4)
temp[,product_columns_lag5[i]] = lag(temp[,product_columns[i]],5)
temp[,product_columns_lag6[i]] = lag(temp[,product_columns[i]],6)
temp[,product_columns_delta[i]] = temp[,product_columns[i]] -temp[,product_columns_lag1[i]]
temp[,product_columns_growth[i]] =  as.integer(temp[,product_columns_delta[i]]>0)
}

temp$num_products_lag1=lag(temp$num_products,1)
temp$num_products_lag2=lag(temp$num_products,2)
temp$num_products_lag3=lag(temp$num_products,3)
temp$num_products_lag4=lag(temp$num_products,4)
temp$num_products_lag5=lag(temp$num_products,5)
temp$num_products_lag6=lag(temp$num_products,6)

# merge lag and delta variables with original 

train_with_lags=dplyr::left_join(train,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,"num_products"))],by=c("ncodpers","fecha_dato"))
test_with_lags=dplyr::left_join(test,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,product_columns_growth,"num_products"))],by=c("ncodpers","fecha_dato"))

train_with_lags=dplyr::arrange(train_with_lags,ncodpers,fecha_dato)

save(train_with_lags,test_with_lags,file="C:/Santander/Data/train_and_test_with_lags.RData")
#load("C:/Santander/Data/train_and_test_with_lags.RData")
# get stats of delta variables

# table(train_with_lags[,product_columns_growth[5]])

for (i in 1:length(product_columns))
{ print(product_columns[i])
  #print("Delta:")
  #print(table(train_with_lags[,product_columns_delta[i]]))
  #print("Growth:")
  print(table(train_with_lags[,product_columns_growth[i]]))
}
