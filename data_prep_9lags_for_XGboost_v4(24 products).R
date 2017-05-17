library(dplyr)
library(pastecs)
# library(pryr)

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

####################################################################################################################################################

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


## Create an account age variable
train$account_age=(as.numeric(as.Date(train$fecha_dato) - as.Date(train$fecha_alta)))/30.417
test$account_age=(as.numeric(as.Date(test$fecha_dato) - as.Date(test$fecha_alta)))/30.417

# train_sample$account_age_days=as.numeric(as.Date(train_sample$fecha_dato) - as.Date(train_sample$fecha_alta))
# train_sample$account_age_months=train_sample$account_age_days/30.417
# train_sample$account_age_years=train_sample$account_age_days/365
# 
# hist(train_sample$antiguedad,50)
# hist(train_sample$account_age_months)
# summary(train_sample$antiguedad)
# summary(round(train_sample$account_age_months))

### Save pre-processed train and test datasets for easy access
save(train,test,file="C:/Santander/Data/train_and_test.RData")

### load the presaved data
load("C:/Santander/Data/train_and_test.RData")

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

# random 300K
unique300K <- dplyr::sample_n(unique_test_rows,300000,replace=FALSE)
names(unique300K)=c("ncodpers")

train300K <- dplyr::inner_join(train,unique300K,by="ncodpers")
# test300K <- dplyr::inner_join(test,unique300K,by="ncodpers")

# random 400K
unique400K <- dplyr::sample_n(unique_test_rows,400000,replace=FALSE)
names(unique400K)=c("ncodpers")

train400K <- dplyr::inner_join(train,unique400K,by="ncodpers")
# test400K <- dplyr::inner_join(test,unique400K,by="ncodpers")

# hist(summary_train100K$growth_product)
# table(summary_train100K$growth_product)


# train10K_sorted=dplyr::arrange(train10K,ncodpers,fecha_dato)
# write.csv(train10K_sorted,file="C:/Santander/Data/train10K.csv")

#########################################################################################################################
###################### FEATURE ENGINEERING ##############################################################################
#########################################################################################################################

# Group factos with many levels, and create binary/dummy variabls for all factors (dummies are required for XGboost)
factor_columns <- c('ind_empleado','pais_residencia','sexo','ind_nuevo','indrel','indrel_1mes','tiprel_1mes',
                    'indresi','indext','conyuemp','canal_entrada','indfall','cod_prov','ind_actividad_cliente','segmento')

# train.factors <- train[,factor_columns]
# test.factors <- test[,factor_columns]

temp.factors <- rbind(train300K[,factor_columns],test[,factor_columns])

# convert factors into character and replace NA and blank values to explicit words NA and MISSING
for (i in 1:length(factor_columns)) {
  temp.factors[,factor_columns[i]] <- as.character(temp.factors[,factor_columns[i]])
  temp.factors[,factor_columns[i]][temp.factors[,factor_columns[i]] == ''] <-"MISSING"
  temp.factors[,factor_columns[i]][is.na(temp.factors[,factor_columns[i]])] <-"NA"
}

# table(temp.factors[,factor_columns[1]],useNA = 'ifany')
# str(temp.factors)

# create group variables
temp.factors$canal_entrada_group=as.character(temp.factors$canal_entrada)
temp.factors$pais_residencia_group=as.character(temp.factors$pais_residencia)

temp.factors$canal_entrada_group[!(temp.factors$canal_entrada_group %in% c('KHE','KAT','KFC','KHQ','KFA','KHK','MISSING','KHM','KHD','KHN','KAS','RED','KAG','KAY','KAA','KAB',
                                                                           'KAE','KCC','KBZ','KHL','KFD','KAI','KEY','KAW','KAR','KAZ','KAF','7','13','KCI','KAH','KAJ',
                                                                           'KCH','KHF','KAQ'))] <- "OTHER"

temp.factors$pais_residencia_group[!(temp.factors$pais_residencia_group %in% c("ES"))] <- "OTHER"

# temp.factors %>% group_by(canal_entrada_group) %>% summarise(count=n())
# temp.factors %>% group_by(pais_residencia_group) %>% summarise(count1=n())
# table(temp.factors$canal_entrada_group,useNA = "ifany")
# table(temp.factors$pais_residencia_group,useNA = "ifany")

# drop the pre-group variables pais_residencia and canal_entrada
temp.factors <- temp.factors[,! (names(temp.factors) %in% c("pais_residencia","canal_entrada"))]

# Convert characters back into factor columns and generate dummies

grouped_factor_columns <- c('ind_empleado','pais_residencia_group','sexo','ind_nuevo','indrel','indrel_1mes','tiprel_1mes',
                            'indresi','indext','conyuemp','canal_entrada_group','indfall','cod_prov','ind_actividad_cliente','segmento')

dummies = rbind(train300K[,c("fecha_dato","ncodpers")],test[,c("fecha_dato","ncodpers")])

for (i in 1:length(grouped_factor_columns)) {
  temp.factors[,grouped_factor_columns[i]] <- as.factor(temp.factors[,grouped_factor_columns[i]])
  temp.subset <- as.data.frame(temp.factors[,grouped_factor_columns[i]])
  names(temp.subset) <- grouped_factor_columns[i]
  dum <- model.matrix(~.-1,data = temp.subset)
  dummies <- cbind(dummies,dum)
}

dummies.df <- as.data.frame(dummies)

save(dummies.df,file="C:/Santander/Data/dummies_train300K_and_test.RData")
load("C:/Santander/Data/dummies_train300K_and_test.RData")
#temporarily reduce the size
dummies.df <- dplyr::left_join(train300K[,c("ncodpers","fecha_dato")],dummies.df)


# write.csv(capture.output(str(dummies.df,list.len=1000)),"C:/Santander/EDA/dummies_contents.csv")
# write.csv(capture.output(colnames(dummies)),"C:/Santander/EDA/dummies_column_names.csv")

#################################################################################################################################
## New business features, created on Nov 30               #######################################################################
#################################################################################################################################
biz.train=train[,c("ncodpers","fecha_dato","renta","age","account_age","num_products")]
biz.test=test[,c("ncodpers","fecha_dato","renta","age","account_age")]

biz.features = dplyr::bind_rows(biz.train,biz.test)

biz.features=dplyr::arrange(biz.features,ncodpers,fecha_dato)

biz.features <- dplyr::mutate(biz.features,
income_by_product	= renta/(lag(num_products)+1),
age_by_product	= age/(lag(num_products)+1),
account_age_by_product = account_age/(lag(num_products)+1),
engagement = log(account_age*(1+lag(num_products))),
financial_maturity = log(age*(1+lag(num_products))),
banking_propensity = log(renta*lag(num_products)+1)
)

biz.features <- biz.features[,!names(biz.features) %in% c("renta","age","account_age","num_products")]
rm(biz.train,biz.test)
gc()

save(biz.features,file="C:/Santander/Data/biz.features.RData")
load("C:/Santander/Data/biz.features.RData")

###########################################################################################################################################
# Compute the year over year growth for each of the 12/24 products on 100K sample, compute last 9 years of lags of products
## Take only half of the products (top half) due to memory issues
###########################################################################################################################################

product_columns=c("ind_ahor_fin_ult1","ind_aval_fin_ult1","ind_cco_fin_ult1","ind_cder_fin_ult1","ind_cno_fin_ult1","ind_ctju_fin_ult1",
                  "ind_ctma_fin_ult1","ind_ctop_fin_ult1","ind_ctpp_fin_ult1","ind_deco_fin_ult1","ind_deme_fin_ult1","ind_dela_fin_ult1",
                  "ind_ecue_fin_ult1","ind_fond_fin_ult1","ind_hip_fin_ult1","ind_plan_fin_ult1","ind_pres_fin_ult1","ind_reca_fin_ult1",
                  "ind_tjcr_fin_ult1","ind_valo_fin_ult1","ind_viv_fin_ult1","ind_nomina_ult1","ind_nom_pens_ult1","ind_recibo_ult1")

top_products=c("ind_recibo_ult1_growth","ind_nom_pens_ult1_growth","ind_nomina_ult1_growth","ind_tjcr_fin_ult1_growth","ind_cco_fin_ult1_growth",
               "ind_cno_fin_ult1_growth","ind_ecue_fin_ult1_growth","ind_dela_fin_ult1_growth","ind_reca_fin_ult1_growth","ind_ctma_fin_ult1_growth",
               "ind_valo_fin_ult1_growth","ind_fond_fin_ult1_growth")


# product_columns=gsub("_growth","",top_products)

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
   product_columns_delta_lag7[i]=paste0(product_columns[i],"_delta_lag9")
   product_columns_delta_lag8[i]=paste0(product_columns[i],"_delta_lag8")
   product_columns_delta_lag9[i]=paste0(product_columns[i],"_delta_lag9")
   
   product_columns_growth[i]=paste0(product_columns[i],"_growth")
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
}

temp$num_products_lag1=lag(temp$num_products,1)
temp$num_products_lag2=lag(temp$num_products,2)
temp$num_products_lag3=lag(temp$num_products,3)
temp$num_products_lag4=lag(temp$num_products,4)
temp$num_products_lag5=lag(temp$num_products,5)
temp$num_products_lag6=lag(temp$num_products,6)
temp$num_products_lag7=lag(temp$num_products,7)
temp$num_products_lag8=lag(temp$num_products,8)
temp$num_products_lag9=lag(temp$num_products,9)

# How many products did customers add in any given month, of the top 12 products in consideration?

temp$num_new_products = 
  temp$ind_recibo_ult1_growth+
  temp$ind_nom_pens_ult1_growth+
  temp$ind_nomina_ult1_growth+
  temp$ind_tjcr_fin_ult1_growth+
  temp$ind_cco_fin_ult1_growth+
  temp$ind_cno_fin_ult1_growth+
  temp$ind_ecue_fin_ult1_growth+
  temp$ind_dela_fin_ult1_growth+
  temp$ind_reca_fin_ult1_growth+
  temp$ind_ctma_fin_ult1_growth+
  temp$ind_valo_fin_ult1_growth+
  temp$ind_fond_fin_ult1_growth

temp$num_new_products_lag1=lag(temp$num_new_products,1)
temp$num_new_products_lag2=lag(temp$num_new_products,2)
temp$num_new_products_lag3=lag(temp$num_new_products,3)
temp$num_new_products_lag4=lag(temp$num_new_products,4)
temp$num_new_products_lag5=lag(temp$num_new_products,5)
temp$num_new_products_lag6=lag(temp$num_new_products,6)
temp$num_new_products_lag7=lag(temp$num_new_products,7)
temp$num_new_products_lag8=lag(temp$num_new_products,8)
temp$num_new_products_lag9=lag(temp$num_new_products,9)

temp$new_products_3mon <- 
  temp$num_new_products_lag1 +
  temp$num_new_products_lag2 +
  temp$num_new_products_lag3

temp$new_products_6mon <- 
  temp$num_new_products_lag1 +
  temp$num_new_products_lag2 +
  temp$num_new_products_lag3 +
  temp$num_new_products_lag4 +
  temp$num_new_products_lag5 +
  temp$num_new_products_lag6

temp$new_products_9mon <- 
  temp$num_new_products_lag1 +
  temp$num_new_products_lag2 +
  temp$num_new_products_lag3 +
  temp$num_new_products_lag4 +
  temp$num_new_products_lag5 +
  temp$num_new_products_lag6 +
  temp$num_new_products_lag7 +
  temp$num_new_products_lag8 +
  temp$num_new_products_lag9

# What is the net change in products (delta)

temp$delta_products = 
  temp$ind_recibo_ult1_delta+
  temp$ind_nom_pens_ult1_delta+
  temp$ind_nomina_ult1_delta+
  temp$ind_tjcr_fin_ult1_delta+
  temp$ind_cco_fin_ult1_delta+
  temp$ind_cno_fin_ult1_delta+
  temp$ind_ecue_fin_ult1_delta+
  temp$ind_dela_fin_ult1_delta+
  temp$ind_reca_fin_ult1_delta+
  temp$ind_ctma_fin_ult1_delta+
  temp$ind_valo_fin_ult1_delta+
  temp$ind_fond_fin_ult1_delta

temp$delta_products_lag1=lag(temp$delta_products,1)
temp$delta_products_lag2=lag(temp$delta_products,2)
temp$delta_products_lag3=lag(temp$delta_products,3)
temp$delta_products_lag4=lag(temp$delta_products,4)
temp$delta_products_lag5=lag(temp$delta_products,5)
temp$delta_products_lag6=lag(temp$delta_products,6)
temp$delta_products_lag7=lag(temp$delta_products,7)
temp$delta_products_lag8=lag(temp$delta_products,8)
temp$delta_products_lag9=lag(temp$delta_products,9)

temp$delta_3mon <- 
  temp$delta_products_lag1 +
  temp$delta_products_lag2 +
  temp$delta_products_lag3

temp$delta_6mon <- 
  temp$delta_products_lag1 +
  temp$delta_products_lag2 +
  temp$delta_products_lag3 +
  temp$delta_products_lag4 +
  temp$delta_products_lag5 +
  temp$delta_products_lag6

temp$delta_9mon <- 
  temp$delta_products_lag1 +
  temp$delta_products_lag2 +
  temp$delta_products_lag3 +
  temp$delta_products_lag4 +
  temp$delta_products_lag5 +
  temp$delta_products_lag6 +
  temp$delta_products_lag7 +
  temp$delta_products_lag8 +
  temp$delta_products_lag9

product_columns_avg_3mon <- paste0(product_columns,"_avg_3mon")
product_columns_avg_6mon <- paste0(product_columns,"_avg_6mon")
product_columns_max_3mon <- paste0(product_columns,"_max_3mon")
product_columns_max_6mon <- paste0(product_columns,"_max_6mon")
product_columns_min_3mon <- paste0(product_columns,"_min_3mon")
product_columns_min_6mon <- paste0(product_columns,"_min_6mon")

## New features, created on Nov 30

temp100K_new_features <- temp %>% dplyr::group_by(ncodpers,fecha_dato) %>% dplyr::summarise(
avg_num_prod_last3 = mean(c(num_products_lag1,num_products_lag2,num_products_lag3),na.rm = T),
avg_num_prod_last6 = mean(c(num_products_lag1,num_products_lag2,num_products_lag3,num_products_lag4,num_products_lag5,num_products_lag6),na.rm = T),
max_num_prod_last3 = max(c(num_products_lag1,num_products_lag2,num_products_lag3),na.rm = T),
max_num_prod_last6 = max(c(num_products_lag1,num_products_lag2,num_products_lag3,num_products_lag4,num_products_lag5,num_products_lag6),na.rm = T), 
min_num_prod_last3 = min(c(num_products_lag1,num_products_lag2,num_products_lag3),na.rm = T),
min_num_prod_last6 = min(c(num_products_lag1,num_products_lag2,num_products_lag3,num_products_lag4,num_products_lag5,num_products_lag6),na.rm = T)
)


# # New features as of Dec 4
# for (i in 1:length(product_columns)) {
#   
# prod_level_new_features <- temp %>% dplyr::group_by(ncodpers,fecha_dato) %>% dplyr::summarise(  
#    product_columns_avg_3mon[i]  = mean(c(product_columns_lag1[i],product_columns_lag2[i],product_columns_lag3[i]),na.rm = T)
#    # product_columns_avg_6mon[i] = mean(c(product_columns_lag1[i],product_columns_lag2[i],product_columns_lag3[i],product_columns_lag4[i],product_columns_lag5[i],product_columns_lag6[i]),na.rm = T),
#    # product_columns_max_3mon[i] = max(c(product_columns_lag1[i],product_columns_lag2[i],product_columns_lag3[i]),na.rm = T),
#    # product_columns_max_6mon[i] = max(c(product_columns_lag1[i],product_columns_lag2[i],product_columns_lag3[i],product_columns_lag4[i],product_columns_lag5[i],product_columns_lag6[i]),na.rm = T),
#    # product_columns_min_3mon[i] = min(c(product_columns_lag1[i],product_columns_lag2[i],product_columns_lag3[i]),na.rm = T),
#    # product_columns_min_6mon[i] = min(c(product_columns_lag1[i],product_columns_lag2[i],product_columns_lag3[i],product_columns_lag4[i],product_columns_lag5[i],product_columns_lag6[i]),na.rm = T)
#  )  
#     # temp[,product_columns_growth[i]]
# # temp100K_new_features <- cbind(temp100K_new_features,prod_level_new_features)
# }



temp100K_new_features <-mutate(temp100K_new_features,
steady_6mon = as.numeric(max_num_prod_last6 == min_num_prod_last6),
steady_zero_products_6mon = as.numeric(max_num_prod_last6 == 0)
)

# summary(temp100K_new_features$steady_zero_products_6mon,na.rm=T)

# merge lag and delta variables with original and dummies

train100K_with_lags=dplyr::left_join(train100K,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,"num_products"))],by=c("ncodpers","fecha_dato"))
# test100K_with_lags=dplyr::left_join(test100K,biz.features,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,product_columns_growth,"num_products"))],by=c("ncodpers","fecha_dato"))

train100K_with_lags <- dplyr::left_join(train100K_with_lags,biz.features,by=c("ncodpers","fecha_dato"))
train100K_with_lags <- dplyr::left_join(train100K_with_lags,temp100K_new_features,by=c("ncodpers","fecha_dato"))

train100K_with_lags=dplyr::arrange(train100K_with_lags,ncodpers,fecha_dato)

train100K_9lags_v4 <- dplyr::left_join(train100K_with_lags[,!(names(train100K_with_lags) %in% factor_columns)],dummies.df,by=c("ncodpers","fecha_dato"))
train100K_9lags_v4 <- dplyr::arrange(train100K_9lags_v4,ncodpers,fecha_dato)

# test_with_lags_dummies <- dplyr::left_join(test_with_lags[,!(names(test_with_lags) %in% factor_columns)],dummies.df,by=c("ncodpers","fecha_dato"))

save(train100K_with_lags,file="C:/Santander/Data/train100K_with_lags.RData")
save(train100K_9lags_v4,file="C:/Santander/Data/train100K_with_lags_dummies.RData")
load("C:/Santander/Data/train100K_with_lags.RData")
load("C:/Santander/Data/train100K_with_lags_dummies.RData")


numeric_columns <- names(train100K_9lags_v4[,sapply(train100K_9lags_v4,is.numeric)])

cor_9lags_v4=stats::cor(train100K_9lags_v4[,numeric_columns],method=c("pearson"),use = "pairwise.complete.obs")
write.csv(cor_9lags_v4,file = "C:/Santander/EDA/cor_train100K_9lags_v4.csv")

#############################################################################################################################
### Compute the year over year growth for each of the 24 products on full data, compute last 9 years of lags of products ####
#############################################################################################################################
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

# product_columns=gsub("_growth","",top_products)

temp_train=subset(train[,c("ncodpers","fecha_dato",product_columns,"num_products")],as.character(fecha_dato) > '2015-08-28')
temp_test=test[,c("ncodpers","fecha_dato")]

temp = dplyr::bind_rows(temp_train,temp_test)

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
product_columns_delta_lag7[i]=paste0(product_columns[i],"_delta_lag9")
product_columns_delta_lag8[i]=paste0(product_columns[i],"_delta_lag8")
product_columns_delta_lag9[i]=paste0(product_columns[i],"_delta_lag9")

product_columns_growth[i]=paste0(product_columns[i],"_growth")
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
}

temp$num_products_lag1=lag(temp$num_products,1)
temp$num_products_lag2=lag(temp$num_products,2)
temp$num_products_lag3=lag(temp$num_products,3)
temp$num_products_lag4=lag(temp$num_products,4)
temp$num_products_lag5=lag(temp$num_products,5)
temp$num_products_lag6=lag(temp$num_products,6)
temp$num_products_lag7=lag(temp$num_products,7)
temp$num_products_lag8=lag(temp$num_products,8)
temp$num_products_lag9=lag(temp$num_products,9)

# How many products did customers add in any given month, of the top 12 products in consideration?
temp <- mutate(temp,num_new_products = 
               ind_ahor_fin_ult1_growth + ind_aval_fin_ult1_growth + ind_cco_fin_ult1_growth + ind_cder_fin_ult1_growth + ind_cno_fin_ult1_growth + ind_ctju_fin_ult1_growth +
               ind_ctma_fin_ult1_growth + ind_ctop_fin_ult1_growth + ind_ctpp_fin_ult1_growth + ind_deco_fin_ult1_growth + ind_deme_fin_ult1_growth + ind_dela_fin_ult1_growth+
               ind_ecue_fin_ult1_growth + ind_fond_fin_ult1_growth + ind_hip_fin_ult1_growth + ind_plan_fin_ult1_growth + ind_pres_fin_ult1_growth + ind_reca_fin_ult1_growth+
               ind_tjcr_fin_ult1_growth + ind_valo_fin_ult1_growth + ind_viv_fin_ult1_growth + ind_nomina_ult1_growth + ind_nom_pens_ult1_growth + ind_recibo_ult1_growth)

temp$num_new_products_lag1=lag(temp$num_new_products,1)
temp$num_new_products_lag2=lag(temp$num_new_products,2)
temp$num_new_products_lag3=lag(temp$num_new_products,3)
temp$num_new_products_lag4=lag(temp$num_new_products,4)
temp$num_new_products_lag5=lag(temp$num_new_products,5)
temp$num_new_products_lag6=lag(temp$num_new_products,6)
temp$num_new_products_lag7=lag(temp$num_new_products,7)
temp$num_new_products_lag8=lag(temp$num_new_products,8)
temp$num_new_products_lag9=lag(temp$num_new_products,9)

temp$new_products_3mon <- 
  temp$num_new_products_lag1 +
  temp$num_new_products_lag2 +
  temp$num_new_products_lag3

temp$new_products_6mon <- 
  temp$num_new_products_lag1 +
  temp$num_new_products_lag2 +
  temp$num_new_products_lag3 +
  temp$num_new_products_lag4 +
  temp$num_new_products_lag5 +
  temp$num_new_products_lag6

temp$new_products_9mon <- 
  temp$num_new_products_lag1 +
  temp$num_new_products_lag2 +
  temp$num_new_products_lag3 +
  temp$num_new_products_lag4 +
  temp$num_new_products_lag5 +
  temp$num_new_products_lag6 +
  temp$num_new_products_lag7 +
  temp$num_new_products_lag8 +
  temp$num_new_products_lag9

# What is the net change in products (delta)
temp <- mutate(temp,delta_products = 
               ind_ahor_fin_ult1_delta + ind_aval_fin_ult1_delta + ind_cco_fin_ult1_delta + ind_cder_fin_ult1_delta + ind_cno_fin_ult1_delta + ind_ctju_fin_ult1_delta +
               ind_ctma_fin_ult1_delta + ind_ctop_fin_ult1_delta + ind_ctpp_fin_ult1_delta + ind_deco_fin_ult1_delta + ind_deme_fin_ult1_delta + ind_dela_fin_ult1_delta+
               ind_ecue_fin_ult1_delta + ind_fond_fin_ult1_delta + ind_hip_fin_ult1_delta + ind_plan_fin_ult1_delta + ind_pres_fin_ult1_delta + ind_reca_fin_ult1_delta+
               ind_tjcr_fin_ult1_delta + ind_valo_fin_ult1_delta + ind_viv_fin_ult1_delta + ind_nomina_ult1_delta + ind_nom_pens_ult1_delta + ind_recibo_ult1_delta)

temp$delta_products_lag1=lag(temp$delta_products,1)
temp$delta_products_lag2=lag(temp$delta_products,2)
temp$delta_products_lag3=lag(temp$delta_products,3)
temp$delta_products_lag4=lag(temp$delta_products,4)
temp$delta_products_lag5=lag(temp$delta_products,5)
temp$delta_products_lag6=lag(temp$delta_products,6)
temp$delta_products_lag7=lag(temp$delta_products,7)
temp$delta_products_lag8=lag(temp$delta_products,8)
temp$delta_products_lag9=lag(temp$delta_products,9)

temp$delta_3mon <- 
  temp$delta_products_lag1 +
  temp$delta_products_lag2 +
  temp$delta_products_lag3

temp$delta_6mon <- 
  temp$delta_products_lag1 +
  temp$delta_products_lag2 +
  temp$delta_products_lag3 +
  temp$delta_products_lag4 +
  temp$delta_products_lag5 +
  temp$delta_products_lag6

temp$delta_9mon <- 
  temp$delta_products_lag1 +
  temp$delta_products_lag2 +
  temp$delta_products_lag3 +
  temp$delta_products_lag4 +
  temp$delta_products_lag5 +
  temp$delta_products_lag6 +
  temp$delta_products_lag7 +
  temp$delta_products_lag8 +
  temp$delta_products_lag9

product_columns_avg_3mon <- paste0(product_columns,"_avg_3mon")
product_columns_avg_6mon <- paste0(product_columns,"_avg_6mon")
product_columns_max_3mon <- paste0(product_columns,"_max_3mon")
product_columns_max_6mon <- paste0(product_columns,"_max_6mon")
product_columns_min_3mon <- paste0(product_columns,"_min_3mon")
product_columns_min_6mon <- paste0(product_columns,"_min_6mon")

## New features, created on Nov 30

temp_new_features <- temp %>% dplyr::group_by(ncodpers,fecha_dato) %>% dplyr::summarise(
  avg_num_prod_last3 = mean(c(num_products_lag1,num_products_lag2,num_products_lag3),na.rm = T),
  avg_num_prod_last6 = mean(c(num_products_lag1,num_products_lag2,num_products_lag3,num_products_lag4,num_products_lag5,num_products_lag6),na.rm = T),
  max_num_prod_last3 = max(c(num_products_lag1,num_products_lag2,num_products_lag3),na.rm = T),
  max_num_prod_last6 = max(c(num_products_lag1,num_products_lag2,num_products_lag3,num_products_lag4,num_products_lag5,num_products_lag6),na.rm = T), 
  min_num_prod_last3 = min(c(num_products_lag1,num_products_lag2,num_products_lag3),na.rm = T),
  min_num_prod_last6 = min(c(num_products_lag1,num_products_lag2,num_products_lag3,num_products_lag4,num_products_lag5,num_products_lag6),na.rm = T)
)

temp_new_features <-mutate(temp_new_features,
                           steady_6mon = as.numeric(max_num_prod_last6 == min_num_prod_last6),
                           steady_zero_products_6mon = as.numeric(max_num_prod_last6 == 0)
)

# summary(temp100K_new_features$steady_zero_products_6mon,na.rm=T)


# merge lag and delta variables with original 

# train_with_lags=dplyr::left_join(train,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,"num_products"))],by=c("ncodpers","fecha_dato"))
test_with_lags=dplyr::left_join(test,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,product_columns_growth,"num_products"))],by=c("ncodpers","fecha_dato"))
gc()
# train_with_lags=dplyr::arrange(train_with_lags,ncodpers,fecha_dato)
load("C:/Santander/Data/biz.features.RData")
test_with_lags <- dplyr::left_join(test_with_lags,biz.features,by=c("ncodpers","fecha_dato"))
gc()
test_with_lags <- dplyr::left_join(test_with_lags,temp_new_features,by=c("ncodpers","fecha_dato"))

load("C:/Santander/Data/dummies_train300K_and_test.RData")
test_9lags_v4 <- dplyr::left_join(test_with_lags[,!(names(test_with_lags) %in% factor_columns)],dummies.df,by=c("ncodpers","fecha_dato"))
test_9lags_v4=dplyr::arrange(test_9lags_v4,ncodpers)
gc()
# save(test_with_lags,file="C:/Santander/Data/test_with_lags.RData")
save(test_9lags_v4,file="C:/Santander/Data/test_9lags_v4.RData")

# load("C:/Santander/Data/test_with_lags_6lags_plus.RData")
load("C:/Santander/Data/test_9lags_v4.RData")






###########################################################################################################################################
# Compute the year over year growth for each of the 12/24 products on 400K sample, compute last 9 years of lags of products
## Take only half of the products (top half) due to memory issues
###########################################################################################################################################

product_columns=c("ind_ahor_fin_ult1","ind_aval_fin_ult1","ind_cco_fin_ult1","ind_cder_fin_ult1","ind_cno_fin_ult1","ind_ctju_fin_ult1",
                  "ind_ctma_fin_ult1","ind_ctop_fin_ult1","ind_ctpp_fin_ult1","ind_deco_fin_ult1","ind_deme_fin_ult1","ind_dela_fin_ult1",
                  "ind_ecue_fin_ult1","ind_fond_fin_ult1","ind_hip_fin_ult1","ind_plan_fin_ult1","ind_pres_fin_ult1","ind_reca_fin_ult1",
                  "ind_tjcr_fin_ult1","ind_valo_fin_ult1","ind_viv_fin_ult1","ind_nomina_ult1","ind_nom_pens_ult1","ind_recibo_ult1")

top_products=c("ind_recibo_ult1_growth","ind_nom_pens_ult1_growth","ind_nomina_ult1_growth","ind_tjcr_fin_ult1_growth","ind_cco_fin_ult1_growth",
               "ind_cno_fin_ult1_growth","ind_ecue_fin_ult1_growth","ind_dela_fin_ult1_growth","ind_reca_fin_ult1_growth","ind_ctma_fin_ult1_growth",
               "ind_valo_fin_ult1_growth","ind_fond_fin_ult1_growth")


# product_columns=gsub("_growth","",top_products)

temp_train300K=train300K[,c("ncodpers","fecha_dato",product_columns,"num_products")]
# temp_test300K=test300K[,c("ncodpers","fecha_dato")]

# temp = dplyr::bind_rows(temp_train300K,temp_test300K)

temp <- temp_train300K

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
product_columns_delta_lag7[i]=paste0(product_columns[i],"_delta_lag9")
product_columns_delta_lag8[i]=paste0(product_columns[i],"_delta_lag8")
product_columns_delta_lag9[i]=paste0(product_columns[i],"_delta_lag9")

product_columns_growth[i]=paste0(product_columns[i],"_growth")
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
}

temp$num_products_lag1=lag(temp$num_products,1)
temp$num_products_lag2=lag(temp$num_products,2)
temp$num_products_lag3=lag(temp$num_products,3)
temp$num_products_lag4=lag(temp$num_products,4)
temp$num_products_lag5=lag(temp$num_products,5)
temp$num_products_lag6=lag(temp$num_products,6)
temp$num_products_lag7=lag(temp$num_products,7)
temp$num_products_lag8=lag(temp$num_products,8)
temp$num_products_lag9=lag(temp$num_products,9)

# How many products did customers add in any given month, of the top 12 products in consideration?
temp <- mutate(temp,num_new_products = 
               ind_ahor_fin_ult1_growth + ind_aval_fin_ult1_growth + ind_cco_fin_ult1_growth + ind_cder_fin_ult1_growth + ind_cno_fin_ult1_growth + ind_ctju_fin_ult1_growth +
               ind_ctma_fin_ult1_growth + ind_ctop_fin_ult1_growth + ind_ctpp_fin_ult1_growth + ind_deco_fin_ult1_growth + ind_deme_fin_ult1_growth + ind_dela_fin_ult1_growth +
               ind_ecue_fin_ult1_growth + ind_fond_fin_ult1_growth + ind_hip_fin_ult1_growth + ind_plan_fin_ult1_growth + ind_pres_fin_ult1_growth + ind_reca_fin_ult1_growth +
               ind_tjcr_fin_ult1_growth + ind_valo_fin_ult1_growth + ind_viv_fin_ult1_growth + ind_nomina_ult1_growth + ind_nom_pens_ult1_growth + ind_recibo_ult1_growth)

temp$num_new_products_lag1=lag(temp$num_new_products,1)
temp$num_new_products_lag2=lag(temp$num_new_products,2)
temp$num_new_products_lag3=lag(temp$num_new_products,3)
temp$num_new_products_lag4=lag(temp$num_new_products,4)
temp$num_new_products_lag5=lag(temp$num_new_products,5)
temp$num_new_products_lag6=lag(temp$num_new_products,6)
temp$num_new_products_lag7=lag(temp$num_new_products,7)
temp$num_new_products_lag8=lag(temp$num_new_products,8)
temp$num_new_products_lag9=lag(temp$num_new_products,9)

temp$new_products_3mon <- 
  temp$num_new_products_lag1 +
  temp$num_new_products_lag2 +
  temp$num_new_products_lag3

temp$new_products_6mon <- 
  temp$num_new_products_lag1 +
  temp$num_new_products_lag2 +
  temp$num_new_products_lag3 +
  temp$num_new_products_lag4 +
  temp$num_new_products_lag5 +
  temp$num_new_products_lag6

temp$new_products_9mon <- 
  temp$num_new_products_lag1 +
  temp$num_new_products_lag2 +
  temp$num_new_products_lag3 +
  temp$num_new_products_lag4 +
  temp$num_new_products_lag5 +
  temp$num_new_products_lag6 +
  temp$num_new_products_lag7 +
  temp$num_new_products_lag8 +
  temp$num_new_products_lag9

# What is the net change in products (delta)
gc()
temp <- mutate(temp,delta_products = 
               ind_ahor_fin_ult1_delta + ind_aval_fin_ult1_delta + ind_cco_fin_ult1_delta + ind_cder_fin_ult1_delta + ind_cno_fin_ult1_delta + ind_ctju_fin_ult1_delta +
               ind_ctma_fin_ult1_delta + ind_ctop_fin_ult1_delta + ind_ctpp_fin_ult1_delta + ind_deco_fin_ult1_delta + ind_deme_fin_ult1_delta + ind_dela_fin_ult1_delta +
               ind_ecue_fin_ult1_delta + ind_fond_fin_ult1_delta + ind_hip_fin_ult1_delta + ind_plan_fin_ult1_delta + ind_pres_fin_ult1_delta + ind_reca_fin_ult1_delta +
               ind_tjcr_fin_ult1_delta + ind_valo_fin_ult1_delta + ind_viv_fin_ult1_delta + ind_nomina_ult1_delta + ind_nom_pens_ult1_delta + ind_recibo_ult1_delta)


temp$delta_products_lag1=lag(temp$delta_products,1)
temp$delta_products_lag2=lag(temp$delta_products,2)
temp$delta_products_lag3=lag(temp$delta_products,3)
temp$delta_products_lag4=lag(temp$delta_products,4)
temp$delta_products_lag5=lag(temp$delta_products,5)
temp$delta_products_lag6=lag(temp$delta_products,6)
temp$delta_products_lag7=lag(temp$delta_products,7)
temp$delta_products_lag8=lag(temp$delta_products,8)
temp$delta_products_lag9=lag(temp$delta_products,9)

temp$delta_3mon <- 
  temp$delta_products_lag1 +
  temp$delta_products_lag2 +
  temp$delta_products_lag3

temp$delta_6mon <- 
  temp$delta_products_lag1 +
  temp$delta_products_lag2 +
  temp$delta_products_lag3 +
  temp$delta_products_lag4 +
  temp$delta_products_lag5 +
  temp$delta_products_lag6

temp$delta_9mon <- 
  temp$delta_products_lag1 +
  temp$delta_products_lag2 +
  temp$delta_products_lag3 +
  temp$delta_products_lag4 +
  temp$delta_products_lag5 +
  temp$delta_products_lag6 +
  temp$delta_products_lag7 +
  temp$delta_products_lag8 +
  temp$delta_products_lag9

product_columns_avg_3mon <- paste0(product_columns,"_avg_3mon")
product_columns_avg_6mon <- paste0(product_columns,"_avg_6mon")
product_columns_max_3mon <- paste0(product_columns,"_max_3mon")
product_columns_max_6mon <- paste0(product_columns,"_max_6mon")
product_columns_min_3mon <- paste0(product_columns,"_min_3mon")
product_columns_min_6mon <- paste0(product_columns,"_min_6mon")
gc()

## New features, created on Nov 30

temp300K_new_features <- temp %>% dplyr::group_by(ncodpers,fecha_dato) %>% dplyr::summarise(
  avg_num_prod_last3 = mean(c(num_products_lag1,num_products_lag2,num_products_lag3),na.rm = T),
  avg_num_prod_last6 = mean(c(num_products_lag1,num_products_lag2,num_products_lag3,num_products_lag4,num_products_lag5,num_products_lag6),na.rm = T),
  max_num_prod_last3 = max(c(num_products_lag1,num_products_lag2,num_products_lag3),na.rm = T),
  max_num_prod_last6 = max(c(num_products_lag1,num_products_lag2,num_products_lag3,num_products_lag4,num_products_lag5,num_products_lag6),na.rm = T), 
  min_num_prod_last3 = min(c(num_products_lag1,num_products_lag2,num_products_lag3),na.rm = T),
  min_num_prod_last6 = min(c(num_products_lag1,num_products_lag2,num_products_lag3,num_products_lag4,num_products_lag5,num_products_lag6),na.rm = T)
)
gc()

temp300K_new_features <-mutate(temp300K_new_features,
                               steady_6mon = as.numeric(max_num_prod_last6 == min_num_prod_last6),
                               steady_zero_products_6mon = as.numeric(max_num_prod_last6 == 0)
)
gc()
# summary(temp300K_new_features$steady_zero_products_6mon,na.rm=T)

# merge lag and delta variables with original and dummies

train300K_with_lags=dplyr::left_join(train300K,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,"num_products"))],by=c("ncodpers","fecha_dato"))
# test300K_with_lags=dplyr::left_join(test300K,biz.features,temp[,!(names(temp) %in% c(product_columns,product_columns_delta,product_columns_growth,"num_products"))],by=c("ncodpers","fecha_dato"))

# load("C:/Santander/Data/dummies_train300K_and_test.RData")
load("C:/Santander/Data/biz.features.RData")
train300K_with_lags <- dplyr::filter(train300K_with_lags,as.character(fecha_dato) > '2015-09-28')
train300K_with_lags <- dplyr::left_join(train300K_with_lags,temp300K_new_features,by=c("ncodpers","fecha_dato"))

# save(train300K_with_lags,file="C:/Santander/Data/train300K_with_lags.RData")

# load("C:/Santander/Data/train300K_with_lags.RData")
load("C:/Santander/Data/biz.features.RData")
train300K_with_lags <- dplyr::left_join(train300K_with_lags,biz.features,by=c("ncodpers","fecha_dato"))


# load("C:/Santander/Data/train300K_with_lags.RData")

# train300K_with_lags=dplyr::arrange(train300K_with_lags,ncodpers,fecha_dato)
gc()
load("C:/Santander/Data/dummies_train300K_and_test.RData")
train300K_9lags_v4 <- dplyr::left_join(train300K_with_lags[,!(names(train300K_with_lags) %in% factor_columns)],dummies.df,by=c("ncodpers","fecha_dato"))
train300K_9lags_v4 <- dplyr::arrange(train300K_9lags_v4,ncodpers,fecha_dato)
# train300K_9lags_v4 <- dplyr::filter(train300K_9lags_v4,fecha_dato > '2015-09-28')
# test_with_lags_dummies <- dplyr::left_join(test_with_lags[,!(names(test_with_lags) %in% factor_columns)],dummies.df,by=c("ncodpers","fecha_dato"))


save(train300K_9lags_v4,file="C:/Santander/Data/train300K_9lags_v4.RData")

load("C:/Santander/Data/train300K_9lags_v4.RData")


numeric_columns <- names(train300K_9lags_v4[,sapply(train300K_9lags_v4,is.numeric)])

cor_9lags_v4=stats::cor(train300K_9lags_v4[,numeric_columns],method=c("pearson"),use = "pairwise.complete.obs")
write.csv(cor_9lags_v4,file = "C:/Santander/EDA/cor_train300K_9lags_v4.csv")
