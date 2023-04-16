library(readxl)
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)
library(lubridate)
library(XML)
library(xmlconvert)
library(ggpubr)
library(epitools)
library(caret)

getwd()
setwd("~/Downloads/SPH6004/assignment2")

set.seed(20230310)

dynamic_d <- read.csv("sph_dynamic.csv")
static_d <- read.csv("sph_static.csv")
colnames(dynamic_d)
colnames(static_d)

#### static_d ####
static_d$icu_intime1 <- as_datetime(static_d$icu_intime)
static_d$vent_start1 <- as_datetime(static_d$vent_start,format="%m/%d/%y %H:%M")
static_d$vent_end1 <- as_datetime(static_d$vent_end,format="%m/%d/%y %H:%M")

for (i in 1:dim(static_d)[1]){
  if ((month(static_d[i,]$icu_intime1)==12)&(month(static_d[i,]$vent_start1)==1)){
    year(static_d[i,]$vent_start1) <- year(static_d[i,]$icu_intime1)+1
  }else{
    year(static_d[i,]$vent_start1) <- year(static_d[i,]$icu_intime1)
  }
  
  if ((month(static_d[i,]$icu_intime1)==12)&(month(static_d[i,]$vent_end1)==1)){
    year(static_d[i,]$vent_end1) <- year(static_d[i,]$icu_intime1)+1
  }else{
    year(static_d[i,]$vent_end1) <- year(static_d[i,]$icu_intime1)
  }
}

static_d <- as.data.table(static_d)
static_d[,vent_start_hour:=((icu_intime1%--%vent_start1) %/% minutes(1))/60]
static_d[,vent_end_hour:=((icu_intime1%--%vent_end1) %/% minutes(1))/60]
static_d[,vent_duration1:=vent_end_hour-vent_start_hour]

# check 
summary(static_d[,vent_duration1-vent_duration])
summary(static_d)

colnames(static_d)
static_d1 <- static_d[,c(1,6:11)]

colnames(static_d1)[2] <- "icu_intime" 
colnames(static_d1)[3] <- "vent_start" 
colnames(static_d1)[4] <- "vent_end" 
colnames(static_d1)[7] <- "vent_duration" 


summary(static_d1)
str(static_d1)

static_d1[,vent_start_hour_1 := vent_start_hour %/% 1]
static_d1[,vent_end_hour_1 := vent_end_hour %/% 1]
static_d1[,vent_duration_1 := vent_end_hour_1 - vent_start_hour_1]

static_d1[,vent_start_hour_2 := vent_start_hour %/% 2]
static_d1[,vent_end_hour_2 := vent_end_hour %/% 2]
static_d1[,vent_duration_2 := vent_end_hour_2 - vent_start_hour_2]

summary(static_d1[,vent_end_hour_2-vent_start_hour_2-vent_duration_2])
summary(static_d1)

#write.csv(static_d1,"all_static_0320.csv",row.names = FALSE)
static_d1 <- read.csv("all_static_0320.csv")
static_d1 <- as.data.table(static_d1)

static_d1[,vent_start_hour_6 := vent_start_hour %/% 6]
static_d1[,vent_end_hour_6 := vent_end_hour %/% 6]
static_d1[,vent_duration_6 := vent_duration %/% 6]

static_d1[,vent_start_hour_12 := vent_start_hour %/% 12]
static_d1[,vent_end_hour_12 := vent_end_hour %/% 12]
static_d1[,vent_duration_12 := vent_duration %/% 12]

static_d1[,vent_start_hour_24 := vent_start_hour %/% 24]
static_d1[,vent_end_hour_24 := vent_end_hour %/% 24]
static_d1[,vent_duration_24 := vent_duration %/% 24]

##### outlier: sigma #####
mean_start <- mean(static_d1$vent_start_hour)
sd_start <- sd(static_d1$vent_start_hour)
outlier_threshold_start <- mean_start + 3*sd_start
# 66.02

mean_duration <- mean(static_d1$vent_duration)
sd_duration <- sd(static_d1$vent_duration)
outlier_threshold_duration <- mean_duration + 3*sd_duration
# 212.39

outlier_static_id <- static_d1[which(static_d1[,(vent_start_hour>outlier_threshold_start)|(vent_duration>outlier_threshold_duration)]),]$stay_id
# 45 outliers
static_d2 <- static_d1[which(static_d1[,!stay_id%in%outlier_static_id]),]
unique(static_d2$vent_duration_24)
summary(static_d2)

#write.csv(static_d2,"all_static_0407.csv",row.names = FALSE)


#### dynamic_d ####
icu_time_data <- static_d1[,1:2]
dynamic_d1 <- merge(icu_time_data,dynamic_d,by="stay_id")
dynamic_d1$charttime1 <- as_datetime(dynamic_d1$charttime)

dynamic_d1 <- as.data.table(dynamic_d1)
dynamic_d1[,charttime_hour:=((icu_intime%--%charttime1) %/% minutes(1))/60]
summary(dynamic_d1$charttime_hour)

dynamic_d1[,charttime_hour_1 := charttime_hour %/% 1]
dynamic_d1[,charttime_hour_2 := charttime_hour %/% 2]

colnames(dynamic_d1)
dynamic_d1_time_data <- dynamic_d1[,c(1:2,34:37)]
dynamic_d1_feature_data <- dynamic_d1[,c(1,34,4:33)]
summary(dynamic_d1_time_data)
##### abnormal data #####
record_dynamic_count <- dynamic_d1[,list(count=.N),by=c("stay_id","charttime1")]
summary(record_dynamic_count)

abnormal_record <- record_dynamic_count[which(record_dynamic_count[,count>1]),]

dynamic_d1_feature_data_abnormal <- NULL
dynamic_d1_feature_data_good <- NULL
i=1
stay_id_this <- dynamic_d1_feature_data[i,]$stay_id
charttime_this <- dynamic_d1_feature_data[i,]$charttime1
if (stay_id_this%in%abnormal_record$stay_id){
  abnormal_time_this <- abnormal_record[which(abnormal_record[,stay_id==stay_id_this]),]$charttime1
  if (charttime_this%in%abnormal_time_this){
    dynamic_d1_feature_data_abnormal <- rbind(dynamic_d1_feature_data_abnormal,dynamic_d1_feature_data[i,])
  }else{
    dynamic_d1_feature_data_good <- rbind(dynamic_d1_feature_data_good,dynamic_d1_feature_data[i,])
  }
}else{
  dynamic_d1_feature_data_good <- rbind(dynamic_d1_feature_data_good,dynamic_d1_feature_data[i,])
}

for (i in 2:dim(dynamic_d1_feature_data)[1]){
  stay_id_this <- dynamic_d1_feature_data[i,]$stay_id
  charttime_this <- dynamic_d1_feature_data[i,]$charttime1
  if (stay_id_this%in%abnormal_record$stay_id){
    abnormal_time_this <- abnormal_record[which(abnormal_record[,stay_id==stay_id_this]),]$charttime1
    if (charttime_this%in%abnormal_time_this){
      dynamic_d1_feature_data_abnormal <- rbind(dynamic_d1_feature_data_abnormal,dynamic_d1_feature_data[i,])
    }else{
      dynamic_d1_feature_data_good <- rbind(dynamic_d1_feature_data_good,dynamic_d1_feature_data[i,])
    }
  }else{
    dynamic_d1_feature_data_good <- rbind(dynamic_d1_feature_data_good,dynamic_d1_feature_data[i,])
  }
  
}

# check
dim(dynamic_d1_feature_data)
dim(dynamic_d1_feature_data_good)
dim(dynamic_d1_feature_data_abnormal)
sum(abnormal_record$count)

dynamic_d1_feature_data_abnormal <- dynamic_d1_feature_data_abnormal[order(dynamic_d1_feature_data_abnormal[,stay_id]),]
colnames(dynamic_d1_feature_data_abnormal)

dynamic_d1_feature_data_correct <- NULL
for (i in 1:dim(abnormal_record)[1]){
  stay_id_ab <- abnormal_record[i,]$stay_id
  time_ab <- abnormal_record[i,]$charttime1
  count_ab <- abnormal_record[i,]$count
  abnormal_this <- dynamic_d1_feature_data_abnormal[which(dynamic_d1_feature_data_abnormal[,stay_id==stay_id_ab])]
  abnormal_this <- abnormal_this[which(abnormal_this[,charttime1==time_ab]),]
  stopifnot(dim(abnormal_this)[1] == count_ab)
  for (j in 3:32){
    mean_value <- mean(abnormal_this[,..j][[1]],na.rm=TRUE)
    set(abnormal_this,NULL,colnames(abnormal_this)[j],mean_value)
  }
  dynamic_d1_feature_data_correct <- rbind(dynamic_d1_feature_data_correct,abnormal_this[1,])
}

# check
i=10
dynamic_d1_feature_data_correct[which(dynamic_d1_feature_data_correct[,stay_id==abnormal_record[i,]$stay_id]),]
dynamic_d1_feature_data_abnormal[which(dynamic_d1_feature_data_abnormal[,stay_id==abnormal_record[i,]$stay_id]),]

dynamic_d1_feature_data1 <- rbind(dynamic_d1_feature_data_good,dynamic_d1_feature_data_correct)

dynamic_d1_time_data_abnormal <- NULL
dynamic_d1_time_data_good <- NULL
for (i in 1:dim(dynamic_d1_time_data)[1]){
  stay_id_this <- dynamic_d1_time_data[i,]$stay_id
  charttime_this <- dynamic_d1_time_data[i,]$charttime1
  if (stay_id_this%in%abnormal_record$stay_id){
    abnormal_time_this <- abnormal_record[which(abnormal_record[,stay_id==stay_id_this]),]$charttime1
    if (charttime_this%in%abnormal_time_this){
      dynamic_d1_time_data_abnormal <- rbind(dynamic_d1_time_data_abnormal,dynamic_d1_time_data[i,])
    }else{
      dynamic_d1_time_data_good <- rbind(dynamic_d1_time_data_good,dynamic_d1_time_data[i,])
    }
  }else{
    dynamic_d1_time_data_good <- rbind(dynamic_d1_time_data_good,dynamic_d1_time_data[i,])
  }
  
}

dim(dynamic_d1_time_data_abnormal)
dim(dynamic_d1_time_data_good)
dynamic_d1_time_data_correct <- NULL
for (i in 1:dim(abnormal_record)[1]){
  stay_id_ab <- abnormal_record[i,]$stay_id
  time_ab <- abnormal_record[i,]$charttime1
  count_ab <- abnormal_record[i,]$count
  abnormal_this <- dynamic_d1_time_data_abnormal[which(dynamic_d1_time_data_abnormal[,stay_id==stay_id_ab])]
  abnormal_this <- abnormal_this[which(abnormal_this[,charttime1==time_ab]),]
  stopifnot(dim(abnormal_this)[1] == count_ab)
  dynamic_d1_time_data_correct <- rbind(dynamic_d1_time_data_correct,abnormal_this[1,])
}

dynamic_d1_time_data1 <- rbind(dynamic_d1_time_data_good,dynamic_d1_time_data_correct)

dim(dynamic_d1)
dynamic_d1 <- merge(dynamic_d1_time_data1,dynamic_d1_feature_data1,by=c("stay_id","charttime1"))

# check
record_dynamic_count1 <- dynamic_d1[,list(count=.N),by=c("stay_id","charttime1")]
summary(record_dynamic_count1)

##### within individual imputation #####
# dynamic_d1 <- merge(dynamic_d1_time_data1,dynamic_d1_feature_data1,by=c("stay_id","charttime1"))
dynamic_d1_feature_data2 <- as.data.frame(dynamic_d1_feature_data1)
stay_id_list <- unique(dynamic_d1$stay_id)
colnames(dynamic_d1_feature_data1)
for (k in 1:length(stay_id_list)){
  stay_id_k <- stay_id_list[k]
  data_k <- dynamic_d1_feature_data1[which(dynamic_d1_feature_data1[,stay_id==stay_id_k])]
  for (j in 3:32){
    mean_value <- mean(data_k[,..j][[1]],na.rm=TRUE)
    if (!is.na(mean_value)){
      set(dynamic_d1_feature_data1,which((dynamic_d1_feature_data1[,stay_id==stay_id_k])&(is.na(dynamic_d1_feature_data1[,..j]))),colnames(dynamic_d1_feature_data1)[j],mean_value)
    }
  }
}

# check
dim(dynamic_d1_feature_data2)
record_dynamic_count2 <- dynamic_d1_feature_data1[,list(count=.N),by="stay_id"]
stay_id_k <- "30023204"
dynamic_d1_feature_data1[which(dynamic_d1_feature_data1[,stay_id==stay_id_k]),]
dynamic_d1_feature_data2[which(dynamic_d1_feature_data1$stay_id==stay_id_k),]

dynamic_d1 <- merge(dynamic_d1_time_data1,dynamic_d1_feature_data1,by=c("stay_id","charttime1"))
dynamic_d1[which(dynamic_d1[,stay_id==stay_id_k]),]

##### missing data #####
miss_situ <- data.table(feature_name=colnames(dynamic_d1)[7:36],column_index=7:36)
j=1
dynamic_d1 <- as.data.frame(dynamic_d1)
for (j in 1:dim(miss_situ)[1]){
  tmp <- dynamic_d1[,miss_situ[j,]$column_index]
  na_prop <- mean(as.integer(is.na(tmp)))
  set(miss_situ,j,"na_prop",na_prop)
}

miss_situ[which(miss_situ[,na_prop>0.8]),]
feature_remove_index <- miss_situ[which(miss_situ[,na_prop>0.8]),]$column_index
dynamic_d2 <- dynamic_d1[,-feature_remove_index]
summary(dynamic_d2)



dynamic_d3 <- as.data.table(dynamic_d2)
colnames(dynamic_d3)[2] <- "charttime"
summary(dynamic_d3)
dim(dynamic_d3)
#write.csv(dynamic_d3,"all_dynamic_0320.csv",row.names = FALSE)

dynamic_d3 <- read.csv("all_dynamic_0320.csv")
dynamic_d3 <- as.data.table(dynamic_d3)

dynamic_d3[,charttime_hour_6 := charttime_hour %/% 6]
dynamic_d3[,charttime_hour_12 := charttime_hour %/% 12]
dynamic_d3[,charttime_hour_24 := charttime_hour %/% 24]

colnames(dynamic_d3)

dynamic_d3 <- dynamic_d3[,c(1:6,21:23,7:20)]

##### outlier:sigma  #####
dynamic_d3 <- dynamic_d3[which(dynamic_d3[,!stay_id%in%outlier_static_id]),]
# remove 140 rows in this step

summary(dynamic_d3)

dynamic_d4 <- as.data.frame(dynamic_d3)
colnames(dynamic_d4)

dynamic_d4$self_index <- 1:dim(dynamic_d4)[1]
j=10
column_data <- dynamic_d4[,j]
mean1 <- mean(column_data,na.rm=TRUE)
sd1 <- sd(column_data,na.rm=TRUE)
outlier_threshold <- mean1 + 3*sd1
outlier_self_index_dynamic <- dynamic_d4[which(dynamic_d4[,j]>outlier_threshold),]$self_index

for (j in 11:23){
  column_data <- dynamic_d4[,j]
  mean1 <- mean(column_data,na.rm=TRUE)
  sd1 <- sd(column_data,na.rm=TRUE)
  outlier_threshold <- mean1 + 3*sd1
  outlier_self_index_dynamic1 <- dynamic_d4[which(dynamic_d4[,j]>outlier_threshold),]$self_index
  print(j)
  print(length(outlier_self_index_dynamic1))
  outlier_self_index_dynamic <- c(outlier_self_index_dynamic,outlier_self_index_dynamic1)
}

outlier_self_index_dynamic <- unique(outlier_self_index_dynamic)
length(outlier_self_index_dynamic)
# 838

dynamic_d5 <- dynamic_d4[which(!dynamic_d4$self_index%in%outlier_self_index_dynamic),1:23]
length(unique(dynamic_d5$stay_id))
length(unique(static_d2$stay_id))

static_d3 <- static_d2[which(static_d2[,stay_id%in%unique(dynamic_d5$stay_id)]),]
length(unique(dynamic_d5$stay_id))
length(unique(static_d3$stay_id))

summary(dynamic_d5)
dim(dynamic_d5)
# 6011   23
colnames(dynamic_d5)

summary(static_d3)
dim(static_d3)
# 1727   22
colnames(static_d3)

#write.csv(static_d3,"all_static_0407.csv",row.names = FALSE)
#write.csv(dynamic_d5,"all_dynamic_0407.csv",row.names = FALSE)

dynamic_d5 <- read.csv("all_dynamic_0407.csv")
dynamic_d5 <- as.data.table(dynamic_d5)

d1 <- dynamic_d5[,list(count=.N),by="stay_id"]
summary(d1)
d1[which(d1[,count>=3]),]

##### missing value imputation with mean #####
#dynamic_d_mean <- as.data.frame(dynamic_d3)
dynamic_d_mean <- as.data.table(dynamic_d5)
colnames(dynamic_d_mean)
for (j in 10:23){
  mean_value <- mean(dynamic_d_mean[,..j][[1]],na.rm=TRUE)
  set(dynamic_d_mean,which(is.na(dynamic_d_mean[,..j])),colnames(dynamic_d_mean)[j],mean_value)
}
summary(dynamic_d_mean)
dim(dynamic_d_mean)
#write.csv(dynamic_d_mean,"all_dynamic_mean_impute_0407.csv",row.names = FALSE)

##### missing value imputation with knn #####
library(VIM)
#dynamic_d5 <- as.data.table(dynamic_d5)
dynamic_d_knn <- as.data.frame(dynamic_d5)
colnames(dynamic_d_knn )
dynamic_d_knn_imput <- dynamic_d_knn[,10:23]
dynamic_d_knn_imput1 <- kNN(dynamic_d_knn_imput)
dim(dynamic_d_knn_imput1)
colnames(dynamic_d_knn_imput1)
summary(dynamic_d_knn_imput1)
dynamic_d_knn_imput2 <- dynamic_d_knn_imput1[,1:14]
dynamic_d_knn_imput3 <- cbind(dynamic_d_knn[,1:9],dynamic_d_knn_imput2)
dim(dynamic_d_knn_imput3)
summary(dynamic_d_knn_imput3)
#write.csv(dynamic_d_knn_imput3,"all_dynamic_knn_impute_0407.csv",row.names = FALSE)

#### data split ####
static_d1 <- read.csv("all_static_0407.csv")
dynamic_d3 <- read.csv("all_dynamic_0407.csv")
dynamic_d_mean <- read.csv("all_dynamic_mean_impute_0407.csv")
dynamic_d_knn_imput3 <- read.csv("all_dynamic_knn_impute_0407.csv")

train_index <- createDataPartition(static_d1$vent_duration,p=0.7)
static_train <- static_d1[train_index[[1]],]
static_test <- static_d1[-train_index[[1]],]
summary(static_train$vent_duration)
summary(static_test$vent_duration)

train_stay_id <- static_train$stay_id
train_stay_id_data <- data.frame(stay_id=train_stay_id)

dynamic_missing_train <- dynamic_d3[which(dynamic_d3$stay_id%in%train_stay_id),]
dynamic_missing_test <- dynamic_d3[which(!dynamic_d3$stay_id%in%train_stay_id),]
dim(dynamic_missing_train)
dim(dynamic_missing_test)
dynamic_mean_train <- dynamic_d_mean[which(dynamic_d_mean$stay_id%in%train_stay_id),]
dynamic_mean_test <- dynamic_d_mean[which(!dynamic_d_mean$stay_id%in%train_stay_id),]

dynamic_knn_train <- dynamic_d_knn_imput3[which(dynamic_d_knn_imput3$stay_id%in%train_stay_id),]
dynamic_knn_test <- dynamic_d_knn_imput3[which(!dynamic_d_knn_imput3$stay_id%in%train_stay_id),]

write.csv(train_stay_id_data,"stayid_for_training_set_0407.csv",row.names = FALSE)
write.csv(static_train,"train_static_0407.csv",row.names = FALSE)
write.csv(dynamic_missing_train,"train_dynamic_with_missing_0407.csv",row.names = FALSE)
write.csv(dynamic_mean_train,"train_dynamic_mean_impute_0407.csv",row.names = FALSE)
write.csv(dynamic_knn_train,"train_dynamic_knn_impute_0407.csv",row.names = FALSE)

write.csv(static_test,"test_static_0407.csv",row.names = FALSE)
write.csv(dynamic_missing_test,"test_dynamic_with_missing_0407.csv",row.names = FALSE)
write.csv(dynamic_mean_test,"test_dynamic_mean_impute_0407.csv",row.names = FALSE)
write.csv(dynamic_knn_test,"test_dynamic_knn_impute_0407.csv",row.names = FALSE)


