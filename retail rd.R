# imports
library(dplyr)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)
library(xgboost)

# data prep
setwd("C:/Users/Samita Gawas/OneDrive/Desktop/Data science course/R studio/Project/Retail")

rt_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
rt_test=read.csv("store_test.csv",stringsAsFactors = FALSE)

rt_test$store=NA

rt_train$data='train'
rt_test$data='test'

library(dplyr)

store_all=rbind(rt_train,rt_test)

glimpse(store_all)

#store_all=store_all %>%
mutate(Areaname=as.factor(gsub(",","",Areaname)) ,
       countyname=as.factor(countyname) , 
       storecode=as.factor(storecode) ,
       countytownname=as.factor(countytownname)
)
#glimpse(store_all)

store_all=select(store_all,-storecode-countytownname-countyname-Areaname)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

cat_cols=c("state_alpha","store_Type")

for(cat in cat_cols){
  store_all=CreateDummies(store_all,cat,50)
}

glimpse(store_all)

store_all=store_all %>%
  select(-country,-State,-storecode,-countytownname,-countyname,-Areaname)

rt_train=store_all %>% filter(data=='train') %>% select(-data)
rt_test=store_all %>% filter(data=='test') %>% select (-data,-store)

set.seed(2)
s=sample(1:nrow(rt_train),0.8*nrow(rt_train))
rt_train1=rt_train[s,]
rt_train2=rt_train[-s,]

####################################Part1RD########################################

library(randomForest)
rf=randomForest(store~.,data=rt_train1,do.trace=T)

test.rf=predict(rf,newdata=rt_train2)

varImpPlot(rf)

library(pROC)
auc(roc(rt_train2$store,test.rf))

test.rf= predict(rf,newdata = rt_test,type="class")

library(pROC)
roccurve = roc(rt_test$store,test.rf)
plot(roccurve)
auc(roccurve)

table(rt_test$store)

# Performance
Acc=(1228+5796)/8141 =0.863
Sn=1228/1963 =0.626
Sp=5796/6178 =0.938

#########################################Part2RD#######################################

library(randomForest)
fit_rt=randomForest(store~.,data=rt_train1)
fit_rt
importance(fit_rt)

varImpPlot(fit_rt)

predict_store=predict(fit_rt,newdata=rt_train2,type="class")
predict_store

library(pROC)
auc(roc(rt_train2$store,predict_store))

test.rt= predict(rf,newdata = rt_test,type="class")

roccurve = roc(rt_test$store,test.rt)
plot(roccurve)
auc(roccurve)

#########################################Dtree#######################################

library(tree)

rt_train1$store=as.factor(rt_train1$store)
single.tree=tree(store~.,data=rt_train1)


cv.single.tree=cv.tree(single.tree,FUN=prune.misclass)

plot(cv.single.tree$size,cv.single.tree$dev,type='b')

pruned.tree=prune.misclass(single.tree,best=5)

plot(pruned.tree)
text(pruned.tree,pretty = 0)

target.tree=predict(pruned.tree,newdata=rt_train2,type="class")

library(pROC)
auc(roc(rt_train2$store,target.tree))

table(rt_train2$store,target.tree)

# Performance
Acc=(316+160)/668= 0.7126
Sn=1025/1963= 0.522
Sp=5862/6178=0.949
###########################################################################################
setwd("C:/Users/Samita Gawas/OneDrive/Desktop/Data science course/R studio/Project/Retail")

rt_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
rt_test=read.csv("store_test.csv",stringsAsFactors = FALSE)

rt_test$store=NA

rt_train$data='train'
rt_test$data='test'

library(dplyr)

store_all=rbind(rt_train,rt_test)

store_all=store_all %>%
  select(-Areaname)

glimpse(store_all)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

cat_cols=c("state_alpha","store_Type","storecode","countytownname","countyname")

for(cat in cat_cols){
  store_all=CreateDummies(store_all,cat,50)
}

glimpse(store_all)

rt_train=store_all %>% filter(data=='train') %>% select(-data)
rt_test=store_all %>% filter(data=='test') %>% select (-data,-store)

set.seed(2)
s=sample(1:nrow(rt_train),0.8*nrow(rt_train))
rt_train1=rt_train[s,]
rt_train2=rt_train[-s,]

library(randomForest)
rf=randomForest(store~.,data=rt_train1,do.trace=T)

test.rf=predict(rf,newdata=rt_train2)

varImpPlot(rf)

library(pROC)
auc(roc(rt_train2$store,test.rf))

test.rf= predict(rf,newdata = rt_test,type="class")
write.csv(test.rf,"Samita_Gawas_P2_part2.csv",row.names = F)


library(pROC)
roccurve = roc(rt_test$store,test.rf)
plot(roccurve)
auc(roccurve)
