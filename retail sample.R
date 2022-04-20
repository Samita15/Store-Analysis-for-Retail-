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
  #select(-Areaname)

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

store_all = CreateDummies(store_all,"countyname",35)
store_all = CreateDummies(store_all,"Areaname",100)
store_all = CreateDummies(store_all,"countytownname",50)
store_all = CreateDummies(store_all,"state_alpha",50)
store_all = CreateDummies(store_all,"store_Type",50)

store_all=store_all %>%
  select(-Id,-storecode)

glimpse(store_all)

lapply(store_all,function(x) sum(is.na(x)))

for(col in names(store_all)){
  
  if(sum(is.na(store_all[,col]))>0 & !(col %in% c("data","store"))){
    
    store_all[is.na(store_all[,col]),col]=mean(store_all[store_all$data=='train',col],na.rm=T)
  }
  
}


rt_train=store_all %>% filter(data=='train') %>% select(-data)
rt_test=store_all %>% filter(data=='test') %>% select (-data,-store)

set.seed(2)
s=sample(1:nrow(rt_train),0.8*nrow(rt_train))
rt_train1=rt_train[s,]
rt_train2=rt_train[-s,]

######################################Randomforest#################################
library(randomForest)
rf=randomForest(store~.,data=rt_train1,do.trace=T)

test.rf=predict(rf,newdata=rt_train2)

varImpPlot(rf)

library(pROC)
auc(roc(rt_train2$store,test.rf))
##0.8109

test.rf= predict(rf,newdata = rt_test,type="class") 
write.csv(test.rf,"Samita_Gawas_P2_part2.csv",row.names = F)

##########################################DTree#########################################

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








