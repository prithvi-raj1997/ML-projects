setwd("C:\\Users\\cpraj\\OneDrive\\Desktop\\PROJECTS\\Retail-2")
getwd()

rd_train=read.csv("store_train.csv")
rd_test=read.csv("store_test.csv")

#### DATA DISCUSSION
names(rd_train)
# "Id"
## drop it

# "sales0","sales1","sales2","sales3","sales4"
## let it be

# "country"
sort(table(rd_train$country),decreasing = T)
## create dummies, Although its given as numeric

# "State"
table(rd_train$State)
table(rd_train$State,rd_train$state_alpha)
## drop it because state and state_has perfect correspondence

# "CouSub"
sort(table(rd_train$CouSub),decreasing = T)
## create dummies

# "countyname"
sort(table(rd_train$countyname),decreasing = T)
## extract last word and create dummies

# "storecode"
sort(table(rd_train$storecode),decreasing = T)
## extract first 5 letters and create dummies

# "Areaname"
sort(table(rd_train$Areaname),decreasing=T)
## drop it bcause some of the word are extracted already

#  "countytownname"
## extract and create dummies 

#  "population" 
table(rd_train$population)
## let it be 

#  "state_alpha" 
table(rd_train$state_alpha)
## create dummies and it has perfect correspondence with state column, drop state column

#  "store_Type" 
table(rd_train$store_Type)
## create dummies

#  "store"
table(rd_train$store)
## target column

#################
CreateDummies=function(data,var,freq_cutoff){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for(cat in categories){
    name=paste(var,cat,sep = "_")
    name=gsub("-","_",name)
    name=gsub(" ","",name)
    name=gsub("\\?","Q_",name)
    name=gsub("\\+","_",name)
    name=gsub("<","LT_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub("\\/","_",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}
###

rd_test$store=NA

rd_train$data="train"
rd_test$data="test"

rd_all=rbind(rd_train,rd_test)

############## Data exploration and creating dummies
glimpse(rd_all)
library(dplyr)
# "country"
sort(table(rd_all$country),decreasing = T)
class(rd_all$country)
rd_all$country=as.character(rd_all$country)
class(rd_all$country)
sum(is.na(rd_all$country))

rd_all=CreateDummies(rd_all,"country",freq_cutoff = 30)


# "State"
table(rd_all$State)
table(rd_all$State,rd_all$state_alpha)
rd_all$State=NULL

# "CouSub"
sort(table(rd_all$CouSub),decreasing = T)
class(rd_all$CouSub)
rd_all$CouSub=as.character(rd_all$CouSub)
rd_all$CouSub=ifelse(rd_all$CouSub == "99999","missing",rd_all$CouSub)

rd_all = rd_all %>% 
  mutate(CouSub=as.numeric(CouSub=="missing"))


# "countyname"
sort(table(rd_all$countyname),decreasing = T)
library(tidyr)
rd_all=rd_all %>% 
  separate(countyname,into=c("c1","c2"),sep = " ")
sort(table(rd_all$c2),decreasing = T)
sort(table(rd_all$c1),decreasing = T)
rd_all$c1=NULL
names(rd_all)[8]="countyname"

# "county"=1, "non-county"=0
rd_all=rd_all %>% 
  mutate(countyname=as.numeric(countyname=="County"))


## extract last word and create dummies

# "storecode"
sort(table(rd_all$storecode),decreasing = T)
rd_all$storecode=substr(rd_all$storecode,1,5)
rd_all=rd_all %>% 
  mutate("storecode"=as.numeric(storecode=="METRO"))
## extract first 5 letters and create dummies

# "Areaname"
sort(table(rd_all$Areaname),decreasing=T)
rd_all$Areaname=NULL

#  "countytownname"
sort(table(rd_all$countytownname),decreasing = T)
rd_all=rd_all %>% 
  separate(countytownname,into=c("t1","t2"),sep=" ")
sort(table(rd_all$t1),decreasing = T)
sort(table(rd_all$t2),decreasing = T)
rd_all$t1=NULL
names(rd_all)[10]="countytownname"
rd_all=CreateDummies(rd_all,"countytownname",20)


#  "population" 
table(rd_all$population)
## let it be 

#  "state_alpha" 
sort(table(rd_all$state_alpha),decreasing = T)
rd_all=CreateDummies(rd_all,"state_alpha",50)
## create dummies and it has perfect correspondence with state column, drop state column

#  "store_Type" 
table(rd_all$store_Type)
rd_all=CreateDummies(rd_all,"store_Type",300)
## create dummies

#  "store"
table(rd_all$store)
## target column
###########
lapply(rd_all,table)
glimpse(rd_all)
summary(rd_all)

## Treating missing values
lapply(rd_all,function(x) sum(is.na(x)))

rd_all=rd_all[!((is.na(rd_all$store)) & rd_all$data=="train"),]

for(col in names(rd_all)){
  if(sum(is.na(rd_all[,col]))>0 & !(col %in% c("data","store"))){
    rd_all[is.na(rd_all[,col]),col]=mean(rd_all[rd_all$data=="train",col],na.rm=T)
  }
}


### separate train and test data
rd_train=rd_all %>% 
  filter(data=="train") %>% 
  select(-data)

rd_test=rd_all %>% 
  filter(data=="test") %>% 
  select(-data,-store)


set.seed(2)
s=sample(1:nrow(rd_train),0.8*nrow(rd_train))

rd_train1=rd_train[s,]
rd_train2=rd_train[-s,]

## Model building : LOGISTIC REGRESSION
library(car)

## Model building and testing on train1 and train2 data
for_vif=lm(store~.-Id,data = rd_train1)
sort(vif(for_vif),decreasing = T)[1:5]

# consider vif to be 10
for_vif=lm(store~.-Id
                  -sales0
                  -CouSub
                  -sales2
                  -sales3
                  -countytownname_County,
           data = rd_train1)
sort(vif(for_vif),decreasing = T)[1:5]

fit=glm(store~.-Id
        -sales0
        -CouSub
        -sales2
        -sales3
        -countytownname_County,
        data = rd_train1,family = "binomial")

fit=step(fit)

summary(fit)

###
formula(fit)

fit=glm(store ~ countyname + storecode +   
           country_51 + country_13 + country_5  ,
         data = rd_train1,
         family="binomial")

summary(fit)

##
library(pROC)

val.pred=predict(fit,newdata = rd_train2,type="response")

auc(roc(rd_train2$store,val.pred))
#Area under the curve: 0.8377


########################################################################################
## Random forest model building
library(randomForest)
library(cvTools)

rd_train$store=as.factor(rd_train$store)
glimpse(rd_train)

param=list(mtry=c(5,10,15,20,30,50),
            ntree=c(50,100,200,250,500,700),
            maxnodes=c(5,10,15,20,30,50,100),
            nodesize=c(1,2,5,10))

# In first experiment 
# mtry ntree maxnodes nodesize
# 618   50    50       20        5

param=list(mtry=c(45,50,60,70,80),
           ntree=c(30,40,45,50,55,60,65,70),
           maxnodes=c(16,18,20,23,25,30,35),
           nodesize=c(3,4,5,6,7,8,9))

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  
  return(subset_para)
}

mycost_auc=function(y, yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  
  return(score)
}


num_trails=50
my_params=subset_paras(param,num_trails)

# cvTuning

myauc=0

for(i in 1:num_trails){
  print(paste0("Starting iteration :", i))
  params=my_params[i,]
  
  k=cvTuning(randomForest,store~.-Id,
             data = rd_train,
             tuning = params,
             folds = cvFolds(nrow(rd_train),K=10,type="random"),
             cost = mycost_auc,
             seed = 2,
             predictArgs = list(type="prob")
             )
  score.this=k$cv[,1]
  
  if(score.this>myauc){
    print(params)
    myauc=score.this
    print(myauc)
    
    best_params=params
  }
  print("DONE")
}

 myauc
best_params
# In first experiment
# myauc: 1

# best_params:
#       mtry ntree maxnodes nodesize
# 618   50    50       20        5

# In second experiment
# myauc: 1
# best_params:
#        mtry ntree maxnodes nodesize
# 1796   45    70       20        9

myauc=1
best_params=data.frame(mtry=45,
                       ntree=70,
                       maxnoodes=20,
                       nodesize=9)

rd.rf.final=randomForest(store~.-Id,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnode=best_params$maxnoodes,
                         nodesize=best_params$nodesize,
                         data=rd_train)

# predicting on test data
test.predict=predict(rd.rf.final,newdata = rd_test,type="prob")[,1]
write.csv(test.predict,file = "choudary_prithvirajrf_P2_part2.csv",row.names=F)
