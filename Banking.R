setwd("C:\\Users\\cpraj\\OneDrive\\Desktop\\PROJECTS\\Banking-5")
getwd()

# fd=financial data

fd_train=read.csv("bank-full_train.csv")
fd_test=read.csv("bank-full_test.csv")

### DATA DISCUSSION
names(fd_train)
 library(dplyr)
glimpse(fd_train)

# "age"
table(fd_train$age)
## let it be

# "job" 
table(fd_train$job)
## create dummies

# "marital" 
table(fd_train$marital)
## create dummies

# "education" 
table(fd_train$education)
## create dummies

# "default"   
table(fd_train$default)
## consider yes : 1 and no :0

# "balance"
table(fd_train$balance)
## let it be

# "housing" 
table(fd_train$housing)
## consider yes : 1 and no : 0

# "loan"
table(fd_train$loan)
## consider yes : 1 and no : 0

# "contact" 
table(fd_train$contact)
## create dummies

# "day"
table(fd_train$day)
## let it be

# "month" 
table(fd_train$month)
## create dummies

# "duration" 
table(fd_train$duration)
## let it be

# "campaign" 
table(fd_train$campaign)
## let it be

# "pdays"  
table(fd_train$pdays)
## consider -1 = "not contacted": 1 and other than -1 = "contacted" : 0

# "previous" 
table(fd_train$previous)
## let it be

# "poutcome"
table(fd_train$poutcome)
## create dummies


# "ID"   
## drop it while model building

# "y" 
table(fd_train$y)
## consider yes : 1 and no : 0

####### CreateDummies function
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for(cat in categories){
    name=paste(var,cat,sep = "_")
    name=gsub("-","_",name)
    name=gsub(" ","",name)
    name=gsub("\\/","_",name)
    name=gsub("\\?","Q_",name)
    name=gsub("<","LT_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}
##

fd_test$y=NA

fd_train$data="train"
fd_test$data="test"

fd_all=rbind(fd_train,fd_test)
################
## Creating dummies

# "job" 
sort(table(fd_all$job),decreasing = T)
fd_all=CreateDummies(fd_all,"job",freq_cutoff = 200)


# "marital" 
table(fd_all$marital)
fd_all=CreateDummies(fd_all,"marital",1000)

# "education" 
table(fd_all$education)
fd_all=CreateDummies(fd_all,"education",1500)

# "default"   
table(fd_all$default)
fd_all=fd_all %>% 
  mutate(default=as.numeric(default=="yes"))


# "housing" 
table(fd_train$housing)
fd_all=fd_all %>% 
  mutate(housing=as.numeric(housing=="yes"))


# "loan"
table(fd_train$loan)
fd_all=fd_all %>% 
  mutate(loan=as.numeric(loan=="yes"))


# "contact" 
table(fd_all$contact)
fd_all=CreateDummies(fd_all,"contact",2000)


# "month" 
sort(table(fd_all$month),decreasing = T)
fd_all=CreateDummies(fd_all,"month",100)


# "pdays"  
table(fd_all$pdays)
fd_all$pdays=ifelse(fd_all$pdays==-1,"not_contacted",fd_all$pdays)
fd_all=fd_all %>% 
  mutate(pdays=as.numeric(pdays=="not_contacted"))
## consider -1 = "not contacted": 1 and other than -1 = "contacted" : 0

# "poutcome"
table(fd_all$poutcome)
fd_all=CreateDummies(fd_all,"poutcome",1000)

# "y" 
table(fd_all$y)
fd_all=fd_all %>% 
  mutate(y=as.numeric(y=="yes"))
######
glimpse(fd_all)

## Treating missing values
lapply(fd_all,function(x) sum(is.na(x)))

#####
## Separating the train and test data

fd_train=fd_all %>% 
  filter(data=="train") %>% 
  select(-data)

fd_test=fd_all %>% 
  filter(data=="test") %>% 
  select(-data,-y)

set.seed(2)
s=sample(1:nrow(fd_train),0.8*nrow(fd_train))

fd_train1=fd_train[s,]
fd_train2=fd_train[-s,]
############
## Model Building : Logistic Regression
## model building on trainl data and testing on train2 data

for_vif=lm(y~.-ID,
           data = fd_train1)
# consider vif to be 10
for_vif=lm(y~.-ID
              -poutcome_unknown
              -month_may
              -job_blue_collar,
           data = fd_train1)

sort(vif(for_vif),decreasing = T)[1:5]

fit=glm(y~.-ID
        -poutcome_unknown
        -month_may
        -job_blue_collar,
        data = fd_train1,
        family = "binomial")

fit=step(fit)

summary(fit)

###
formula(fit)

fit=glm(y ~ balance + housing + loan + day + duration + campaign + pdays + 
          job_student + job_housemaid + job_retired + job_admin. + 
          job_technician + job_management + marital_married + education_primary + 
          contact_unknown + month_mar + month_sep + month_oct + month_jan + 
          month_feb + month_apr + month_nov + month_jun + month_aug + 
          month_jul + poutcome_other + poutcome_failure,
        data = fd_train1,
        family = "binomial")

fit=glm(y ~ balance + housing + loan + day + duration + campaign + pdays + 
          job_student + job_housemaid + job_retired + job_admin. + 
          job_technician + job_management + marital_married + education_primary + 
          contact_unknown + month_mar + month_sep + month_oct + month_jan + 
          month_feb + month_apr + month_nov + month_jun + month_aug + 
          month_jul + poutcome_other + poutcome_failure,
        data = fd_train1,
        family = "binomial")

summary(fit)

val.pred=predict(fit,newdata = fd_train2,type = "response")

library(pROC)
auc(roc(fd_train2$y,val.pred))
# Area under the curve: 0.9065

##########################################################################################

## Random forest model

fd_train$y=as.factor(fd_train$y)
glimpse(fd_train)

library(randomForest)
library(cvTools)

param=list(mtry=c(5,10,15,20,30,40),
           ntree=c(50,100,150,200,250,500,700),
           maxnodes=c(1,5,20,30,40,60,70),
           nodesize=c(1,2,5,10,15))
# In first experiment
#        mtry ntree maxnodes nodesize
# 1418   10   500       60       15


param=list(mtry=c(8,9,10,12,14,16,20),
           ntree=c(400,450,480,500,520,550,600),
           maxnodes=c(50,55,60,65,70),
           nodesize=c(15,20,25,30,35))

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  
  return(subset_para)
}

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  
  return(score)
}

num_trails=10
my_params=subset_paras(param,num_trails)

# cvTuning

myauc=0

for(i in 1:num_trails){
  print(paste0("Starting iteration :",i))
  
  params=my_params[i,]
  
  k=cvTuning(randomForest,y~.-ID,
             data = fd_train,
             tuning = params,
             folds = cvFolds(nrow(fd_train),K=10,type="random"),
             cost = mycost_auc,
             seed = 2,
             predictArgs = list(type="prob")
             )
  score.this=k$cv[,2]
  
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
# myauc: 0.8963272
# best_params:
#        mtry ntree maxnodes nodesize
# 1418   10   500       60       15

# myauc: 0.9006872
# best_params:
#       mtry ntree maxnodes nodesize
# 969   10   550       70       30

## Model building
myauc=0.9006872
best_params=data.frame(mtry=10,
                       ntree=550,
                       maxnodes=70,
                       nodesize=30)

fd.rf.final=randomForest(y~.-ID,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data = fd_train)

## predicting on test data
test.predict=predict(fd.rf.final,newdata = fd_test,type = "prob")[,1]
write.csv(test.predict,file="choudary_prithvirajrf.csv",row.names = F)


### for hard classes
test.predict=predict(fd.rf.final,newdata = fd_test,type = "prob")

real=fd_train$y

cutoffs=seq(0.001,0.999,by=0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,ks=99,F.1=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(test.predict>cutoff)
  
  TP=sum(real==1 & predicted==1)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  TN=sum(real==0 & predicted==0)
  
  P=TP+FN
  N=FP+TN
  
  Sn=TP/P
  Sp=TN/N
  accuracy=TP+TN/P+N
  precision=TP/(TP+FP)
  recall=Sn
  
  ks=(TP/P)-(FP/N)
  F.1=(2*precision*recall)/(precision+recall)
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,ks,F.1))
}

cutoff_data=cutoff_data[-1,]

###
my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$ks)]
my_cutoff

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$F.1)]
my_cutoff

###
library(ggplot2)

ggplot(cutoff_data,aes(x=cutoff,y=ks))+geom_line()

test_predicted=as.numeric(test.predict>cutoff)
table(test_predicted)

write.csv(test_predicted,file = "choudary_prithvirajrf_P5_part2.csv",row.names = F)
