setwd("C:\\Users\\cpraj\\OneDrive\\Desktop\\PROJECTS\\HR-4")
getwd()

# ed:employee data

ed_train=read.csv("hr_train.csv")
ed_test=read.csv("hr_test.csv")

#######Data discussion
names(ed_train)
library(dplyr)

glimpse(ed_train)

# "satisfaction_level"
# "last_evaluation"       
# "number_project"        
# "average_montly_hours" 
# "time_spend_company"    
# "Work_accident"         
# "left"  : Target column                 
# "promotion_last_5years"

lapply(ed_train,table)
## let it be

# "sales" 
table(ed_train$sales)
## create dummies

# "salary" 
table(ed_train$salary)
## create dummies

#### CreateDummies function
CreateDummies=function(data,var,freq_cutoff){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for(cat in categories){
    name=paste(var,cat,sep = "_")
    name=gsub("-","_",name)
    name=gsub(" ","",name)
    name=gsub("\\/","_",name)
    name=gsub("<","LT_",name)
    name=gsub(">", "GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub("\\?","Q_",name)
    
    data[,name]=as.numeric(data[,var]==cat)
    
  }
  data[,var]=NULL
  return(data)
}
####

ed_test$left=NA

ed_train$data="train"
ed_test$data="test"

ed_all=rbind(ed_train,ed_test)
############
# Creating dummies
# "sales" 
sort(table(ed_train$sales),decreasing = T)
ed_all=CreateDummies(ed_all,"sales",freq_cutoff = 400)

# "salary" 
table(ed_train$salary)
ed_all=CreateDummies(ed_all,"salary",freq_cutoff = 500)

## Treating missing values
summary(ed_all)

lapply(ed_all,function(x) sum(is.na(x)))

##########
## separating the train and data
ed_train=ed_all %>% 
  filter(data=="train") %>% 
  select(-data)

ed_test=ed_all %>% 
  filter(data=="test") %>% 
  select(-data,-left)

set.seed(2)
s=sample(1:nrow(ed_train),0.8*nrow(ed_train))

ed_train1=ed_train[s,]
ed_train2=ed_train[-s,]
## MOdel building : logistic regression

# model building on train1 data and testing on train2 data
for_vif=lm(left~.-sales_sales,
           data = ed_train1)
# consider vif to be 5
sort(vif(for_vif),decreasing = T)[1:5]

fit=glm(left~.-sales_sales,
        data = ed_train1,
        family = "binomial")

fit=step(fit)
##
formula(fit)

fit=glm(left ~ satisfaction_level + last_evaluation + number_project + 
          average_montly_hours + time_spend_company + Work_accident + 
          promotion_last_5years + sales_hr + sales_accounting + sales_marketing + 
          sales_IT + sales_technical + salary_medium + salary_low,
        data = ed_train1,
        family = "binomial")

fit=glm(left ~ satisfaction_level + last_evaluation + number_project + 
          average_montly_hours + time_spend_company + Work_accident + 
           sales_hr + sales_accounting + sales_marketing + 
           sales_technical + salary_medium + salary_low,
        data = ed_train1,
        family = "binomial")

summary(fit)

val.pred=predict(fit,newdata = ed_train2,type="response")

library(pROC)

auc(roc(ed_train2$left,val.pred))
# Area under the curve: 0.7253

#####################################################################################

# Random forest model building
ed_train$left=as.factor(ed_train$left)
glimpse(ed_train)

library(randomForest)
library(cvTools)

param=list(mtry=c(3,5,8,10,15,18),
           ntree=c(20,50,100,250,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10,15))

#       mtry ntree maxnodes nodesize
# 200    5   250       50        1

param=list(mtry=c(4,5,6,7,8,9),
           ntree=c(150,180,200,250,280,300),
           maxnodes=c(45,48,50,55,60),
           nodesize=c(1,2,3,4,5,6))

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
}

mycost_auc=function(y,yhat){
  rocscore=pROC::roc(y,yhat)
  score=pROC::auc(rocscore)
  
  return(score)
}

num_trails=50
my_params=subset_paras(param,num_trails)

# cvTuning

myauc=0

for(i in 1:num_trails){
  print(paste0("Starting iteration :",i))
  
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~.,
             data = ed_train,
             tuning = params,
             folds = cvFolds(nrow(ed_train),K=10,type="random"),
             cost = mycost_auc,
             seed=2,
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
## In first experiment
# myauc: 1
# best_params 
#      mtry ntree maxnodes nodesize
# 200    5   250       50        1

# In second eperiment
# myauc: 1
# best_params: 
#       mtry ntree maxnodes nodesize
# 1053    6   180       60        6

########

myauc=1
best_params=data.frame(mtry=6,
                       ntree=180,
                       maxnodes=60,
                       nodesize=6)


ed_rf_final=randomForest(left~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data = ed_train)

test.predict=predict(ed_rf_final,newdata = ed_test,type = "prob")[,1]
write.csv(test.predict,file="choudary_prithvirajrf_P4_part2.csv", row.names = F)
