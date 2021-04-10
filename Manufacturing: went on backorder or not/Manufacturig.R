setwd("C:\\Users\\cpraj\\OneDrive\\Desktop\\PROJECTS\\Manufacturing-3")
getwd()

# pd : product data

pd_train=read.csv("product_train.csv")
pd_test=read.csv("product_test.csv")

library(dplyr)

###### DATA DISCUSSION
glimpse(pd_train)
names(pd_train)
# "sku" 
## treat it as ID : drop it while modelling

# "national_inv"
table(pd_train$national_inv)
IQR(pd_train$national_inv)
quantile(pd_train$national_inv)

# q1-1.5*IQR and q3+1.5*IQR
4-1.5*72=-104
76+1.5*72=184
sum(pd_train$national_inv< -104)
sum(pd_train$national_inv>184)

## let it be

# "lead_time"
table(pd_train$lead_time)
## let it be

# "in_transit_qty"
table(pd_train$in_transit_qty)
## let it be

# "forecast_3_month"
table(pd_train$forecast_3_month)
## let it be

# "forecast_6_month"
table(pd_train$forecast_6_month)
## let it be

# "forecast_9_month"
table(pd_train$forecast_9_month)
## let it be

# "sales_1_month"
# "sales_3_month"     
# "sales_6_month"     
# "sales_9_month"
table(pd_train$sales_1_month)
table(pd_train$sales_3_month)
table(pd_train$sales_6_month)
table(pd_train$sales_9_month)
## let it be

# "min_bank"
table(pd_train$min_bank)
## let it be

# "potential_issue"
table(pd_train$potential_issue)
# consider yes : 1 and no : 0

# "pieces_past_due"
table(pd_train$pieces_past_due)
## let it be

# "perf_6_month_avg" 
table(pd_train$perf_6_month_avg)
## let it be

# "perf_12_month_avg"
table(pd_train$perf_12_month_avg)
## let it be

# "local_bo_qty"
table(pd_train$local_bo_qty)
## let it be

# "deck_risk"         
# "oe_constraint"     
# "ppap_risk"        
# "stop_auto_buy"     
# "rev_stop"          
# "went_on_backorder" : Target column
table(pd_train$deck_risk)
table(pd_train$oe_constraint)
table(pd_train$ppap_risk)
table(pd_train$stop_auto_buy)
table(pd_train$rev_stop)
table(pd_train$went_on_backorder)
## consider yes : 1 and no : 0

############
## CreateDummies function
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
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub("\\?","Q_",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}
###########

pd_test$went_on_backorder=NA

pd_train$data="train"
pd_test$data="test"

pd_all=rbind(pd_train,pd_test)
########
# creating dummies

# "potential_issue"
# "deck_risk"         
# "oe_constraint"     
# "ppap_risk"        
# "stop_auto_buy"     
# "rev_stop"          
# "went_on_backorder" : Target column
table(pd_all$potential_issue)
table(pd_all$deck_risk)
table(pd_all$oe_constraint)
table(pd_all$ppap_risk)
table(pd_all$stop_auto_buy)
table(pd_all$rev_stop)
table(pd_all$went_on_backorder)
# consider yes : 1 and no : 0

pd_all=pd_all %>% 
  mutate(potential_issue=as.numeric(potential_issue=="Yes"),
         deck_risk=as.numeric(deck_risk=="Yes"),
         oe_constraint=as.numeric(oe_constraint=="Yes")
         )

pd_all=pd_all %>% 
  mutate(ppap_risk=as.numeric(ppap_risk=="Yes"),
         stop_auto_buy=as.numeric(stop_auto_buy=="Yes"),
         rev_stop=as.numeric(rev_stop=="Yes"),
         went_on_backorder=as.numeric(went_on_backorder=="Yes"))

####################################
# Treating missing values

lapply(pd_all, function(x) sum(is.na(x)))
###########################
## separating train and test data
pd_train=pd_all %>% 
  filter(data=="train") %>% 
  select(-data)

pd_test=pd_all %>% 
  filter(data=="test") %>% 
  select(-data,-went_on_backorder)

set.seed(2)
s=sample(1:nrow(pd_train),0.8*nrow(pd_train))

pd_train1=pd_train[s,]
pd_train2=pd_train[-s,]

# MOdel building : LOGISTIC REGRESSION
## model building on train1 data and testing on train2 data

library(car)
for_vif=lm(went_on_backorder~.-sku,
           data =pd_train1)

# consider vi to be 10
for_vif=lm(went_on_backorder~.-sku
                              -forecast_6_month
                              -sales_6_month
                              -sales_9_month
                              -forecast_9_month
                              -sales_1_month,
           data =pd_train1)
sort(vif(for_vif),decreasing = T)[1:5]

fit=glm(went_on_backorder~.-sku
                            -forecast_6_month
                            -sales_6_month
                            -sales_9_month
                            -forecast_9_month
                            -sales_1_month,
        data = pd_train1,
        family = "binomial")

fit=step(fit)

###
formula(fit)
fit=glm(went_on_backorder ~ national_inv + lead_time + in_transit_qty + 
          forecast_3_month + min_bank + pieces_past_due + perf_6_month_avg + 
          deck_risk + rev_stop,
        data = pd_train1,
        family = "binomial")

fit=glm(went_on_backorder ~ national_inv + lead_time + in_transit_qty + 
          forecast_3_month + min_bank + pieces_past_due + perf_6_month_avg + 
          deck_risk + rev_stop,
        data = pd_train1,
        family = "binomial")

summary(fit)

val.pred=predict(fit,newdata = pd_train2,type="response")

library(pROC)

auc(roc(pd_train2$went_on_backorder,val.pred))
# Area under the curve: 0.6568

#########
test.predict=predict(fit.final,newdata = pd_test,type="response")
real=pd_train$went_on_backorder
cutoffs=seq(0.001,0.999,by=0.001)

 
cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,ks=99,F.1=99)

for(cutoff in cutoffs){
  predicted=as.numeric(test.predict>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=FP+TN
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  ks=(TP/P)-(FP/N)
  F.1=(2*precision*recall)/(precision+recall)
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,ks,F.1))
  
}

cutoff_data=cutoff_data[-1,]

library(ggplot2)

ggplot(cutoff_data,aes(x=cutoff,y=ks))+geom_line()

my_cutoff=cutoff_data$ks[which.max(cutoff_data$ks)]
my_cutoff

#################################################################################
