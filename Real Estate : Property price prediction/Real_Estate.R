setwd("C:\\Users\\cpraj\\OneDrive\\Desktop\\PROJECTS\\Real_Estate-1")

rs_train=read.csv("housing_train.csv")
rs_test=read.csv("housing_test.csv")

## Data discussion

# "Suburb"
sort(table(rs_train$Suburb),decreasing=T)
## create dummies

# "Address"
table(rs_train$Address)
## drop it in first experiment
## In next iteration extract last 2 letters from it

# "Rooms"
table(rs_train$Rooms)
## let it be

# "Type"
table(rs_train$Type)
## create dummies

# "Price"
table(rs_train$Price)
## let it be : target column

# "Method" 
table(rs_train$Method)
## create dummies

# "SellerG"
sort(table(rs_train$SellerG),decreasing = T)
## create dummies

# "Distance"
table(rs_train$Distance)
## let it be

# "Postcode"
sort(table(rs_train$Postcode),decreasing=T)
class(rs_train$Postcode)
## its a character type because its unique ID though its integer type, drop it

# "Bedroom2"
table(rs_train$Bedroom2)
table(rs_train$Bedroom2,rs_train$Rooms)
# "Bathroom"
table(rs_train$Bathroom)
# "Car" 
table(rs_train$Car)
# "Landsize"
table(rs_train$Landsize)
# "BuildingArea"
table(rs_train$BuildingArea)

## for Bedroom2 : BuildingArea take group by mean [group by is Rooms]

# "YearBuilt"
table(rs_train$YearBuilt)  
## almost 50% data is missing, so then 
# create flag variables 1=missing and 0=not missing


# "CouncilArea"
table(rs_train$CouncilArea)
## Replace with word missing for missing vales and then create dummies 


###### 
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for(cat in categories){
    name=paste(var,cat,sep = "_")
    name=gsub("-","_",name)
    name=gsub(" ","",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("<","LT_",name)
    name=gsub("=","EQ_",name)
    name=gsub("\\?","Q_",name)
    
    data[,name]=as.numeric(data[,var]==cat)
    
  }
  data[,var]=NULL
  return(data)
}
############

### COMBINING TRAIN AND TEST DATA
rs_test$Price=NA

rs_train$data="train"
rs_test$data="test"

rs_all=rbind(rs_train,rs_test)


############# DATA PREPARATION
library(dplyr)
glimpse(rs_all)

# Address
rs_all$Address=NULL

# "postcode"
class(rs_all$Postcode)
rs_all$Postcode=NULL


# "YearBuilt"
sort(table(rs_all$YearBuilt),decreasing = T)

sum(is.na(rs_all$YearBuilt))

rs_all$YearBuilt=ifelse(is.na(rs_all$YearBuilt),"missing",rs_all$YearBuilt)

rs_all = rs_all %>% 
  mutate(YearBuilt=as.numeric(YearBuilt=="missing"))

# "CouncilArea"
rs_all$CouncilArea=ifelse(rs_all$CouncilArea=="","missing",rs_all$CouncilArea)

# Creating dummies for these columns
names(rs_all)
# "Suburb"  "Type"  "Method"  "SellerG"  "Postcode" "CouncilArea" 
glimpse(rs_all)
summary(rs_all)

# "Suburb"
sort(table(rs_all$Suburb),decreasing = T)
sum(is.na(rs_all$Suburb))
rs_all=CreateDummies(rs_all,"Suburb",freq_cutoff = 80)

# "Type"
sort(table(rs_all$Type))
sum(is.na(rs_all$Type))
rs_all=CreateDummies(rs_all,"Type",freq_cutoff = 500)

# "Method"
sort(table(rs_all$Method))
sum(is.na(rs_all$Method))
rs_all=CreateDummies(rs_all,"Method",freq_cutoff = 500)

# "SellerG"
sort(table(rs_all$SellerG),decreasing = T)
sum(is.na(rs_all$SellerG))
rs_all=CreateDummies(rs_all,"SellerG",freq_cutoff = 300)

# "CouncilArea" 
sort(table(rs_all$CouncilArea),decreasing = T)
sum(is.na(rs_all$CouncilArea))
rs_all=CreateDummies(rs_all,"CouncilArea" ,freq_cutoff = 200)

### REPLACING MISSING VALUES

# "Bedroom2" "Bathroom"     "Car"   "Landsize"     "BuildingArea" 

rs_all[!(is.na((rs_all$Price)) & rs_all$data=="train"),]
lapply(rs_all,function(x) sum(is.na(x)))

rs_all %>% 
  group_by(Rooms) %>% 
  summarise(avg=mean(Bedroom2,na.rm=T))

for(col in names(rs_all)){
  if(sum(is.na(rs_all[,col]))>0 & !(col %in% c("data","Price"))){
    rs_all[is.na(rs_all[,col]),col]=rs_all %>% 
      group_by(Rooms) %>% 
      mutate(mean(rs_all[rs_all$data=="train",col],na.rm=T))
  }
}

##### SEPARATE train and test DATA
rs_train=rs_all %>% 
  filter(data=="train") %>% 
  select(-data)

rs_test=rs_all %>% 
  filter(data=="test") %>% 
  select(-data,-Price)

set.seed(2)
s=sample(1:nrow(rs_train),0.8*nrow(rs_train))

rs_train1=rs_train[s,]
rs_train2=rs_train[-s,]

################
# Dropping variables on vif values
library(car)

## vif= 10
for_vif=lm(Price~., data=rs_train1)

sort(vif(for_vif),decreasing = T)[1:5]
rm(for_vif)

###
fit=lm(Price~., data=rs_train1)
fit=step(fit)

### Dropping variables on the basis of p-values, 0.05
summary(fit)
formula(fit)

fit=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
         BuildingArea + YearBuilt + 
         Suburb_KeilorEast + 
         Suburb_HawthornEast + Suburb_Prahran + Suburb_SurreyHills + 
         Suburb_Kensington + Suburb_Sunshine + Suburb_Toorak + Suburb_Elwood + 
         Suburb_Maribyrnong + Suburb_Newport + Suburb_Doncaster + 
         Suburb_AscotVale +  Suburb_Hampton + Suburb_Balwyn + 
         Suburb_MalvernEast + Suburb_Camberwell + 
          Suburb_Bentleigh + Suburb_BrightonEast + 
         Suburb_Hawthorn + Suburb_BalwynNorth +  Suburb_Kew + 
         Suburb_Brighton + Suburb_Glenroy + Suburb_GlenIris + Suburb_Essendon + 
          Suburb_Preston + Suburb_Richmond + 
         Suburb_Reservoir + Type_u + Type_h + Method_PI + Method_S + 
         SellerG_Buxton + SellerG_Marshall + SellerG_Barry + SellerG_hockingstuart + 
         SellerG_Jellis + SellerG_Nelson +  
         CouncilArea_Melbourne + CouncilArea_Banyule + CouncilArea_PortPhillip + 
         CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
         CouncilArea_GlenEira + CouncilArea_MooneeValley + CouncilArea_Moreland + 
         CouncilArea_Boroondara + CouncilArea_missing,
       data=rs_train1)
summary(fit)

## Testing the model on train2 data
val.pred=predict(fit,newdata = rs_train2)

errors=rs_train2$Price-val.pred

errors**2 %>% mean() %>% sqrt()
#  402549
# Score =212467/RMSE
212467/402549

plot(fit,1)
plot(fit,2)
plot(fit,3)
plot(fit,4)
#############################################################################################



#### RANDOM FOREST MODEL BUILDING
library(randomForest)
library(cvTools)


param=list(mtry=c(5,10,15,20,25),
             ntree=c(50,100,200,500,700),
             maxnodes=c(5,10,15,20,30,50),
             nodesize=c(1,2,5,10))

expand.grid(param)

#      mtry ntree maxnodes nodesize
# 590   25   200       50       10

param=list(mtry=c(20,25,30,35,40),
           ntree=c(150,180,200,250,300,400,450),
           maxnodes=c(45,50,55,60,65,70,80),
           nodesize=c(8,10,15,20,25))

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trails=50
my_params=subset_paras(param,num_trails)

## cvtuning for Regression model

myerror=9999999

for(i in 1:num_trails){
  print(paste0("Starting iteration : ", i))
  params=my_params[i,]
  
  k=cvTuning(randomForest,Price~.,
             data=rs_train,
             tuning = params,
             folds = cvFolds(nrow(rs_train),K=10,type="random"),
             seed = 2
             )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    myerror=score.this
    print(myerror)
    best_params=params
  }
  print("DONE")
}

myerror
best_params
# Score =212467/RMSE

# In first experiment
# myerror: 410775.4

# best_params:
#      mtry ntree maxnodes nodesize
# 590   25   200       50       10

# In Second Experiment
# myerror: 387627
# best_params:
#       mtry ntree maxnodes nodesize
# 1225   40   450       80       25


myerror=387627
# 212467/387627
best_params=data.frame(mtry=40,
                       ntree=450,
                       maxnodes=80,
                       nodesize=25)

