library(dplyr)
md <- read.csv("D:/R material/R/Data/Existing Base.csv",stringsAsFactors = FALSE)
glimpse(md)
View(md)

## Data preperation 

## Y DEPENDENT VARIABLE revenue_grid
    
  md$Revenue.Grid <-  as.numeric(md$Revenue.Grid == 1,0)
##converting the data in category 0,1,2,3,4 childrens
table(md$children)

md <-  md %>%
  mutate(children = ifelse(children=="Zero",0,substr(children,1,1)),
         children = as.numeric(children))

table(md$children)
glimpse(md)

##converting the range of age to mid values
table(md$age_band)
md <- md %>%
  mutate(lower_limit = as.numeric(substr(age_band,1,2)),
         upper_limit = as.numeric(substr(age_band,4,5)),
         age = ifelse(substr(age_band,1,2)=="71",71,ifelse(age_band=="unknown",NA,0.5*(upper_limit+lower_limit)))) %>%
  select(-lower_limit,-upper_limit,-age_band)%>%
  na.omit()
table(md$age)
table(is.na(md$age))
glimpse(md)
## converting in dummy variable of status

table(md$status)
md <- md%>%
  mutate(status_Divoreced_seperated = as.numeric(status == "Divorced/Separated"),
         status_Partner = as.numeric(status == "Partner"),
         status_Single_Never_Married = as.numeric(status == "Single/Never Married" ),
         status_Windowed = as.numeric(status == "Widowed")) %>%
  select(-status)

glimpse(md)

## categorizing occupation
round(prop.table(table(md$occupation,md$Revenue.Grid),1),2)
table(md$occupation)
md <- md %>%
  mutate(occupation_BM = as.numeric(occupation == "Business Manager"),
         occupation_HW = as.numeric(occupation == "Housewife"),
         occupation_MW = as.numeric(occupation == "Manual Worker"),
         occupation_OT = as.numeric(occupation == "Other"),
         occupation_PR = as.numeric(occupation == "Professional"),
         occupation_RE = as.numeric(occupation == "Retired"),
         occupation_SA = as.numeric(occupation == "Secretarial/Admin"),
         occupation_ST = as.numeric(occupation == "Student")) %>%
  select(-occupation)

glimpse(md)

## categorizing partner occupation 
table(md$occupation_partner)
md <- md %>%
  mutate(part_occupation_BM = as.numeric(occupation_partner == "Business Manager"),
         part_occupation_HW = as.numeric(occupation_partner == "Housewife"),
         part_occupation_MW = as.numeric(occupation_partner == "Manual Worker"),
         part_occupation_OT = as.numeric(occupation_partner == "Other"),
         part_occupation_PR = as.numeric(occupation_partner == "Professional"),
         part_occupation_RE = as.numeric(occupation_partner == "Retired"),
         part_occupation_SA = as.numeric(occupation_partner == "Secretarial/Admin"),
         part_occupation_ST = as.numeric(occupation_partner == "Student")) %>%
  select(-occupation_partner)
glimpse(md)

##categorizing home_status

table(md$home_status)
md <- md %>%
  mutate(home_status_LIPH = as.numeric(home_status == "Live in Parental Hom"),
         home_status_OWHM = as.numeric(home_status == "Own Home"),
         home_status_RFCH = as.numeric(home_status == "Rent from Council/HA")) %>%
  select(-home_status)

glimpse(md)
##categorizing family_income
table(md$family_income)
md <- md %>%
  mutate(fi_1=as.numeric(family_income %in% c("< 4,000","< 8,000, >= 4,000")),
       fi_2=as.numeric(family_income %in% c("<12,500, >=10,000","<25,000, >=22,500","<27,500, >=25,000")),
       fi_3=as.numeric(family_income %in% c("<10,000, >= 8,000","<15,000, >=12,500","<20,000, >=17,500",">=35,000")),
       fi_4=as.numeric(family_income %in% c("<17,500, >=15,000","<22,500, >=20,000","<30,000, >=27,500"))) %>%
  select(-family_income)

glimpse(md)

##self Employed
table(md$self_employed)
md <- md%>%
  mutate(self_employed = ifelse(self_employed == "No",0,1))
table(md$self_employed)

##self emolployed partner
table(md$self_employed_partner)
md <- md%>%
  mutate(self_employed_partner = ifelse(self_employed_partner == "No",0,1))
table(md$self_employed_partner)

##gender male or female
glimpse(md)
md <- md%>%
  mutate(gender_f = as.numeric(gender == "Female"),
         gender_m = as.numeric(gender == "Male"))
md <- md%>%
select(-gender)
glimpse(md)

#removing unwanted col
md <- md%>%
select(-TVarea,-post_code,-post_area,-region)

glimpse(md)

table(md$year_last_moved)

md <- md%>%
  filter(!(year_last_moved==0))


##data preperation complete
##train test split

set.seed(7)
s =sample(1:nrow(md),0.7*nrow(md))
train_md_data = md[s,]
test_md_data = md[-s,]

#train linerar model

library(car)
for_vif = lm(Revenue.Grid~.-REF_NO,data = train_md_data)
sort(vif(for_vif),decreasing = TRUE)[1:3]


for_vif = lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity,data = train_md_data)
sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif = lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative,data = train_md_data)
sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif = lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity,data = train_md_data)
sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif = lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-status_Partner,data = train_md_data)
sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif = lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-status_Partner-gender_m,data = train_md_data)
sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif = lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-status_Partner-gender_m-fi_3,data = train_md_data)
sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif = lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-status_Partner-gender_m-fi_3-Portfolio.Balance,data = train_md_data)
sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif = lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-status_Partner-gender_m-fi_3-Portfolio.Balance-occupation_RE,data = train_md_data)
sort(vif(for_vif),decreasing = TRUE)[1:3]


rg_fit <-  train_md_data %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-status_Partner,-gender_m,-fi_3,-Portfolio.Balance)

names(rg_fit)
glimpse(rg_fit)



first_step = glm(Revenue.Grid~.,family = "binomial" , data = rg_fit)
summary(first_step)

fit = step(first_step)
summary(fit)

formula(fit)

fit = glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + Balance.Transfer + 
            Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
            Personal.Loan + Investment.Tax.Saving.Bond + Home.Loan + 
            Online.Purchase.Amount + age + status_Divoreced_seperated + 
            status_Windowed + part_occupation_HW + part_occupation_RE,data=train_md_data,family = "binomial")
summary(fit)

fit = glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + Balance.Transfer + 
            Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
            Personal.Loan + Investment.Tax.Saving.Bond + Home.Loan + 
            Online.Purchase.Amount  + status_Divoreced_seperated + 
            status_Windowed + part_occupation_HW + part_occupation_RE,data=train_md_data,family = "binomial")
summary(fit)

fit = glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + Balance.Transfer + 
            Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
            Personal.Loan + Investment.Tax.Saving.Bond + Home.Loan + 
            Online.Purchase.Amount  + status_Divoreced_seperated + 
            status_Windowed + part_occupation_HW ,data=train_md_data,family = "binomial")

summary(fit)


fit = glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + Balance.Transfer + 
            Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
            Personal.Loan + Investment.Tax.Saving.Bond + Home.Loan + 
            Online.Purchase.Amount  + status_Divoreced_seperated + 
            status_Windowed ,data=train_md_data,family = "binomial")
summary(fit)

fit = glm(Revenue.Grid ~ children + Average.Credit.Card.Transaction + Balance.Transfer + 
            Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
            Personal.Loan + Investment.Tax.Saving.Bond + Home.Loan + 
            Online.Purchase.Amount + 
            status_Windowed ,data=train_md_data,family = "binomial")

summary(fit)
formula(fit)


train_md_data$score = predict(fit ,newdata = train_md_data ,type = "response")
predict_train <- predict(fit,newdata = test_md_data,type = "response")
head(predict_train)
head(train_md_data$score)
#-------Reciever Operating Characteristic----------------
library(ROCR)

ROCRPred <- prediction(train_md_data$score,train_md_data$Revenue.Grid)
ROCRPerf <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(.1, by = 0.1))
abline(a=0, b=1)
# Area under the curve
auc <- performance(ROCRPred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc,4)
auc
legend(.5, .3, auc, title = "AUC", cex = .75)

res <- predict(fit, test_md_data, type = "response")

# table(ActualValue=d_test$Type, PredictedValue=res>0.4)
PredictedValue <- res>.4
head(train_md_data$Revenue.Grid)
head(res)

head(PredictedValue)
pv <- as.numeric(PredictedValue)
head(pv)
pv <- as.factor(pv)
train_md_data$Revenue.Grid <- as.factor(train_md_data$Revenue.Grid)
library(caret)

confusionMatrix(pv, train_md_data$Revenue.Grid)
##Confusion Matrix and Statistics
?confusionMatrix
##Reference
##Prediction    0    1
##0 6149  236
##1  131  519

##Accuracy : 0.9478          
##95% CI : (0.9424, 0.9529)
##No Information Rate : 0.8927          
##P-Value [Acc > NIR] : < 2.2e-16       

##Kappa : 0.71            

##Mcnemar's Test P-Value : 5.675e-08       
                                          
##            Sensitivity : 0.9791          
##            Specificity : 0.6874          
##         Pos Pred Value : 0.9630          
##         Neg Pred Value : 0.7985          
##             Prevalence : 0.8927          
##         Detection Rate : 0.8741          
##   Detection Prevalence : 0.9076          
##      Balanced Accuracy : 0.8333          
                                          
##       'Positive' Class : 0               
