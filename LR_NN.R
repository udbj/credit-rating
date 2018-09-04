library(nnet)
library(gmodels)
set.seed(1024)

#################################################################
# Data pre-processing
#################################################################
#Read credit assessment data
credit<-read.table("[path-here]/german.data")
str(credit)

#Assign column names

names(credit)<-c("account_status","duration","credit_history","purpose",
                 "credit_amt","savings_amt","unemployment_duration",
                 "installment_rate_perc_income", "gender_marital_status"
                 ,"guarantors","present_residence_since","property","age"
                 ,"other_installment_plans", "housing","existing_credits_in_bank",
                 "job","dependants", "telephone", "foreign_worker","rating")


#Change ratings to 0/1 and factorise
credit$rating<-ifelse(test = credit$rating == 2,yes = 0,no = 1)
credit$rating<-as.factor(credit$rating)

#Get columns with numerical values
num_cols<-sapply(credit,is.numeric)

#normalisation
normalise<-function(x){ (x-min(x))/(max(x)-min(x)) }
credit[,num_cols]<-lapply(credit[,num_cols],normalise)

str(credit)

#Training set for single models
single_train<-credit[1:300,]

#Testing set for LM-Hybrid
lm_hybrid_test<-credit[301:700,]

#Common testing set for all models
common_test<-credit[701:1000,]

####################################################################
# LM
####################################################################

#Train logistic regression model
lm_model <- glm(rating ~.,data=single_train,family = binomial(link = "logit") )
summary(lm_model)

#Calculate results
lm_hybrid_result<-predict(object = lm_model,newdata = lm_hybrid_test,type = "response")
lm_solo_result<-predict(object = lm_model,newdata = common_test,type = "response")

#Convert probabilities to yes/no with 0.5 as threshold
lm_hybrid_result<-ifelse(lm_hybrid_result >= 0.5,1,0)
lm_solo_result<-ifelse(lm_solo_result >= 0.5,1,0)

#Inspect crosstable
CrossTable(x = lm_hybrid_test$rating,y = lm_hybrid_result,chisq = F)

########################################################################
# NN HYBRID
########################################################################


#Create training set for NN, using correct results from LM
nn_hybrid_train<-lm_hybrid_test[lm_hybrid_result == lm_hybrid_test$rating,]

#create NN model and predict

nn_model_hybrid<-nnet(rating~.,data = nn_hybrid_train,size = 16)
nn_prediction_hybrid<-predict(nn_model_hybrid,common_test,type = "class")


#######################################################################
# NN SOLO
#######################################################################
nn_solo_model<-nnet(rating~.,data = single_train,size = 16)
nn_prediction_solo<-predict(nn_solo_model,common_test,type = "class")

#######################################################################
# Comparison of models
#######################################################################
print("Logistic Regression: ")
CrossTable(x = common_test$rating,y = lm_solo_result,chisq = F)
print("Neural Network: ")
CrossTable(x = common_test$rating,y = nn_prediction_solo,chisq = F)
print("Logistic Regression + Neural Network: ")
CrossTable(x = common_test$rating,y = nn_prediction_hybrid,chisq = F)








