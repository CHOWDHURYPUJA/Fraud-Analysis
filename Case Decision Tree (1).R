library(caret)
library(stringr)
library("tidyverse") #needed for functions: select() and fct_recode()

fraud_data_rpart <- fraud_data %>% 
  select(-c('PWD_UPDT_TS','PH_NUM_UPDT_TS',
            'CUST_SINCE_DT','TRAN_TS','TRAN_DT',
            'lat','long','tran_lat','tran_long',
            'CUST_ZIP','RGN_NAME')) %>% #remove date fields
  mutate(STATE_PRVNC_TXT = ifelse(TRAN_INTL == 1 | 
                                    STATE_PRVNC_TXT == 'dublin', 'International',STATE_PRVNC_TXT),
         CARR_NAME = ifelse(CARR_NAME %in% top_9_carriers$CARR_NAME_new, CARR_NAME, 'Other'))

set.seed(222) #set random seed

index <- createDataPartition(fraud_data_rpart$FRAUD_NONFRAUD, p = .85,list = FALSE)
fraud_train <-fraud_data_rpart[index,]
fraud_test <- fraud_data_rpart[-index,]

library(rpart)
set.seed(333)
fraud_tree_model_lowcp <- train(FRAUD_NONFRAUD ~ .,
                                data = fraud_train,
                                method = "rpart",
                                trControl =trainControl(method = "cv",number = 5,
                                                        ## Estimate class probabilities
                                                        classProbs = TRUE,
                                                        #needed to get ROC
                                                        summaryFunction = twoClassSummary),
                                tuneGrid = expand.grid(cp=0.001),
                                metric="ROC")

fraud_tree_model_highcp <- train(FRAUD_NONFRAUD ~ .,
                                data = fraud_train,
                                method = "rpart",
                                trControl =trainControl(method = "cv",number = 5,
                                                        ## Estimate class probabilities
                                                        classProbs = TRUE,
                                                        #needed to get ROC
                                                        summaryFunction = twoClassSummary),
                                tuneGrid = expand.grid(cp=0.01),
                                metric="ROC")

#plot variable importance
plot(varImp(fraud_tree_model_lowcp), top = 20)

#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(fraud_tree_model_lowcp$finalModel, type=5)

#First, get the predicted probabilities of the test data.
fraud_tree_highcp_pred_prob<-predict(fraud_tree_model_highcp, fraud_test, type="prob")
fraud_tree_lowcp_pred_prob<-predict(fraud_tree_model_lowcp, fraud_test, type="prob")

library(ROCR)
fraud_tree_highcp_pred <- prediction(fraud_tree_highcp_pred_prob$Fraud, fraud_test$FRAUD_NONFRAUD,label.ordering = c('nonFraud','Fraud'))
fraud_tree_highcp_perf <- performance(fraud_tree_highcp_pred, "tpr", "fpr")
plot(fraud_tree_highcp_perf, colorize=TRUE)
#Get the AUC
fraud_tree_highcp_auc<-unlist(slot(performance(fraud_tree_highcp_pred, "auc"), "y.values"))

fraud_tree_lowcp_pred <- prediction(fraud_tree_lowcp_pred_prob$Fraud, fraud_test$FRAUD_NONFRAUD,label.ordering = c('nonFraud','Fraud'))
fraud_tree_lowcp_perf <- performance(fraud_tree_lowcp_pred, "tpr", "fpr")
plot(fraud_tree_lowcp_perf, colorize=TRUE)
#Get the AUC
fraud_tree_lowcp_auc<-unlist(slot(performance(fraud_tree_lowcp_pred, "auc"), "y.values"))

