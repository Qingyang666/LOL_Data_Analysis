
library(tidyverse)
game_statistic<- read_csv('data/game_statistic.csv')

##################################Modeling######################################

model_data <- game_statistic %>%
                select(result,
                      year, 
                      month,
                      weekdays, 
                      time.period, 
                      gamelength,
                      game,
                      ends_with("diffat10.blue"),
                      ends_with("diffat10_bot.blue"),
                      ends_with("diffat10_mid.blue"),
                      ends_with("diffat10_sup.blue"),
                      ends_with("diffat10_top.blue"),
                      ends_with("diffat10.red"),
                      ends_with("diffat10_bot.red"),
                      ends_with("diffat10_jng.red"),
                      ends_with("diffat10_mid.red"),
                      ends_with("diffat10_sup.red"),
                      ends_with("diffat10_top.red")) %>%
  mutate(result = as.factor(result),
         month = factor(month, ordered = F),
         weekdays = factor(weekdays, ordered = F),
         time.period = factor(time.period, ordered = F),
         gamelength = factor(gamelength, ordered = F),
         game = as.factor(game)) %>%
  drop_na()

# correlation of predictor variables
corr_matrix <- round(cor(model_data[,-c(1:7)]), 2)
# transfer to absolute value
corr_matrix_abs <- abs(corr_matrix)

# generate function to choose features that need to remove due to high correlation with other features
select_x<-function(c,m){                
  c<-ifelse(c>m,1,0)
  c<-as.data.frame(c)
  n<-nrow(c)
  for(i in 1:n){
    for(j in 1:n){
      c[i,j]<-ifelse(i>j,c[i,j],0)
    }
  }
  index<-which(c==1)[!(which(c==1) %in% c((1:n)^2))]
  index_c<-ifelse(index %% n==0,n,index %% n)
  index_r<-(index-index_c) %/% n
  c$name<-row.names(c)
  name_index_c<-c$name[index_c]
  name_index_r<-c$name[index_r]
  cat("\n the variables that have correlation greater than",m," include：\n")
  for(i in 1:length(name_index_r)){
    cat("     ",i,":",name_index_r[i],"---",name_index_c[i],"\n")
  }
  return(unique(name_index_c))
}

# features that need to remove, due to the higher correlation
# generally we want to remove features with an absolute correlation of 0.8 or higher.
delete_varname <- select_x(corr_matrix_abs,0.8)

# drop specified features
model_data <- model_data[,!(colnames(model_data) %in% delete_varname)]

# split into train_data and test_data  
train_data <- model_data %>%
  filter(year %in% c(2016, 2017, 2018, 2019, 2020)) %>%
  mutate_at(vars(ends_with(".blue")), scale) %>%
  mutate_at(vars(ends_with(".red")), scale) 

test_data <- model_data %>%
  filter(year == 2021) %>%
  mutate_at(vars(ends_with(".blue")), scale) %>%
  mutate_at(vars(ends_with(".red")), scale) 

#########################logistic regression####################################
library(ROCR)
library(MASS)
set.seed(1234)
model1 <- glm(result ~. ,data = train_data, family = "binomial")

# make prediction on the test data set called as test_logit
test_logit <- test_data %>%
  mutate(predicted_probability = predict(model1, test_data, type = 'response'),
         predictions_point_3 = case_when(predicted_probability >= 0.3 ~ "1",
                                         predicted_probability < 0.3 ~ "0"),
         predictions_point_5 = case_when(predicted_probability >= 0.5 ~ "1",
                                         predicted_probability < 0.5 ~ "0"),
         predictions_point_7 = case_when(predicted_probability >= 0.7 ~ "1",
                                         predicted_probability < 0.7 ~ "0"))

# Threshold Analysis: calculate accuracy at different threshold
logit_accuracy_point_3 <- length(which(test_logit$predictions_point_3 == test_logit$result) == TRUE)/length(test_logit$result)
cat('when the threshold is 0.3, the accuracy of the prediction is', logit_accuracy_point_3)

logit_accuracy_point_5 <- length(which(test_logit$predictions_point_5 == test_logit$result) == TRUE)/length(test_logit$result)
cat('when the threshold is 0.5, the accuracy of the prediction is', logit_accuracy_point_5)

logit_accuracy_point_7 <- length(which(test_logit$predictions_point_7 == test_logit$result) == TRUE)/length(test_logit$result)
cat('when the threshold is 0.7, the accuracy of the prediction is', logit_accuracy_point_7)

# compute the AUC
test_logit_pred <- prediction(test_logit$predicted_probability, 
                              test_logit$result)
test_logit_perf <- performance(test_logit_pred, 'auc')
cat('the auc score by using logistic regression model is ', test_logit_perf@y.values[[1]], "\n")

###################################random forest model##########################

library(ranger)
model2 <- ranger(result ~.,
                 data = train_data,
                 num.trees = 1000,
                 respect.unordered.factors = TRUE,
                 probability = TRUE)

# make prediction on the test data set
test_rf <- test_data %>%
  mutate(predicted_probability = predict(model2, data = test_data, num.trees = 1000, type = 'response')$predictions[,2],
         predictions_point_3 = case_when(predicted_probability >= 0.3 ~ "1",
                                         predicted_probability < 0.3 ~ "0"),
         predictions_point_5 = case_when(predicted_probability >= 0.5 ~ "1",
                                         predicted_probability < 0.5 ~ "0"),
         predictions_point_7 = case_when(predicted_probability >= 0.7 ~ "1",
                                         predicted_probability < 0.7 ~ "0"))

# calculate accuracy at different threshold
rf_accuracy_point_3 <- length(which(test_rf$predictions_point_3 == test_rf$result) == TRUE)/length(test_rf$result)
cat('when the threshold is 0.3, the accuracy of the prediction is', rf_accuracy_point_3)

rf_accuracy_point_5 <- length(which(test_rf$predictions_point_5 == test_rf$result) == TRUE)/length(test_rf$result)
cat('when the threshold is 0.5, the accuracy of the prediction is', rf_accuracy_point_5)

rf_accuracy_point_7 <- length(which(test_rf$predictions_point_7 == test_rf$result) == TRUE)/length(test_rf$result)
cat('when the threshold is 0.7, the accuracy of the prediction is', rf_accuracy_point_7)

# compute the AUC of the model
test_rf_pred <- prediction(test_rf$predicted_probability, test_rf$result)
test_rf_perf <- performance(test_rf_pred, 'auc')
cat('the auc score by using random forest model is ', test_rf_perf@y.values[[1]], "\n")

#################################decision tree##################################

library(rpart)
model3 <- rpart(result ~ ., data=train_data, method="class", minbucket = 150)

# make prediction on the test data set
test_dt <- test_data %>%
  mutate(predicted_probability = predict(model3, newdata = test_data)[,2],
         predictions_point_3 = case_when(predicted_probability >= 0.3 ~ "1",
                                         predicted_probability < 0.3 ~ "0"),
         predictions_point_5 = case_when(predicted_probability >= 0.5 ~ "1",
                                         predicted_probability < 0.5 ~ "0"),
         predictions_point_7 = case_when(predicted_probability >= 0.7 ~ "1",
                                         predicted_probability < 0.7 ~ "0"))
# calculate accuracy at different threshold
dt_accuracy_point_3 <- length(which(test_dt$predictions_point_3 == test_dt$result) == TRUE)/length(test_dt$result)
cat('when the threshold is 0.3, the accuracy of the prediction is', dt_accuracy_point_3)

dt_accuracy_point_5 <- length(which(test_dt$predictions_point_5 == test_dt$result) == TRUE)/length(test_dt$result)
cat('when the threshold is 0.5, the accuracy of the prediction is', dt_accuracy_point_5)

dt_accuracy_point_7 <- length(which(test_dt$predictions_point_7 == test_dt$result) == TRUE)/length(test_dt$result)
cat('when the threshold is 0.7, the accuracy of the prediction is', dt_accuracy_point_7)

# compute the AUC of the model
test_dt_pred <- prediction(test_dt$predicted_probability, test_dt$result)
test_dt_perf <- performance(test_dt_pred, 'auc')
cat('the auc score by using random forest model is ', test_dt_perf@y.values[[1]], "\n")

######################Model Statistic Table#####################################

webshot::install_phantomjs()
knitr::kable(data.frame(model_name = c("logistic regression", "random forest", "decision tree"),
                        accuracy_at_0.3 = c(logit_accuracy_point_3,rf_accuracy_point_3, dt_accuracy_point_3),
                        accuracy_at_0.5 = c(logit_accuracy_point_5,rf_accuracy_point_5, dt_accuracy_point_5), 
                        accuracy_at_0.7 = c(logit_accuracy_point_7,rf_accuracy_point_7, dt_accuracy_point_7),
                        auc_score = c(test_logit_perf@y.values[[1]],test_rf_perf@y.values[[1]],test_dt_perf@y.values[[1]]))) %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  kableExtra::save_kable('figures/model_performance.pdf')

# By comparing the accuracy at different thresholds of the three models, 
# it is clear that the random forest model is the best one.
# By comparing the AUC score of three models, it is also clear that the random forest model is the best one.
# We will use the random forest model to predict the the winning teams. 


