print("The code section below will first install two R packages: ResourceSelection and pROC.") 
print("Please do not move to the next step until the packages are fully installed.") 
print("This will take some time.") 
print("Once the installation is complete, this step will print first 6 rows of the data set.")

# Loading R packages that are needed for some calculations below
install.packages("ResourceSelection")
install.packages("pROC")

# Loading credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
credit_default <- within(credit_default, {
   default <- factor(default)
   sex <- factor(sex)
   education <- factor(education)
   marriage <- factor(marriage)
   assets <- factor(assets)
   missed_payment <- factor(missed_payment)
})

print("installation completed!")

# Print the number of rows and columns within the dataset
print(paste0("Records: ", nrow(credit_default)))
print(paste0("Fields: ", ncol(credit_default)))
print("data set (first 6 observations)")
head(credit_default, 6)


# ================ model1 analysis ================ #


# Create the complete logistic regression model
logit <- glm(default ~ credit_utilize + education , data = credit_default, family = "binomial")
summary(logit)

# Print percentage increase in defaulting odds based on each 
# unit increase in credit usage within a static model
print(paste0("Percentage increase in defaulting odds: ", round(exp(0.343869)-1, 4)))


# +------------------------Testing the model1 fit------------------------+ #
# Generate Confusion matrix for the logit model
print("Prediction: education is high school (education='1'), credit utilization is 40% (credit_utilize=0.40)")
newdata1 <- data.frame(education="1", credit_utilize=0.40)
pred1 <- predict(logit, newdata1, type='response')
round(pred1, 4)

print("Prediction: education is postgraduate (education='3'), credit utilization is 35% (credit_utilize=0.35)")
newdata2 <- data.frame(education="3", credit_utilize=0.35)
pred2 <- predict(logit, newdata2, type='response')
round(pred2, 4)

# ============================= #

# Wald confidence interval for logit
conf_int <- confint.default(logit, level=0.95)
round(conf_int,4)

# ============================= #

# Goodness of fit test for the logit model
library(ResourceSelection)
print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit$y, fitted(logit), g=50)
hl

# ============================= #

# Creating Confusion matrix for the logit model performance
# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('education', 'credit_utilize')]
pred <- predict(logit, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

# Print the Acurracy, Precision, and Recal of the model based on the 
# confusion matrix' binary counts
TN = 254
FP = 22
FN = 21
TP = 303
print(paste0("Model Accuracy: ", round((TP+TN)/(TP+TN+FP+FN), 4)))
print(paste0("Model Precision: ", round(TP/(TP+FP), 4)))
print(paste0("Model Recall: ", round(TP/(TP+FN), 4)))

# ============================= #

# Generate ROC curve for logit values
library(pROC)
labels <- credit_default$default
predictions <- logit$fitted.values
roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

# +---------------------End of testing the model1 fit---------------------+ #


# Utilizing logit prediction model within scenarios
print("Prediction: education is high school (education='1'), credit utilization is 40% (credit_utilize=0.40)")
newdata1 <- data.frame(education="1", credit_utilize=0.40)
pred1 <- predict(logit, newdata1, type='response')
round(pred1, 4)

print("Prediction: education is postgraduate (education='3'), credit utilization is 35% (credit_utilize=0.35)")
newdata2 <- data.frame(education="3", credit_utilize=0.35)
pred2 <- predict(logit, newdata2, type='response')
round(pred2, 4)


# ================ model2 analysis ================ #


# Create the second complete logistic regression model
logit2 <- glm(default ~ credit_utilize + assets + missed_payment, data = credit_default, family = "binomial")
summary(logit2)


# +----------------------Testing the model2 fit----------------------------+ #
# Generate Confusion matrix for the logit2 model
# Predict default or no_default for the data set using the model
default_model_data2 <- credit_default[c('assets', 'missed_payment', 'credit_utilize')]
pred2 <- predict(logit2, newdata=default_model_data2, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0') 
depvar_pred2 = as.factor(ifelse(pred2 >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix2 <- table(credit_default$default, depvar_pred2)[c('0','1'),c('0','1')]
rownames(conf.matrix2) <- paste("Actual", rownames(conf.matrix2), sep = ": default=")
colnames(conf.matrix2) <- paste("Prediction", colnames(conf.matrix2), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix2,justify="centre",digit=2)

# Print the Acurracy, Precision, and Recal of the model based on the 
# confusion matrix' binary counts
TN = 262
FP = 14
FN = 21
TP = 303
print(paste0("Model Accuracy: ", round((TP+TN)/(TP+TN+FP+FN), 4)))
print(paste0("Model Precision: ", round(TP/(TP+FP), 4)))
print(paste0("Model Recall: ", round(TP/(TP+FN), 4)))

# ============================= #

# Goodness of fit test
library(ResourceSelection)
print("Hosmer-Lemeshow Goodness of Fit Test")
h2 = hoslem.test(logit2$y, fitted(logit2), g=50)
h2

# ============================= #

# Wald confidence interval test
conf_int2 <- confint.default(logit2, level=0.95)
round(conf_int2,4)

# ============================= #

# ROC curve
library(pROC)
labels2 <- credit_default$default
predictions2 <- logit2$fitted.values
roc2 <- roc(labels2 ~ predictions2)

print("Area Under the Curve (AUC)")
round(auc(roc2),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc2, legacy.axes = TRUE)

# +---------------------End of testing the model2 fit---------------------+ #


# Utilizing logit2 prediction model within scenarios
print("Prediction: asset is only car, missed a payment, & credit utilization is 35% (credit_utilize=0.35)")
newdata3 <- data.frame(assets='1', missed_payment='1', credit_utilize=0.35)
pred3 <- predict(logit2, newdata2, type='response')
round(pred3, 4)

print("Prediction: asset is car and house, did not miss a payment, & credit utilization is 35% (credit_utilize=0.35)")
newdata4 <- data.frame(assets='3', missed_payment='0', credit_utilize=0.35)
pred4 <- predict(logit2, newdata4, type='response')
round(pred4, 4)
