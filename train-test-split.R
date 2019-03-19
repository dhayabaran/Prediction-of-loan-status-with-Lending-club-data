library(lubridate)
library(dplyr)
library(randomForest)

#read data
data = read.csv("loan_stat542.csv")
psych::describe(data)
summary(data)

#function for removing the outliers (check)
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#function for getting mode
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#check if na or infinity is present in data and get count 
apply(data, 2, function(x) any(is.na(x)))
apply(data, 2, function(x) any(is.infinite(x)))
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

#histogram to see outliers
hist(data[, "dti"])

#check data types of variables
str(data)

#removing columns
data <- subset(data, select = -c(zip_code, title, emp_title, addr_state, sub_grade))

data <- subset(data, select = -c(zip_code, title, emp_title))


#converting the dependant variable to 0 and 1
data$loan_status = as.factor(as.character(data$loan_status))
data$loan_status <- ifelse(data$loan_status == "Fully Paid", 0, 1)


#keeping Jan 2007 as reference point for earliest_cr_line (god knows why)
data$earliest_cr_line <- lapply(data$earliest_cr_line, function(x) paste('01-', x))
data$earliest_cr_line <- dmy(data$earliest_cr_line)
base_date =rep.int(("01-Jan-07"), 844006)
base_date = dmy(base_date)
cr_line_dates = interval(base_date, data$earliest_cr_line) %/% months(1)
data$earliest_cr_line = cr_line_dates

#replace na

# Modes(data$emp_title)
# levels(data$emp_title) <- c(levels(data$emp_title), "None")
# data$emp_title[is.na(data$emp_title)] <- "None"

Modes(data$emp_length)
levels(data$emp_length) <- c(levels(data$emp_length), "None")
data$emp_length[is.na(data$emp_length)] <- "None"

# Modes(data$title)
# levels(data$title) <- c(levels(data$title), "None")
# data$title[is.na(data$title)] <- "None"

mean(data$dti, na.rm = "True")
median(data$dti, na.rm = "True")
data$dti[is.na(data$dti)] <- 17.5

mean(data$revol_util, na.rm = "True")
median(data$revol_util, na.rm = "True")
data$revol_util[is.na(data$revol_util)] <- 52.8

mean(data$mort_acc, na.rm = "True")
median(data$mort_acc, na.rm = "True")
data$mort_acc[is.na(data$mort_acc)] <- 1

mean(data$pub_rec_bankruptcies, na.rm = "True")
median(data$pub_rec_bankruptcies, na.rm = "True")
data$pub_rec_bankruptcies[is.na(data$pub_rec_bankruptcies)] <- 0


#splitting data
all.test.id = read.csv('Project3_test_id.csv')
test_data = data[data$id %in% all.test.id[,"test1"],]
train_data = data[!data$id %in% all.test.id[,"test1"],]
write.csv(test_data,'test.csv', row.names = FALSE)
write.csv(train_data,'train.csv', row.names = FALSE)

all.test.id = read.csv('Project3_test_id.csv')
test_data = data_onehot[data_onehot$id %in% all.test.id[,"test3"],]
train_data = data_onehot[!data_onehot$id %in% all.test.id[,"test3"],]


#one-hot
fake.y = rep(0, length(data[,1]))
one_hot = model.matrix(~.,data = data)
one_hot = data.frame(one_hot[, -1])  # remove the 1st column (the intercept) of tmp
# write.csv(one_hot,'one_hot.csv')
data_onehot = one_hot


#train data
train <- subset(train_data, select = -c(id))
Y_train = as.matrix(train$loan_status)
X_train = as.matrix(train[,-30])

Y_train = (train$loan_status)
X_train = (train[,-64])

#test data
test <- subset(test_data, select = -c(id))
#test = test_data[,-1]
X_test = test[,-64]
Y_test = test_data$loan_status

#random forest
rf_classifier = randomForest(Y_train ~ ., data=X_train, ntree=50, importance=TRUE)
prediction<- predict(rf_classifier,X_test)
output1 = data.frame(test_data$id, prediction)
colnames(output1) = c('id','Prob')
write.csv(output1,'output1.txt',row.names = FALSE)

#logistic regression
model <- glm(Y_train ~ ., data=X_train, family = binomial, control = list(maxit = 50))

prediction <- predict(model, X_test, type="response")
output1 = data.frame(test_data$id, prediction)
colnames(output1) = c('id','Prob')
write.csv(output1,'output2.txt',row.names = FALSE)


summary(model)

#correlation between variables and sales price
library(lsr)
cor = correlate(data, data$loan_status, test=FALSE, corr.method="pearson", p.adjust.method="holm")

cor



#########################################################################
# log-loss function
logLoss = function(y, p){
  if (length(p) != length(y)){
    stop('Lengths of prediction and labels do not match.')
  }
  
  if (any(p < 0)){
    stop('Negative probability provided.')
  }
  
  p = pmax(pmin(p, 1 - 10^(-15)), 10^(-15))
  mean(ifelse(y == 1, -log(p), -log(1 - p)))
}

#########################################################################

logLoss(Y_test, prediction)

