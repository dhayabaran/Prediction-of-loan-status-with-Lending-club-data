# 
# #function for removing the outliers (check)
# remove_outliers <- function(x, na.rm = TRUE, ...) {
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#   H <- 1.5 * IQR(x, na.rm = na.rm)
#   y <- x
#   y[x < (qnt[1] - H)] <- NA
#   y[x > (qnt[2] + H)] <- NA
#   y
# }


# read in data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$loan_status <- ifelse(train$loan_status == "Fully Paid", 0, 1)
test = data.frame(test, loan_status = rep(0,nrow(test)))
data <- rbind(train, test)

#keeping Jan 2007 as reference point for earliest_cr_line
data$earliest_cr_line <- lapply(data$earliest_cr_line, function(x) paste('01-', x))
data$earliest_cr_line <- dmy(data$earliest_cr_line)
base_date =rep.int(("01-Jan-07"), 844006)
base_date = dmy(base_date)
cr_line_dates = interval(base_date, data$earliest_cr_line) %/% months(1)
data$earliest_cr_line = cr_line_dates

#replace na
levels(data$emp_length) <- c(levels(data$emp_length), "None")
data$emp_length[is.na(data$emp_length)] <- "None"

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

data$annual_inc = log(1 + data$annual_inc)
data$revol_bal = log(1 + data$revol_bal)
data$dti[data$dti==-1] = 0
data$dti = log(1 + data$dti)
data$revol_util = log(1 + data$revol_util)
data$pub_rec_bankruptcies = log(1 + data$pub_rec_bankruptcies)
data$mort_acc = log(1 + data$mort_acc)
data$open_acc = log(1 + data$open_acc)
data$fico_range_low = log(1 + data$fico_range_low)
data$fico_range_high = log(1 + data$fico_range_high)


#remove columns
data <- subset(data, select = -c(zip_code, title, emp_title))

#one-hot
fake.y = rep(0, length(data[,1]))
one_hot = model.matrix(~.,data = data)
one_hot = data.frame(one_hot[, -1])  # remove the 1st column (the intercept) of tmp
data_onehot = one_hot

#split
train_data = data_onehot[1:nrow(train),]
test_data = data_onehot[nrow(train)+1:nrow(test), names(data_onehot) != "loan_status"]

#train data
train <- subset(train_data, select = -c(id))
Y_train = (train$loan_status)

#test data
test <- subset(test_data, select = -c(id))
X_test = test


#logistic regression
model <- glm(loan_status ~ ., data=train, family = binomial)
prediction <- predict(model, X_test, type="response")
output1 = data.frame(test_data$id, prediction)
colnames(output1) = c('id','prob')
write.csv(output1,'mysubmission1.txt',row.names = FALSE)


# #########################################################################
# # log-loss function
# logLoss = function(y, p){
#   if (length(p) != length(y)){
#     stop('Lengths of prediction and labels do not match.')
#   }
# 
#   if (any(p < 0)){
#     stop('Negative probability provided.')
#   }
# 
#   p = pmax(pmin(p, 1 - 10^(-15)), 10^(-15))
#   mean(ifelse(y == 1, -log(p), -log(1 - p)))
# }
# 
# #########################################################################
# 
# logLoss(Y_test, prediction)
