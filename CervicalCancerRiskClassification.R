install.packages("matrixStats")
install.packages("fitdistrplus")
install.packages("nnet")
install.packages("UBL")
install.packages("DMwR")
install.packages("knitr")

install.packages("xgboost")
install.packages("dplyr")
install.packages("caret")
install.packages("e1071")
install.packages("Ckmeans.1d.dp")


library(matrixStats)
library(fitdistrplus)
library(nnet)
library(UBL)
library(knitr)
library(xgboost)
library("dplyr") 
library("caret")
library("e1071")
library("Ckmeans.1d.dp")

setwd("C:/Users/TushaCr7/Desktop/LearningDataScience/Cervical Cancer")




rm(list=ls())

MyData <- read.csv(file="cervical_cancer.csv", header=TRUE)
head(MyData)
colnames(MyData)

sum(is.na(MyData))  #0 null values

##### to check ? character
which(MyData=='?', arr.ind=TRUE)

##### to convert those ? chars to NA
MyData[MyData=="?"]<- NA  #3622 N/A values now

######## to  count NA values in each columns
na_count <-sapply(MyData, function(y) sum(length(which(is.na(y)))))

na_count <- data.frame(na_count)

barplot(na_count$na_count, names= (rownames(na_count))
         ,xlab = "Variable", ylab = "NA Count",
         main = "Total NA per column")

##### to check factor/categorical variables and continuous variables
f <- sapply(MyData, is.factor)
which(f)   #### factor/categorical variables index

mat <- as.matrix(MyData)
mode(mat) = "numeric"
str(mat)




#### replace NA with column medians
mat[, "Number.of.sexual.partners"] <- ifelse(is.na(mat[, "Number.of.sexual.partners"]), median(mat[, "Number.of.sexual.partners"], 
                                                                                               na.rm=TRUE), mat[, "Number.of.sexual.partners"])

mat[, "First.sexual.intercourse"] <- ifelse(is.na(mat[, "First.sexual.intercourse"]), median(mat[, "First.sexual.intercourse"], 
                                                                                             na.rm=TRUE), mat[, "First.sexual.intercourse"])

mat[, "Num.of.pregnancies"] <- ifelse(is.na(mat[, "Num.of.pregnancies"]), median(mat[, "Num.of.pregnancies"], 
                                                                                 na.rm=TRUE), mat[, "Num.of.pregnancies"])

####Replace NA with 0 in "smoke" column as there are 13 NA entries and most of the patients don't smoke
mat[, "Smokes"] <- ifelse(is.na(mat[, "Smokes"]), median(mat[, "Smokes"], 
                                                         na.rm=TRUE), mat[, "Smokes"])

######### In "Smokes..years."  NA should be replaced by 0 as the value of "smoke" column(which was NA before) for 
######### that patient , is 0 now as modified in above step
mat[, "Smokes..years."] <- ifelse(is.na(mat[, "Smokes..years."]), 0, mat[, "Smokes..years."])

###### The above argument applies for  "Smokes..packs.year." as well.
mat[, "Smokes..packs.year."] <- ifelse(is.na(mat[, "Smokes..packs.year."]), 0, mat[, "Smokes..packs.year."])

mat[, "Hormonal.Contraceptives"] <- ifelse(is.na(mat[, "Hormonal.Contraceptives"]), median(mat[, "Hormonal.Contraceptives"], 
                                                                                           na.rm=TRUE), mat[, "Hormonal.Contraceptives"])

###### took mean here..check if median is needed by finding accuracy at last
mat[, "Hormonal.Contraceptives..years."] <- ifelse(is.na(mat[, "Hormonal.Contraceptives..years."]), mean(mat[, "Hormonal.Contraceptives..years."], 
                                                                                                         na.rm=TRUE), mat[, "Hormonal.Contraceptives..years."])
mat[, "IUD"] <- ifelse(is.na(mat[, "IUD"]), median(mat[, "IUD"], na.rm=TRUE), mat[, "IUD"])

mat[, "IUD..years."] <- ifelse(is.na(mat[, "IUD..years."]), median(mat[, "IUD..years."], na.rm=TRUE), mat[, "IUD..years."])


mat[, "STDs"] <- ifelse(is.na(mat[, "STDs"]), median(mat[, "STDs"], na.rm=TRUE), mat[, "STDs"])

#### since median value   of STD is 0 and we filled 0 in place of NA in column "STDs",
####  We can replace NA in next 13 columns with 0 as it's the type of STD the person is having.

mat[, "STDs..number."] <- ifelse(is.na(mat[, "STDs..number."]), 0, mat[, "STDs..number."])
mat[, "STDs.condylomatosis"] <- ifelse(is.na(mat[, "STDs.condylomatosis"]), 0, mat[, "STDs.condylomatosis"])
mat[, "STDs.cervical.condylomatosis"] <- ifelse(is.na(mat[, "STDs.cervical.condylomatosis"]), 0, mat[, "STDs.cervical.condylomatosis"])
mat[, "STDs.vaginal.condylomatosis"] <- ifelse(is.na(mat[, "STDs.vaginal.condylomatosis"]), 0, mat[, "STDs.vaginal.condylomatosis"])
mat[, "STDs.vulvo.perineal.condylomatosis"] <- ifelse(is.na(mat[, "STDs.vulvo.perineal.condylomatosis"]), 0, mat[, "STDs.vulvo.perineal.condylomatosis"])
mat[, "STDs.syphilis"] <- ifelse(is.na(mat[, "STDs.syphilis"]), 0, mat[, "STDs.syphilis"])
mat[, "STDs.pelvic.inflammatory.disease"] <- ifelse(is.na(mat[, "STDs.pelvic.inflammatory.disease"]), 0, mat[, "STDs.pelvic.inflammatory.disease"])
mat[, "STDs.genital.herpes"] <- ifelse(is.na(mat[, "STDs.genital.herpes"]), 0, mat[, "STDs.genital.herpes"])
mat[, "STDs.molluscum.contagiosum"] <- ifelse(is.na(mat[, "STDs.molluscum.contagiosum"]), 0, mat[, "STDs.molluscum.contagiosum"])
mat[, "STDs.AIDS"] <- ifelse(is.na(mat[, "STDs.AIDS"]), 0, mat[, "STDs.AIDS"])
mat[, "STDs.HIV"] <- ifelse(is.na(mat[, "STDs.HIV"]), 0, mat[, "STDs.HIV"])
mat[, "STDs.Hepatitis.B"] <- ifelse(is.na(mat[, "STDs.Hepatitis.B"]), 0, mat[, "STDs.Hepatitis.B"])
mat[, "STDs.HPV"] <- ifelse(is.na(mat[, "STDs.HPV"]), 0, mat[, "STDs.HPV"])


##### "STDs..Time.since.first.diagnosis" column and "STDs..Time.since.last.diagnosis" have NA values
##### only where "STDs..Number.of.diagnosis" column has entries as 0. Hence we can replace NA
##### with 0 for the above two columns
mat[, "STDs..Time.since.first.diagnosis"] <- ifelse(is.na(mat[, "STDs..Time.since.first.diagnosis"]), 0, mat[, "STDs..Time.since.first.diagnosis"])
mat[, "STDs..Time.since.last.diagnosis"] <- ifelse(is.na(mat[, "STDs..Time.since.last.diagnosis"]), 0, mat[, "STDs..Time.since.last.diagnosis"])

##### All NA entries has been replaced. Let's verify
any(is.na(mat))


## code to create last column

data <- as.data.frame(mat)

data$CancerRisk = data$Hinselmann + data$Schiller + data$Citology + data$Biopsy
data$Risk[data$CancerRisk < 1] <- 0   ##### "No risk"
data$Risk[data$CancerRisk >=1  & data$CancerRisk<= 2] <- 1  #####"medium risk"
data$Risk[data$CancerRisk >= 3 & data$CancerRisk<= 4] <- 2  ##### "high risk"


#### removing the colums 
dat <- data[ -c(33,34,35,36,37) ]

#### to write updated data set to new csv
write.csv(dat, "Updated_file.csv")
#dat = read.csv("Updated_file.csv")
#### round is rounding the output of prop.table to 2 digits
round(prop.table(table(dat$Risk)),2)

hist(dat$Risk, ylim = c(0,900),col = 2 , main="Cancer Risk Category", xlab="Category")


###########################################
##### check if the data is balanced
x11()
descdist(dat$Risk, discrete = FALSE)

normal_dist <- fitdist(dat$Risk, "norm")
plot(normal_dist) 
#Oversampling
library(DMwR)

unbalanced<- dat
set.seed(123)


# Make split index
train_index <- sample(1:nrow(unbalanced), nrow(unbalanced)*0.75)

# Full data set
data_variables <- as.matrix(unbalanced[,-33])
data_label <- unbalanced[,"Risk"]

data_matrix <- xgb.DMatrix(data = as.matrix(unbalanced), label = data_label)


# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

numberOfClasses <- length(unique(unbalanced$Risk))


xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 200 # number of XGBoost rounds



#########Train Full Model and Assess Test Set Error
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)


# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))


confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

names <-  colnames(unbalanced[,-33])
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)

x11()
gp = xgb.ggplot.importance(importance_matrix)
print(gp) 






##############################
########## Xg boost with balanced data
#############################
# 3 class oversampling


# 
#BALANCED OVERSAMPLE with weight for each weak classifier package used UBL


C.perc = list('2' = 19, '1' = 12 )
mybalanced <- RandOverClassif(Risk~., unbalanced, C.perc)
table(mybalanced$Risk)
set.seed(131)
train_index <- sample(1:nrow(mybalanced), nrow(mybalanced)*0.75)
# Full data set
data_variables <- as.matrix(mybalanced[,-33])
data_label <- mybalanced[,"Risk"]
data_matrix <- xgb.DMatrix(data = as.matrix(mybalanced), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


numberOfClasses <- length(unique(mybalanced$Risk))

xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 200 # number of XGBoost rounds

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)

# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

names <-  colnames(unbalanced[,-33])
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)

x11()
gp = xgb.ggplot.importance(importance_matrix)
print(gp)
