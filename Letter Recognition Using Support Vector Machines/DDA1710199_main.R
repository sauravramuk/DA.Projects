library(caret)
library(caTools)
library("kernlab")
library(dplyr)


digits.train <- read.csv("mnist_train.csv")
digits.test <- read.csv("mnist_test.csv")


#There are no NA or non numeric values for the records

colnames(digits.train)[1] <- "Digit"
colnames(digits.test)[1] <- "Digit"


#Function that tries to extract crude 
#features from the raw pixel data. The features
#extracted are very simple, due to lack of knowledge in the domain, 
#computational and time constraints :)

#df : data frame 
#param1 : This is a value to control which pixels will be 
#counted in the function. Higher values means darker pixels 
#will be counted
generateFeatures <- function(df,param1){
  
  #convert the dataframe into a matrix where each row is a datapoint (pixel data of an image) 
  temp.df.mat <- sapply(X = df, FUN = function(x) return(matrix(x, byrow = T, nrow =nrow(df) , ncol = 1)))
  
  #run the function row-wise 
  temp.featureSet <- apply(X = temp.df.mat, 
              MARGIN = 1, 
              FUN = function(x){
                
                #Each row is converted into a 28x28 matrix upon which feature extraction is performed
                temp.mat <- matrix(as.vector(x[2:785]), nrow = 28, ncol = 28, byrow = T)
                
                #this vector records the following: 
                #28 Vertical: count of pixels which have value 
                #higher than the threshold value set  by param1, ROW-WISE
                #28 Horizontal: count of pixels which have value 
                #higher than the threshold value set  by param1, COLUMN-WISE
                #1 for Height
                #1 for Width
                #1 for Height/Width ratio
                x.y.axis.counter <- integer(59)
                
                #this will go through each pixel and record them if their value
                #is higher than the threshold set. (param1)
                for(i in 1:28){
                  for(j in 1:28){
                    if(temp.mat[i,j]>param1){
                      x.y.axis.counter[j] <-x.y.axis.counter[j] + 1
                      x.y.axis.counter[28+i] <- x.y.axis.counter[28 + i] + 1
                    }
                  }
                }
                
                #Find digit width and height
                
                a <- length(x.y.axis.counter)
                b <- a - 56
                
                #Flags that will be set false when the 
                #first non zero value is encountered
                x.beg <- T
                x.end <- T
                y.beg <- T
                y.end <- T
                
                #to store index value when the first non 
                #zero value is encountered
                x.beg.v <- integer(0)
                x.end.v <- integer(0)
                y.beg.v <- integer(0)
                y.end.v <- integer(0)
                
                #this loop will have 4 apporach, one  from left & one from right, 
                #for both the horizontal and the vertical axis
                for(i in 1:14){
                  
                  if(x.beg & x.y.axis.counter[i] > 0){
                    x.beg.v <- i
                    x.beg <- F
                  }
                  
                  if(x.end & x.y.axis.counter[29-i] > 0){
                    x.end.v <- 29-i
                    x.end <- F
                  }
                  
                  if(y.beg & x.y.axis.counter[28 +i] > 0){
                    y.beg.v <- 28 + i
                    y.beg <- F
                  }
                  
                  if(y.end & x.y.axis.counter[a - b - i] > 0){
                    y.end.v <- a-b-i
                    y.end <- F
                  }
                  
                  if(!x.beg & !x.end & !y.beg & !y.end)
                    break()
                  
                  #Critical check: its possible that because of the param1 value
                  #Counts in the "x.y.axis.counter" variable is not even registerd(Cases like
                  #edges of lines especially for the digit 1). When this is encountered the 
                  #value is set as the index value
                  if(i==14){
                    if(x.beg)
                      x.beg.v <- i
                    if(x.end)
                      x.end.v <- i
                    if(y.beg)
                      y.beg.v <- i
                    if(y.end)
                      y.end.v <- i
                  }
                }
                
                #height
                x.y.axis.counter[a-1] <- (y.end.v - y.beg.v)
                
                #Width
                x.y.axis.counter[a-2] <- (x.end.v - x.beg.v)
                
                #Height to Width Ratio
                x.y.axis.counter[a] <- (y.end.v - y.beg.v)/(x.end.v - x.beg.v)
                
                #Return the vector
                return(x.y.axis.counter)
              }
  )
  
  #transpose
  temp.featureSet <- t(temp.featureSet)
  
  #convert to data frame
  final.feature.df <- as.data.frame(temp.featureSet)
  
  #combinig the dependent variable
  final.feature.df <- cbind(Digit = df$Digit, final.feature.df)
  
  #assiginig standard column names
  colnames(final.feature.df) <- c("Digit",
                                  "H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14",
                                  "H15","H16","H17","H18","H19","H20","H21","H22","H23","H24","H25","H26","H27","H28",
                                  "V28","V27","V26","V25","V24","V23","V22","V21","V20","V19","V18","V17","V16","V15",
                                  "V14","V13","V12","V11","V10","V9","V8","V7","V6","V5","V4","V3","V2","V1",
                                  "Width", "Height", "H.W.Ratio")
  
  #return the final df
  return(final.feature.df)
}


#Using the above function we try to extract features from the data sets
train.small <- generateFeatures(digits.train,150)
train.small$Digit <- as.character(train.small$Digit)
test.small <- generateFeatures(digits.test,150)
test.small$Digit <- as.character(test.small$Digit)


#Principal Component analysis to reduce the dimensionality
#using the bas prcomp function
train.small.pca <- prcomp(train.small[c(-1,-2,-30)], scale. = T)

names(train.small.pca)

#the rotation matrix generated 
train.small.pca$rotation

#to see for the first 2 Principal components
biplot(train.small.pca, scale = 0)


#SD
train.small.pca$sdev
var_trn <- train.small.pca$sdev^2
var_trn <- var_trn/sum(var_trn)
var_trn <- var_trn*100


#plot to see what percentage of variance does each principal components explain
plot(var_trn, xlab = "Principal Component",
     ylab = "Percentage of Variance Explained",
     type = "b")

#plot to see the cumulative percentage explained by th PCs
plot(cumsum(var_trn), xlab = "Principal Component",
     ylab = "Cumulative Percentage of Variance Explained",
     type = "b")
#The first 45 PC explains 98.5 % of the variance in the data set
sum(var_trn[1:45])

#We will go ahead and select 45 PCs

#Train data
train.small <- as.data.frame(cbind(Digit = train.small$Digit, train.small.pca$x))
#train.small <- as.data.frame(train.small)
train.small <- train.small[,1:46]

temp. <- as.data.frame(sapply(X = train.small[,-1], FUN = function(x){
  x <- as.numeric(as.character(x))
  return(round(x,6))  
} 
))
train.small <- cbind(Digit = train.small$Digit,  temp.)

#Testdata
test.small1 <- as.data.frame(predict(train.small.pca, newdata = test.small[,c(-1,-2,-30)]))
test.small1 <- test.small1[,1:45]

temp. <- as.data.frame(sapply(X = test.small1, FUN = function(x){
  x <- as.numeric(as.character(x))
  return(round(x,6))  
} 
))
test.small1 <- cbind(Digit = test.small$Digit,  temp.)


#Need to sample a smaller dataset due to computational constraints
set.seed(10)
indices.train <- sample(1:nrow(train.small),  10000)
digit.small.train <- train.small[indices.train,]

set.seed(100)
indices.test <- sample(1:nrow(test.small1),  5000)
digit.small.test <- test.small1[indices.test,]

#Running a radial svm on a smaller data set
model1 <- ksvm(Digit~.,
               data = digit.small.train,
               scaled  = F,
               kernel = "rbfdot"
)
pr <- predict(object = svm.fit, digit.small.test)
conf.mat <- confusionMatrix(pr, digit.small.test$Digit)


#We need to find an optimised C and sigma value
#performing hypertuning and crossvalidation
#creating grid of sigma = 0.01 to 0.1 (10 values)
# C = 1 to 5 (5 values)
#folds = 6
#5000 records

#Need to sample a smaller dataset due to computational constraints
set.seed(10)
indices.train <- sample(1:nrow(train.small),  5000)
digit.small.train <- train.small[indices.train,]

set.seed(100)
indices.test <- sample(1:nrow(test.small1),  5000)
digit.small.test <- test.small1[indices.test,]
metrc <- "Accuracy"
trn.ctrl <- trainControl(method = "cv",number = 6)
tune.grd <- expand.grid(.sigma = seq(0.01, 0.1, 0.01), .C = seq(1,5,1))

svm.fit <- train(Digit~.,
                 data = digit.small.train,
                 method = "svmRadial",
                 tuneGrid = tune.grd,
                 trControl = trn.ctrl,
                 metric = "Accuracy"

)

#getting an overall accuracy of 86%, can be done better
plot(svm.fit)
print(svm.fit)
##RESULT(Best c and sigma):
# sigma  C  Accuracy   Kappa  
# 0.01   5  0.8594031  0.8437216

pr <- predict(object = svm.fit, digit.small.test)
conf.mat <- confusionMatrix(pr, digit.small.test$Digit)


#30000 Records
#Need to sample a smaller dataset due to computational constraints
set.seed(10)
indices.train <- sample(1:nrow(train.small),  30000)
digit.small.train <- train.small[indices.train,]

set.seed(100)
indices.test <- sample(1:nrow(test.small1),  9999)
digit.small.test <- test.small1[indices.test,]



#30000 records,  c = 6,7,8,9, and c = 0.01, 0.02
metrc <- "Accuracy"
trn.ctrl <- trainControl(method = "cv",number = 6)
tune.grd <- expand.grid(.sigma = seq(0.01, 0.02, 0.01), .C = seq(7,9,1))

svm.fit <- train(Digit~.,
                 data = digit.small.train,
                 method = "svmRadial",
                 tuneGrid = tune.grd,
                 trControl = trn.ctrl,
                 metric = "Accuracy"
                 
)

plot(svm.fit)
print(svm.fit)
pr <- predict(object = svm.fit, digit.small.test)
conf.mat <- confusionMatrix(pr, digit.small.test$Digit)

#trying with sigma = 0.01 and C = 9 with the whole datasset
set.seed(10)
indices.train <- sample(1:nrow(train.small),  59999)
digit.small.train <- train.small[indices.train,]

set.seed(100)
indices.test <- sample(1:nrow(test.small1),  9999)
digit.small.test <- test.small1[indices.test,]



#30000 records,  c = 6,7,8,9, and c = 0.01, 0.02
metrc <- "Accuracy"
trn.ctrl <- trainControl(method = "cv",number = 6)
tune.grd <- expand.grid(.sigma = c(0.01), .C = c(9))

svm.fit <- train(Digit~.,
                 data = digit.small.train,
                 method = "svmRadial",
                 tuneGrid = tune.grd,
                 trControl = trn.ctrl,
                 metric = "Accuracy"
                 
)

plot(svm.fit)
print(svm.fit)
pr <- predict(object = svm.fit, digit.small.test)
conf.mat <- confusionMatrix(pr, digit.small.test$Digit)

#using crude feature extraction, Principal Component Analysis and 
#Support vector model an Accuracy of 91.15 % was achieved.
#sigma = 0.01 and C = 9
# Accuracy : 0.9115         
# 95% CI : (0.9058, 0.917)
# No Information Rate : 0.1135         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.9016
