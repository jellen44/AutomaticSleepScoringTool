start_time <- Sys.time()

#FOR THE USER:

#This script aims to show that the accuracy levels of this model do not change dramatically
#when using very large percentages of the dataset to train versus using very small subsets.

#It takes in processed files and scoring files and runs them with different percentages of
#training data. It outputs a figure that shows the REM, non-REM and Wake sensitivity compared
#with different amounts of training data.

#The script can use all of the files of one rat by looping through each file and testing the file
#with different amounts of training data before moving onto the next file.

#Enter rat name and the dates of the files you want to train below.
#neilyoung
#ratname <- "neilyoung"
#v <- c("111719", "111819","112119","112219","112519", "112619", "112919")

#cheaptrick
ratname <- "cheaptrick"
v <- c("110619", "110719", 
      "111119", 
       "111219", "111319")

#pinkfloyd
#ratname <- "pinkfloyd"
#datevec <- c("101519", "101719", "102319", "102419", "102919", "103019", "103119")


#Below, enter the path to the folder that contains these files for a single rat
pathtoprocessedfolder <- '/Users/jacobellen/desktop/Rats/CheapTrick'
pathtostagefilefolder <- '/Users/jacobellen/desktop/Rats/CheapTrick'
#Example Path: '/Users/jacobellen/desktop/Rats/CheapTrick' 

#If you want the raw excel file formed to make a different figure, you can put "Yes" below and 
#then the output path to put the csv file in that folder. 
excelfileoutput <- "No"
excelfile_outputpath <- ""

#This is the vector of what percentages of the data you would like to use to train, which you
#can adjust as you see fit.
num <- c(
  #.7, .6, 
  .5,.05, .01) #from 70 to .5%



#SCRIPT

#Initializing Vectors
accuracyvec1 <- NULL
accuracyvec2 <- NULL 
accuracyvec3 <- NULL 
accuracyvec4 <- NULL 
accuracyvec5 <- NULL 
vec <- rep(0,length(v))

#Packages
if(!require("edf")) install.packages("edf",repos = "http://cran.us.r-project.org")
if(!require("tidyverse")) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require("keras")) install.packages("keras",repos = "http://cran.us.r-project.org")
if(!require("pracma")) install.packages("pracma",repos = "http://cran.us.r-project.org")
if(!require("oce")) install.packages("oce",repos = "http://cran.us.r-project.org")
if(!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require("caret")) install.packages("caret", repos = "http://cran.us.r-project.org")

library(edf)
library(dplyr)
library(tidyverse)
library(keras)
library(pracma)
library(oce)
library(caret)

#Starting two loops. One loops through each file and the other loops through each data percentage.
for (j in num) {
for (i in 1:length(v)) {
  
#Loading in data
final <- read.csv(paste0(pathtoprocessedfolder, "/fft",ratname, "_", v[i], ".csv"))
score <- read.csv(paste0(pathtostagefilefolder, "/stageinfo_", v[i], ".csv"))

if ("X" %in% colnames(final)) {
final <- final %>%
      dplyr::select(-X)
}

  
#Setting up the Score Files
colnames(score) <- c("Date", "Time", "TimeStamp", "TimeFromStart", "Score", "Delete")
score <- score %>%
  dplyr::select(Date, Time, TimeStamp, TimeFromStart, Score)
score <- score[11:length(score$Score),]
for (c in 1:length(score$Score)) {
  if (score$Score[c]=="1.31E+02" | score$Score[c]=="1.29E+02" | score$Score[c]=="abigailg_0_Numeric" | score$Score[c]=="2.55E+02" | score$Score[c]=="1.30E+02") {
    score$Score[c] <- NA
  }
}
levels(score$Score)[levels(score$Score)=="1.00E+00"] <- 0
levels(score$Score)[levels(score$Score)=="2.00E+00"] <- 1
levels(score$Score)[levels(score$Score)=="3.00E+00"] <- 2
preds <- as.matrix(score$Score)

#Creating Dataset for Training (splits the data by the "num" variable)
ml <- cbind(final, preds)
ml <- na.omit(ml)
ml <- ml[sample(nrow(ml)),]
xtrain <- ml[1:(j*nrow(ml)),1:(ncol(ml)-1)]
ytrain <- ml[(1:(j*nrow(ml))),(ncol(ml))]
xtest <- ml[(j*nrow(ml)):nrow(ml),1:(ncol(ml)-1)]
ytest <- ml[(j*nrow(ml)):nrow(ml),ncol(ml)]
ytrain = to_categorical(ytrain)
ytest = to_categorical(ytest)

#Running Model
input = ncol(xtrain)
xtrain <- as.matrix(xtrain)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(input), kernel_regularizer = regularizer_l2(.01)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax")

model %>% compile(
  loss='categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  xtrain, ytrain,
  epochs = 12, batch_size = 10, 
  validation_split = 0.18
)
plot(history)
xtest <- as.matrix(xtest)

#Extracting Predictions
yp <- predict_classes(model, xtest) %>% as.vector()
accuracy <- as.data.frame(cbind(ytest,yp))
predilection <- rep(0, length(accuracy$V1))
accuracy <- cbind(accuracy, predilection)
for (b in 1:length(accuracy$V1)) {
  if (accuracy$V1[b]==1) {
    accuracy$predilection[b] <- 0
  }
  if (accuracy$V2[b]==1) {
    accuracy$predilection[b] <- 1
  }
  if (accuracy$V3[b]==1) {
    accuracy$predilection[b] <- 2
  }
}
check <- cbind(accuracy$yp, accuracy$predilection)

#Overall Accuracy
vecvalue <- (sum(accuracy$yp==accuracy$predilection))/length(accuracy$predilection)

#Calculating Sensitivity for Each (Overall Accuracy for Each Class)
print(confusionMatrix(factor(accuracy$yp), factor(accuracy$predilection)))
conf.table <- confusionMatrix(factor(accuracy$yp), factor(accuracy$predilection))$byClass
vecvalue <- (sum(accuracy$yp ==accuracy$predilection))/length(accuracy$predilection)

#Storing All of the Values in Vectors
accuracyvec2[i] <- vecvalue
accuracyvec3[i] <- conf.table[1,1]
accuracyvec4[i] <- conf.table[2,1]
accuracyvec5[i] <- conf.table[3,1]
paste0("TOTAL Accuracy is ", vecvalue)
}
accuracyvec <- as.data.frame(cbind(accuracyvec2, accuracyvec3, accuracyvec4, accuracyvec5))
accuracyvec <- na.omit(accuracyvec)
vec <- cbind(vec, accuracyvec)
}


#Data Wranging (this is messy - sorry about that)

#Generating Column Names
filenum <- length(v)
seq1 <- seq(filenum)
initialstring <- c("Place")
for (g in num) {
  a <- paste0("Accuracy.", g)
  b <- paste0("Wake.", g)
  c <-paste0("NonREM.",g)
  d <- paste0("REM.", g)
  initialstring <- c(initialstring, a,b,c,d)

}
colnames(vec) <- initialstring
lowsample <- vec
lowsample <- lowsample %>%
  dplyr::select(-c(Place))
lowsample.gather <- gather(lowsample, key= "Level", value="Accuracy")


lowsample.gather <- lowsample.gather %>%
  group_by(Level) %>%
  summarise(Accuracy = mean(Accuracy))

a <- rep("Accuracy", length(num))
d <- rep("Wake", length(num))
b <- rep("Non-REM", length(num))
c <- rep("REM", length(num))
vec2 <- c(a,b,c,d)

l <- rev(num)*100
vec.new <- rep(l,4)
lowsample.means.g <- as.data.frame(cbind(lowsample.gather, vec.new))
lowsample.means.g <- as.data.frame(cbind(lowsample.means.g, vec2))

lowsample.means.g %>%
  ggplot(aes(x=factor(vec.new), y=Accuracy, color=vec2)) + geom_point() + facet_wrap(~factor(vec2)) +
  labs(x="Percentage of 24-Hour File Trained On", y= "Proportion Correct") +  theme(legend.position = "none")

#CSV Output
if (excelfileoutput == "Yes") {
  write.csv(lowsample.means.g, excelfile_outputpath)
}

end_time <- Sys.time()
time <- end_time - start_time
paste0("Script Finished in ", time)

