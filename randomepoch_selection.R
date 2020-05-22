start_time <- Sys.time()

#FOR THE USER:

#This script tests our actual epoch selection methodology by taking ten random series of ten 
#epochs throughout the file, adding ten random REM epochs and training on that data while testing
#on all of the other data in that file. 

ratname <- "cheaptrick"
date <- "110719"
folderfilepath_fft <- '/Users/jacobellen/desktop/Rats/CheapTrick'
folderfilepath_scorefiles <- '/Users/jacobellen/desktop/Rats/CheapTrick'
#Example: '/Users/jacobellen/desktop/Rats/CheapTrick'

#How many iterations do you want to do?
iterations <- 30

#If you want the raw excel file formed to make a different figure, you can put "Yes" below and 
#then the output path to put the csv file in that folder. 
excelfileoutput <- "No"
excelfile_outputpath <- ""

#While this script only takes one file at the moment, in the future we can adjust it to do
#multiple files at once.



#SCRIPT:

#Initializing Vectors to Store Loop Data
rem.prop <- NULL
actual.rem <- NULL
OverallAccuracy <- NULL 
wake.sensitivity <- NULL 
nonrem.sensitivity <- NULL 
rem.sensitivity <- NULL 
wake.specificity <- NULL 
nonrem.specificity <- NULL 
rem.specificity <- NULL 


#Reading in data for histogram of one file
final <- read.csv(paste0(folderfilepath_fft, '/fft',ratname, '_' ,date, '.csv'))
score <- read.csv(paste0(folderfilepath_scorefiles,'/stageinfo_', date, '.csv'))
if ("X" %in% colnames(final)) {
  final <- final %>%
    dplyr::select(-X)
}



#Preprocessing Score File
colnames(score) <- c("Date", "Time", "TimeStamp", "TimeFromStart", "Score", "Delete")
score <- score %>%
  dplyr::select(Date, Time, TimeStamp, TimeFromStart, Score)
score <- score[11:length(score$Score),]
for (i in 1:length(score$Score)) {
  if (score$Score[i]=="1.31E+02" | score$Score[i]=="1.29E+02" | score$Score[i]=="abigailg_0_Numeric" | score$Score[i]=="2.55E+02" | score$Score[i]=="1.30E+02") {
    score$Score[i] <- NA
  }
}
levels(score$Score)[levels(score$Score)=="1.00E+00"] <- 0
levels(score$Score)[levels(score$Score)=="2.00E+00"] <- 1
levels(score$Score)[levels(score$Score)=="3.00E+00"] <- 2
preds <- as.matrix(score$Score)

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


#Starting the Loop
for (m in 1:iterations) {
ml <- as.data.frame(cbind(final, preds))
#putting dataset in random order
ml <- ml[sample(nrow(ml)),]
exclude <- as.vector(which(is.na(ml), arr.ind=TRUE)[,1])
ml <- na.omit(ml)

#Calculating Actual REM
dataset.rem <- (length(which(ml$preds==2)))/(length(ml$preds))

#Finding Random Epoch Procedure
rem <- sample(which(ml$preds==2),15) #finding ten random REM epochs
samp <- c(1:2590)
samp <- samp[-c(exclude)]
for (x in 1:30) {
  sequences <-list()
  rows <- sample(samp, 30)
  for (l in 1:length(rows)) {
    seq1 <- seq(from= rows[l], rows[l]+9)
    sequences[[l]] <- seq1
  }    
  rowvector <- unlist(sequences)
  overall <- c(rowvector
               ,rem
               )
  ml.train <- ml[overall,] #making sure there is some rem in dataset
  test <- ml[rowvector,]
  if (2 %in% test$preds) { #if there's some rem in initial dataset, stop loop and take that data
    break   
  }
}

#Table of Distribution of Classes in this Dataset
print(table(ml.train$preds))

#Setting up Data for Predictions
ml <- as.matrix(ml)
xtrain <- ml.train[1:(nrow(ml.train)),1:(ncol(ml.train)-1)]
xtest <- ml.train[1:(nrow(ml.train)),(ncol(ml.train))]
ml <- ml [-overall,] #predicting on all data that isn't in training dataset
ytrain <- ml[1:nrow(ml),1:(ncol(ml)-1)]
class(ytrain) <- "numeric"
ytest <- ml[1:nrow(ml),ncol(ml)]
xtest = to_categorical(xtest)
ytest = to_categorical(ytest)


#Running Model
input = ncol(xtrain)
xtrain <- as.matrix(xtrain)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 128, activation = "relu", input_shape = c(input), kernel_regularizer = regularizer_l1(.01)) %>%
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
  xtrain, xtest,
  epochs = 15, batch_size = 10, 
  validation_split = 0.18
)
plot(history)

#Extracting Predictions
yp <- predict_classes(model, ytrain) %>% as.vector()
accuracy <- as.data.frame(cbind(ytest,yp))
predilection <- rep(0, length(accuracy$V1))
accuracy <- cbind(accuracy, predilection)
for (k in 1:length(accuracy$V1)) {
  if (accuracy$V1[k]==1) {
    accuracy$predilection[k] <- 0
  }
  if (accuracy$V2[k]==1) {
    accuracy$predilection[k] <- 1
  }
  if (accuracy$V3[k]==1) {
    accuracy$predilection[k] <- 2
  }
}
check <- cbind(accuracy$yp, accuracy$predilection)

#Printing both the confusion matrix for each model
print(confusionMatrix(factor(accuracy$yp), factor(accuracy$predilection)))
conf.table <- confusionMatrix(factor(accuracy$yp), factor(accuracy$predilection))$byClass
vecvalue <- (sum(accuracy$yp ==accuracy$predilection))/length(accuracy$predilection)

#Storing All of the Values in Vectors
OverallAccuracy[m] <- vecvalue
wake.sensitivity[m] <- conf.table[1,1]
nonrem.sensitivity[m] <- conf.table[2,1]
rem.sensitivity[m] <- conf.table[3,1]
wake.specificity[m] <- conf.table[1,2]
nonrem.specificity[m] <- conf.table[2,2]
rem.specificity[m] <- conf.table[3,2]
your.rem <- table(ml.train$preds)[3]/sum(table(ml.train$preds))
rem.prop[m] <- your.rem
}

#Concatenating data into one overall data frame
results <- as.data.frame(cbind(OverallAccuracy,
                                   wake.sensitivity, wake.specificity, nonrem.sensitivity, nonrem.specificity, rem.sensitivity, 
                                   rem.specificity, rem.prop))

#CSV Output
if (excelfileoutput == "Yes") {
  write.csv(results, excelfile_outputpath)
}


#Making Histograms from the Data
results %>%
  ggplot(aes(x=OverallAccuracy)) + geom_histogram(fill="blue", color="black", bins=30) + 
  labs(x="Overall Accuracy", y="Frequency") + theme_bw()

results %>%
  ggplot(aes(x=wake.sensitivity)) + geom_histogram(fill="blue", color="black", bins=30) + labs(x="Wake Sensitivity", y="Frequency") + theme_bw()

results %>%
  ggplot(aes(x=nonrem.sensitivity)) + geom_histogram(fill="blue", color="black", bins=30) + labs(x="NonREM Sensitivity", y="Frequency") + theme_bw()

results %>%
  ggplot(aes(x=rem.sensitivity)) + geom_histogram(fill="blue", color="black", bins=30) + labs(x="REM Sensitivity", y="Frequency") + theme_bw()


#look at how rem proportion affects the rem sensitivity and overall accuracy
results %>%
  ggplot(aes(x=rem.prop, y=rem.sensitivity)) + geom_point() + geom_smooth(method="lm")

results %>%
  ggplot(aes(x=rem.prop, y=OverallAccuracy)) + geom_point() + geom_smooth(method="lm")



#The dashed line is the actual REM, while the histogram represents the proportion of rem in
#our dataset
results %>%
  ggplot(aes(x=rem.prop)) + geom_histogram(fill="blue", color="black", bins=15) + 
  geom_vline(xintercept = dataset.rem, linetype="dashed", color = "dark red", size=2) +
  labs(x="REM Proportion", y="Frequency") + 
  theme_bw() 


end_time <- Sys.time()
time <- end_time - start_time
paste0("Script Finished in ", time)

