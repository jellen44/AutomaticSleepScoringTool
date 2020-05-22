start_time <- Sys.time()

#FOR THE USER

#This script tests the accuracy of 

#It takes in as an input the processed file that comes out of the "preprocessingfft" R script.
#Below, enter the path to the folder that contains these files for a single rat
pathtoprocessedfolder <- ''
pathtostagefilefolder <- ''
#Example Path: '/Users/jacobellen/desktop/Rats/CheapTrick' 
#(for me I have all the cheaptrick files in one large "CheapTrick" folder, so it would be 
#the same for both)
  

#OUTPUT:

#This script produces an output that is the table with accuracy and standard deviation I showed 
#you before (although it actually needs to be in latex so it won't look good here, so if you 
#want the figure let me know and I can produce that table in a pdf).

#If you want the raw excel file formed to make a different figure, you can put "Yes" below and 
#then the output path to put the csv file in that folder. 
excelfileoutput <- "No"
excelfile_outputpath <- ""


#neilyoung
#ratname <- "neilyoung"
#v <- c("111719", "111819","112119","112219","112519", "112619", "112919")

#cheaptrick
ratname <- "cheaptrick"
v <- c("110619", "110719", "111119", "111219", "111319")

#pinkfloyd
#ratname <- "pinkfloyd"
#datevec <- c("101519", "101719", "102319", "102419", "102919", "103019", "103119")


#SCRIPT:

#Initializing Vectors to Fill in the Loop
OverallAccuracy <- NULL 
wake.sensitivity <- NULL 
nonrem.sensitivity <- NULL 
rem.sensitivity <- NULL 
wake.specificity <- NULL 
nonrem.specificity <- NULL 
rem.specificity <- NULL 
vec <- rep(0,length(v))

#Packages
if(!require("edf")) install.packages("edf",repos = "http://cran.us.r-project.org")
if(!require("tidyverse")) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require("keras")) install.packages("keras",repos = "http://cran.us.r-project.org")
if(!require("pracma")) install.packages("pracma",repos = "http://cran.us.r-project.org")
if(!require("xtable")) install.packages("xtable",repos = "http://cran.us.r-project.org")
if(!require("oce")) install.packages("oce",repos = "http://cran.us.r-project.org")
if(!require("entropy")) install.packages("entropy", repos = "http://cran.us.r-project.org")
if(!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require("pander")) install.packages("pander", repos = "http://cran.us.r-project.org")
if(!require("qwraps2")) install.packages("qwraps2", repos = "http://cran.us.r-project.org")
if(!require("caret")) install.packages("caret", repos = "http://cran.us.r-project.org")

library(edf)
library(dplyr)
library(tidyverse)
library(keras)
library(pracma)
library(oce)
library(entropy)
library(xtable)
library(pander)
library(qwraps2)
library(caret)


#Starting Loop
  for (i in 1:length(v)) {
  
    #Reading in the fft file and the stageinfo file for the same rat on the same date
    final <- read.csv(paste0(pathtoprocessedfolder, "/fft",ratname, "_", v[i], ".csv"))
    score <- read.csv(paste0(pathtostagefilefolder, "/stageinfo_", v[i], ".csv"))
    
    if ("X" %in% colnames(final)) {
    final <- final %>%
      dplyr::select(-X)
    }
    
    #Processing Score (stageinfo) File and Extracting the Scoring Data
    colnames(score) <- c("Date", "Time", "TimeStamp", "TimeFromStart", "Score", "Delete")
    score <- score %>%
      dplyr::select(Date, Time, TimeStamp, TimeFromStart, Score)
    score <- score[11:length(score$Score),]
    for (j in 1:length(score$Score)) {
      if (score$Score[j]=="1.31E+02" | score$Score[j]=="1.29E+02" | score$Score[j]=="abigailg_0_Numeric" | score$Score[j]=="2.55E+02" | score$Score[j]=="1.30E+02") {
        score$Score[j] <- NA }
    }
    
    #Changing Factor labels of the Scoring Data (0 is wake, 1 is nonrem, 2 is REM)
    levels(score$Score)[levels(score$Score)=="1.00E+00"] <- 0
    levels(score$Score)[levels(score$Score)=="2.00E+00"] <- 1
    levels(score$Score)[levels(score$Score)=="3.00E+00"] <- 2
    
    #Setting up Training Data for Model
    preds <- as.matrix(score$Score)
    ml <- cbind(final, preds)
    ml <- na.omit(ml)
    ml <- ml[sample(nrow(ml)),] 
    
    #Some New Features that Might Help
    #newfeature <- ml$delta+ml$gamma
    #newfeature2 <- ml$delta+ml$sum
    #newfeature3 <- ml$rat7*abs(ml$rat6)
    #ml <- as.data.frame(cbind(newfeature, newfeature2, newfeature3, ml))
    
    #Splitting up the data into testing and training sets (the training set is 2% of each file)
    xtrain <- ml[1:(.02*nrow(ml)),1:(ncol(ml)-1)]
    ytrain <- ml[(1:(.02*nrow(ml))),(ncol(ml))]
    xtest <- ml[(.02*nrow(ml)):nrow(ml),1:(ncol(ml)-1)]
    ytest <- ml[(.02*nrow(ml)):nrow(ml),ncol(ml)]
    ytrain = to_categorical(ytrain)
    ytest = to_categorical(ytest)
    
  
    #Initializing and Training Model
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
      validation_split = 0.2
    )
    plot(history)
    xtest <- as.matrix(xtest)
    
    #Extraction Predictions 
    yp <- predict_classes(model, xtest) %>% as.vector()
    accuracy <- cbind(ytest,yp)
    accuracy <- as.data.frame(accuracy)
    
    
    #Getting Predictions and Correct Answers in the Same Format to Compare
    predilection <- rep(0, length(accuracy$V1))
    accuracy <- cbind(accuracy, predilection)
    for (q in 1:length(accuracy$V1)) {
      if (accuracy$V1[q]==1) {
        accuracy$predilection[q] <- 0
      }
      if (accuracy$V2[q]==1) {
        accuracy$predilection[q] <- 1
      }
      if (accuracy$V3[q]==1) {
        accuracy$predilection[q] <- 2
      }
    }
    check <- cbind(accuracy$yp, accuracy$predilection)
    

    #Printing both the confusion matrix for each model
    print(confusionMatrix(factor(accuracy$yp), factor(accuracy$predilection)))
    conf.table <- confusionMatrix(factor(accuracy$yp), factor(accuracy$predilection))$byClass
    vecvalue <- (sum(accuracy$yp ==accuracy$predilection))/length(accuracy$predilection)
   
    #Storing All of the Values in Vectors
    OverallAccuracy[i] <- vecvalue
    wake.sensitivity[i] <- conf.table[1,1]
    nonrem.sensitivity[i] <- conf.table[2,1]
    rem.sensitivity[i] <- conf.table[3,1]
    wake.specificity[i] <- conf.table[1,2]
    nonrem.specificity[i] <- conf.table[2,2]
    rem.specificity[i] <- conf.table[3,2]
    
    #Printing overall accuracy
    print(paste0("TOTAL Accuracy is ", vecvalue))
  }

#Saving all of these value
accuracyvec <- as.data.frame(cbind(OverallAccuracy,
  wake.sensitivity, wake.specificity, nonrem.sensitivity, nonrem.specificity, rem.sensitivity, 
  rem.specificity))
  accuracyvec <- na.omit(accuracyvec)
#  vec <- cbind(vec, accuracyvec)

  
#ANALYSIS
#  vec <- vec %>%
#    dplyr::select(-vec)
colnames(vec) <- c("Overall.Accuracy", "Wake.Accuracy", "NonREM.Accuracy", "REM.Accuracy", "Wake.Sensitivity", "Wake.Specificity", "NonREM.Sensitivity", "NonREM.Specificity", "REM.Sensitivity", "REM.Specificity")
Summary.Statistics <- vec


#Creating the Summary Table
our_summary1 <-
  list("Summary Statistics" =
         list("Accuracy" = ~ qwraps2::mean_sd(vec$Overall.Accuracy),
              "Wake Accuracy" = ~ qwraps2::mean_sd(vec$Wake.Accuracy),
              "Non-REM Accuracy" = ~ qwraps2::mean_sd(vec$NonREM.Accuracy),
              "REM Accuracy" = ~ qwraps2::mean_sd(REM.Accuracy),
              "Wake Sensitivty" = ~ qwraps2::mean_sd(vec$Wake.Sensitivity),
              "Non-REM Sensitivity" = ~ qwraps2::mean_sd(vec$NonREM.Sensitivity),
              "REM Sensitivity" = ~ qwraps2::mean_sd(vec$REM.Sensitivity),
              "Wake Specificity" = ~ qwraps2::mean_sd(vec$Wake.Specificity),
              "Non-REM Specificity" = ~ qwraps2::mean_sd(vec$NonREM.Specificity),
              "REM Specificity" = ~ qwraps2::mean_sd(vec$REM.Specificity)
              )
  )
whole <- summary_table(Summary.Statistics, our_summary1)
pander(whole)

#Creating an Excel File of the Accuracies
print(vec)
if (excelfileoutput == "Yes") {
 write.csv(vec, excelfile_outputpath)
}
  
end_time <- Sys.time()
time <- end_time - start_time
paste0("This Script Took ", time)

