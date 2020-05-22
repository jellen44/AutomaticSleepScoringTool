start_time <- Sys.time()

#FOR THE USER:

#This script looks at the ability of this algorithm to generalize to other files of the same
#rat and of different rats. Specifically, it trains the model on a small subset of one rat's data,
#evaluates the model on that subset, and also evaluates it in making predictions for all of the
#other files of that rat.

#This script outputs a figure showing differences in accuracy between files of the same rat
#and files of other rats.

#FIRST RAT:
#Enter the Rat Name and the dates of the files you would like to use
v <- c("111719", "111819","112119","112219","112519", "112619", "112919")
ratname <- "neilyoung"
pathtoprocessedfolder <- "~/neilyoung/neilyoung"
pathtostagefilefolder <- "~/neilyoung/neilyoung"

#ratname <- "elvis"
#v <- c("102319", "102419", "102819, "102919", "103019", "103119", "110319", "110419")

#SECOND RAT:
#Enter the Second Rat Name and the dates of the files you would like to use
v2 <- c("110619", "110719", #"111119",#"111219",
        "111319")
ratname2 <- "cheaptrick"
pathtoprocessedfolder2 <- '/Users/jacobellen/desktop/Rats/CheapTrick'
pathtostagefilefolder2 <- '/Users/jacobellen/desktop/Rats/CheapTrick'

#If you want the raw excel file formed to make a different figure, you can put "Yes" below and 
#then the output path to put the csv file in that folder. 
excelfileoutput <- "No"
excelfile_outputpath <- ""




#SCRIPT

#Reading in Files of RAT1
final.list <- list()
score.list <- list()
for (g in 1:length(v)) {
  final.list[[g]] <- read.csv(paste0(pathtoprocessedfolder, "/fft", ratname, "_", v[g], ".csv"))
  score.list[[g]] <- read.csv(paste0(pathtostagefilefolder, "/stageinfo_", v[g], ".csv"))
}

#Reading in Files of RAT2
final.list2 <- list()
score.list2 <- list()
for (a in 1:length(v2)) {
  final.list2[[a]] <- read.csv(paste0(pathtoprocessedfolder2, "/fft", ratname2, "_", v2[a], ".csv"))
  score.list2[[a]] <- read.csv(paste0(pathtostagefilefolder2, "/stageinfo_", v2[a], ".csv"))
}

#Initializing Vectors
accuracyvec3 <- NULL 
accuracyvec4 <- NULL 
accuracyvec5 <- NULL 
accuracyvec6 <- NULL 
accuracyvec7 <- NULL 
accuracyvec8 <- NULL 
accuracyvec9 <- NULL 
accuracyvec10 <- NULL 
accuracyvec11 <- NULL 
accuracyvec12 <- NULL 
accuracyvec13 <- NULL 
accuracysamefile <- NULL
vec <- rep(0,length(v))

#Processing Score Files
#Rat 1
for (i in 1:length(score.list)) {
  colnames(score.list[[i]]) <- c("Date", "Time", "TimeStamp", "TimeFromStart", "Score", "Delete")
  score.list[[i]] <- score.list[[i]] %>%
    dplyr::select(Date, Time, TimeStamp, TimeFromStart, Score)
  score.list[[i]] <- score.list[[i]][11:length(score.list[[i]]$Score),]
  for (j in 1:length(score.list[[i]]$Score)) {
    if (score.list[[i]]$Score[j]=="1.31E+02" | score.list[[i]]$Score[j]=="1.29E+02" | score.list[[i]]$Score[j]=="abigailg_0_Numeric" | score.list[[i]]$Score[j]=="2.55E+02" | score.list[[i]]$Score[j]=="1.30E+02") {
      score.list[[i]]$Score[j] <- NA }
  }
  levels(score.list[[i]]$Score)[levels(score.list[[i]]$Score)=="1.00E+00"] <- 0
  levels(score.list[[i]]$Score)[levels(score.list[[i]]$Score)=="2.00E+00"] <- 1
  levels(score.list[[i]]$Score)[levels(score.list[[i]]$Score)=="3.00E+00"] <- 2
}

#Rat 2
for (n in 1:length(score.list2)) {
  colnames(score.list2[[n]]) <- c("Date", "Time", "TimeStamp", "TimeFromStart", "Score", "Delete")
  score.list2[[n]] <- score.list2[[n]] %>%
    dplyr::select(Date, Time, TimeStamp, TimeFromStart, Score)
  score.list2[[n]] <- score.list2[[n]][11:length(score.list2[[n]]$Score),]
  for (q in 1:length(score.list2[[n]]$Score)) {
    if (score.list2[[n]]$Score[q]=="1.31E+02" | score.list2[[n]]$Score[q]=="1.29E+02" | score.list2[[n]]$Score[q]=="abigailg_0_Numeric" | score.list2[[n]]$Score[q]=="2.55E+02" | score.list2[[n]]$Score[q]=="1.30E+02") {
      score.list2[[n]]$Score[q] <- NA }
  }
  levels(score.list2[[n]]$Score)[levels(score.list2[[n]]$Score)=="1.00E+00"] <- 0
  levels(score.list2[[n]]$Score)[levels(score.list2[[n]]$Score)=="2.00E+00"] <- 1
  levels(score.list2[[n]]$Score)[levels(score.list2[[n]]$Score)=="3.00E+00"] <- 2
}

#Packages
if(!require("edf")) install.packages("edf",repos = "http://cran.us.r-project.org")
if(!require("tidyverse")) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require("keras")) install.packages("keras",repos = "http://cran.us.r-project.org")
if(!require("pracma")) install.packages("pracma",repos = "http://cran.us.r-project.org")
if(!require("dplyr")) install.packages("dplyr",repos = "http://cran.us.r-project.org")
if(!require("oce")) install.packages("oce",repos = "http://cran.us.r-project.org")
if(!require("plotrix")) install.packages("plotrix", repos = "http://cran.us.r-project.org")
if(!require("xtable")) install.packages("xtable", repos = "http://cran.us.r-project.org")
if(!require("caret")) install.packages("caret", repos = "http://cran.us.r-project.org")

library(edf)
library(tidyverse)
library(keras)
library(pracma)
library(oce)
library(xtable)
library(caret)
library(plotrix)
library(dplyr)

#Starting the Overall Loop
for (i in 1:length(v)) {
  #Taking one file to train on in each iteration and leaving the rest for testing
  final.use <- as.data.frame(final.list[i])
  score.use <- as.data.frame(score.list[i])
  new.listfinal <- final.list[-i]
  new.listscore <- score.list[-i]
  
  #Forming one large data frame for testing from the lists
  final <- bind_rows(new.listfinal)
 suppressWarnings({score <- bind_rows(new.listscore)})
  
 #Getting rid of rowname column of the processsed data
  if ("X" %in% colnames(final.use)) {
    final.use <- final.use %>%
      dplyr::select(-X)
  }
  if ("X" %in% colnames(final)) {
  final <- final %>%
    dplyr::select(-X)
  }
  
  #predictions and datasets for the training file and the testing files
  preds <- as.matrix(score$Score)
  preds.use <- as.matrix(score.use$Score)
  
  ml <- cbind(final, preds)
  ml <- na.omit(ml)
  ml <- ml[sample(nrow(ml)),]
  
  ml.use <- cbind(final.use, preds.use)
  ml.use <- na.omit(ml.use)
  ml.use <- ml[sample(nrow(ml.use)),]
  
  #Taking the "use" or the training file and splitting it into training and test datasets.
  xtrain <- ml.use[1:(.02*nrow(ml.use)),1:(ncol(ml.use)-1)]
  ytrain <- ml.use[(1:(.02*nrow(ml.use))),(ncol(ml.use))]
  xtest.use <- ml.use[(.1*nrow(ml.use)):nrow(ml.use),1:(ncol(ml.use)-1)]
  ytest.use <- ml.use[(.1*nrow(ml.use)):nrow(ml.use),ncol(ml.use)]
  
  #All the other files go into testing data
  xtest <- ml[1:nrow(ml),1:(ncol(ml)-1)]
  ytest <- ml[1:nrow(ml),ncol(ml)]
  ytrain = to_categorical(ytrain)
  ytest = to_categorical(ytest)
  ytest.use = to_categorical(ytest.use)
  
  #Training the Model
  u = nrow(xtrain)
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
  
  #Extracting Predictions for Same File Testing
  xtest <- as.matrix(xtest)
  xtest.use <- as.matrix(xtest.use)
  yp <- predict_classes(model, xtest.use) %>% as.vector()
  accuracy <- cbind(ytest.use,yp)
  accuracy <- as.data.frame(accuracy)
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
  
  #Finding Overall Accuracy
  accuracy$value <- accuracy$predilection - accuracy$yp
  vecvalue <- round((length(which(accuracy$value==0))/length(accuracy$yp)),4)
  
  #Finding Sensitivity of Each Class
  conf.table <- confusionMatrix(factor(accuracy$yp), factor(accuracy$predilection))$byClass
  wake <- conf.table[1,1]
  wake2 <- conf.table[2,1]
  wake3 <- conf.table[3,1]
  
  #Extracting Predictions for Other Files of Same Rat
  yp.total <- predict_classes(model, xtest) %>% as.vector()
  accuracy.total <- as.data.frame(cbind(ytest,yp.total))

  #Finding Sensitivity of Each Class
  predilection.total <- rep(0, length(accuracy.total$V1))
  accuracy.total <- cbind(accuracy.total, predilection.total)
  for (f in 1:length(accuracy.total$V1)) {
    if (accuracy.total$V1[f]==1) {
      accuracy.total$predilection.total[f] <- 0
    }
    if (accuracy.total$V2[f]==1) {
      accuracy.total$predilection.total[f] <- 1
    }
    if (accuracy.total$V3[f]==1) {
      accuracy.total$predilection.total[f] <- 2
    }
  }

  #Overall Accuracy
  accuracy.total$value.total <- accuracy.total$predilection.total - accuracy.total$yp.total
  vecvalue.total <- round((length(which(accuracy.total$value.total==0))/length(accuracy.total$yp.total)),4)
  
  #Finding Sensitivity of Each Class
  conf.table2 <- confusionMatrix(factor(accuracy.total$yp.total), factor(accuracy.total$predilection.total))$byClass
  wake.total <- conf.table2[1,1]
  wake2.total <- conf.table2[2,1]
  wake3.total <- conf.table2[3,1]
  
  #Storing Values in Vectors
  accuracysamefile[i] <- vecvalue
  accuracyvec3[i] <- wake
  accuracyvec4[i] <- wake2
  accuracyvec5[i] <- wake3
  accuracyvec6[i] <- vecvalue.total
  accuracyvec7[i] <- wake.total
  accuracyvec8[i] <- wake2.total
  accuracyvec9[i] <- wake3.total

  
#HOW DOES THIS MODEL GENERALIZE TO OTHER FILES:
#Repeating much of the procedure above but for cheap trick's files:

  #forming one overall "cheaptrick" dataset
  final.other <- bind_rows(final.list2)
  suppressWarnings({score.other <- bind_rows(score.list2)})
  
  if ("X" %in% colnames(final.other)) {
    final.other <- final.other %>%
      dplyr::select(-X)
  }
  
  #Forming Testing Dataset to Test the Model Above on
  preds.other <- as.matrix(score.other$Score)
  ml.other <- cbind(final.other, preds.other)
  ml.other <- na.omit(ml.other)
  ml.other <- ml.other[sample(nrow(ml.other)),]
  
  #All the other files go into testing data
  xtest.other <- ml.other[1:nrow(ml.other),1:(ncol(ml.other)-1)]
  ytest.other <- ml.other[1:nrow(ml.other),ncol(ml.other)]
  ytest.other = to_categorical(ytest.other)
  
  #Extracting Predictions for Other File Testing
  xtest.other <- as.matrix(xtest.other)
  yp.other <- predict_classes(model, xtest.other) %>% as.vector()
  accuracy.other <- as.data.frame(cbind(ytest.other,yp.other))
  predilection.other <- rep(0, length(accuracy.other$V1))
  accuracy.other <- cbind(accuracy.other, predilection.other)
  for (w in 1:length(accuracy.other$V1)) {
    if (accuracy.other$V1[w]==1) {
      accuracy.other$predilection.other[w] <- 0
    }
    if (accuracy.other$V2[w]==1) {
      accuracy.other$predilection.other[w] <- 1
    }
    if (accuracy.other$V3[w]==1) {
      accuracy.other$predilection.other[w] <- 2
    }
  }

  #Finding Overall Accuracy
  accuracy.other$value.other <- accuracy.other$predilection.other - accuracy.other$yp.other
  vecvalue.other <- round((length(which(accuracy.other$value.other==0))/length(accuracy.other$yp.other)),4)
  
  #Finding Sensitivity of Each Class
  conf.table.other <- confusionMatrix(factor(accuracy.other$yp.other), factor(accuracy.other$predilection.other))$byClass
  wake.other <- conf.table.other[1,1]
  wake2.other <- conf.table.other[2,1]
  wake3.other <- conf.table.other[3,1]
  
  accuracyvec10[i] <- vecvalue.other
  accuracyvec11[i] <- wake.other
  accuracyvec12[i] <- wake2.other
  accuracyvec13[i] <- wake3.other
}


accuracyvec <- as.data.frame(cbind(accuracysamefile, accuracyvec3, accuracyvec4, 
                                   accuracyvec5, accuracyvec6, accuracyvec7, 
                                   accuracyvec8, accuracyvec9, accuracyvec10,
                                   accuracyvec11,accuracyvec12,accuracyvec13))
accuracyvec <- na.omit(accuracyvec)
colnames(accuracyvec) <- c("Accuracy Same File", "Wake Sensitivity Same File",
                           "NonREM Sensitivity Same File", "REM Sensitivity Same File",
                           "Accuracy General", "Wake Sensitivity General",
                           "NonREM Sensitivity General", "REM Sensitivity General",
                           "Accuracy Other File", "Wake Sensitivity Other File", 
                           "NonREM Sensitivity Other File", "REM Sensitivity Other File")

plot.acc <- gather(accuracyvec, key="Accuracy" , value="Types")

plot.acc <- plot.acc %>%
  group_by(Accuracy) %>%
  summarise(Mean.Accuracy = mean(Types), SE = std.error(Types))

Training <- c("SameRat", "OtherRat", "SameFile", "SameRat", "OtherRat", "SameFile",
              "SameRat", "OtherRat", "SameFile","SameRat", "OtherRat", "SameFile")
Class <- c("Overall", "Overall", "Overall", "NonREM", "NonREM", "NonREM",
           "REM", "REM", "REM", "Wake", "Wake", "Wake")
plot.acc <- as.data.frame(cbind(plot.acc, Training, Class))

plot.acc %>%
  ggplot(aes(x=Training, y=Mean.Accuracy, color=Class)) + geom_point() + geom_errorbar(aes(ymax=Mean.Accuracy+SE, ymin=Mean.Accuracy-SE)) +
  facet_wrap(~Class) + theme_classic() + labs(x="Testing Performance", y= "Accuracy")

#CSV Output
if (excelfileoutput == "Yes") {
  write.csv(plot.acc, excelfile_outputpath)
}

end_time <- Sys.time()
time <- end_time - start_time
paste0("Script Finished in ", time, " Minutes")

