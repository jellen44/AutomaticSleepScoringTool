#FOR USERS:

#This script explores the probabilities of each predictions and creates a few different figures
#showing differences in model probabilities between classes. There is also a part at the
#end that looks at the class distribution of uncertain epochs (all probabilities lower than
#60%). A rescoring system that extracts and treats these uncertain epochs differently is something
#I can look into it at some point.

#Enter a ratname and a date of the file:
ratname <- "cheaptrick"
date <- "110719"

#Enter the paths to the folders containing the processed file and the scored file:
folderfilepath_fft <- '/Users/jacobellen/desktop/Rats/CheapTrick'
folderfilepath_scorefiles <- '/Users/jacobellen/desktop/Rats/CheapTrick'
#Example: '/Users/jacobellen/desktop/Rats/CheapTrick'

#If you want the raw excel file formed to make a different figure, you can put "Yes" below and 
#then the output path to put the csv file in that folder. 
excelfileoutput <- "No"
excelfile_outputpath <- ""



#SCRIPT

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

#Reading in Data
final <- read.csv(paste0(folderfilepath_fft, '/fft',ratname, '_' ,date, '.csv'))
score <- read.csv(paste0(folderfilepath_scorefiles,'/stageinfo_', date, '.csv'))

if ("X" %in% colnames(final)) {
  final <- final %>%
    dplyr::select(-X)
}


#Processing Score Files
colnames(score) <- c("Date", "Time", "TimeStamp", "TimeFromStart", "Score", "Delete")
score <- score %>%
  select(Date, Time, TimeStamp, TimeFromStart, Score)
score <- score[11:length(score$Score),]

print(length(score$Score))
for (i in 1:length(score$Score)) {
  if (score$Score[i]=="1.31E+02" | score$Score[i]=="1.29E+02" | score$Score[i]=="abigailg_0_Numeric" | score$Score[i]=="2.55E+02" | score$Score[i]=="1.30E+02") {
    score$Score[i] <- NA
  }
}
levels(score$Score)[levels(score$Score)=="1.00E+00"] <- 0
levels(score$Score)[levels(score$Score)=="2.00E+00"] <- 1
levels(score$Score)[levels(score$Score)=="3.00E+00"] <- 2
preds <- as.matrix(score$Score)

#Putting Togethe Dataset
ml <- cbind(final, preds)
ml <- na.omit(ml)
ml <- ml[sample(nrow(ml)),]
xtrain <- ml[1:(.1*nrow(ml)),1:(ncol(ml)-1)]
ytrain <- ml[(1:(.1*nrow(ml))),(ncol(ml))]
xtest <- ml[(.1*nrow(ml)):nrow(ml),1:(ncol(ml)-1)]
ytest <- ml[(.1*nrow(ml)):nrow(ml),ncol(ml)]
ytrain = to_categorical(ytrain)
ytest = to_categorical(ytest)

#Training Model
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

#Extracting the Predictions and the Prediction Probabilities 
xtest <- as.matrix(xtest)
yp <- predict_classes(model, xtest) %>% as.vector()
yp2 <- predict_proba(model, xtest)
accuracy <- as.data.frame(cbind(ytest,yp, yp2))

predilection <- rep(0, length(accuracy$V1))
accuracy <- cbind(accuracy, predilection)
for (i in 1:length(accuracy$V1)) {
  if (accuracy$V1[i]==1) {
    accuracy$predilection[i] <- 0
  }
  if (accuracy$V2[i]==1) {
    accuracy$predilection[i] <- 1
  }
  if (accuracy$V3[i]==1) {
    accuracy$predilection[i] <- 2
  }
}
check <- cbind(accuracy$yp, accuracy$V1, accuracy$V2, accuracy$V3, accuracy$predilection)

#Getting Confusion Matrix and Overall Accuracy
conf.table <- confusionMatrix(factor(accuracy$yp), factor(accuracy$predilection))$byClass
vecvalue <- (sum(accuracy$yp ==accuracy$predilection))/length(accuracy$predilection)
Accuracy <- vecvalue


#Figure of Probabilities when the answer is actually REM
probs <- accuracy %>%
  dplyr::select(c(V5, V6, V7, predilection))
remprobs <- probs %>%
  filter(predilection==2) %>%
  summarise_all(funs(mean, sd))
remprobs <- as.data.frame(t(remprobs))
remprobs <- as.data.frame(cbind(remprobs, rownames(remprobs)))
colnames(remprobs) <- c("Mean_Probability", "Category")
final.rem <- cbind(remprobs[1:3,1], remprobs[5:7,1])
Category <- c("Wake", "NonREM", "REM")
final.rem <- as.data.frame(cbind(final.rem, Category))
colnames(final.rem)  <- c("Mean_Probability", "SD", "Category") 
final.rem$Mean_Probability <- as.character(final.rem$Mean_Probability)
final.rem$Mean_Probability <- as.numeric(final.rem$Mean_Probability)
final.rem$SD <- as.character(final.rem$SD)
final.rem$SD <- as.numeric(final.rem$SD)

final.rem %>%
  ggplot(aes(x= reorder(Category, Mean_Probability), y= Mean_Probability)) + geom_col(color="black", fill="blue") +
  geom_errorbar(aes(ymin=Mean_Probability- SD, ymax=Mean_Probability+SD)) + labs(title="REM Epochs") +
  labs(x="Sleep Stage", y="Mean Probability Predictions Each Class") + theme_bw()


#Figure of Probabilities when the answer is actually nonREM
probs <- accuracy %>%
  dplyr::select(c(V5, V6, V7, predilection))
remprobs <- probs %>%
  filter(predilection==1) %>%
  summarise_all(funs(mean, sd))
remprobs <- as.data.frame(t(remprobs))
remprobs <- as.data.frame(cbind(remprobs, rownames(remprobs)))
colnames(remprobs) <- c("Mean_Probability", "Category")

final.rem <- cbind(remprobs[1:3,1], remprobs[5:7,1])
Category <- c("Wake", "NonREM", "REM")
final.rem <- as.data.frame(cbind(final.rem, Category))
colnames(final.rem)  <- c("Mean_Probability", "SD", "Category") 
final.rem$Mean_Probability <- as.character(final.rem$Mean_Probability)
final.rem$Mean_Probability <- as.numeric(final.rem$Mean_Probability)
final.rem$SD <- as.character(final.rem$SD)
final.rem$SD <- as.numeric(final.rem$SD)

final.rem %>%
  ggplot(aes(x= reorder(Category, Mean_Probability), y= Mean_Probability)) + geom_col(color="black", fill="blue") +
  geom_errorbar(aes(ymin=Mean_Probability- SD, ymax=Mean_Probability+SD)) + labs(title="nonREM Epochs") +
  labs(x="Sleep Stage", y="Mean Probability Predictions Each Class") + theme_bw()

#Figure of Probabilities when the answer is actually Wake
remprobs <- accuracy %>%
  filter(predilection==2) %>%
  summarise(meanwake = mean(V5), meannonrem = mean(V6), meanrem = mean(V7)) 

probs <- accuracy %>%
  dplyr::select(c(V5, V6, V7, predilection))
remprobs <- probs %>%
  filter(predilection==0) %>%
  summarise_all(funs(mean, sd))
remprobs <- as.data.frame(t(remprobs))
remprobs <- as.data.frame(cbind(remprobs, rownames(remprobs)))
colnames(remprobs) <- c("Mean_Probability", "Category")

final.rem <- cbind(remprobs[1:3,1], remprobs[5:7,1])
Category <- c("Wake", "NonREM", "REM")
final.rem <- as.data.frame(cbind(final.rem, Category))
colnames(final.rem)  <- c("Mean_Probability", "SD", "Category") 
final.rem$Mean_Probability <- as.character(final.rem$Mean_Probability)
final.rem$Mean_Probability <- as.numeric(final.rem$Mean_Probability)
final.rem$SD <- as.character(final.rem$SD)
final.rem$SD <- as.numeric(final.rem$SD)

final.rem %>%
  ggplot(aes(x= reorder(Category, Mean_Probability), y= Mean_Probability)) + geom_col(color="black", fill="blue") +
  geom_errorbar(aes(ymin=Mean_Probability- SD, ymax=Mean_Probability+SD)) + labs(title="Wake Epochs") +
  labs(x="Sleep Stage", y="Mean Probability Predictions Each Class") + theme_bw()


#Looking at the Class Distribution of Uncertain Epochs:
lowconf <- rescore %>%
  filter(V5 < .6 & V6 < .6 & V7 < .6)

score$Score <- as.factor(score$Score)

lowconf <- lowconf %>%
  group_by(predilection) %>%
  summarise(Frequency = n()) 

levels(lowconf$predilection)[levels(lowconf$predilection)=="0"] <- "Wake"
levels(lowconf$predilection)[levels(lowconf$predilection)=="1"] <- "NonREM"
levels(lowconf$predilection)[levels(lowconf$predilection)=="2"] <- "REM"

lowconf %>%
  ggplot(aes(x=reorder(predilection, Frequency), y=Frequency)) + geom_col(fill="blue", color="black") +
  labs(x="Classes", title="Class Distribution of 'Uncertain' Epochs") + theme_bw()


#TRYING A RESCORING METHOD: Quickly tried to see if just making all of the uncertain epochs REM
#would help, but it didn't really. A rescoring procedure for uncertain epochs
#is something to look into going forward.

rescore <- accuracy %>%
  dplyr::select(c(V5, V6, V7, predilection, yp))

for (i in 1:nrow(rescore)) {
  if (rescore$V5[i] < .6 & rescore$V6[i] < .6 & rescore$V7[i] < .6) {
   rescore$yp[i] <- 2 #MAKING ALL EPOCHS WITH NO PROBABILITY OVER .6 FOR ANY CLASS "REM"
  }
}

#ACCURACY BEFORE RESCORE
100*with(accuracy, sum(accuracy$predilection==accuracy$yp))/nrow(accuracy)
#ACCURACY WITH RESCORE
100*with(rescore, sum(rescore$predilection==rescore$yp))/nrow(rescore)

#CSV Output
if (excelfileoutput == "Yes") {
  probs.output <- accuracy %>%
    dplyr::select(c(V5, V6, V7, predilection)) 
  colnames(probs.output) <- c("Wake Probability", "NonREM Probability", "REM Probability", "Actual Answer")
  write.csv(probs.output, excelfile_outputpath)
}

end_time <- Sys.time()
time <- end_time - start_time
paste0("Script Finished in ", time)



