start_time <- Sys.time()

#FOR THE USER

#Please enter the file paths of the EEG sleep scoring files
#These files should be your csv file of raw EEG data with the datapoints of each sleep epoch 
#being each row.
#The last column for each row should denote either a "0" for Wake, 
#a "1" for Nonrem and a "2" for REM epochs. 
#It is crucial that this column is either the final one in the dataset or it's own csv file (see below)
#Unscored epochs can be left blank within the "Score" column.

#Example of a file path: "/Users/YOURNAME/desktop/YOURFILE.csv"
file_path1_eeg <- "/Users/jacobellen/desktop/eeg1exampledata.csv"
file_path2_eeg <- "/Users/jacobellen/desktop/eeg2exampledata.csv" #NOTE: If not using two eeg files, there is no need to change this line,
#but you must change the eeg_answer argument below to "One."


#File Path of the EMG File
#This file should also be a csv file
file_path_emg <- "/Users/jacobellen/desktop/emgexampledata.csv"

#File Path of the Output CSV File With All of the Predictions
final_outputpath <- "/Users/jacobellen/desktop/finaloutput.csv"



#OPTIONAL TUNING PARAMETERS:

#ONE VS TWO EEGs

#Would you like to use one or two EEGs?
#If the user would only like to use one EEG, replace "Two" with "One" below
eeg_answer <- "Two"


#OPTION TO INPUT EACH EEG AND EMG DATASET AS ONE LONG VECTOR AND SUBMIT SCORING DATA 
#IN A SEPARATE CSV FILE

#If you would like to input the EEG1, EEG2 and EMG datasets as long vector rather than in rows
#and columns (but still in a csv file), you can answer "Yes" below. 

#In this case, you need to put the paths of .csv files that are just one long column for each
#in the EEG1, EEG2 and EMG file path arguments above.
vectorinput <- "No"
numberofdatapointsperepoch <- 1000
#In this case, you need to upload your Scoring file column as a separate file. This file should
#in a .csv and the spreadsheet will just be one column (with some scored epochs, but mostly empty)
scoringcolumn_filepath <- ""



#EXTRACTING PROCESSED FILE

#If you would like to get a copy of the frequency-domain data in csv form, input "Yes" below 
#along with a file name and path to output it.
#There's no to change this section if you don't want the transformed data file
frequency_answer <- "No"
file_output_path <- "/Users/YOURNAME/desktop/spectraldata.csv" #Example file path



#SCRIPT 
#Loading the packages required for this task
if(!require("edf")) install.packages("edf",repos = "http://cran.us.r-project.org")
if(!require("tidyverse")) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require("keras")) install.packages("keras",repos = "http://cran.us.r-project.org")
if(!require("pracma")) install.packages("pracma",repos = "http://cran.us.r-project.org")
if(!require("oce")) install.packages("oce",repos = "http://cran.us.r-project.org")
if(!require("entropy")) install.packages("entropy", repos = "http://cran.us.r-project.org")
if(!require("stringr")) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require("data.table")) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require("tibble")) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(edf)
library(tidyverse)
library(keras)
library(pracma)
library(oce)
library(entropy)
library(stringr)
library(data.table)
library(tibble)

#Performing Spectral Analysis on First EEG Data
eeg1 <- fread(file_path1_eeg)
eeg1 <- as.data.frame(eeg1)
#eeg1 <- eeg1[1:20000,1:600]
if(eeg1[1,1]==1 & eeg1[2,1] ==2) {
  eeg1 <- eeg1[, -1]
}

scoringcolumn <- ""
if (vectorinput=="Yes") {
  eeg1 <- as.matrix(eeg1)
  eeg1 <- t(eeg1)
  num <- length(eeg1)/numberofdatapointsperepoch
  eeg1 <- matrix(eeg1, num, numberofdatapointsperepoch, byrow = T)
  eeg1 <- as.data.frame(eeg1)
  scoringcolumn <- "separate"
}

original.order <- eeg1 %>% mutate(index=(1:nrow(eeg1)))

if (scoringcolumn == "separate") {
  Score <- fread(scoringcolumn_filepath)
  Score <- as.matrix(Score)
  addnas <- nrow(eeg1) - dim(Score)[1]
  addna.vec <- as.matrix(rep(NA, addnas))
  preds <- c(Score, addna.vec)
  eeg1 <- as.data.frame(cbind(eeg1,preds))
}

Score <- eeg1[1:nrow(eeg1), ncol(eeg1)]
eeg1 <- eeg1[1:nrow(eeg1), 1:(ncol(eeg1)-1)]

if (is.data.frame(eeg1)==TRUE) {
eeg1 <- as.matrix(eeg1)
}

fs <- matrix(0, nrow(eeg1), 125)
for (x in 1:nrow(eeg1)) {
  print(paste0("Processing First EEG Data", x))
  spectrum3 <- oce::pwelch(eeg1[x,], points = ncol(eeg1), plot=FALSE)
  length(spectrum3$spec)
  fs[x,(1:length(spectrum3$spec))] <- spectrum3$spec 
}


#Performing Spectral Analysis on Second EEG Data
if (eeg_answer=="Two") {
eeg2 <- fread(file_path2_eeg)
eeg2 <- as.data.frame(eeg2)

if(eeg2[1,1]==1 & eeg2[2,1] ==2) {
  eeg2 <- eeg2[, -1]
}

  if (vectorinput=="Yes") {
    eeg2 <- as.matrix(eeg2)
    eeg2 <- t(eeg2)
    num2 <- length(eeg2)/numberofdatapointsperepoch
    eeg2 <- matrix(eeg2, num, numberofdatapointsperepoch, byrow = T)
    eeg2 <- as.data.frame(eeg2)
  }



if (is.data.frame(eeg2)==TRUE) {
  eeg2 <- as.matrix(eeg2)
}


fs2 <- matrix(0, nrow(eeg2), 125)
for (x in 1:nrow(eeg2)) {
  print(paste0("Processing Second EEG Data", x))
  spectrum3 <- oce::pwelch(eeg2[x,], points = ncol(eeg2), plot=FALSE)
  fs2[x,(1:length(spectrum3$spec))] <- spectrum3$spec 
}
}


# Feature Extraction from EEGs

delta <- as.vector(rowSums(fs[,1:4]))
theta <- as.vector(rowSums(fs[,4:7]))
upper_theta <- as.vector(rowSums(fs[,7:9]))
alpha <- as.vector(rowSums(fs[,8:12]))
beta <- as.vector(rowSums(fs[,13:30]))
lower_gamma <- as.vector(rowSums(fs[,30:50]))
medium_gamma <- as.vector(rowSums(fs[,50:75]))
high_gamma <- as.vector(rowSums(fs[,76:125]))
ratio_betadelta <- (beta/delta)
ratio_betalowergamma <- (beta/lower_gamma)
ratio_betamediumgamma <- (beta/medium_gamma)
ratio_betahighgamma <- (beta/high_gamma)
ratio_thetadelta <- (theta/delta)
ratio_thetagamma <- (theta/medium_gamma)
final <- as.data.frame(cbind(delta, theta,upper_theta,alpha, beta, lower_gamma, medium_gamma, 
                             high_gamma, ratio_betadelta, ratio_betalowergamma, ratio_betahighgamma,
                             ratio_thetadelta, ratio_thetagamma))


if (eeg_answer=="Two") {
delta2 <- as.vector(rowSums(fs2[,1:4]))
theta2 <- as.vector(rowSums(fs2[,4:7]))
upper_theta2 <- as.vector(rowSums(fs2[,7:9]))
alpha2 <- as.vector(rowSums(fs2[,8:12]))
beta2 <- as.vector(rowSums(fs2[,13:30]))
lower_gamma2 <- as.vector(rowSums(fs2[,30:50]))
medium_gamma2 <- as.vector(rowSums(fs2[,50:75]))
high_gamma2 <- as.vector(rowSums(fs2[,77:125]))
ratio_thetagamma2 <- (theta2/medium_gamma2)
ratio_betadelta2 <- (beta2/delta2)
ratio_betalowergamma2 <- (beta2/lower_gamma2)
ratio_betamediumgamma2 <- (beta2/medium_gamma2)
ratio_betahighgamma2 <- (beta2/high_gamma2)
ratio_thetadelta2 <- (theta2/delta2)
final <- as.data.frame(cbind(final, delta2, theta2,upper_theta2,alpha2, beta2, lower_gamma2, 
                    medium_gamma2, high_gamma2, ratio_betadelta2, ratio_betalowergamma2,
                    ratio_betamediumgamma2, ratio_betahighgamma2, ratio_thetadelta2))
}


#Processing EMG Data
emg <- fread(file_path_emg)
emg <- as.data.frame(emg)
if(emg[1,1]==1 & emg[2,1] ==2) {
  emg <- emg[, -1]
}

if (vectorinput=="Yes") {
  emg <- as.matrix(emg)
  emg <- t(emg)
  emg <- length(emg)/numberofdatapointsperepoch
  emg <- matrix(emg, num, numberofdatapointsperepoch, byrow = T)
}

RMS <- as.vector(sqrt(abs(rowSums(emg))))
RMS_power <- as.vector(rowSums(abs(emg)))

RMSplusgamma <- RMS+medium_gamma
RMSpowerplusgamma <- (RMS_power+medium_gamma)

final <- as.data.frame(cbind(final, RMS, RMS_power, RMSplusgamma, RMSpowerplusgamma))

final <- scale(final)

if (frequency_answer=="Yes") {
write.csv(final, file_output_path)
}


#Setting up the Algorithm

ml <- as.data.frame(cbind(final, Score))

if (class(ml$Score)=="numeric") {
  class(ml$Score) <- "character"
}
if (class(ml$Score)=="factor") {
  class(ml$Score) <- "character"
}

ml <- ml %>%
  rownames_to_column('index')

ml.train <- ml %>%
  filter(Score=="0" | Score =="1" | Score=="2") 
ml.train <- ml.train %>% column_to_rownames('index')

ml.test <- ml %>%
  filter(is.na(ml$Score==TRUE))
ml.test <- ml.test %>% column_to_rownames('index')

#Warning produced if less than the training dataset is less than 5% REM or more than 12%
if (length(which(ml.train$Score=="2"))/(length(ml.train$Score)) < .05) {
  print("Warning: you have less than 5% REM in your training data, so you should add more REM to help the algorithm work properly")
}  

ml.train <- as.matrix(ml.train)
ml.test <- as.matrix(ml.test)
ml.train <- ml.train[sample(nrow(ml.train)),]

#Dividing the dataset into "training" and "testing" datasets
xtrain <- ml.train[1:nrow(ml.train), 1:(ncol(ml.train)-1)]
ytrain <- ml.train[1:nrow(ml.train), ncol(ml.train)]

xtest <- ml.test[1:nrow(ml.test), 1:(ncol(ml.test)-1)]

ytrain = to_categorical(ytrain)

if (class(xtrain[1,1])=="character") {
class(xtrain) <- "numeric"
}
if (class(xtest[1,1])=="character") {
  class(xtest) <- "numeric"
}

#Running the Model
xtest <- as.matrix(xtest)
predictions.matrix <- matrix(0, nrow(xtest), 5)

input = ncol(xtrain)
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(input), kernel_regularizer = regularizer_l2(.01)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 3, activation = "softmax")

model %>% compile(
  loss='categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

for (n in 1:5) {
history <- model %>% fit(
  xtrain, ytrain,
  epochs = 12, batch_size = 10, 
  validation_split = 0.18
)
plot(history)

#Extracting Predictions
predictions <- predict_classes(model, xtest) %>% as.vector()
predictions.matrix[,n] <- predictions
}

#Averaging Five Runs of the Model
class(predictions.matrix) <- "character"
mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
predictions.df <- as.data.frame(predictions.matrix)
predictions <- apply(predictions.df, 1, mode)
predictions <- as.numeric(predictions)

#Finalizing Predictions and Formatting the Final Output
predictions.output1 <- as.data.frame(cbind(xtest,predictions))
colnames(predictions.output1)[ncol(predictions.output1)] <- "Score"
suppressWarnings({predictions.outputfinal <- as.data.frame(rbind(ml.train, predictions.output1))})
predictions.outputfinal <- predictions.outputfinal %>% rownames_to_column('index')
predictions.outputfinal <- predictions.outputfinal %>% dplyr::select(c(index, Score))
colnames(predictions.outputfinal)[ncol(predictions.outputfinal)] <- "ModelPredictions"
finaloutput <- merge(original.order, predictions.outputfinal, by ="index")
finaloutput <- finaloutput %>% dplyr::select(-c(index))

#Outputting final CSV File with an additional column with all of the predictions (the last column
#titled "ModelPredictions" and an index column with the number of each epoch.
write.csv(finaloutput, final_outputpath)

end_time <- Sys.time()
time <- end_time - start_time
paste0("Script Finished in ", time, " Minutes")

#FOR ME TO CHECK THAT ITS ACTUALLY WORKING

#check <- as.data.frame(cbind(preds,finaloutput$ModelPredictions))
#check <- na.omit(check)
#check$V1 <- as.character(check$V1)
#check$V1 <- as.numeric(check$V1)
#check$V2 <- as.character(check$V2)
#check$V2 <- as.numeric(check$V2)
#check$V2 <- check$V2-1
#sum(check$V1==check$V2)/(nrow(check))

#check$V2 <- as.factor(check$V2)
#check$V1 <- as.factor(check$V1)
#confusionMatrix(check$V2, check$V1)






