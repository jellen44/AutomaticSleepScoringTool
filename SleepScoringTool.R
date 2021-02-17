
#MANDATORY FOR THE USER

#Please enter the file paths of the EEG sleep scoring files
#These files should be your csv file of raw EEG data with the datapoints of each sleep epoch 
#being each row.
#The last column for each row should denote either a "0" for Wake, 
#a "1" for Nonrem and a "2" for REM epochs. 
#It is crucial that this column is either the final one in the dataset or it's own csv file 
#(see below)
#Unscored epochs can be left blank within the "Score" column.

#Example of a file path: "/Users/YOURNAME/desktop/YOURFILE.csv"
file_path1_eeg <- "/Users/jacobellen/desktop/eeg1exampledata.csv"
file_path2_eeg <- "/Users/jacobellen/desktop/eeg2exampledata.csv"
#NOTE: If not using two eeg files, there is no need to change the 'file_path2_eeg' line, but you must change the 
#eeg_answer argument below to "One."


#File Path of the EMG File
#This file should also be a csv file
file_path_emg <- "/Users/jacobellen/desktop/emgexampledata.csv"
#EXAMPLE PATH: "/Users/jacobellen/desktop/emgexampledata.csv"

#File Path of the Output CSV File With All of the Predictions
final_outputpath <- "/Users/jacobellen/desktop/finaloutput.csv"
#EXAMPLE PATH: /Users/jacobellen/desktop/finaloutput.csv


#Sampling Rate Information
samplingrate <- 175
#NOTE: The default is 250 Hz, but the script can work effectively with a minimum of 150 Hz and anything larger than 250 Hz
#The script will not run with sampling rates under 150 Hz







#ADDITIONAL (OPTIONAL) TUNING PARAMETERS:



#ONE VS TWO EEGs

#Would you like to use one or two EEGs? The default value is two.
#If the user would only like to use one EEG, replace "Two" with "One" below
eeg_answer <- "Two"


#OPTION TO RUN THE SAME MODEL ON OTHER FILES 
#NOTE: You don't need any training data for additional files (algorithm performs better on other files of same rat)
#Change the "No" to "Yes" Below
#You can list the paths of as many additional files as you want, just add a comma and 
#specify a new path in the for each file
#These files do not need any "scoring" column with results, only the raw data for the EEGs and EMG.
scoreotherfileswithsamemodel <- "No"
filepatheeg1_additionalfiles <- c("/Users/jacobellen/desktop/eeg1exampledata2.csv","/Users/jacobellen/desktop/eeg1exampledata3.csv")
filepatheeg2_additionalfiles <- c("/Users/jacobellen/desktop/eeg2exampledata2.csv","/Users/jacobellen/desktop/eeg1exampledata3.csv")
filepathemg_additionalfiles <- c("/Users/jacobellen/desktop/emgexampledata2.csv","/Users/jacobellen/desktop/eeg1exampledata3.csv") 
#EXAMPLE PATH: "/Users/YOURNAME/desktop/eeg1exampledata_additionalfile.csv"
#EXAMPLE OF USING MULTIPLE FILE PATHS: 
#You can list the paths separated by a comma
#c("/Users/jacobellen/desktop/eeg1exampledata_additionalfile.csv", "/Users/jacobellen/desktop/eeg1exampledata_additionalfile2.csv")



#OPTION TO INPUT EACH EEG AND EMG DATASET AS ONE LONG VECTOR AND SUBMIT SCORING DATA 
#IN A SEPARATE CSV FILE

#If you would like to input the EEG1, EEG2 and EMG datasets as long vector rather than in rows
#and columns (but still in a csv file), you can answer "Yes" below. 

#In this case, you need to put the paths of .csv files that are just one long column for each
#in the EEG1, EEG2 and EMG file path arguments above.
#Also need to input Sampling Rate (in Hertz) and Number of Seconds per Epoch Below so the Cutoffs can be made automatically
vectorinput <- "No"
epochlength <- 2
#In this case, you need to upload your Scoring file column as a separate file. This file should
#in a .csv and the spreadsheet will just be one column (with some scored epochs, but mostly empty)
scoringcolumn_filepath <- ""



#EXTRACTING PROCESSED FOURIER TRANSFORMED FILE

#If you would like to get a copy of the frequency-domain data in csv form, input "Yes" below 
#along with a file name and path to output it.
#There's no to change this section if you don't want the transformed data file
frequency_answer <- "No"
file_output_path <- "" 
#EXAMPLE FILE PATH:"/Users/jacobellen/desktop/spectraldata.csv"


#SCRIPT 
#Loading the packages required for this task
if(!require("edf")) install.packages("edf",repos = "http://cran.us.r-project.org")
if(!require("tidyverse")) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require("keras")) install.packages("keras",repos = "http://cran.us.r-project.org")
if(!require("pracma")) install.packages("pracma",repos = "http://cran.us.r-project.org")
if(!require("oce")) install.packages("oce",repos = "http://cran.us.r-project.org")
if(!require("stringr")) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require("data.table")) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require("tibble")) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require("caret")) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require("tensorflow")) install.packages("tensorflow", repos = "http://cran.us.r-project.org")
if(!require("eegkit")) install.packages("eegkit", repos = "http://cran.us.r-project.org")
if(!require("stats")) install.packages("stats", repos = "http://cran.us.r-project.org")

library(edf)
library(tidyverse)
library(keras)
library(pracma)
library(oce)
library(stringr)
library(data.table)
library(tibble)
library(caret)
library(tensorflow)
library(eegkit)
library(stats)


#Delete This After First Time Running Script
#install_tensorflow(restart_session = FALSE)


#Making Sure Sampling Rate is Above 150 Hz
if (samplingrate >= 150) {


#Performing Spectral Analysis on First EEG Data
eeg1 <- fread(file_path1_eeg)
eeg1 <- as.data.frame(eeg1)
if(eeg1[1,1]==1 & eeg1[2,1] ==2) {
  eeg1 <- eeg1[, -1]
}

scoringcolumn <- ""
if (vectorinput=="Yes") {
  eeg1 <- as.matrix(eeg1)
  eeg1 <- t(eeg1)
  num <- length(eeg1)/(samplingrate*epochlength)
  eeg1 <- matrix(eeg1, num, (samplingrate*epochlength), byrow = T)
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

#Sampling to 1,000 data points
eeg1_final <- matrix(0, nrow(eeg1), 1000)
if (ncol(eeg1) !=1000) {
for (rowsi in 1:nrow(eeg1)) {
  eeg1_final[rowsi,] <- eegresample(eeg1[rowsi,], 1000)
}
} else {
  eeg1_final <- eeg1
}

fs <- matrix(0, nrow(eeg1_final), 125)
for (x in 1:nrow(eeg1_final)) {
  print(paste0("Processing First EEG Data Row Number: ", x))
  spectrum3 <- oce::pwelch(eeg1_final[x,], points = 125, plot=FALSE)
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
    num2 <- length(eeg2)/(samplingrate*epochlength)
    eeg2 <- matrix(eeg2, num, (samplingrate*epochlength), byrow = T)
    eeg2 <- as.data.frame(eeg2)
  }



if (is.data.frame(eeg2)==TRUE) {
  eeg2 <- as.matrix(eeg2)
}

#Sampling to 1,000 data points
eeg2_final <- matrix(0, nrow(eeg2), 1000)
if (ncol(eeg2) !=1000) {
  for (rowsi in 1:nrow(eeg2)) {
    eeg2_final[rowsi,] <- eegresample(eeg2[rowsi,], 1000)
  }
} else {
  eeg2_final <- eeg2
}


fs2 <- matrix(0, nrow(eeg2_final), 125)
for (x in 1:nrow(eeg2_final)) {
  print(paste0("Processing Second EEG Data", x))
  spectrum3 <- oce::pwelch(eeg2_final[x,], points = 125, plot=FALSE)
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

if (samplingrate >= 150 & samplingrate < 250) {
  final <- as.data.frame(cbind(delta, theta,upper_theta,alpha, beta, lower_gamma, medium_gamma, ratio_betadelta, 
                               ratio_betalowergamma, ratio_thetadelta, ratio_thetagamma))
}

  

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


if (samplingrate >= 150 & samplingrate < 250) {
  final <- final %>%
    dplyr::select(-c(high_gamma2, ratio_betahighgamma2))
    
}
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
  emg <- length(emg)/(samplingrate*epochlength)
  emg <- matrix(emg, num, (samplingrate*epochlength), byrow = T)
}

#Sampling to 1,000 data points
emg_final <- matrix(0, nrow(emg), 1000)
if (ncol(emg) !=1000) {
  for (rowsi in 1:nrow(emg)) {
    emg_final[rowsi,] <- eegresample(emg[rowsi,], 1000)
  }
} else {
  emg_final <- emg
}


RMS <- as.vector(sqrt(abs(rowSums(emg_final))))
RMS_power <- as.vector(rowSums(abs(emg_final)))

#Outputting Warning if EMG Coefficient of Variation is <1.67 (see paper)
variation <- sd(as.matrix(emg_final))/mean(sqrt(((c(as.matrix(emg_final))^2))))
if (variation < 1.67) {
  print("The EMG quality of your signal is poor and the algorithm will struggle to classify REM epochs effectively")
}

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
  dplyr::filter(Score=="0" | Score =="1" | Score=="2") 
ml.train <- ml.train %>% column_to_rownames('index')

ml.test <- ml %>%
  dplyr::filter(is.na(ml$Score==TRUE))
ml.test <- ml.test %>% column_to_rownames('index')

#Warning produced if less than the training dataset is less than 10% REM
if (length(which(ml.train$Score=="2"))/(length(ml.train$Score)) < .10) {
  print("Warning: you have less than 10% REM in your training data, so you should add more REM to help the algorithm work properly")
}  


#Upsampling Dataset to Include Equal Numbers of Each Class
ml.train <- as.data.frame(ml.train)
ml.train$Score <- as.factor(ml.train$Score)
ml.train_up <- upSample(x= ml.train[(1:nrow(ml.train)),1:(ncol(ml.train)-1)], y= factor(ml.train$Score), yname = "Score")


#Putting Data into Matrix Form and Random Order
ml.train_up  <- as.matrix(ml.train_up)
ml.test <- as.matrix(ml.test)
ml.train_up <- ml.train_up[sample(nrow(ml.train_up)),]

#Dividing the dataset into prediction data and labeled data
xtrain <- ml.train_up[1:nrow(ml.train_up), 1:(ncol(ml.train_up)-1)]
ytrain <- ml.train_up[1:nrow(ml.train_up), ncol(ml.train_up)]

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
probabilities.matrix <- matrix(0,nrow(xtest), 5)

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


#Five Runs of Model
for (n in 1:5) {
history <- model %>% fit(
  xtrain, ytrain,
  epochs = 100, batch_size = 10
)

#Extracting Predictions and Probabilities
predictions <- predict_classes(model, xtest) %>% as.vector()
predictions.matrix[,n] <- predictions
probabilities <- predict_proba(model, xtest) 
probabilities.matrix[,n] <- apply(probabilities, 1, max)
}

probabilities.df <- as.data.frame(probabilities.matrix)
probabilities.df2 <- mutate(probabilities.df, Mean_Certainty = rowMeans(probabilities.df))
probabilities.df2 <- mutate(probabilities.df2, Certainty = ifelse((Mean_Certainty < .9), "Uncertain", ""))

#Averaging Three Runs of the Model
class(predictions.matrix) <- "character"
mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
predictions.df <- as.data.frame(predictions.matrix)
predictions <- apply(predictions.df, 1, mode)
predictions <- as.numeric(predictions)
predictions_final <- as.data.frame(cbind(predictions, probabilities.df2$Certainty))

#Finalizing Predictions and Formatting the Final Output
predictions.output1 <- as.data.frame(cbind(xtest,predictions_final))
colnames(predictions.output1)[ncol(predictions.output1)-1] <- "Score"
colnames(predictions.output1)[ncol(predictions.output1)] <- "Certainty"
ml.train2 <- data.frame(ml.train, stringsAsFactors = FALSE)
ml.train2 <- ml.train2 %>% rownames_to_column('index')
ml.train3 <- as.data.frame(lapply(ml.train2[1:nrow(ml.train2), 1:(ncol(ml.train2)-1)], as.numeric))
ml.train3 <- as.data.frame(cbind(ml.train3, ml.train2$Score))
colnames(ml.train3)[ncol(ml.train3)] <- "Score"


#Combining Training and Test Data for Final Output
predictions.output1 <- predictions.output1 %>% rownames_to_column('index')
ml.train3$index <- as.character(ml.train3$index)
suppressWarnings({predictions.outputfinal <- as.data.frame(dplyr::bind_rows(ml.train3, predictions.output1))})
predictions.outputfinal <- predictions.outputfinal %>% dplyr::select(c(index, Score, Certainty))
colnames(predictions.outputfinal)[ncol(predictions.outputfinal)] <- "Certainty"
colnames(predictions.outputfinal)[ncol(predictions.outputfinal)-1] <- "ModelPredictions"
finaloutput <- merge(original.order, predictions.outputfinal, by ="index")
finaloutput <- finaloutput %>% dplyr::select(-c(index))
colnames(finaloutput)[ncol(finaloutput)-2] <- "OriginalScores"
finaloutput$Certainty <- as.character(finaloutput$Certainty)
finaloutput$Certainty[is.na(finaloutput$Certainty)] <- ""



#Applying Smoothing Heuristic

#Replace sporadic waking/non-rem inside a consolidated rem bout
rmf <- which(finaloutput$ModelPredictions==2)  #finding rem epochs from neural network scoring
rmfd <- diff(rmf) #index difference of rem epochs (i.e. 1=a rem next to another rem)
b <- 1
while (b < length(rmfd)) {
  epend <- NA
  epst <- NA
  if (rmfd[b] <= 3) {
    epst <- rmf[b]
     b <- b+1
  while (b < length(rmf) & rmfd[b]<=3) {
         epend <- rmf[b] + 1
         b <- b+1 } }
  if (is.na(epend)) { }
  else {finaloutput$ModelPredictions[epst:epend] <- 2 }
    b <- b+1 }

#Getting rid of rem following wake
for (rfi in 2:(length(finaloutput$ModelPredictions)-1)) {
   if ((finaloutput$ModelPredictions[rfi-1] == 0) & (finaloutput$ModelPredictions[rfi]==2)) {
     finaloutput$ModelPredictions[rfi] <- 0
     } }

#to get rid of singleton data
for (rfi in 2:(length(finaloutput$ModelPredictions)-1)) {
 if (finaloutput$ModelPredictions[rfi-1] == finaloutput$ModelPredictions[rfi+1]) {
   finaloutput$ModelPredictions[rfi] <- finaloutput$ModelPredictions[rfi+1] } }

#Outputting final CSV File with an additional column with all of the predictions (the last column
#titled "ModelPredictions" and an index column with the number of each epoch.
write.csv(finaloutput, final_outputpath)



#If you want to score the different files of the same rat using the model trained above
if (scoreotherfileswithsamemodel == "Yes") {
for (c in 1:(length(filepatheeg1_additionalfiles))) {
  #Performing Spectral Analysis on First EEG Data
  eeg1 <- fread(filepatheeg1_additionalfiles[c])
  eeg1 <- as.data.frame(eeg1)
  if(eeg1[1,1]==1 & eeg1[2,1] ==2) {
    eeg1 <- eeg1[, -1]
  }
  
  scoringcolumn <- ""
  if (vectorinput=="Yes") {
    eeg1 <- as.matrix(eeg1)
    eeg1 <- t(eeg1)
    num <- length(eeg1)/(samplingrate*epochlength)
    eeg1 <- matrix(eeg1, num, (samplingrate*epochlength), byrow = T)
    eeg1 <- as.data.frame(eeg1)
    scoringcolumn <- "separate"
  }
  
  
  
  if (is.data.frame(eeg1)==TRUE) {
    eeg1 <- as.matrix(eeg1)
  }
  
  
  #Sampling to 1,000 data points
  eeg1_final <- matrix(0, nrow(eeg1), 1000)
  if (ncol(eeg1) !=1000) {
    for (rowsi in 1:nrow(eeg1)) {
      eeg1_final[rowsi,] <- eegresample(eeg1[rowsi,], 1000)
    }
  } else {
    eeg1_final <- eeg1
  }
  
  fs <- matrix(0, nrow(eeg1_final), 125)
  for (x in 1:nrow(eeg1_final)) {
    print(paste0("Processing First EEG Data Row Number: ", x))
    spectrum3 <- oce::pwelch(eeg1_final[x,], points = 125, plot=FALSE)
    fs[x,(1:length(spectrum3$spec))] <- spectrum3$spec 
  }
  
  #Performing Spectral Analysis on Second EEG Data
  if (eeg_answer=="Two") {
    eeg2 <- fread(filepatheeg2_additionalfiles[c])
    eeg2 <- as.data.frame(eeg2)
    
    if(eeg2[1,1]==1 & eeg2[2,1] ==2) {
      eeg2 <- eeg2[, -1]
    }
    
    if (vectorinput=="Yes") {
      eeg2 <- as.matrix(eeg2)
      eeg2 <- t(eeg2)
      num2 <- length(eeg2)/(samplingrate*epochlength)
      eeg2 <- matrix(eeg2, num, (samplingrate*epochlength), byrow = T)
      eeg2 <- as.data.frame(eeg2)
    }
    
    if (is.data.frame(eeg2)==TRUE) {
      eeg2 <- as.matrix(eeg2)
    }
    
    eeg2_final <- matrix(0, nrow(eeg2), 1000)
    if (ncol(eeg2) !=1000) {
      for (rowsi in 1:nrow(eeg2)) {
        eeg2_final[rowsi,] <- eegresample(eeg2[rowsi,], 1000)
      }
    } else {
      eeg2_final <- eeg2
    }
    
    
    fs2 <- matrix(0, nrow(eeg2_final), 125)
    for (x in 1:nrow(eeg2_final)) {
      print(paste0("Processing Second EEG Data", x))
      spectrum3 <- oce::pwelch(eeg2_final[x,], points = 125, plot=FALSE)
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
  emg <- fread(filepathemg_additionalfiles[c])
  emg <- as.data.frame(emg)
  if(emg[1,1]==1 & emg[2,1] ==2) {
    emg <- emg[, -1]
  }
  
  if (vectorinput=="Yes") {
    emg <- as.matrix(emg)
    emg <- t(emg)
    emg <- length(emg)/(samplingrate*epochlength)
    emg <- matrix(emg, num, (samplingrate*epochlength), byrow = T)
  }
  
  #Sampling to 1,000 data points
  emg_final <- matrix(0, nrow(emg), 1000)
  if (ncol(emg) !=1000) {
    for (rowsi in 1:nrow(emg)) {
      emg_final[rowsi,] <- eegresample(emg[rowsi,], 1000)
    }
  } else {
    emg_final <- emg
  }
  
  RMS <- as.vector(sqrt(abs(rowSums(emg_final))))
  RMS_power <- as.vector(rowSums(abs(emg_final)))
  
  RMSplusgamma <- RMS+medium_gamma
  RMSpowerplusgamma <- (RMS_power+medium_gamma)
  
  
  #Outputting Warning if EMG Coefficient of Variation is <1.67 (see paper)
  variation <- sd(as.matrix(emg_final))/mean(sqrt(((c(as.matrix(emg_final))^2))))
  if (variation < 1.67) {
    print("The EMG quality of your signal (additional file) is poor and the algorithm will struggle to classify REM epochs effectively")
  }
  
  final <- as.data.frame(cbind(final, RMS, RMS_power, RMSplusgamma, RMSpowerplusgamma))
  
  final <- scale(final)
  
  iter <- c+1
  
  if (frequency_answer=="Yes") {
    write.csv(final, paste0(sub("\\..*", "", file_output_path), "_file", iter, ".csv"))
  }
  
  
  #Setting up the Algorithm
  ml <- as.data.frame(final)
  ml.train <- as.matrix(ml)

  if (class(ml.train[1,1])=="character") {
    class(ml.train) <- "numeric"
  }
  
  #Generating Predictions
  predictions.matrix_newfile <- matrix(0,nrow(ml.train), 5)
  probabilities.matrix_newfile <- matrix(0,nrow(ml.train), 5)
  
  for (n in 1:5) {
    #Extracting Predictions and Probabilities
    predictions_newfile <- predict_classes(model, ml.train) %>% as.vector()
    predictions.matrix_newfile[,n] <- predictions_newfile 
    probabilities_newfile <- predict_proba(model, ml.train) 
    probabilities.matrix_newfile[,n] <- apply(probabilities_newfile, 1, max)
  }
  
  #Finalizing Probabilities and Predictions for Each Epoch
  class(predictions.matrix_newfile) <- "character"
  predictions.df_newfile <- as.data.frame(predictions.matrix_newfile)
  predictions_newfile <- apply(predictions.df_newfile, 1, mode)
  predictions_newfile <- as.numeric(predictions_newfile)
  probabilities.df_newfile <- as.data.frame(probabilities.matrix_newfile)
  probabilities.df2_newfile <- mutate(probabilities.df_newfile, Mean_Certainty = rowMeans(probabilities.df_newfile))
  probabilities.df2_newfile <- mutate(probabilities.df2_newfile, Certainty = ifelse((Mean_Certainty < .9), "Uncertain", ""))
  predictions_final <- as.data.frame(cbind(predictions_newfile, probabilities.df2_newfile$Certainty))
  
 
  #Finalizing Output
  predictions.output1 <- as.data.frame(cbind(eeg1,predictions_final))
  colnames(predictions.output1)[ncol(predictions.output1)-1] <- "ModelPredictions"
  colnames(predictions.output1)[ncol(predictions.output1)] <- "ModelCertainty"
  finaloutput_newfile <- predictions.output1
  
  
  #Applying Smoothing Heuristic
  
  #Replace sporadic waking/non-rem inside a consolidated rem bout
  rmf <- which(finaloutput_newfile$ModelPredictions==2)  #finding rem epochs from neural network scoring
  rmfd <- diff(rmf) #index difference of rem epochs (i.e. 1=a rem next to another rem)
  b <- 1
  while (b < length(rmfd)) {
    epend <- NA
    epst <- NA
    if (rmfd[b] <= 3) {
      epst <- rmf[b]
      b <- b+1
      while (b < length(rmf) & rmfd[b]<=3) {
        epend <- rmf[b] + 1
        b <- b+1 } }
    if (is.na(epend)) { }
    else {finaloutput_newfile$ModelPredictions[epst:epend] <- 2 }
    b <- b+1 }
  
  #Getting rid of rem following wake
  for (rfi in 2:(length(finaloutput_newfile$ModelPredictions)-1)) {
    if ((finaloutput_newfile$ModelPredictions[rfi-1] == 0) & (finaloutput_newfile$ModelPredictions[rfi]==2)) {
      finaloutput_newfile$ModelPredictions[rfi] <- 0
    } }
  
  #to get rid of singleton data
  for (rfi in 2:(length(finaloutput_newfile$ModelPredictions)-1)) {
    if (finaloutput_newfile$ModelPredictions[rfi-1] == finaloutput_newfile$ModelPredictions[rfi+1]) {
      finaloutput_newfile$ModelPredictions[rfi] <- finaloutput_newfile$ModelPredictions[rfi+1] } }


#Outputting final CSV File with an additional column with all of the predictions (the last column
#titled "ModelPredictions") and a column specifying if an epoch is uncertain for potential re-scoring.
write.csv(finaloutput_newfile, paste0(sub("\\..*", "", final_outputpath), "_file", iter, ".csv"))

}
}
} else (stop("Your Sampling Rate Cannot be Below 150 Hz"))

