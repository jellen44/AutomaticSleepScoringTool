start_time <- Sys.time()

#FOR THE USER:

#Preprocessing Script
#You can choose a rat and specific file dates for that rat below by uncommenting them.

#This script loops through each rat's file, performs the pwelch function on each and outputs a  
#file to the same folder with dimensions of the same number of rows and 125 columns ()

#Your rat edf file names must be in the form "RATNAME-DATE.edf" (which they are in middfiles)

#I have it set up right now to do it one rat at a time, but we could also add a loop for each 
#rat to just do all of them at the same time.

# File Path 
# This is the file path to the folder that all of your scoring files are in
# The output of this script will be csv file with the same name as the scoring file but with
# fft in front to signify that the file has been processed.
#EXAMPLE: '/Users/jacobellen/desktop/Rats/CheapTrick'
filepath <- '/Users/jacobellen/desktop/Rats/Elvis/'


#Choosing a Rat

#pinkfloyd
#ratname <- "pinkfloyd"
#datevec <- c("101519", "101719", "102319", "102419", "102919", "103019", "103119")

#neilyoung
#ratname <- "neilyoung"
#datevec <- c("111719", "111819", "112119", "112219", "112519", "112619", "112919")

#elvis
ratname <- "elvis"
datevec <- c("102319", "102419", "102819"
#, "102919", "103019", "103119", "110319", "110419"
)

#cheaptrick
#ratname <- "cheaptrick"
#datevec <- c("110619", "110719", "111119" , "111219", "111319")


#SCRIPT:

#Loading Required Packages (they will be installed if needed)
if(!require("edf")) install.packages("edf",repos = "http://cran.us.r-project.org")
if(!require("tidyverse")) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require("keras")) install.packages("keras",repos = "http://cran.us.r-project.org")
if(!require("pracma")) install.packages("pracma",repos = "http://cran.us.r-project.org")
if(!require("oce")) install.packages("oce",repos = "http://cran.us.r-project.org")
if(!require("entropy")) install.packages("entropy", repos = "http://cran.us.r-project.org")
if(!require("stringr")) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(edf)
library(tidyverse)
library(keras)
library(pracma)
library(oce)
library(entropy)
library(stringr)
library(dplyr)


# Creating a Vector of all of the File Names to Process
v <- NULL
for (d in 1:length(datevec)) {
  v[d] <- paste0(ratname, "_" , datevec[d], ".edf")
}


#Starting the loop for each file
for (i in v) { 

#Reading in each file
datapoints <- read.edf(paste0(filepath, '/',i))
sep <- str_sub(i, start=1, end=(nchar(i)-4))

#First EEG Data
if("EEG_1" %in% names(datapoints$signal)) {
  eeg1 <- as.numeric(datapoints$signal$EEG_1$data) 
  }

if ("EEG_EEG_1A_B" %in% names(datapoints$signal)) {
  eeg1 <- as.numeric(datapoints$signal$EEG_EEG_1A_B$data) 
  }

eeg1 <- as.matrix(eeg1)
eeg1 <- t(eeg1)
num <- length(eeg1)/1000
new <- matrix(eeg1, num, 1000, byrow = T)
fs <- matrix(0, 21600, 125)
for (x in 1:dim(new)[1]) {
  print(paste0("Processing First EEG Data", x))
  spectrum <- oce::pwelch(new[x,], points = 1000, overlap = 250, padding = 1000, fs = 250,
                           plot=FALSE)
  fs[x,] <- spectrum$spec 
}


#Second EEG Data
if("EEG_2" %in% names(datapoints$signal)) {
  eeg2 <- as.numeric(datapoints$signal$EEG_2$data)
}

if ("EEG_EEG_2A_B" %in% names(datapoints$signal)) {
  eeg2 <- as.numeric(datapoints$signal$EEG_EEG_2A_B$data)
}

eeg2 <- as.matrix(eeg2)
eeg2 <- t(eeg2)
num2 <- length(eeg2)/1000
new2 <- matrix(eeg2, num2, 1000, byrow = T)
fs2 <- matrix(0, 21600, 125)
for (s in 1:dim(new2)[1]) {
    print(paste0("Processing Second EEG Data", s))
    spectrum2 <- oce::pwelch(new2[s,], points = 1000, overlap = 250, 
                             padding = 1000, fs = 250, plot=FALSE)
    fs2[s,] <- spectrum2$spec 
}


# Feature Extraction from EEGs
#EEG1
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
#EEG2
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

#Processing EMG Data
  if ("EMG" %in% names(datapoints$signal)) {
    emg <- as.numeric(datapoints$signal$EMG$data)
  }
  
  if ("EMG_EMG" %in% names(datapoints$signal)) {
    emg <-  as.numeric(datapoints$signal$EMG_EMG$data)
  }

emg <- as.matrix(emg)
emg <- t(emg)
num <- length(emg)/1000
new3 <- matrix(emg, num, 1000, byrow = T)

RMS <- as.vector(sqrt(abs(rowSums(new3))))
RMS_power <- as.vector(rowSums(abs(new3)))

RMSplusgamma <- RMS+medium_gamma
RMSpowerplusgamma <- (RMS_power+medium_gamma)

final <- as.data.frame(cbind(final, RMS, RMS_power, RMSplusgamma, RMSpowerplusgamma))

final <- scale(final)


write.csv(final, paste0(filepath, "/fft", sep, ".csv"))
}


end_time <- Sys.time()
time <- end_time - start_time
paste0("This Script Took ", time, " Minutes")
