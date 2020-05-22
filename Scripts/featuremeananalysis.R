start_time <- Sys.time()

#FOR USERS:

#This script looks at the feature separation between each of the three classes we are trying to predict.
#It takes in the fft processed files and the stage info files for as many files as you want
#and outputs one overall feature plot and the 3D plot I showed you earlier.

#File Path
#Enter the path to the folder where the processed and score files are stored
folderfilepath_fft <- "/Users/jacobellen/desktop/Rats/CheapTrick"
folderfilepath_scorefiles <- "/Users/jacobellen/desktop/Rats/CheapTrick"
#Example: "/Users/jacobellen/desktop/Rats/CheapTrick"


#If you want the raw excel file formed to make a different figure, you can put "Yes" below and 
#then the output path to put the csv file in that folder. 
excelfileoutput <- "No"
excelfile_outputpath <- ""


#Enter the Dates and the Rat you want to Look at Below
v <- c("110619", "110719",
       #"111119",
       "111219","111319")
ratname <- "cheaptrick"



#SCRIPT

#Reading in Files
final.list <- list()
score.list <- list()
for (g in 1:length(v)) {
  final.list[[g]] <- read.csv(paste0(folderfilepath_fft, "/fft", ratname, "_", v[g], ".csv"))
  score.list[[g]] <- read.csv(paste0(folderfilepath_scorefiles, "/stageinfo_", v[g], ".csv"))
}
final <- bind_rows(final.list)


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

#Packages
  if(!require("edf")) install.packages("edf",repos = "http://cran.us.r-project.org")
  if(!require("tidyverse")) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
  if(!require("keras")) install.packages("keras",repos = "http://cran.us.r-project.org")
  if(!require("pracma")) install.packages("pracma",repos = "http://cran.us.r-project.org")
  if(!require("oce")) install.packages("oce",repos = "http://cran.us.r-project.org")
  if(!require("plot3D")) install.packages("plot3D", repos = "http://cran.us.r-project.org")
  if(!require("data.table")) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require("dplyr")) install.packages("dplyr", repos = "http://cran.us.r-project.org")
  if(!require("xtable")) install.packages("xtable", repos = "http://cran.us.r-project.org")
if(!require("caret")) install.packages("caret", repos = "http://cran.us.r-project.org")

  library(edf)
  library(tidyverse)
  library(keras)
  library(pracma)
  library(oce)
  library(plot3D)
  library(data.table)
  library(dplyr)
  library(xtable)
  library(caret)


#Making one overall dataframe
score <- rbindlist(score.list)
final <- final %>%
  dplyr::select(-X)
preds <- as.matrix(score$Score)
ml <- as.data.frame(cbind(final, preds))
ml <- na.omit(ml)


#Creating dataframe with average and standard deviations of each prediction
suppressWarnings({
bars <- ml %>%
  group_by(preds)  %>%
  summarise_each(funs(mean,sd))
})
  
if (excelfileoutput == "Yes") {
  write.csv(bars, excelfile_outputpath)
}


#Making one overall figure
bar.gather <- gather(bars, key="Frequency", value="Mean.Power", -preds)

graph.gather <- bar.gather %>% filter(!grepl("sd",Frequency))
graph.gather %>%
  ggplot(aes(x= Frequency, y= Mean.Power)) + 
  geom_col(aes(fill=factor(preds)), position="dodge", width=.5) +
  scale_fill_discrete(name= "Sleep State", labels = c("Wake", "NonREM", "REM")) + theme(axis.text.x = element_text(angle = 60, size=.1)) +
  labs(x=NULL, y="Mean Power") + theme_bw() + coord_flip()


#Making a 3D Plot
ml.plot <- ml %>% filter(medium_gamma<=7)
ml.plot <- ml.plot %>% filter(upper_theta <=7)
ml.plot <- ml.plot %>% filter(ratio_betadelta <=7)
x <- ml.plot$medium_gamma
y <- ml.plot$upper_theta
z <- ml.plot$ratio_betadelta
scatter3D(x, y, z,colvar = as.numeric(ml.plot$preds), cex=.3, pch=19, bty='b2', xlab = "Gamma",
          ylab ="Upper_Theta", zlab = "Beta/Delta",
          colkey = list(at = c(0, 2, 1), length=.3, width=.4,dist=-.09, 
          labels = c("Wake", "REM", "NonREM")
          ))





end_time <- Sys.time()
time <- end_time - start_time
paste0("This Script Took ", time)
