getwd()
setwd("/Users/kyleparmley/Downloads/KGF_AdjustedBLUPsAllDays_thinned_Oct24.csv")
library(randomForest)
#devtools::install_github('topepo/caret/pkg/caret')
library(caret)
#Enable Parallel processing for quicker computational time
library(parallel)
#install.packages("doSNOW")
library(doSNOW)
corebaby <- detectCores()
NumberOfCluster <- corebaby  #Set as the number of cores found from detectCores function
cl <- makeCluster(NumberOfCluster) #Make clusters from assigned cores
registerDoSNOW(cl)


#################
#Combined Data analysis
#################
###Import raw data with import dataset function
data<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/GWAS/KGF_AdjustedBLUPsAllDays_thinned_Oct24.csv", header = T)
cols <- colnames(data[,1:31])
data[cols] <- lapply(data[cols], factor)
data[1:10,1:14]

str(data)
colnames(data)

set.seed(8)
# Kyle starts here

#######################################################
# Step 1: Subset df by predictors to include in model #
#######################################################
dat_weight_included = data[,c(1:ncol(data))]
dat_no_weight = data[,c(1:31,35:ncol(data))]

########################################################
# Step 2: Pre-process data using z-score normalization #
########################################################
x<-preProcess(dat_weight_included[,32:ncol(dat_weight_included)],method = c("scale","center"),na.remove=F,verbose=T) #All columns were standardized (z-score) 
dat1<-predict(x,dat_weight_included) #Only normalize physiological traits

x<-preProcess(dat_no_weight[,32:ncol(dat_no_weight)],method = c("scale","center"),na.remove=F,verbose=T) #All columns were standardized (z-score) 
dat2<-predict(x,dat_no_weight) #Only normalize physiological traits

rf.train.control <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 5)

#begin iteration process here
#################################################
# Step 3: Fit RF model using diff response vars #
#################################################
# Day 6
# Dat 1 with weights
day <- "Day6"
info <- "withWeights"
# Make dataframe to place model fit and variable importance results
dat1_results = data.frame()
dat1_importance = data.frame(matrix(nrow = 42))
colnum=c(3:6,9,13,22)
for (i in 1:length(colnum)){
  x=colnum[i]  #set the current [i] column as x
  df_01_1 <- dat1[,c(x,32:73)]
  y = colnames(df_01_1)[1]
  colnames(df_01_1)[1]="y"
  caret.rf <- train(y ~ ., 
                    data = df_01_1,
                    method = "rf", 
                    tuneLength = 10,
                    trControl = rf.train.control,importance=T,ntree=500)
  bestModel = caret.rf$results[which.max(caret.rf$results$Accuracy),]
  rownames(bestModel) <- colnames(dat1)[colnum[i]]
  dat1_results = rbind(dat1_results,bestModel)
  rfi<-varImp(caret.rf)
  variableImportance = rfi$importance
  colnames(variableImportance) <- y
  dat1_importance = cbind(dat1_importance,variableImportance)
}

#########################################################
# Step 4: Output the RF results and variable importance #
#########################################################

currentDate <- Sys.Date() 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Results_",day, info, currentDate,".csv",sep="") 
write.csv(dat1_results, file=csvFileName) 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Importance_", day, info, currentDate,".csv",sep="") 
write.csv(dat1_importance, file=csvFileName) 


#################################################
# Step 3: Fit RF model using diff response vars #
#################################################
# Day 9
# Dat 1 with weights
day <- "Day9"
info <- "withWeights"
# Make dataframe to place model fit and variable importance results
dat1_results = data.frame()
dat1_importance = data.frame(matrix(nrow = 42))
colnum=c(3:5,7,10,16,22)
for (i in 1:length(colnum)){
  x=colnum[i]  #set the current [i] column as x
  df_01_1 <- dat1[,c(x,32:34,74:112)] #Pull out the Day 9 data (and weights) from the larger df
  y = colnames(df_01_1)[1]
  colnames(df_01_1)[1]="y"
  caret.rf <- train(y ~ ., 
                    data = df_01_1,
                    method = "rf", 
                    tuneLength = 10,
                    trControl = rf.train.control,importance=T,ntree=500)
  bestModel = caret.rf$results[which.max(caret.rf$results$Accuracy),]
  rownames(bestModel) <- colnames(dat1)[colnum[i]]
  dat1_results = rbind(dat1_results,bestModel)
  rfi<-varImp(caret.rf)
  variableImportance = rfi$importance
  colnames(variableImportance) <- y
  dat1_importance = cbind(dat1_importance,variableImportance)
}

#########################################################
# Step 4: Output the RF results and variable importance #
#########################################################

currentDate <- Sys.Date() 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Results_",day, info, currentDate,".csv",sep="") 
write.csv(dat1_results, file=csvFileName) 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Importance_", day, info, currentDate,".csv",sep="") 
write.csv(dat1_importance, file=csvFileName) 


#################################################
# Step 3: Fit RF model using diff response vars #
#################################################
# Day 12
# Dat 1 with weights
# Make dataframe to place model fit and variable importance results
day <- "Day12"
info <- "withWeights"
dat1_results = data.frame()
dat1_importance = data.frame(matrix(nrow = 41))
colnum=c(3:7,10,19,22)
for (i in 1:length(colnum)){
  x=colnum[i]  #set the current [i] column as x
  df_01_1 <- dat1[,c(x,32:34,113:150)]#Pull out the Day 12 data (and weights) from the larger df
  y = colnames(df_01_1)[1]
  colnames(df_01_1)[1]="y"
  caret.rf <- train(y ~ ., 
                    data = df_01_1,
                    method = "rf", 
                    tuneLength = 10,
                    trControl = rf.train.control,importance=T,ntree=500)
  bestModel = caret.rf$results[which.max(caret.rf$results$Accuracy),]
  rownames(bestModel) <- colnames(dat1)[colnum[i]]
  dat1_results = rbind(dat1_results,bestModel)
  rfi<-varImp(caret.rf)
  variableImportance = rfi$importance
  colnames(variableImportance) <- y
  dat1_importance = cbind(dat1_importance,variableImportance)
}

#########################################################
# Step 4: Output the RF results and variable importance #
#########################################################

currentDate <- Sys.Date() 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Results_",day, info, currentDate,".csv",sep="") 
write.csv(dat1_results, file=csvFileName) 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Importance_", day, info, currentDate,".csv",sep="") 
write.csv(dat1_importance, file=csvFileName) 


#################################################
# Step 3: Fit RF model using diff response vars #
#################################################
# Day 6
# Dat 2 no weights
day <- "Day6"
info <- "noWeights"
# Make dataframe to place model fit and variable importance results
dat2_results = data.frame()
dat2_importance = data.frame(matrix(nrow = 39))
i=1
colnum=c(3:6,9)
for (i in 1:length(colnum)){
  x=colnum[i]  #set the current [i] column as x
  df_01_1 <- dat2[,c(x,32:70)]
  y = colnames(df_01_1)[1]
  colnames(df_01_1)[1]="y"
  caret.rf <- train(y ~ ., 
                    data = df_01_1,
                    method = "rf", 
                    tuneLength = 10,
                    trControl = rf.train.control,importance=T,ntree=500)
  bestModel = caret.rf$results[which.max(caret.rf$results$Accuracy),]
  rownames(bestModel) <- colnames(dat2)[colnum[i]]
  dat2_results = rbind(dat2_results,bestModel)
  rfi<-varImp(caret.rf)
  variableImportance = rfi$importance
  colnames(variableImportance) <- y
  dat2_importance = cbind(dat2_importance,variableImportance)
}

#########################################################
# Step 4: Output the RF results and variable importance #
#########################################################

currentDate <- Sys.Date() 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Results_",day, info, currentDate,".csv",sep="") 
write.csv(dat1_results, file=csvFileName) 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Importance_", day, info, currentDate,".csv",sep="") 
write.csv(dat1_importance, file=csvFileName) 

#################################################
# Step 3: Fit RF model using diff response vars #
#################################################
# Day 9
# Dat 2 no weights
day <- "Day9"
info <- "noWeights"
# Make dataframe to place model fit and variable importance results
dat2_results = data.frame()
dat2_importance = data.frame(matrix(nrow = 39))
i=1
colnum=c(3:7,10,16,22)
for (i in 1:length(colnum)){
  x=colnum[i]  #set the current [i] column as x
  df_01_1 <- dat2[,c(x,71:109)]
  y = colnames(df_01_1)[1]
  colnames(df_01_1)[1]="y"
  caret.rf <- train(y ~ ., 
                    data = df_01_1,
                    method = "rf", 
                    tuneLength = 10,
                    trControl = rf.train.control,importance=T,ntree=500)
  bestModel = caret.rf$results[which.max(caret.rf$results$Accuracy),]
  rownames(bestModel) <- colnames(dat2)[colnum[i]]
  dat2_results = rbind(dat2_results,bestModel)
  rfi<-varImp(caret.rf)
  variableImportance = rfi$importance
  colnames(variableImportance) <- y
  dat2_importance = cbind(dat2_importance,variableImportance)
}

#########################################################
# Step 4: Output the RF results and variable importance #
#########################################################

currentDate <- Sys.Date() 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Results_",day, info, currentDate,".csv",sep="") 
write.csv(dat1_results, file=csvFileName) 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Importance_", day, info, currentDate,".csv",sep="") 
write.csv(dat1_importance, file=csvFileName) 

#################################################
# Step 3: Fit RF model using diff response vars #
#################################################
# Day 12
# Dat 2 no weights
day <- "Day12"
info <- "noWeights"
# Make dataframe to place model fit and variable importance results
dat2_results = data.frame()
dat2_importance = data.frame(matrix(nrow = 38))
i=1
colnum=c(3:5,8,11,19,22)
for (i in 1:length(colnum)){
  x=colnum[i]  #set the current [i] column as x
  df_01_1 <- dat2[,c(x,110:147)]
  y = colnames(df_01_1)[1]
  colnames(df_01_1)[1]="y"
  caret.rf <- train(y ~ ., 
                    data = df_01_1,
                    method = "rf", 
                    tuneLength = 10,
                    trControl = rf.train.control,importance=T,ntree=500)
  bestModel = caret.rf$results[which.max(caret.rf$results$Accuracy),]
  rownames(bestModel) <- colnames(dat2)[colnum[i]]
  dat2_results = rbind(dat2_results,bestModel)
  rfi<-varImp(caret.rf)
  variableImportance = rfi$importance
  colnames(variableImportance) <- y
  dat2_importance = cbind(dat2_importance,variableImportance)
}

#########################################################
# Step 4: Output the RF results and variable importance #
#########################################################

currentDate <- Sys.Date() 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Results_",day, info, currentDate,".csv",sep="") 
write.csv(dat1_results, file=csvFileName) 
csvFileName <- paste("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest/RF_Importance_", day, info, currentDate,".csv",sep="") 
write.csv(dat1_importance, file=csvFileName) 



pdf('KGF_9Clusters_mtry_plot_Oct24.pdf')#saves plot of training hyperparameters 
plot(caret.rf)
dev.off()

pdf('KGF_9Clusters_1_oob_performance_plot_Oct24.pdf')
plot(resampleHist((caret.rf))) #Density plots
dev.off()


