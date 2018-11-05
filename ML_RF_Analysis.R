getwd()
setwd("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest")
install.packages(c("xgboost","randomForest","doSNOW","devtools"))
install.packages('caret',dependencies = T)
library(xgboost)
library(randomForest)
devtools::install_github('topepo/caret/pkg/caret')
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
setwd("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/RandomForest")
data<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/GWAS/KGF_AdjustedBLUPsAllDays_thinned_Oct19.csv")
data[1:10,1:10]

data[5]<- as.character(data$Cluster.9)
data[4]<- as.character(data$Cluster.8)
data[3]<- as.character(data$Cluster.6)


str(data)
only_data<-data[,5,12:ncol(data)]
colnames(only_data)
#Remove rows with missing yield data
#df<-only_data[complete.cases(only_data),] 

###Pre-process data by scaling and normalzing
x<-preProcess(only_data[,1:ncol(only_data)],method = c("scale","center"),na.remove=F,verbose=T) #All columns were standardized (z-score) 
y<-predict(x,only_data) #Only normalize physiological traits
head(y)
#Partition data into training and testing
set.seed(11)
#indexes<-createDataPartition(y$Cluster.9, # the outcome variable
#                             p = 0.8, # the percentage of data in the training sample
#                           list = FALSE) # should the results be in a list?
#Creates training and test dataset
#trainSet <- y[indexes,]  
#testSet<- y[-indexes,]   

#Set training cross-validation structure a hyperparameter optimization
rf.train.control <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 5)
#rf.tunegrid <- expand.grid(.mtry=c(10,20,30,40,50,60))

#Train RF model on combined data set
set.seed(9)
caret.rf <- train(Cluster.9. ~ ., 
                   data = y,
                   method = "rf", 
                   tuneLength = 10,
                   trControl = rf.train.control,importance=T,ntree=500) 

#Set wd to save model results

# Examine caret's processing results
caret.rf$results
summary(caret.rf)
caret.rf$bestTune

#Training Accuracy
rf_train_preds <- predict(caret.rf, trainSet)
b<-trainSet[,1]
plot(b,rf_train_preds)
cor(b,rf_train_preds)

# Make predictions on the test set using a Random Forest model 
# trained on all rows of the training set
# finds optimal number of mtry.
rf_preds <- predict(caret.rf, testSet)
RMSE <- sqrt(sum((rf_preds - testSet$Yield)^2)/length(rf_preds)) #RMSE of predicted values vs actual
print(RMSE/mean(testSet$Yield)) 

#Variable importance
rf_variable_importance<-varImp(caret.rf)
rf_var_importance<-print(rf_variable_importance)
plot(rf_variable_importance,top=15)
a<-testSet[,1]
plot(a,rf_preds)
cor(a,rf_preds)

#Save results
caret.rf_tain<-cbind(b,rf_train_preds)
caret.rf_results<-caret.rf$results #save training iterations of hyperparameters
caret.rf_kfoldPerformance<-caret.rf$resample #save training k-fold accuracy
caret.rf_preds_actual<-cbind(rf_preds,testSet[,1]) #save actual by predicted values of training set
caret.rf_variable_importance <- as.matrix(varImp(caret.rf)$importance) #save variable importance from training set
saveRDS(caret.rf,file="KGF_9Clusters_1_.rda") #save model
caret.rf_plot<-plot(caret.rf) #save hyperparameter plots of model performance
write.table(caret.rf_tain, file= "KGF_9Clusters_1__train_predsByActual_results.csv",row.names = F,sep=',')
write.table(caret.rf_results, file= "KGF_9Clusters_1__train_hyperperameter.csv",row.names = F,sep=',')
write.table(caret.rf_kfoldPerformance, file="KGF_9Clusters_1__kfold.csv",row.names = F,sep=',')
write.table(caret.rf_preds_actual, file="KGF_9Clusters_1__predsByactual.csv",row.names = F,sep=',',col.names = c('predicted',"actual"))
write.table(caret.rf_variable_importance, file="KGF_9Clusters_1__variable_importance.csv",row.names = T,sep=',')

pdf('KGF_9Clusters_1__mtry_plot.pdf')#saves plot of training hyperparameters 
plot(caret.rf)
dev.off()
pdf('KGF_9Clusters_1__oob_performance_plot.pdf')
plot(resampleHist((caret.rf))) #Density plots
dev.off()


