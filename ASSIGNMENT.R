download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile="training.csv")
download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile="testing.csv")
trainingsource<-read.csv("./training.csv")
testing<-read.csv("./testing.csv")
head(trainingsource)
#datacleaning
training_clean<-trainingsource[,colSums(is.na(trainingsource))==0]
testing_clean<-testing[,colSums(is.na(testing))==0]
head(training_clean)
names(training_clean)
#remove unrelevant variables
removelist<-c('x','user_name','raw_timestamp_part_a','raw_timestamp_part_2','cvtd_timestamp','new_window','num_window')
training_clean<-training_clean[,!names(training_clean)%in% removelist]
names(training_clean)
#check the variance of the the variables and clear the one with low varaibility
library(caret)
zero<-nearZeroVar(training_clean[sapply(training_clean,is.numeric)],saveMetrics=TRUE)
training_clean<-training_clean[,zero[,'nzv']==0]
#remove highly correlated variables
corre<-cor(na.omit(training_clean[sapply(training_clean,is.numeric)]))
removehighc<-findCorrelation(corre,cutoff=0.90,verbose=TRUE)
training_removed<-training_clean[,-removehighc]
#split data into k=5 folds
intrain<-createDataPartition(y=training_removed$classe, p=0.7,list=FALSE)
trainingset<-training_removed[intrain,]
testingset<-training_removed[-intrain,]
fitcontrol<-trainControl(method="repeatedcv",number=5,repeats=10)
#data analysis using random forest
model_rf<-train(classe~.,method='rf',data=trainingset)

 #forecasting

value<-predict(model_rf,testing)

