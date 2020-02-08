#libraries

library(corrplot)
library(randomForest)
library(caret)
library(ggplot2)
library(DMwR)

getwd()

#Read the file into R

customerCredibility <- read.csv('German_credit.csv')
str(customerCredibility)

#Convert target variable into factor for further analysis
customerCredibility$Creditability<-as.factor(customerCredibility$Creditability)

#Check for missing values
sapply(customerCredibility, function(x) sum(is.na(x)))

#Map the dependent variable to understandable factors
customerCredibility$Creditability <- as.factor(mapvalues(customerCredibility$Creditability, 
                                           from=c("0","1"),
                                           to=c("Bad","Good")))
#Write the new values to a file
write.csv(customerCredibility,file = "newGerman_credit.csv")



#correlation plot
customerCredibility.numeric<-as.data.frame(lapply(customerCredibility,as.numeric))
corrplot(cor(customerCredibility.numeric, method="spearman"))

#removing factors which are insignificant
customerCredibility$Purpose<-NULL
customerCredibility$Guarantors<-NULL
customerCredibility$Type.of.apartment<-NULL
customerCredibility$Duration.in.Current.address<-NULL
customerCredibility$No.of.dependents<-NULL
customerCredibility$Occupation<-NULL
customerCredibility$Telephone<-NULL

#plotting correlation again to verify the above actions
customerCredibility.numeric<-as.data.frame(lapply(customerCredibility,as.numeric))
corrplot(cor(customerCredibility.numeric, method="spearman"))



#Splitting data into train and test for building model
intrain<- createDataPartition(customerCredibility$Creditability,p=0.75,list=FALSE)
set.seed(2019)
training<- customerCredibility[intrain,]
testing<- customerCredibility[-intrain,]
dim(training); dim(testing)

#Imbalance check
table(customerCredibility$Creditability)

#Applying Smote
balanced.data <- SMOTE(Creditability ~.,training, perc.over = 400, k = 5, perc.under = 125)
as.data.frame(table(balanced.data$Creditability))

#Model
rfModel <- randomForest(Creditability ~., importance = TRUE, data = training)
pred_rf <- predict(rfModel, testing)
confusionMatrix(pred_rf, testing$Creditability)

t <- tuneRF(training[, -14], training[, 14], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)
rfModel_new <- randomForest(Creditability ~., data = training, ntree = 200, mtry = 4, importance = TRUE, proximity = TRUE)
print(rfModel_new)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(rfModel$err.rate), times=3),
  Type=rep(c("OOB", "Bad", "Good"), each=nrow(rfModel$err.rate)),
  Error=c(rfModel$err.rate[,"OOB"],
          rfModel$err.rate[,"Bad"],
          rfModel$err.rate[,"Good"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

plot(rfModel)
var<-varImpPlot(rfModel)
