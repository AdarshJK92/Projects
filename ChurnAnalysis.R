library(corrplot)
library(randomForest)



churn <- read.csv('CustomerChurn.csv')
str(churn)


sapply(churn, function(x) sum(is.na(x)))
churn <- churn[complete.cases(churn), ]


cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
churn[,cols_recode1][,i] <- as.factor(mapvalues
(churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}


churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
from=c("No phone service"),
to=c("No")))


group_tenure <- function(tenure){
if (tenure >= 0 & tenure <= 12){
return('0-12 Month')
}else if(tenure > 12 & tenure <= 24){
return('12-24 Month')
}else if (tenure > 24 & tenure <= 48){
return('24-48 Month')
}else if (tenure > 48 & tenure <=60){
return('48-60 Month')
}else if (tenure > 60){
return('> 60 Month')
}
}

churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)



churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
from=c("0","1"),
to=c("No", "Yes")))



churn$customerID <- NULL
churn$tenure <- NULL




churn.numeric<-as.data.frame(lapply(churn,as.numeric))
corrplot(cor(churn.numeric, method="spearman"))

churn$TotalCharges <- NULL



intrain<- createDataPartition(churn$Churn,p=0.75,list=FALSE)
set.seed(2019)
training<- churn[intrain,]
testing<- churn[-intrain,]



dim(training); dim(testing)

rfModel <- randomForest(Churn ~., data = training)


pred_rf <- predict(rfModel, testing)
confusionMatrix(pred_rf, testing$Churn)
