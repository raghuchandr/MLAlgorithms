

rm(list=ls())

filename<-file.choose()
dsn<- read.csv(filename,header = TRUE
               ,colClasses =c("Class"="factor",
                              "Sex"="factor",
                              "Age"="factor",
                              "Survived"="factor")  )
  


 
?randomForest()
?importance()
?tuneRF()
 


 
 
dsn2<-na.omit(dsn)
set.seed(111)
 


index<-sort(sample(nrow(dsn2),round(.30*nrow(dsn2))))
training<-dsn[-index,]
test<-dsn[index,]

#install.packages('randomForest')
library(randomForest)
fit <- randomForest( Survived~., data=training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,4],Prediction)


wrong<- (test[,4]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

 
