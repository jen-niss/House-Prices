#Modeling House Prices

#install packages
install.packages("readr")
install.packages("randomForest")
install.packages("caret")
library(readr)
library(randomForest)
library(caret)

#read in data
train <-read.csv("train.csv",stringsAsFactors = FALSE)
test <- read.csv("test.csv",stringsAsFactors = FALSE)
str(train)

<<<<<<< HEAD

=======
>>>>>>> e4a79ce50ec2c32c86b4f1ca8686f6a6a0952280
#Extract certain features with N/A removed and such
extractFeatures <- function(data) {
  features <- c("LotFrontage",
                "LotArea",
                "MSSubClass",
                "YearBuilt",
                "GarageCars",
                "GarageArea",
                "FullBath",
                "GrLivArea",
                "X1stFlrSF",
                "X2ndFlrSF"
                
  )
  fea <- data[,features]
  fea$LotFrontage[is.na(fea$LotFrontage)] <- median(fea$LotFrontage, na.rm=TRUE)
  fea$LotArea[is.na(fea$LotArea)] <- median(fea$LotArea, na.rm=TRUE)
  fea$MSSubClass[is.na(fea$MSSubClass)] <- median(fea$MSSubClass, na.rm=TRUE)
  fea$YearBuilt[is.na(fea$YearBuilt)] <- median(fea$YearBuilt, na.rm=TRUE)
  fea$GarageCars[is.na(fea$GarageCars)] <- median(fea$GarageCars, na.rm=TRUE)
<<<<<<< HEAD
  fea$GarageArea[is.na(fea$GarageArea)] <- median(fea$GarageArea, na.rm=TRUE)
=======
  fea$LotArea[is.na(fea$GarageArea)] <- median(fea$GarageArea, na.rm=TRUE)
>>>>>>> e4a79ce50ec2c32c86b4f1ca8686f6a6a0952280
  fea$FullBath[is.na(fea$FullBath)] <- median(fea$FullBath, na.rm=TRUE)
  fea$GrLivArea[is.na(fea$GrLivArea)] <- median(fea$GrLivArea, na.rm=TRUE)
  
  #Make sure 2nd floor is not bigger than first floor when using median
  fea$X1stFlrSF[is.na(fea$X1stFlrSF)] <- median(fea$X1stFlrSF, na.rm=TRUE)
  fea$X2ndFlrSF[is.na(fea$X2ndFlrSF)] <- median(fea$X2ndFlrSF, na.rm=TRUE) 
  return(fea)
}

<<<<<<< HEAD
#create the random forest model
rf <- randomForest(extractFeatures(train), as.factor(train$SalePrice), ntree=100, importance=TRUE)

#Create submission csv
submission <- data.frame(Id = test$Id)
submission$SalePrice <- predict(rf, extractFeatures(test))
write.csv(submission, file = "house_random_forest_r_submission.csv", row.names=FALSE)

any(is.na(extractFeatures(test)$GarageArea))
any(is.na(extractFeatures(train)))

=======
#create random forest model
rf <- randomForest(extractFeatures(train), as.factor(train$SalePrice), ntree=100, importance=TRUE)

#Create submission csv
submission <- data.frame(PassengerId = test$Id)
submission$SalePrice <- predict(rf, extractFeatures(test))
write.csv(submission, file = "house_random_forest_r_submission.csv", row.names=FALSE)

>>>>>>> e4a79ce50ec2c32c86b4f1ca8686f6a6a0952280
#importance plot
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))
p

