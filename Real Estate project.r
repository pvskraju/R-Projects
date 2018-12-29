
#Loading the data set to environment
RealEstate <- read.csv("E:\\Programing Code\\R Code\\Real Estate Project\\Input Files\\RealEstate.csv",
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  strip.white = TRUE ,
                  sep = ',', na.strings = c("","NA"))

str(RealEstate)

head(RealEstate)

#Identification missing values through graphical representation.
install.packages("Amelia")
library(Amelia)

# visualization of missing values through Map
missmap(RealEstate, main = "Missing values vs observed")

# To print a logical vector that indicates complete and missing rows 
RealEstate[!complete.cases(RealEstate),]

#To view the count of missing and nin missing values
table(is.na(RealEstate))



# ******************************* Data Cleaning ********************************************

# Seperate data  set is created for 
RealEstate.df <- data.frame(RealEstate)
RealEstate.df


#Instaling required packages
install.packages("dplyr")
library(dplyr)


#Altering the price column
RealEstate.df1 <- mutate(RealEstate.df, altPrice= round(Price/100000),2)
str(RealEstate.df1)

#Removing Blanks from Price column)(Price is considered as prediting variable)
RealEstate.pr <- subset(RealEstate.df1, altPrice >0 )
str(RealEstate.pr)

#cross checcking the for the NA values in Price Column
table(is.na(RealEstate.pr$altPrice))

#to know how much % and how many values are missing
sum(is.na(RealEstate.pr))
mean(is.na(RealEstate.pr))

#****************************** Imputation of Missing Values************************************

install.packages("VIM")
library(VIM)
install.packages("Amelia")
library(Amelia)

#identifying the missing values before imputation
aggr(RealEstate.pr, prop = F, numbers = T)

#creating mode function for replacing the NA's in Bedroomand & Bathrooms columns.

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}


#Identifying the mode for Bedroom & Bathroom columns
getmode(RealEstate.pr$Bedroom2)
getmode(RealEstate.pr$Bathroom)

#Repalcing the NA's in Bedroom and bathroom columns with mode 

RealEstate.pr$Bedroom2[is.na(RealEstate.pr$Bedroom2)] <- 3
RealEstate.pr$Bathroom[is.na(RealEstate.pr$Bathroom)] <- 1

#Repalcing the NA's in Landsize and Building Area columns with Mean
RealEstate.pr$Landsize[is.na(RealEstate.pr$Landsize)] <- mean(RealEstate.pr$Landsize,na.rm=T)
RealEstate.pr$BuildingArea[is.na(RealEstate.pr$BuildingArea)] <- mean(RealEstate.pr$BuildingArea,na.rm=T)

#Cross checking the data after repalcing the NA's
table(is.na(RealEstate.pr$Bedroom2))
table(is.na(RealEstate.pr$Bathroom))
table(is.na(RealEstate.pr$Landsize))
table(is.na(RealEstate.pr$BuildingArea))

#identifying the missing values after imputation with plot
aggr(RealEstate.pr, prop = F, numbers = T)

#not replacing the NA's in Car,Year built,Council Area,Lattitude and langtitude columns
table(is.na(RealEstate.pr$Car))
table(is.na(RealEstate.pr$YearBuilt))
table(is.na(RealEstate.pr$CouncilArea))


table(is.na(RealEstate.pr$Lattitude))
table(is.na(RealEstate.pr$Longtitude))



#*******************************************Analysis of Data************************

#Univariate Analysis

# Measures of central tendancy:
#    Mean
#    Median
#    Mode
#Measures of dispersion:
#    Min
#    Max
#    Range
#    Quartiles
#    Variance
#    Standard deviation
#Other measures include:
#    Skewness
#    Kurtosis


install.packages("psych")
library(psych)

#*****************************Univariate Analysis ********************************

df1 <- describe(RealEstate.pr$altPrice)
df2 <- describe(RealEstate.pr$Rooms)
df3 <- describe(RealEstate.pr$Landsize)
df4 <- describe(RealEstate.pr$BuildingArea)

Univariate_variables <- c("Price","Rooms","Landsize","BuildingArea")
Univariate_test <- c("vars","n","mean","sd",
                  "median","trimmed","mad","min",
                  "max","range","skew","kurtosis","se")

cal_univariate <- matrix(c(df1,df2,df3,df4),
                           nrow = 13, byrow = FALSE, 
                           dimnames = list(Univariate_test,Univariate_variables))

cal_univariate

#Histogram
hist(RealEstate.pr$altPrice,
     xlab = "Price",
     col = "green",
     border = "red", 
     xlim = c(0,80),
     ylim = c(0,5000))

#Box Plot

boxplot(RealEstate.pr$altPrice,
        main = toupper("Boxplot for  Price"),
        ylab = "Frequency",
        col = "blue")

#Kernal Density
Ker.pr <- density(RealEstate.pr$altPrice, na.rm=TRUE)
plot(Ker.pr, main = "Kernel density of Price")
polygon(Ker.pr, col = "green", border = "blue")

#Chi- Square Test.
# Between 2 groups

RealChiSquare = table(RealEstate.pr$Type, RealEstate.pr$altPrice) 
print(RealChiSquare)

print(chisq.test(RealChiSquare))

# Chi-squared test
# continuity correction
chisq.test(RealEstate.pr$Type, RealEstate.pr$altPrice, correct=FALSE) 

# Chi-squared test
# continuity correction
chisq.test(RealEstate.pr$Type, RealEstate.pr$altPrice, correct=TRUE) 



#**********************************Bivariate analysys************************************
print(str(RealEstate.pr))


# t - tests

#Price
t.test(RealEstate.pr$altPrice[RealEstate.pr$Type=="h"],
       RealEstate.pr$altPrice[RealEstate.pr$Type =="t"])

t.test(RealEstate.pr$altPrice[RealEstate.pr$Type=="t"],
       RealEstate.pr$altPrice[RealEstate.pr$Type =="u"])

t.test(RealEstate.pr$altPrice[RealEstate.pr$Type=="h"],
       RealEstate.pr$altPrice[RealEstate.pr$Type =="u"])



#Distance

t.test(RealEstate.pr$Distance[RealEstate.pr$Type=="h"],
       RealEstate.pr$Distance[RealEstate.pr$Type =="t"])

t.test(RealEstate.pr$Distance[RealEstate.pr$Type=="t"],
       RealEstate.pr$Distance[RealEstate.pr$Type =="u"])

t.test(RealEstate.pr$Distance[RealEstate.pr$Type=="h"],
       RealEstate.pr$Distance[RealEstate.pr$Type =="u"])

#Rooms

t.test(RealEstate.pr$Rooms[RealEstate.pr$Type=="h"],
       RealEstate.pr$Rooms[RealEstate.pr$Type =="t"])

t.test(RealEstate.pr$Rooms[RealEstate.pr$Type=="t"],
       RealEstate.pr$Rooms[RealEstate.pr$Type =="u"])

t.test(RealEstate.pr$Rooms[RealEstate.pr$Type=="h"],
       RealEstate.pr$Rooms[RealEstate.pr$Type =="u"])

#Landsize

t.test(RealEstate.pr$Landsize[RealEstate.pr$Type=="h"],
       RealEstate.pr$Landsize[RealEstate.pr$Type =="t"])

t.test(RealEstate.pr$Landsize[RealEstate.pr$Type=="t"],
       RealEstate.pr$Landsize[RealEstate.pr$Type =="u"])

t.test(RealEstate.pr$Landsize[RealEstate.pr$Type=="h"],
       RealEstate.pr$Landsize[RealEstate.pr$Type =="u"])


t.test(RealEstate.pr$Landsize,RealEstate.pr$Type=="h")
t.test(RealEstate.pr$Landsize,RealEstate.pr$Type=="t")
t.test(RealEstate.pr$Landsize,RealEstate.pr$Type=="u")

# t - test (Paired)

t.test(RealEstate.pr$altPrice,  RealEstate.pr$Rooms, paired=TRUE)
t.test(RealEstate.pr$altPrice,  RealEstate.pr$Distance, paired=TRUE)
t.test(RealEstate.pr$altPrice,  RealEstate.pr$Bedroom2, paired=TRUE)
t.test(RealEstate.pr$altPrice,  RealEstate.pr$Landsize, paired=TRUE)
t.test(RealEstate.pr$altPrice,  RealEstate.pr$BuildingArea, paired=TRUE)
t.test(RealEstate.pr$altPrice,  RealEstate.pr$YearBuilt, paired=TRUE)

Reg.DataSet <- data.frame(RealEstate.pr$altPrice,RealEstate.pr$Rooms,RealEstate.pr$Distance,RealEstate.pr$Bedroom2,RealEstate.pr$Bathroom,RealEstate.pr$Landsize,RealEstate.pr$BuildingArea)
str(Reg.DataSet)
round(cor(Reg.DataSet),2)

RealEst.lmMod <- lm(RealEstate.pr.altPrice ~ .,Reg.DataSet)
summary(RealEst.lmMod)

#*************************checking multicollinearity***********************
install.packages("faraway")
library(faraway)

#multicollinearity
round(vif(RealEst.lmMod),2)
#values (VIF- Varience inflation factor)in the output is below 5 for all the variables ,so there is no multi collinarity exist in between the x variables.

#************************ANOVA (one-way ANOVA model)**********************

library(dplyr)

aov(RealEstate.pr$altPrice ~ RealEstate.pr$Type, data=RealEstate.pr)
summary(aov(RealEstate.pr$altPrice ~ RealEstate.pr$Type, data=RealEstate.pr))


aov(RealEstate.pr$altPrice ~ RealEstate.pr$Method, data=RealEstate.pr) 
summary(aov(RealEstate.pr$altPrice ~ RealEstate.pr$Method, data=RealEstate.pr))

    

aov(RealEstate.pr$altPrice ~ RealEstate.pr$SellerG, data=RealEstate.pr)
summary(aov(RealEstate.pr$altPrice ~ RealEstate.pr$SellerG, data=RealEstate.pr))

aov(RealEstate.pr$altPrice ~ RealEstate.pr$Distance, data=RealEstate.pr)
summary(aov(RealEstate.pr$altPrice ~ RealEstate.pr$Distance, data=RealEstate.pr))

aov(RealEstate.pr$altPrice ~ RealEstate.pr$Landsize, data=RealEstate.pr)
summary(aov(RealEstate.pr$altPrice ~ RealEstate.pr$Landsize, data=RealEstate.pr))

aov(RealEstate.pr$altPrice ~ RealEstate.pr$Rooms, data=RealEstate.pr)
summary(aov(RealEstate.pr$altPrice ~ RealEstate.pr$Rooms, data=RealEstate.pr))

# Tukey multiple pairwise-comparisons

paired.anova.test <- aov(altPrice ~ Method * Type, data = RealEstate.pr)
summary(paired.anova.test)

paired.anova.test1 <- aov(altPrice ~ SellerG * Distance, data = RealEstate.pr)
summary(paired.anova.test1)

paired.anova.test2 <- aov(altPrice ~ Bedroom2 * Bathroom, data = RealEstate.pr)
summary(paired.anova.test2)

paired.anova.test3 <- aov(altPrice ~ Landsize * BuildingArea, data = RealEstate.pr)
summary(paired.anova.test3)

paired.anova.test4 <- aov(altPrice ~ Rooms * BuildingArea, data = RealEstate.pr)
summary(paired.anova.test4)



#*********************** Regression*********************************
#Finding Correlation and Covarience in between the variables

library(dplyr)

set.seed(100) 
trainingRowIndex <- sample(1:nrow(RealEstate.pr), 0.8*nrow(RealEstate.pr))
RealEst.trainingData <- RealEstate.pr[trainingRowIndex, ] 
RealEst.testData  <- RealEstate.pr[-trainingRowIndex, ]   

str(RealEst.trainingData)

#Finding Correlation and Covarience in between the variables.

# Train Data

Price.Landsize.cov <- cov(RealEst.trainingData$altPrice,RealEst.trainingData$Landsize)
Price.Landsize.cor <- cor(RealEst.trainingData$altPrice,RealEst.trainingData$Landsize)

Price.Rooms.cov <- cov(RealEst.trainingData$altPrice,RealEst.trainingData$Rooms)
Price.Rooms.cor <- cor(RealEst.trainingData$altPrice,RealEst.trainingData$Rooms)


Price.Bedrooms.cov <- cov(RealEst.trainingData$altPrice,RealEst.trainingData$Bedroom2)
Price.Bedrooms.cor <- cor(RealEst.trainingData$altPrice,RealEst.trainingData$Bedroom2)


Price.BuiltArea.cov <- cov(RealEst.trainingData$altPrice,RealEst.trainingData$BuildingArea)
Price.BuiltArea.cor <- cor(RealEst.trainingData$altPrice,RealEst.trainingData$BuildingArea)

col<- c("Covarience","Correlation")
row <- c("Price.Landsize","Price.Rooms","Price.Bedrooms","Price.BuiltArea")

RealEst.High.cor <- matrix(c(Price.Landsize.cov,Price.Landsize.cor,Price.Rooms.cov,
        Price.Rooms.cor,Price.Bedrooms.cov,Price.Bedrooms.cor,Price.BuiltArea.cov,Price.BuiltArea.cor),
        nrow = 4, byrow = TRUE, dimnames = list(row , col))

print(RealEst.High.cor)

str(RealEst.testData)

#Correlation of Test data.

Price.Landsize.cov.t <- cov(RealEst.testData$altPrice,RealEst.testData$Landsize)
Price.Landsize.cor.t <- cor(RealEst.testData$altPrice,RealEst.testData$Landsize)

Price.Rooms.cov.t <- cov(RealEst.testData$altPrice,RealEst.testData$Rooms)
Price.Rooms.cor.t <- cor(RealEst.testData$altPrice,RealEst.testData$Rooms)


Price.Bedrooms.cov.t <- cov(RealEst.testData$altPrice,RealEst.testData$Bedroom2)
Price.Bedrooms.cor.t <- cor(RealEst.testData$altPrice,RealEst.testData$Bedroom2)


Price.BuiltArea.cov.t <- cov(RealEst.testData$altPrice,RealEst.testData$BuildingArea)
Price.BuiltArea.cor.t <- cor(RealEst.testData$altPrice,RealEst.testData$BuildingArea)

col.t<- c("Covarience","Correlation")
row.t <- c("Price.Landsize","Price.Rooms","Price.Bedrooms","Price.BuiltArea")

RealEst.High.cor.t <- matrix(c(Price.Landsize.cov.t,Price.Landsize.cor.t,Price.Rooms.cov.t,
                             Price.Rooms.cor.t,Price.Bedrooms.cov.t,Price.Bedrooms.cor.t,Price.BuiltArea.cov.t,Price.BuiltArea.cor.t),
                           nrow = 4, byrow = TRUE, dimnames = list(row.t , col.t))

print(RealEst.High.cor.t)

#Linear Model
RealEst.linmod <- lm(RealEstate.pr$altPrice~ RealEstate.pr$Rooms, data=RealEstate.pr)
print(RealEst.linmod)

summary(RealEst.linmod)

#Plot
x = 0:100
Y = -77.99+ ( 3809.88*x)
print(Y)
plot(x,Y,lwd=2,typ="l")



# Build the model on training data -
RealEst.lm.testd <- lm(altPrice ~ Rooms, data=RealEst.testData) 
print(RealEst.lm.testd)
distPred <- predict(RealEst.lm.testd, RealEst.testData)
print(distPred )
print(testData)

data.frame(altPrice = RealEst.testData$altPrice , Rooms = RealEst.testData$Rooms, fitted = distPred)

data.frame(altPrice = RealEst.testData$altPrice , Rooms = RealEst.testData$Rooms,fitted = distPred , Difference = (distPred/RealEst.testData$altPrice)/100)

plot(RealEstate.pr$altPrice ~ RealEstate.pr$Rooms,col = "red",main = "Regression")
abline(lm(RealEstate.pr$Rooms ~ RealEstate.pr$Rooms))


library(ggplot2)
ggplot(RealEst.lm.testd, aes(x=Rooms,y=altPrice, color="red")) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Rooms") +
  xlab("altPrice") +
  ggtitle("Price Vs Rooms")



#********************************* Logistic Regression*****************************

#seperating the data based on Types (h,t,u)

#seperated data set based on types = h and t
logit.data.ht <- subset(RealEstate.pr, Type %in% c("h","t"))
str(logit.data.ht)


#added binary variable (0,1) for type h and t
logit.data.ht$Type [logit.data.ht$Type == "h"] <- 1
logit.data.ht$Type [logit.data.ht$Type == "t"] <- 0
logit.data.ht$Binary.ht <- as.integer(logit.data.ht$Type)
Binary.ht=ifelse(logit.data.ht$Type=="h",1,0)
logit.data.ht

str(logit.data.ht)



#seperated data set based on types = t and u
logit.data.tu <- subset(RealEstate.pr, Type %in% c("t","u"))
str(logit.data.tu)

#added binary variable (0,1) for type t and u
logit.data.tu$Type [logit.data.tu$Type == "t"] <- 1
logit.data.tu$Type [logit.data.tu$Type == "u"] <- 0
logit.data.tu$Binary.tu <- as.integer(logit.data.tu$Type)
Binary.tu=ifelse(logit.data.tu$Type=="t",1,0)
logit.data.tu

str(logit.data.tu)



#seperated data set based on types = h and u
logit.data.hu <- subset(RealEstate.pr, Type %in% c("h","u"))
str(logit.data.hu)

#added binary variable (0,1) for type h and u
logit.data.hu$Type [logit.data.hu$Type == "h"] <- 1
logit.data.hu$Type [logit.data.hu$Type == "u"] <- 0
logit.data.hu$Binary.hu <- as.integer(logit.data.hu$Type)
Binary.hu=ifelse(logit.data.hu$Type=="h",1,0)
logit.data.hu

str(logit.data.hu)


##***********************************************Logistic Model*************************
    

head(logit.data.ht)
factorA <- factor(logit.data.ht$Binary.ht)
lv <- levels(factorA)
lv

#(h=1 & t=0)(t=1 & u=0) (h=1 & u=0)
table(logit.data.ht$Binary.ht)
table(logit.data.tu$Binary.tu)
table(logit.data.hu$Binary.hu)


logit.ht <- glm(Binary.ht ~ altPrice, 
                data=logit.data.ht, 
                family=binomial(link="logit"))

logit.tu <- glm(Binary.tu ~ altPrice, 
                data=logit.data.tu, 
                family=binomial(link="logit"))

logit.hu <- glm(Binary.hu ~ altPrice, 
                data=logit.data.hu, 
                family=binomial(link="logit"))        


summary(logit.ht)


summary(logit.tu)


summary(logit.hu) 

#Prediction
predicted.ht <- predict(logit.ht, logit.data.ht, type="response")
predicted.tu <- predict(logit.tu, logit.data.tu, type="response")
predicted.hu <- predict(logit.hu, logit.data.hu, type="response")

#confusion matrix
##(h=1 & t=0)(t=1 & u=0) (h=1 & u=0)

table(logit.data.ht$Binary.ht, predicted.ht > 0.8)
table(logit.data.tu$Binary.tu, predicted.tu > 0.8)
table(logit.data.hu$Binary.hu, predicted.hu > 0.8)


install.packages("InformationValue")
library(InformationValue)

plotROC(logit.data.ht, predicted.ht)

set.seed(100) 
trainingRowIndex <- sample(1:nrow(logit.data.ht), 0.6*nrow(logit.data.ht))
RealLogit.trainingData <- logit.data.ht[trainingRowIndex, ] 
RealLogit.testData  <- logit.data.ht[-trainingRowIndex, ]

str(RealLogit.trainingData)

#sigmoid curve

fit = glm(Binary.ht ~ altPrice, data=RealLogit.trainingData, family=binomial)


newdat <- data.frame(
  altPrice=seq(min(RealLogit.trainingData$altPrice), max(RealLogit.trainingData$altPrice),
    
               len=100))

#newdat

newdat$Binary.ht = predict(fit, newdata=newdat, type="response")

plot(Binary.ht ~ altPrice, data=RealLogit.trainingData, col="red4")

lines(Binary.ht ~ altPrice, newdat, col="green4", lwd=2)



#************************* Support Vector Machine*****************************

install.packages("e1071")
library(e1071)

str(RealEstate.pr)

RealEst.svm.df <- data.frame(RealEstate.pr$altPrice,RealEstate.pr$Type,RealEstate.pr$Rooms,RealEstate.pr$Distance,RealEstate.pr$Bedroom2,RealEstate.pr$Bathroom,RealEstate.pr$Landsize,RealEstate.pr$BuildingArea)

str(RealEst.svm.df)

x <- subset(RealEst.svm.df,select = -RealEstate.pr.Type)
y <- subset(RealEst.svm.df,select = RealEstate.pr.Type)

head(x)

head(y)

RealEst.svm <- svm(RealEstate.pr.Type ~ ., data = RealEst.svm.df)
summary(RealEst.svm)

predx <- predict(RealEst.svm, x)
predx
#table(pred,Real.svm.df)

data(RealEst.svm.df)
RealEst.svm1 <- svm(RealEstate.pr.Type ~ RealEstate.pr.altPrice + RealEstate.pr.Rooms, data = RealEst.svm.df, kernel = "radial")
plot(RealEst.svm.df$RealEstate.pr.altPrice,RealEst.svm.df$RealEstate.pr.Rooms , col = as.integer(RealEst.svm.df[, 2]), 
     pch = c("o","+")[1:150 %in% RealEst.svm1$index + 1], cex = 2,
     xlab = "Price", ylab = "Rooms")
legend(x = 3.37, y=7.5, legend = c("Price", "Rooms"),fill = c('blue','red'))

RealEst.svm1

data(RealEst.svm.df)
RealEst.svm2 <- svm(RealEstate.pr.Type ~ RealEstate.pr.Bedroom2 + RealEstate.pr.Bathroom, data = RealEst.svm.df, kernel = "radial")
RealEst.svm2

plot(RealEst.svm.df$RealEstate.pr.Bedroom2,RealEst.svm.df$RealEstate.pr.Bathroom , col = as.integer(RealEst.svm.df[, 2]), 
     pch = c("o","+")[1:150 %in% RealEst.svm2$index + 1], cex = 2, 
     xlab = "Bedroom2", ylab = "Bathroom")
legend(x = 3.37, y=7.5, legend = c("Bedroom2","Bathroom"),fill = c('blue','red'))

data(RealEst.svm.df)
RealEst.svm3 <- svm(RealEstate.pr.Type ~ RealEstate.pr.Distance + RealEstate.pr.Landsize, data = RealEst.svm.df, kernel = "radial")
plot(RealEst.svm.df$RealEstate.pr.Distance,RealEst.svm.df$RealEstate.pr.Landsize , col = as.integer(RealEst.svm.df[,2 ]), 
     pch = c("o","+")[1:150 %in% RealEst.svm3$index + 1], cex = 2, 
     xlab = "Distance", ylab = "Landsize")
legend(x = 3.37, y=7.5, legend = c("Distance","Landsize"),fill = c('blue','red'))

data(RealEst.svm.df)
RealEst.svm4 <- svm(RealEstate.pr.Type ~ RealEstate.pr.Landsize + RealEstate.pr.BuildingArea, data = RealEst.svm.df, kernel = "radial")
plot(RealEst.svm.df$RealEstate.pr.Landsize,RealEst.svm.df$RealEstate.pr.BuildingArea, col = as.integer(RealEst.svm.df[,2 ]), 
     pch = c("o","+")[1:150 %in% RealEst.svm4$index + 1], cex = 2, 
     xlab = "Landsize", ylab = "BuildingArea")
#legend(x = 3.37, y=7.5, legend = c("Landsize","BuildingArea"),fill = c('blue','red'))


#install.packages("e1071")
library(e1071)
str(RealEst.svm.df)

plot(RealEst.svm1, RealEst.svm.df, RealEstate.pr.altPrice ~ RealEstate.pr.Rooms, 
      col = c("orange","green","blue"))

plot(RealEst.svm1, RealEst.svm.df, RealEstate.pr.altPrice ~ RealEstate.pr.Rooms, 
      col = c("orange","green","yellow")) 
points(RealEst.svm.df[RealEst.svm1$index,c(1,2,3)],col="red",cex=2)



plot(RealEst.svm2, RealEst.svm.df, RealEstate.pr.Bedroom2 ~ RealEstate.pr.Bathroom, 
      col = c("orange","green","blue"))

plot(RealEst.svm3, RealEst.svm.df, RealEstate.pr.Landsize ~ RealEstate.pr.Distance, 
      col = c("orange","green","blue"))

plot(RealEst.svm3, RealEst.svm.df, RealEstate.pr.Landsize ~ RealEstate.pr.BuildingArea, 
      col = c("orange","green","blue"))



#***************************** Decision Tree*****************************
install.packages("party")
library(party)


install.packages("partykit")
library(partykit)

install.packages("rpart")
library(rpart)


install.packages("rattle")
library(rattle)

#Party

RealEst.ctree.ht <- ctree(altPrice ~ Binary.ht, data=logit.data.ht)
print(RealEst.ctree.ht)

plot(as.simpleparty(RealEst.ctree.ht), type="simple")
#H - 1 & T = 0) 

RealEst.ctree.tu <- ctree(altPrice ~ Binary.tu, data=logit.data.tu)
print(RealEst.ctree.tu)

plot(as.simpleparty(RealEst.ctree.tu), type="simple")
#t - 1 & u = 0) 

RealEst.ctree.hu <- ctree(altPrice ~ Binary.hu, data=logit.data.hu)
print(RealEst.ctree.hu)

plot(as.simpleparty(RealEst.ctree.hu), type="simple")
#t - 1 & u = 0) 

#Party

RealEst.ctree.ht.rdlb <- ctree(altPrice ~ Rooms + Distance + Landsize + BuildingArea, data=logit.data.ht)
print(RealEst.ctree.ht.rdlb)

plot(as.simpleparty(RealEst.ctree.ht.rdlb), type="simple")

#Party

RealEst.ctree.tu.rdlb <- ctree(altPrice ~ Rooms + Distance + Landsize + BuildingArea, data=logit.data.tu)
print(RealEst.ctree.tu.rdlb)

plot(as.simpleparty(RealEst.ctree.tu.rdlb), type="simple")
#t - 1 & u = 0) 

#Party

RealEst.ctree.hu.rdlb <- ctree(altPrice ~ Rooms + Distance + Landsize + BuildingArea, data=logit.data.hu)
print(RealEst.ctree.hu.rdlb)

plot(as.simpleparty(RealEst.ctree.tu.rdlb), type="simple")
#H - 1 & u = 0) 

#*********************************************************************************************************************

#*****************************Google Map - Plotting ********************************************************

install.packages("RgoogleMaps")
library(RgoogleMaps)

install.packages("ggplot2")
library(ggplot2)

install.packages("ggmap")
library(ggmap)

install.packages("googleVis")
library(googleVis)

install.packages("devtools")
library(devtools)

install.packages("tidyverse")
library(tidyverse)

api_key <- "AIzaSyClJVqN08I1oNYLYRw0mh_HCHP9rYoPZCw"

GmapData <- read.csv("E:\\Programing Code\\R Code\\Real Estate Project\\Input Files\\GmapData.csv",
                       header = TRUE,
                       stringsAsFactors = FALSE,
                       strip.white = TRUE ,
                       sep = ',', na.strings = c("","NA"))

str(GmapData)

head(GmapData)

GmapData$Location <- gsub(",",":",GmapData$Location)
require(googleVis)
placeNames <- as.character(GmapData$Cities)
plotData <- data.frame(name = placeNames,LatLong = unlist(GmapData$Location), 
                       stringsAsFactors = FALSE)
sites <- gvisMap(plotData,locationvar = "LatLong",tipvar="name",
                 options=list(displayMode = "Markers", mapType= 'normal',
                              colorAxis = "{colors:['red','grey']}",
                 useMapTypeControl=TRUE,enableScrollWheel = 'TRUE')) 

plot(sites)


