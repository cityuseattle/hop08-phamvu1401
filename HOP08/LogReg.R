#Logestic Regression
library(ggplot2)

#Read file and assign it to df.train variable
df.train <- read.csv('titanic_train.csv')
#View(head(df.train))
#print(str(df.train))

#Visualize the survived col
print(ggplot(df.train, aes(Survived)) + geom_bar())

#Visualize the Pclass col with colors
print(ggplot(df.train, aes(Pclass))+ geom_bar(aes(fill=factor(Pclass))))

#Visualize the gendr col
print(ggplot(df.train, aes(Sex)) + geom_bar(aes(fill=factor(Sex))))

#Visualize the Age col
print(ggplot(df.train, aes(Age)) + geom_histogram(bins=20, alpha=0.5, fill='blue'))

#Visualize age vs pclass 
p1 <- ggplot(df.train, aes(Pclass, Age))
p1 <- p1 + geom_boxplot(aes(group=Pclass, fill=factor(Pclass), alpha=0.4))
print(p1 + scale_y_continuous(breaks=seq(min(0),max(80), by=2)))

#Clean the data by adding values to the NA values in age col
 impute_age <- function(age,class){
     out <- age
     for (i in 1:length(age)){

         if (is.na(age[i])){

             if (class[i] == 1){
                 out[i] <- 37
             }else if (class[i] == 2){
                 out[i] <- 29
             }else{
                 out[i] <- 24
             }

         }else{
             out[i] <- age[i]
         }
     }
     return(out)
 }

 #Call the function 
 fixed.ages <- impute_age(df.train$Age, df.train$Pclass)

 #Assign the fixed to ages colm to the age col in df.train
 df.train$Age <- fixed.ages
 
 #If the output is false, then no NA values in the Age col
 print(any(is.na(df.train$Age)))

 #Remove the variable that we are not going to use
 library(dplyr)

 df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
 print(head(df.train))

 #Convert the data from integer to factor 
 df.train$Survived <- factor(df.train$Survived)
 df.train$Pclass <- factor(df.train$Pclass)
 df.train$Parch <- factor(df.train$Parch)
 df.train$SibSp <- factor(df.train$SibSp)

 library(caTools)
 set.seed(101)

 #Split to make test set out of training set
 split = sample.split(df.train$Survived, SplitRatio = 0.70)

 #1st subset is final.train
 #We will re run our model using final.train set
 final.train = subset(df.train, split == TRUE)

 #2nd subset is final.test
 final.test = subset(df.train, split == FALSE)

 #Train the model using final.train set 
 final.log.model <- glm(Survived ~ . , family=binomial(link='logit'), data=final.train)
 print(summary(final.log.model))

 #predict using final.log.model
 fitted.probabilities <- predict(final.log.model,newdata=final.test,type='response')
 fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

 misClasificError <- mean(fitted.results != final.test$Survived)
 print("Acuracy is equal to:")
 print(1-misClasificError)