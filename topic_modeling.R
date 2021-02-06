#---------------------------------LDA : TopicModels-------------------------------------------#
#install.packages('knitr')
library(RTextTools)
library(e1071)
library(topicmodels)
library(tm)
library(slam)
library(tidytext)
library(stringr)
library(tidyr)
library(topicmodels)
library(dplyr)
#install.packages('devtools')
library(devtools)
library(knitr)

slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
#install_url(slam_url)
library(slam)
data(NYTimes)
data <- NYTimes[sample(1:3100,size=1000,replace=FALSE),]
data$ID <- seq.int(nrow(data))

dim(data)
head(data)

#Create a Document Term Matrix

matrix= create_matrix(cbind(as.vector(data$Title),as.vector(data$Subject)), 
                      language="english",removeNumbers=TRUE, removePunctuation=TRUE, 
                      removeSparseTerms=0, 
                      removeStopwords=TRUE, stripWhitespace=TRUE, toLower=TRUE)

inspect(matrix[1:10,1:10])

#Choose number of topics already existing
k <- length(unique(data$Topic.Code))

#Divide Data into training and testing

train <- matrix[1:700,]
test <- matrix[701:1000,]

#Building model on train data
train.lda <- LDA(train,k)

get_topics(train.lda,5)
get_terms(train.lda,5)

topics(train.lda)
terms(train.lda)

setwd("E:\\Text mining\\lda")
write.csv(data.frame(get_terms(train.lda,27)),"terms_nytimes.csv",row.names=F)

#Get the top topics 
train.topics <- topics(train.lda)

#Testing the model
test.topics <- posterior(train.lda,test)

test.topics$topics[1:10,1:5]
test.topics <- apply(test.topics$topics, 1, which.max)

#Joining the predicted Topic number to the original test Data
test<-data[701:1000,]
final<-data.frame(Title=test$Title,Subject=test$Subject,Pred_topic=test.topics)
View(final)
#Analysis
table(final$Pred_topic)
View(final[final$Pred_topic==22,])

##--------------------Another method to choose the optimal number of topics ---------#

#------Checking best number of topics--------#

library(topicmodel)
best.model <- lapply(seq(2,10, by=1), function(k){LDA(matrix,k)})

best_model<- as.data.frame(as.matrix(lapply(best.model, logLik)))
final_best_model <- data.frame(topics=c(seq(2,10, by=1)), 
                               log_likelihood=as.numeric(as.matrix(best_model)))

head(final_best_model)
library(ggplot2)
with(final_best_model,qplot(topics,log_likelihood,color="red"))

#Based on the graph, we can choose the best model
k=final_best_model[which.max(final_best_model$log_likelihood),1]

cat("Best topic number is k=",k)


