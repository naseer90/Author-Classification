library(randomForest)
library(e1071)
library(MASS)
library(rpart)
library(lattice)
library(nnet)
library(cluster)
library(stats)
library(mclust)
library(psych)
library(class)
library(corrgram)

author<-read.table("author.txt",sep=",",header = T)
names(author)
author<-author[c(60,24,21,50,29,25,7,35,10,37,30,52,40,67,71)]
table(author$Author)
error<-function(x){((sum(x)-sum(diag(x)))/sum(x))*100}

##Colors
author$colour =  NA
author$colour[author$Author == "Austen"] = 'blue'
author$colour[author$Author == "London"] = 'red'
author$colour[author$Author == "Milton"] = 'green'
author$colour[author$Author == "Shakespeare"] = 'orange'

##Training and Test Sets
select7030<-c(32:45,111:138,148:151,217:244,296:317,354:368,411:428,442:446,466:473,542:571,601:613,646:659,665:668,678:681,692:696,708:712,724:728,737:740,752:756,763:766,779:783,791:793,807:811,822:826,837:841)
author.train<-author[-select7030,]
author.test<-author[select7030,]

author.numtr<-author.train[-c(16)]
author.numte<-author.test[-c(16)]

##Exploratory Analysis

##LDA
author.lda<-lda(Author~.,data=author.numtr)
print(author.lda)

author.ldapredicttrain<-predict(author.lda,author.numtr)
tldatrain<-table(author.numtr$Author,author.ldapredicttrain$class)
error(tldatrain)

author.ldapredicttest<-predict(author.lda,author.numte)
tldatest<-table(author.numte$Author,author.ldapredicttest$class)
error(tldatest)

plot(author.lda,col=author.train$colour)
plot(author.lda,dimen=1,type="both")
plot(author.lda,dimen=1,type="density")

##Random Forest
authorRF<-randomForest(Author~.,data=author.numtr,ntree=5000,importance=TRUE)
print(authorRF)

hist(treesize(authorRF))
importance(authorRF)
varImpPlot(authorRF)

author.predictrf<-predict(authorRF,author.numtr,type="class")
rftrain<-table(author.numtr$Author,author.predictrf)
error(rftrain)

author.predictrftest<-predict(authorRF,author.numte,type="class")
rftest<-table(author.numte$Author,author.predictrftest)
error(rftest)

#SVM
author.svm = svm(Author ~ ., data=author.numtr,kernel="linear")
summary(author.svm)

author.pred=predict(author.svm,author.numtr,decision.values=T)
ttrain = table(author.numtr$Author,author.pred)
error(ttrain)

author.predtestsvm = predict(author.svm,author.numte,decision.values=T)
ttest = table(author.numte$Author,author.predtestsvm)
error(ttest)

#Logistic Regression
lr = multinom(Author ~., data=author.numtr)
lr.train = predict(lr, author.numtr, type = "class")
error(table(author.numtr$Author,lr.train))

lr.test = predict(lr, author.numte, type = "class")
error(table(author.test$Author,lr.test))

#trees
author.tree = rpart(Author ~ ., data=author.numtr)
printcp(author.tree)
plotcp(author.tree)
plot(author.tree, margin = 0.1)
text(author.tree, use.n = T)


author.predict.train = predict(author.tree, author.numtr, type = "class")
ttreetrain = table(author.numtr$Author,author.predict.train)
error(ttreetrain)

author.predict.test = predict(author.tree, author.numte, type = "class")
ttreetest = table(author.numte$Author,author.predict.test)
error(ttreetest)

#PrunedTree1
author.treepruned1=prune(author.tree,cp=0.027624)
plot(author.treepruned1, margin = 0.1)
text(author.treepruned1, use.n = T)

author.predict.pruned1 = predict(author.treepruned1, author.numtr, type = "class")
ttreepruned1train = table(author.numtr$Author,author.predict.pruned1)
error(ttreepruned1train)

author.predict.pruned1.test = predict(author.treepruned1, author.numte, type = "class")
ttreepruned1test = table(author.numte$Author,author.predict.pruned1.test)
error(ttreepruned1test)

#KNN
Er= rep(0,258*3)
Erm = matrix(Er,3,258)
for ( i in 1:258)
{
  author.knntrain=knn(author.numtr[c(-14)],author.numtr[c(-14)],author.numtr$Author,k=i,prob=T)
  author.knntest=knn(author.numtr[c(-14)],author.numte[c(-14)],author.numtr$Author,k=i,prob=T)
  author.knncv=knn.cv(author.numtr[c(-14)],author.numtr$Author, k =i, prob = T)
  Erm[1,i]=error(table(author.numtr$Author, author.knntrain))
  Erm[2,i]=error(table(author.numte$Author, author.knntest))
  Erm[3,i]=error(table(author.numtr$Author, author.knncv))
  
}
which.min(Erm[1,])
which.min(Erm[2,])
which.min(Erm[3,])
min(Erm[1,])
min(Erm[2,])
min(Erm[3,])

plot.ts(Erm[1,], main="Errors", col = "red")
lines.ts(Erm[2,], col = "green")
lines.ts(Erm[3,], col = "Blue")


##### Random Forest per Two Classes Analysis #####



#Austen and London
AL.train<-subset(author.numtr,Author%in%c("Austen","London"))
AL.train$Author<-factor(AL.train$Author)
AL.test<-subset(author.numte,Author%in%c("Austen","London"))
AL.test$Author<-factor(AL.test$Author)

ALRandomForest<-randomForest(AL.train$Author~.,data=AL.train,ntree=5000,importance=TRUE)
print(ALRandomForest)

hist(treesize(ALRandomForest))
importance(ALRandomForest)
varImpPlot(ALRandomForest)

AL.predictrf<-predict(ALRandomForest,AL.train,type="class")
ALrftrain<-table(AL.train$Author,AL.predictrf)
error(ALrftrain)

AL.predictrftest<-predict(ALRandomForest,AL.test,type="class")
ALrftest<-table(AL.test$Author,AL.predictrftest)
error(ALrftest)

#Austen and Milton
AM.train<-subset(author.numtr, Author %in% c("Austen","Milton"))
AM.train$Author<-factor(AM.train$Author)
AM.test<-subset(author.numte, Author %in% c( "Austen","Milton"))
AM.test$Author<-factor(AM.test$Author)

AMRandomForest<-randomForest(AM.train$Author~.,data=AM.train,ntree=5000,importance=TRUE)
print(AMRandomForest)

hist(treesize(AMRandomForest))
importance(AMRandomForest)
varImpPlot(AMRandomForest)

AM.predictrf<-predict(AMRandomForest,AM.train,type="class")
AMrftrain<-table(AM.train$Author,AM.predictrf)
error(AMrftrain)

AM.predictrftest<-predict(AMRandomForest,AM.test,type="class")
AMrftest<-table(AM.test$Author,AM.predictrftest)
error(AMrftest)

#Austen and Shakespeare
AS.train<-subset(author.numtr,Author%in%c("Austen","Shakespeare"))
AS.train$Author<-factor(AS.train$Author)
AS.test<-subset(author.numte,Author%in%c("Austen","Shakespeare"))
AS.test$Author<-factor(AS.test$Author)

ASRandomForest<-randomForest(AS.train$Author~., data = AS.train,ntree=5000,importance=TRUE)
print(ASRandomForest)

hist(treesize(ASRandomForest))
importance(ASRandomForest)
varImpPlot(ASRandomForest)

AS.predictrf<-predict(ASRandomForest,AS.train,type="class")
ASrftrain<-table(AS.train$Author,AS.predictrf)
error(ASrftrain)

AS.predictrftest<-predict(ASRandomForest,AS.test,type="class")
ASrftest<-table(AS.test$Author,AS.predictrftest)
error(ASrftest)

#London and Milton
LM.train<-subset(author.numtr,Author%in%c("London","Milton"))
LM.train$Author<-factor(LM.train$Author)
LM.test<-subset(author.numte,Author%in%c("London","Milton"))
LM.test$Author<-factor(LM.test$Author)

LMRandomForest<-randomForest(LM.train$Author~.,data=LM.train,ntree=5000,importance=TRUE)
print(LMRandomForest)

hist(treesize(LMRandomForest))
importance(LMRandomForest)
varImpPlot(LMRandomForest)

LM.predictrf<-predict(LMRandomForest,LM.train,type="class")
LMrftrain<-table(LM.train$Author,LM.predictrf)
error(LMrftrain)

LM.predictrftest<-predict(LMRandomForest,LM.test,type="class")
LMrftest<-table(LM.test$Author,LM.predictrftest)
error(LMrftest)

#London and Shakespeare
LS.train<-subset(author.numtr,Author%in%c("London","Shakespeare"))
LS.train$Author<-factor(LS.train$Author)
LS.test<-subset(author.numte,Author%in%c("London","Shakespeare"))
LS.test$Author<-factor(LS.test$Author)

LSRandomForest<-randomForest(LS.train$Author~.,data=LS.train,ntree=5000,importance=TRUE)
print(LSRandomForest)

hist(treesize(LSRandomForest))
importance(LSRandomForest)
varImpPlot(LSRandomForest)

LS.predictrf<-predict(LSRandomForest,LS.train,type="class")
LSrftrain<-table(LS.train$Author,LS.predictrf)
error(LSrftrain)

LS.predictrftest<-predict(LSRandomForest,LS.test,type="class")
LSrftest<-table(LS.test$Author,LS.predictrftest)
error(LSrftest)

#Milton and Shakespeare
MS.train<-subset(author.numtr,Author%in%c("Milton","Shakespeare"))
MS.train$Author<-factor(MS.train$Author)
MS.test<-subset(author.numte,Author%in%c("Milton","Shakespeare"))
MS.test$Author<-factor(MS.test$Author)

MSRandomForest<-randomForest(MS.train$Author~.,data=MS.train,ntree=5000,importance=TRUE)
print(MSRandomForest)

hist(treesize(MSRandomForest))
importance(MSRandomForest)
varImpPlot(MSRandomForest)

MS.predictrf<-predict(MSRandomForest,MS.train,type="class")
MSrftrain<-table(MS.train$Author,MS.predictrf)
error(MSrftrain)

MS.predictrftest<-predict(MSRandomForest,MS.test,type="class")
MSrftest<-table(MS.test$Author,MS.predictrftest)
error(MSrftest)
