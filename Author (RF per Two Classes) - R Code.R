
##### Random Forest per Two Classes Analysis #####

library(randomForest)
library(e1071)
library(MASS)
library(rpart)
library(lattice)
library(nnet)
library(cluster)

author<-read.table("author.txt",sep=",",header = T)
names(author)
author<-author[-c(1,39,57,5,51)]
table(author$Author)
error<-function(x){((sum(x)-sum(diag(x)))/sum(x))*100}

##Training and Test Sets
select7030<-c(32:45,111:138,148:151,217:244,296:317,354:368,411:428,442:446,466:473,542:571,601:613,646:659,665:668,678:681,692:696,708:712,724:728,737:740,752:756,763:766,779:783,791:793,807:811,822:826,837:841)
author.train<-author[-select7030,]
author.test<-author[select7030,]

#Austen and London
AL.train<-subset(author.train,Author%in%c("Austen","London"))
AL.train$Author<-factor(AL.train$Author)
AL.test<-subset(author.test,Author%in%c("Austen","London"))
AL.test$Author<-factor(AL.test$Author)

ALRandomForest<-randomForest(AL.train$Author~all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+on+one+only+or+our+should+so+some+such+than+that+their+then+there+things+this+up+upon+was+were+what+when+which+who+will+with+would+your,data=AL.train,ntree=5000,importance=TRUE)
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
AM.train<-subset(author.train, Author %in% c("Austen","Milton"))
AM.train$Author<-factor(AM.train$Author)
AM.test<-subset(author.test, Author %in% c( "Austen","Milton"))
AM.test$Author<-factor(AM.test$Author)

AMRandomForest<-randomForest(AM.train$Author~all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+on+one+only+or+our+should+so+some+such+than+that+their+then+there+things+this+up+upon+was+were+what+when+which+who+will+with+would+your,data=AM.train,ntree=5000,importance=TRUE)
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
AS.train<-subset(author.train,Author%in%c("Austen","Shakespeare"))
AS.train$Author<-factor(AS.train$Author)
AS.test<-subset(author.test,Author%in%c("Austen","Shakespeare"))
AS.test$Author<-factor(AS.test$Author)

ASRandomForest<-randomForest(AS.train$Author~all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+on+one+only+or+our+should+so+some+such+than+that+their+then+there+things+this+up+upon+was+were+what+when+which+who+will+with+would+your,data=AS.train,ntree=5000,importance=TRUE)
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
LM.train<-subset(author.train,Author%in%c("London","Milton"))
LM.train$Author<-factor(LM.train$Author)
LM.test<-subset(author.test,Author%in%c("London","Milton"))
LM.test$Author<-factor(LM.test$Author)

LMRandomForest<-randomForest(LM.train$Author~all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+on+one+only+or+our+should+so+some+such+than+that+their+then+there+things+this+up+upon+was+were+what+when+which+who+will+with+would+your,data=LM.train,ntree=5000,importance=TRUE)
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
LS.train<-subset(author.train,Author%in%c("London","Shakespeare"))
LS.train$Author<-factor(LS.train$Author)
LS.test<-subset(author.test,Author%in%c("London","Shakespeare"))
LS.test$Author<-factor(LS.test$Author)

LSRandomForest<-randomForest(LS.train$Author~all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+on+one+only+or+our+should+so+some+such+than+that+their+then+there+things+this+up+upon+was+were+what+when+which+who+will+with+would+your,data=LS.train,ntree=5000,importance=TRUE)
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
MS.train<-subset(author.train,Author%in%c("Milton","Shakespeare"))
MS.train$Author<-factor(MS.train$Author)
MS.test<-subset(author.test,Author%in%c("Milton","Shakespeare"))
MS.test$Author<-factor(MS.test$Author)

MSRandomForest<-randomForest(MS.train$Author~all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+on+one+only+or+our+should+so+some+such+than+that+their+then+there+things+this+up+upon+was+were+what+when+which+who+will+with+would+your,data=MS.train,ntree=5000,importance=TRUE)
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
