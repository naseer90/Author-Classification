

##### Per Class Analysis #####

library(randomForest)
library(e1071)
library(MASS)
library(rpart)
library(lattice)
library(nnet)
library(cluster)

author<-read.table("author.txt",sep=",",header = T)
names(author)
table(author$Author)
error<-function(x){((sum(x)-sum(diag(x)))/sum(x))*100}

##Training and Test Sets
select7030<-c(32:45,111:138,148:151,217:244,296:317,354:368,411:428,442:446,466:473,542:571,601:613,646:659,665:668,678:681,692:696,708:712,724:728,737:740,752:756,763:766,779:783,791:793,807:811,822:826,837:841)
author.train<-author[-select7030,]
author.test<-author[select7030,]

austen.train<-author[c(1:221),]
london.train<-author[c(222:428),]
milton.train<-author[c(429:465),]
shakespeare.train<-author[c(466:583),]

austen.test<-author[c(1:96),]
london.test<-author[c(97:185),]
milton.test<-author[c(186:203),]
shakespeare.test<-author[c(204:258),]

##Subset by Authors
austen<-author[c(1:317),]
london<-author[c(318:613),]
milton<-author[c(614:668),]
shakespeare<-author[c(669:841),]

#####ADD IN COLORS BASED ON BOOKS?

##PCA

#Austen
apcnumeric<-austen[-c(70:72)]
apca.cov<-princomp(apcnumeric,cor=F)
summary(apca.cov)
loadings(apca.cov)

apca.cov.pred<-predict(apca.cov)
plot(apca.cov.pred[,1],apca.cov.pred[,2],type="n",xlab="PC1",ylab="PC2")
text(apca.cov.pred[,1],apca.cov.pred[,2])
title("PC1 vs. PC2 from Covariance Matrix for Austen")

apca.cov.pred<-predict(apca.cov)
plot(apca.cov.pred[,2],apca.cov.pred[,3],type="n",xlab="PC2",ylab="PC3")
text(apca.cov.pred[,2],apca.cov.pred[,3])
title("PC2 vs. PC3 from Covariance Matrix for Austen")

apca.cov.pred<-predict(apca.cov)
plot(apca.cov.pred[,1],apca.cov.pred[,3],type="n",xlab="PC1",ylab="PC3")
text(apca.cov.pred[,1],apca.cov.pred[,3])
title("PC1 vs. PC3 from Covariance Matrix for Austen")

#London
lpcnumeric<-london[-c(70:72)]
lpca.cov<-princomp(lpcnumeric,cor=F)
summary(lpca.cov)
loadings(lpca.cov)

lpca.cov.pred<-predict(lpca.cov)
plot(lpca.cov.pred[,1],lpca.cov.pred[,2],type="n",xlab="PC1",ylab="PC2")
text(lpca.cov.pred[,1],lpca.cov.pred[,2])
title("PC1 vs. PC2 from Covariance Matrix for London")

lpca.cov.pred<-predict(lpca.cov)
plot(lpca.cov.pred[,2],lpca.cov.pred[,3],type="n",xlab="PC1",ylab="PC2")
text(lpca.cov.pred[,2],lpca.cov.pred[,3])
title("PC2 vs. PC3 from Covariance Matrix for London")

lpca.cov.pred<-predict(lpca.cov)
plot(lpca.cov.pred[,1],lpca.cov.pred[,3],type="n",xlab="PC1",ylab="PC2")
text(lpca.cov.pred[,1],lpca.cov.pred[,3])
title("PC1 vs. PC3 from Covariance Matrix for London")

#Milton
mpcnumeric<-milton[-c(70:72)]
mpca.cov<-princomp(mpcnumeric,cor=F)
summary(mpca.cov)
loadings(mpca.cov)

mpca.cov.pred<-predict(mpca.cov)
plot(mpca.cov.pred[,1],mpca.cov.pred[,2],type="n",xlab="PC1",ylab="PC2")
text(mpca.cov.pred[,1],mpca.cov.pred[,2])
title("PC1 vs. PC2 from Covariance Matrix for Milton")

mpca.cov.pred<-predict(mpca.cov)
plot(mpca.cov.pred[,2],mpca.cov.pred[,3],type="n",xlab="PC2",ylab="PC3")
text(mpca.cov.pred[,2],mpca.cov.pred[,3])
title("PC2 vs. PC3 from Covariance Matrix for Milton")

mpca.cov.pred<-predict(mpca.cov)
plot(mpca.cov.pred[,1],mpca.cov.pred[,3],type="n",xlab="PC1",ylab="PC3")
text(mpca.cov.pred[,1],mpca.cov.pred[,3])
title("PC1 vs. PC3 from Covariance Matrix for Milton")

#Shakespeare
spcnumeric<-shakespeare[-c(70:72)]
spca.cov<-princomp(spcnumeric,cor=F)
summary(spca.cov)
loadings(spca.cov)

spca.cov.pred<-predict(spca.cov)
plot(spca.cov.pred[,1],spca.cov.pred[,2],type="n",xlab="PC1",ylab="PC2")
text(spca.cov.pred[,1],spca.cov.pred[,2])
title("PC1 vs. PC2 from Covariance Matrix for Shakespeare")

spca.cov.pred<-predict(spca.cov)
plot(spca.cov.pred[,2],spca.cov.pred[,3],type="n",xlab="PC1",ylab="PC2")
text(spca.cov.pred[,2],spca.cov.pred[,3])
title("PC2 vs. PC3 from Covariance Matrix for Shakespeare")

spca.cov.pred<-predict(spca.cov)
plot(spca.cov.pred[,1],spca.cov.pred[,3],type="n",xlab="PC1",ylab="PC2")
text(spca.cov.pred[,1],spca.cov.pred[,3])
title("PC1 vs. PC2 from Covariance Matrix for Shakespeare")

##FA

#Austen
afanumeric<-austen[-c(70:72)]
afa<-factanal(factors=16,afanumeric,scores="Bartlett")
sort(abs(afa$loadings[,1]))

plot(afa$scores[,1],afa$scores[,2],type="n",xlab="FA1",ylab="FA2")
text(afa$scores[,1],afa$scores[,2])
title("FA1 vs. FA2 from Covariance Matrix based on Austen")

plot(afa$scores[,2],afa$scores[,3],type="n",xlab="FA2",ylab="FA3")
text(afa$scores[,2],afa$scores[,3])
title("FA2 vs. FA3 from Covariance Matrix based on Austen")

plot(afa$scores[,1],afa$scores[,3],type="n",xlab="FA1",ylab="FA3")
text(afa$scores[,1],afa$scores[,3])
title("FA1 vs. FA3 from Covariance Matrix based on Austen")

#London
lfanumeric<-london[-c(70:72)]
lfa<-factanal(factors=21,lfanumeric,scores="Bartlett")
sort(abs(lfa$loadings[,1]))

plot(lfa$scores[,1],lfa$scores[,2],type="n",xlab="FA1",ylab="FA2")
text(lfa$scores[,1],lfa$scores[,2])
title("FA1 vs. FA2 from Covariance Matrix based on London")

plot(lfa$scores[,2],lfa$scores[,3],type="n",xlab="FA2",ylab="FA3")
text(lfa$scores[,2],lfa$scores[,3])
title("FA2 vs. FA3 from Covariance Matrix based on London")

plot(lfa$scores[,1],lfa$scores[,3],type="n",xlab="FA1",ylab="FA3")
text(lfa$scores[,1],lfa$scores[,3])
title("FA1 vs. FA3 from Covariance Matrix based on London")

#Milton #####DOESNT WORK#####
# mfanumeric<-milton[-c(70:72)]
# mfa<-factanal(factors=2,mfanumeric,scores="Bartlett")
# sort(abs(mfa$loadings[,1]))
# 
# plot(mfa$scores[,1],mfa$scores[,2],type="n",xlab="FA1",ylab="FA2")
# text(mfa$scores[,1],mfa$scores[,2])
# title("FA1 vs. FA2 from Covariance Matrix based on Milton")
# 
# plot(mfa$scores[,2],mfa$scores[,3],type="n",xlab="FA2",ylab="FA3")
# text(mfa$scores[,2],mfa$scores[,3])
# title("FA2 vs. FA3 from Covariance Matrix based on Milton")
# 
# plot(mfa$scores[,1],mfa$scores[,3],type="n",xlab="FA1",ylab="FA3")
# text(mfa$scores[,1],mfa$scores[,3])
# title("FA1 vs. FA3 from Covariance Matrix based on Milton")

#Shakespeare
sfanumeric<-shakespeare[-c(70:72)]
sfa<-factanal(factors=11,sfanumeric,scores="Bartlett")
sort(abs(sfa$loadings[,1]))

plot(sfa$scores[,1],sfa$scores[,2],type="n",xlab="FA1",ylab="FA2")
text(sfa$scores[,1],sfa$scores[,2])
title("FA1 vs. FA2 from Covariance Matrix based on Shakespeare")

plot(sfa$scores[,2],sfa$scores[,3],type="n",xlab="FA2",ylab="FA3")
text(sfa$scores[,2],sfa$scores[,3])
title("FA2 vs. FA3 from Covariance Matrix based on Shakespeare")

plot(sfa$scores[,1],sfa$scores[,3],type="n",xlab="FA1",ylab="FA3")
text(sfa$scores[,1],sfa$scores[,3])
title("FA1 vs. FA3 from Covariance Matrix based on Shakespeare")

##MDS
austen.cor<-cor(austen[-c(70:72)])
levelplot(austen.cor)

london.cor<-cor(london[-c(70:72)])
levelplot(london.cor)

milton.cor<-cor(milton[-c(70:72)])
levelplot(milton.cor)

shakespeare.cor<-cor(shakespeare[-c(70:72)])
levelplot(shakespeare.cor)

##LDA

#Austen
austen.lda<-lda(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=austen.train)
print(austen.lda)

austen.ldapredicttrain<-predict(austen.lda,austen.train)
atldatrain<-table(austen.train$BookID,austen.ldapredicttrain$class)
error(atldatrain)

austen.ldapredicttest<-predict(austen.lda,austen.test)
atldatest<-table(austen.test$BookID,austen.ldapredicttest$class)
error(atldatest)

#London
london.lda<-lda(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=london.train)
print(london.lda)

london.ldapredicttrain<-predict(london.lda,london.train)
ltldatrain<-table(london.train$BookID,london.ldapredicttrain$class)
error(ltldatrain)

london.ldapredicttest<-predict(london.lda,london.test)
ltldatest<-table(london.test$BookID,london.ldapredicttest$class)
error(ltldatest)

#Milton
milton.lda<-lda(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=milton.train)
print(milton.lda)

milton.ldapredicttrain<-predict(milton.lda,milton.train)
mtldatrain<-table(milton.train$BookID,milton.ldapredicttrain$class)
error(mtldatrain)

milton.ldapredicttest<-predict(milton.lda,milton.test)
mtldatest<-table(milton.test$BookID,milton.ldapredicttest$class)
error(mtldatest)

#Shakespeare
shakespeare.lda<-lda(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=shakespeare.train)
print(shakespeare.lda)

shakespeare.ldapredicttrain<-predict(shakespeare.lda,shakespeare.train)
stldatrain<-table(shakespeare.train$BookID,shakespeare.ldapredicttrain$class)
error(stldatrain)

shakespeare.ldapredicttest<-predict(shakespeare.lda,shakespeare.test)
stldatest<-table(shakespeare.test$BookID,shakespeare.ldapredicttest$class)
error(stldatest)

##K-NN

##SVM

#Austen
austen.svm<-svm(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=austen.train,kernel="linear")
summary(austen.svm)

austen.pred<-predict(austen.svm,austen.train,decision.values=T)
atsvmtrain<-table(austen.train$BookID,austen.pred)
error(atsvmtrain)

austen.predtestsvm<-predict(austen.svm,austen.test,decision.values=T)
atsvmtest<-table(austen.test$BookID,austen.predtestsvm)
error(atsvmtest)

#London
london.svm<-svm(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=london.train,kernel="linear")
summary(london.svm)

london.pred<-predict(london.svm,london.train,decision.values=T)
ltsvmtrain<-table(london.train$BookID,london.pred)
error(ltsvmtrain)

london.predtestsvm<-predict(london.svm,london.test,decision.values=T)
ltsvmtest<-table(london.test$BookID,london.predtestsvm)
error(ltsvmtest)

#Milton
milton.svm<-svm(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=milton.train,kernel="linear")
summary(milton.svm)

milton.pred<-predict(milton.svm,milton.train,decision.values=T)
mtsvmtrain<-table(milton.train$BookID,milton.pred)
error(mtsvmtrain)

milton.predtestsvm<-predict(milton.svm,milton.test,decision.values=T)
mtsvmtest<-table(milton.test$BookID,milton.predtestsvm)
error(mtsvmtest)

#Shakespeare
shakespeare.svm<-svm(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=shakespeare.train,kernel="linear")
summary(shakespeare.svm)

shakespeare.pred<-predict(shakespeare.svm,shakespeare.train,decision.values=T)
stsvmtrain<-table(shakespeare.train$BookID,shakespeare.pred)
error(stsvmtrain)

shakespeare.predtestsvm<-predict(shakespeare.svm,shakespeare.test,decision.values=T)
stsvmtest<-table(shakespeare.test$BookID,shakespeare.predtestsvm)
error(stsvmtest)

##Trees

##RandomForest

#Austen
austenRF<-randomForest(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=austen.train,ntree=5000,importance=TRUE)
print(austenRF)

hist(treesize(austenRF))
importance(austenRF)
varImpPlot(austenRF)

austen.predictrf<-predict(austenRF,austen.train,type="class")
arftrain<-table(austen.train$BookID,austen.predictrf)
error(arftrain)

austen.predictrftest<-predict(austenRF,austen.test,type="class")
arftest<-table(austen.test$BookID,austen.predictrftest)
error(arftest)

#London
londonRF<-randomForest(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=london.train,ntree=5000,importance=TRUE)
print(londonRF)

hist(treesize(londonRF))
importance(londonRF)
varImpPlot(londonRF)

london.predictrf<-predict(londonRF,london.train,type="class")
lrftrain<-table(london.train$BookID,london.predictrf)
error(lrftrain)

london.predictrftest<-predict(londonRF,london.test,type="class")
lrftest<-table(london.test$BookID,london.predictrftest)
error(lrftest)

#Milton
miltonRF<-randomForest(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=milton.train,ntree=5000,importance=TRUE)
print(miltonRF)

hist(treesize(miltonRF))
importance(miltonRF)
varImpPlot(miltonRF)

milton.predictrf<-predict(miltonRF,milton.train,type="class")
mrftrain<-table(milton.train$BookID,milton.predictrf)
error(mrftrain)

milton.predictrftest<-predict(miltonRF,milton.test,type="class")
mrftest<-table(milton.test$BookID,milton.predictrftest)
error(mrftest)

#Shakespeare
shakespeareRF<-randomForest(BookID~a+all+also+an+any+are+as+at+be+been+but+by+can+do+down+even+every+for.+from+had+has+have+her+his+if.+in.+into+is+it+its+may+more+must+my+no+not+now+of+on+one+only+or+our+should+so+some+such+than+that+the+their+then+there+things+this+to+up+upon+was+were+what+when+which+who+will+with+would+your,data=shakespeare.train,ntree=5000,importance=TRUE)
print(shakespeareRF)

hist(treesize(shakespeareRF))
importance(shakespeareRF)
varImpPlot(shakespeareRF)

shakespeare.predictrf<-predict(shakespeareRF,shakespeare.train,type="class")
srftrain<-table(shakespeare.train$BookID,shakespeare.predictrf)
error(srftrain)

shakespeare.predictrftest<-predict(shakespeareRF,shakespeare.test,type="class")
srftest<-table(shakespeare.test$BookID,shakespeare.predictrftest)
error(srftest)

##KMeans

##Hierarchical Clustering

#Austen
austen.single<-agnes(austen[-c(70:72)],diss=F,method="single")
diss_austen<-daisy(austen[-c(70:72)])
asil.single<-silhouette(cutree(austen.single,k=5),diss_austen)
plot(asil.single)
plot(austen.single,which.plots=2)

austen.complete<-agnes(austen[-c(70:72)],diss=F,method="complete")
diss_author<-daisy(austen[-c(70:72)])
asil.compelete<-silhouette(cutree(austen.complete,k=5),diss_austen)
plot(asil.compelete)
plot(austen.complete,which.plots=2)

austen.average<-agnes(austen[-c(70:72)],diss=F,method="average")
diss_austen<-daisy(austen[-c(70:72)])
asil.average<-silhouette(cutree(austen.average,k=5),diss_austen)
plot(asil.average)
plot(austen.average,which.plots=2)

austen.ward<-agnes(austen[-c(70:72)],diss=F,method="ward")
diss_austen<-daisy(austen[-c(70:72)])
asil.ward<-silhouette(cutree(austen.ward,k=4),diss_austen)
plot(asil.ward)
plot(austen.ward,which.plots=2)

#London
london.single<-agnes(london[-c(70:72)],diss=F,method="single")
diss_london<-daisy(london[-c(70:72)])
lsil.single<-silhouette(cutree(london.single,k=5),diss_london)
plot(lsil.single)
plot(london.single,which.plots=2)

london.complete<-agnes(london[-c(70:72)],diss=F,method="complete")
diss_london<-daisy(london[-c(70:72)])
lsil.compelete<-silhouette(cutree(london.complete,k=5),diss_london)
plot(lsil.compelete)
plot(london.complete,which.plots=2)

london.average<-agnes(london[-c(70:72)],diss=F,method="average")
diss_london<-daisy(london[-c(70:72)])
lsil.average<-silhouette(cutree(london.average,k=5),diss_london)
plot(lsil.average)
plot(london.average,which.plots=2)

london.ward<-agnes(london[-c(70:72)],diss=F,method="ward")
diss_london<-daisy(london[-c(70:72)])
lsil.ward<-silhouette(cutree(london.ward,k=4),diss_london)
plot(lsil.ward)
plot(london.ward,which.plots=2)

#Milton
milton.single<-agnes(milton[-c(70:72)],diss=F,method="single")
diss_milton<-daisy(milton[-c(70:72)])
msil.single<-silhouette(cutree(milton.single,k=5),diss_milton)
plot(msil.single)
plot(milton.single,which.plots=2)

milton.complete<-agnes(milton[-c(70:72)],diss=F,method="complete")
diss_milton<-daisy(milton[-c(70:72)])
msil.complete<-silhouette(cutree(milton.complete,k=5),diss_milton)
plot(msil.complete)
plot(milton.complete,which.plots=2)

milton.average<-agnes(milton[-c(70:72)],diss=F,method="average")
diss_milton<-daisy(milton[-c(70:72)])
msil.average<-silhouette(cutree(milton.average,k=5),diss_milton)
plot(msil.average)
plot(milton.average,which.plots=2)

milton.ward<-agnes(milton[-c(70:72)],diss=F,method="ward")
diss_milton<-daisy(milton[-c(70:72)])
msil.ward<-silhouette(cutree(milton.ward,k=5),diss_milton)
plot(msil.ward)
plot(milton.ward,which.plots=2)

#Shakespeare
shakespeare.single<-agnes(shakespeare[-c(70:72)],diss=F,method="single")
diss_milton<-daisy(shakespeare[-c(70:72)])
ssil.single<-silhouette(cutree(shakespeare.single,k=5),diss_shakespeare)
plot(ssil.single)
plot(shakespeare.single,which.plots=2)

shakespeare.complete<-agnes(shakespeare[-c(70:72)],diss=F,method="complete")
diss_milton<-daisy(shakespeare[-c(70:72)])
ssil.complete<-silhouette(cutree(shakespeare.complete,k=5),diss_shakespeare)
plot(ssil.complete)
plot(shakespeare.complete,which.plots=2)

shakespeare.average<-agnes(shakespeare[-c(70:72)],diss=F,method="average")
diss_milton<-daisy(shakespeare[-c(70:72)])
ssil.average<-silhouette(cutree(shakespeare.average,k=5),diss_shakespeare)
plot(ssil.average)
plot(shakespeare.average,which.plots=2)

shakespeare.ward<-agnes(shakespeare[-c(70:72)],diss=F,method="ward")
diss_milton<-daisy(shakespeare[-c(70:72)])
ssil.ward<-silhouette(cutree(shakespeare.ward,k=5),diss_shakespeare)
plot(ssil.ward)
plot(shakespeare.ward,which.plots=2)



