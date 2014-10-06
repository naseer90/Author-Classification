
ld1<-coef(author.lda)[,1]
barplot(ld1,main="LD1 for All Authors",xlab="Words",ylab="Loadings",col="red",horiz=F,las=2) 

ld2<-coef(author.lda)[,2]
barplot(ld2,main="LD2 for All Authors",xlab="Words",ylab="Loadings",col="red",horiz=F,las=2) 

ld3<-coef(author.lda)[,3]
barplot(ld3,main="LD3 for All Authors",xlab="Words",ylab="Loadings",col="red",horiz=F,las=2) 

pc1<-loadings(pca.cov)[,1]
barplot(pc1,main="PC1 for All Authors",xlab="Words",ylab="Loadings",col="blue",horiz=F,las=2) 

pc2<-loadings(pca.cov)[,2]
barplot(pc2,main="PC2 for All Authors",xlab="Words",ylab="Loadings",col="blue",horiz=F,las=2) 

pc3<-loadings(pca.cov)[,3]
barplot(pc3,main="PC3 for All Authors",xlab="Words",ylab="Loadings",col="blue",horiz=F,las=2) 

fa1<-fa$loadings[,1]
barplot(fa1,main="FA1 for All Authors",xlab="Words",ylab="Loadings",col="green",horiz=F,las=2) 

fa2<-fa$loadings[,2]
barplot(fa2,main="FA2 for All Authors",xlab="Words",ylab="Loadings",col="green",horiz=F,las=2) 

fa3<-fa$loadings[,3]
barplot(fa3,main="FA3 for All Authors",xlab="Words",ylab="Loadings",col="green",horiz=F,las=2) 



