#GLM
require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)
train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
summary(glm.fit)

#Linear Discriminant Analysis
require(ISLR)
require(MASS)
?lda
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

# K-nearest neighbors
library(class)
?knn
attach(Smarket)
xlag=cbind(Lag1,Lag2)
xlag[1:5,]
train=Year<2005
knn.pred=knn(xlag[train,],xlag[!train,],Direction[train],k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])
# k=2
knn.pred2=knn(xlag[train,],xlag[!train,],Direction[train],k=2)
table(knn.pred2, Direction[!train])
mean(knn.pred2==Direction[!train])
# k=4
knn.pred4=knn(xlag[train,],xlag[!train,],Direction[train],k=4)
table(knn.pred4, Direction[!train])
mean(knn.pred4==Direction[!train])
# k=8
knn.pred8=knn(xlag[train,],xlag[!train,],Direction[train],k=8)
table(knn.pred8, Direction[!train])
mean(knn.pred8==Direction[!train])
# k=16
knn.pred16=knn(xlag[train,],xlag[!train,],Direction[train],k=16)
table(knn.pred16, Direction[!train])
mean(knn.pred16==Direction[!train])
