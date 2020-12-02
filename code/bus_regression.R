rm(list=ls())
library(tidyverse)
library(caret)
library(glmnet)
library(MVA)
library(car)
library(ggplot2)
library(plotly)
library(ggcorrplot)
pos<-read.csv('pos_bsns.csv')
neg<-read.csv('neg_bsns.csv')
bus1<-pos$business_id
bus2<-neg$business_id
for (i in 1:length(bus2)) {
  if(!bus2[i]%in%bus1){
    print(i)
  }
}
out1in2<-bus1[225]
out2in1<-bus2[c(58,198)]
pos[,1]<-c(1:dim(pos)[1])
poss<-pos[-225,]

negg<-neg[-c(58,198),]
id1<-as.vector(poss$business_id)
id2<-as.vector(negg$business_id)



#pos_topics = {0:'Menu/Bar/Happy hour', 1:'Other main dishes and stype',
# 2:'Service and atmosphere', 3:'Quality of steaks'}
#neg_topics = {0:'Service/Bar/Menu', 1:"Temperature of Steaks" , 2:'Service time', 
# 3:'Quality of other sides and snacks'}

steak<-cbind(poss[1:10],negg[7:10],poss[11],negg[11])

for(i in 1:dim(steak)[1]){
  if(as.vector(steak$state[i])=="PA"){
    steak[i,17]=1
  }else{
    steak[i,17]=0
  }
}
for(i in 1:dim(steak)[1]){
  if(as.vector(steak$state[i])=="OH"){
    steak[i,18]=1
  }else{
    steak[i,18]=0
  }
}
for(i in 1:dim(steak)[1]){
  if(as.vector(steak$state[i])=="IL"){
    steak[i,19]=1
  }else{
    steak[i,19]=0
  }
}
names(steak)<-c("index","businessid","name",'city','state','stars','v1','v2','v3','v4','v5','v6','v7','v8','postext','negtext','pa','oh','il')
for(i in 11:14){
  steak[,i]<-steak[,i]*steak$negtext/(steak$postext+steak$negtext)
}
for(i in 7:10){
  steak[,i]<-steak[,i]*steak$postext/(steak$postext+steak$negtext)
}






#################EDA part
cr<-cor(steak[,6:14], method = "pearson")
ggcorrplot::ggcorrplot(cr)

pairs(steak[,6:14])
#histograms in notebook
par(mfrow=c(2,4),mar=c(4,4,2,2))
for (i in 7:14) {
  bvbox(cbind(steak[,i],steak[,6]),col="white",xlab = colnames(steak)[i],ylab="stars")
  text(steak[,i],steak[,6],label=steak$index,col="plum4")
}


library(gridExtra)
out1 = lapply(steak[,7:14], function(x){
  ggplot(data.frame(x), aes(x)) + geom_histogram(bins=30)+labs(x="Weight of Topic", y = "Count")})

grid.arrange(out1[[1]],out1[[2]],out1[[3]],out1[[4]],out1[[5]],out1[[6]],out1[[7]],out1[[8]], ncol=4)




###263 outlier in residual

#142
steak_rm<-steak[-c(142,262),]







##########MLR model###########################
###all possible subsets
if (!require("leaps")) { 
  install.packages("leaps") 
  stopifnot(require("leaps"))
}
options(width = 90) # just for better views (not required)
myleaps <- regsubsets(stars~.,data = steak_rm[,6:14], nbest=8)
myleaps <- regsubsets(stars~(v1+v2+v3+v4+v5+v6+v7+v8)*(1+pa+oh+il),data = steak_rm[,6:19]) 
(myleaps.summary <- summary(myleaps)) 

bettertable <- cbind(myleaps.summary$which, myleaps.summary$rsq, myleaps.summary$rss,
                     myleaps.summary$adjr2, myleaps.summary$cp, myleaps.summary$bic) 
dimnames(bettertable)[[2]] <- c(dimnames(myleaps.summary$which)[[2]],"rsq", "rss", "adjr2", "cp", "bic")
show(bettertable)
par(mfrow=c(1,3), pty="s")
plot(myleaps, scale = "adjr2"); plot(myleaps, scale = "Cp"); plot(myleaps, scale = "bic");                                                                                          show(bettertable)

##不加interaction结果和stepwise一样

#加interactions：
m3<-lm(stars~v2+v5+v6+v7+v8,data = steak_rm[,6:19])#bic
m6<-lm(stars~v1+v2+v3+v4+v2:oh+v7:oh+v7:il+v8:oh,data = steak_rm[,6:19])#r2 & cp
summary(m3)
summary(m6)

###stepwise selection
#fit1<-lm(stars~.,data = steak[,6:14])
fit1<-lm(stars~.,data = steak_rm[,6:14])
fit2<-lm(stars~(v1+v2+v3+v4+v5+v6+v7+v8)*(1+pa+oh+il),data = steak_rm[,6:19])####add state interaction
fitnull<-lm(stars~1,data = steak_rm[,6:14])
summary(fit1)
summary(fit2)
n<-dim(steak_rm)[1]
m1<-step(fit1,direction = "both",k=log(n))##v2+v5+v6+v7+v8
#m2<-step(fit1,direction = "backward",k=log(n))
summary(m1)

##state
m4<-step(fit2,direction = "both",k=log(n))# v1 + v2 + v3 + v4 + v7
m5<-step(fit2,direction = "both",k=2)
summary(m4)
#anova(m4,m5)
#########*********final model: m4***********
anova(m1,m4)
anova(m4)

vif(m1)
vif(m4)

anova(m4,fit1)
######cross validation
set.seed(100)
#training.samples <- steak$stars %>%
#  createDataPartition(p = 0.8, list = FALSE)
#train.data  <- steak[training.samples, 6:14]
#test.data <- steak[-training.samples, 6:14]

training.samples <- steak_rm$stars %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- steak_rm[training.samples, 6:19]
test.data <- steak_rm[-training.samples, 6:19]
# Build the model
model1<-lm(formula = stars~., data = train.data)
model1 <- lm(formula = stars~v2+v5+v6+v7+v8, data = train.data)
#model2<-lm(formula = stars~v5+v6+v7+v8, data = train.data)
model2<-lm(formula = stars ~ v1 + v2 + v3 + v4 + v7, 
           data = train.data)
model3<-lm(formula = stars ~ v1+v2+v3+v4+v2:oh+v7:oh+v7:il+v8:oh, 
           data = train.data)
model5<-lm(formula = stars ~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + 
             pa + oh + il + v1:pa + v1:oh + v2:pa + v2:oh + v3:pa + v3:oh + 
             v4:pa + v4:oh + v5:pa + v5:oh + v6:pa + v6:oh + v7:pa + v7:oh + 
             v7:il + v8:pa, data = train.data)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model1 %>% predict(test.data)
results1<-data.frame( R2 = R2(predictions, test.data$stars),
                      RMSE = RMSE(predictions, test.data$stars),
                      MAE = MAE(predictions, test.data$stars))
predictions <- model2 %>% predict(test.data)
results2<-data.frame( R2 = R2(predictions, test.data$stars),
                      RMSE = RMSE(predictions, test.data$stars),
                      MAE = MAE(predictions, test.data$stars))
predictions <- model3 %>% predict(test.data)
results3<-data.frame( R2 = R2(predictions, test.data$stars),
                      RMSE = RMSE(predictions, test.data$stars),
                      MAE = MAE(predictions, test.data$stars))
predictions <- model5 %>% predict(test.data)
results5<-data.frame( R2 = R2(predictions, test.data$stars),
                      RMSE = RMSE(predictions, test.data$stars),
                      MAE = MAE(predictions, test.data$stars))


#lasso
x_var<-as.matrix(train.data[,-c(1,10:14)])
y_var<-train.data$stars
lambda_seq <- 10^seq(2, -2, by = -.1)
c<-glmnet(x_var, y_var , standardize=TRUE, alpha=1)
plot(c)
cv_output <- cv.glmnet(x_var, y_var,
                       alpha = 1, lambda = lambda_seq) 
best_lam <- cv_output$lambda.min
best_lam
lasso_best <- glmnet(x_var, y_var, alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = as.matrix(test.data[,-c(1,10:14)]))
final <- cbind(test.data$stars, pred)
coef(lasso_best)
rel<-data.frame( R2 = R2(pred, test.data$stars),
                 RMSE = RMSE(pred, test.data$stars),
                 MAE = MAE(pred, test.data$stars))
colnames(rel)<-c("R2","RMSE","MAE")

(results<-rbind(results1,results2,results3,rel))


#summary(myfit)
myfit<-m4
car::Anova(myfit,type=3)
#ci
confint(myfit)


##########Diagnostics#########################
par(mfrow=c(1,1))
##check normality
qqnorm(rstandard(myfit),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)
text(qqnorm(rstandard(myfit)),labels = steak[,1],col="darkblue")#########index 263,(61,164) in steak: violate normal & equal variance
# 1. Types of Residuals and Identifying Outliers
# compare residuals (raw, standardized, studentized)
cbind(resid(myfit), rstandard(myfit), rstudent(myfit))
# manually compute and compare
X <- model.matrix(myfit)
H <- X %*% solve(t(X)%*%X) %*% t(X)
# or,
myhat <- hatvalues(myfit)
cbind(myhat, diag(H))
mysigma <- sigma(myfit)
myrstandard <- myfit$residuals/sqrt(mysigma^2 * (1-myhat)) # standardized
cbind(myrstandard, rstandard(myfit))
# can extract sigma_{(i)}
myinfluence <- influence(myfit)
myinfluence$sigma
# studentized
myrstudent <- myfit$residuals/sqrt(myinfluence$sigma^2 * (1-myinfluence$hat)) 
cbind(myrstudent, rstudent(myfit))
par(mfrow=c(1,3))
# Outlying in Y
# check standardized residuals
plot(myfit, which=3)
plot(myfit$fitted.values, sqrt(abs(rstandard(myfit))))
plot(myfit$fitted.values, rstandard(myfit))
# check studentized residuals
plot(myfit$fitted.values, rstudent(myfit))
plot(myfit$fitted.values, rstudent(myfit), ylim=c(-4,4)) 
abline(h=c(-3,3), col="red") # rule of thumb
p <- dim(model.matrix(myfit))[2] 
alpha <- 0.05
t.critical <- qt(1-alpha/(2*n), n-p-1) # Bonferroni correction 
abline(h=c(-t.critical, t.critical), col="green")
text(myfit$fitted.values, rstudent(myfit),label=steak_rm[,1],col="darkblue")
# Outlying in X #######some high leverage points: index 174,173,85,138,3 and so on ##m6:~+277,48
n <- dim(model.matrix(myfit))[1] 
p <- dim(model.matrix(myfit))[2] 
plot(myinfluence$hat,type = "n") 
abline(h=2*p/n, col="red")
text(myinfluence$hat,label=steak_rm[,1],col="darkblue")
# 2. Identifying Influential Observations###########no influential points
# DFFITS
dffits(myfit)
plot(dffits(myfit),ylim=c(-1,1.2),type = "n")
abline(h=1, col="red") 
abline(h=2*sqrt(p/n), col="green")
text(dffits(myfit),label=steak_rm[,1],col="darkblue")
# Cook's distance
cooks.distance(myfit)
plot(myfit, which = 4) 
plot(cooks.distance(myfit),ylim = c(0,1.2),type = "n") 
abline(h=1, col="red")
abline(h=qf(0.5, p, n-p), col="green") 
abline(h=4/n, col="blue") 
abline(h=4/(n-p-1-1), col="orange")
text(cooks.distance(myfit),label=steak_rm[,1],col="darkblue")
# DFBETAS
dfbetas(myfit)
plot(dfbetas(myfit)[,2],ylim = c(-0.5,1.2),type = "n") # DFBETAS_{1(i)} 
abline(h=1, col="red")
abline(h=2/sqrt(n), col="blue")
text(dfbetas(myfit)[,2],label=steak[,1],col="darkblue")

########conclusion: m4 predicts best, but point 61 may be an influential point, (if delete, models all become v5+v6+v7+v8)


#################plots for diagnostics in one figure####################
par(mfrow=c(2,2),mar=c(4,4,1,1))
qqnorm(rstandard(myfit),pch=19,cex=1.2,cex.lab=1.2,cex.main=1.2,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)
plot(myfit$fitted.values, rstandard(myfit),ylim=c(-4,4),ylab = "standardized residual",xlab = "predicted stars",main = "standardized residual plot")
plot(myinfluence$hat,type = "n",ylab = "leverage",xlab = "index",main = "leverage plot") 
abline(h=2*p/n, col="red")
text(myinfluence$hat,label=steak_rm[,1],col="darkblue")
plot(myfit, which = 4) 









