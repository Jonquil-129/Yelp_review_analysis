rm(list=ls())
library(tidyverse)
library(caret)
library(glmnet)
library(MVA)
library(car)
pos<-read.csv('pos_bsns.csv')
neg<-read.csv('neg_bsns.csv')
bus1<-pos$business_id
bus2<-neg$business_id
for (i in 1:length(bus2)) {
  if(bus2[i]%in%bus1){
    print(i)
  }
}
out1in2<-bus1[225]
out2in1<-bus2[c(58,198)]
poss<-pos[-225,]
negg<-neg[-c(58,198),]
id1<-as.vector(poss$business_id)
id2<-as.vector(negg$business_id)


#pos_topics = {0:'Menu/Bar/Happy hour', 1:'Other main dishes and stype',
# 2:'Service and atmosphere', 3:'Quality of steaks'}
#neg_topics = {0:'Service/Bar/Menu', 1:"Temperature of Steaks" , 2:'Service time', 
# 3:'Quality of other sides and snacks'}

steak<-cbind(poss,negg[7:10])
steak[,1]<-c(1:dim(steak)[1])
names(steak)<-c("index","businessid","name",'city','state','stars','v1','v2','v3','v4','v5','v6','v7','v8')
pairs(steak[6:14])

par(mfrow=c(2,4),mar=c(4,4,2,2))

library(MVA)
for (i in 7:14) {
  bvbox(cbind(steak[,i],steak[,6]),col="white",xlab = colnames(steak)[i],ylab="stars")
  text(steak[,i],steak[,6],label=steak$index,col="plum4")
}
#142,85,262
steak_rm<-steak[-c(142,85,262),]

##########MLR model###########################


###stepwise selection
fit1<-lm(stars~.,data = steak[,6:14])
fitnull<-lm(stars~1,data = steak[,6:14])
summary(fit1)
n=9
m1<-step(fit1,direction = "both",k=log(n))

m2<-step(fit1,direction = "both",k=2)

summary(m1)
anova(m1,m2)
anova(m1)



vif(m1)


######cross validation
set.seed(100)
training.samples <- steak$stars %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- steak[training.samples, 6:14]
test.data <- steak[-training.samples, 6:14]
# Build the model

model1 <- lm(formula = stars~v3+v4+v5+v7, data = train.data)
model2<-lm(formula = stars~v1+v3+v4+v5+v7, data = train.data)

# Make predictions and compute the R2, RMSE and MAE
predictions <- model1 %>% predict(test.data)
results1<-data.frame( R2 = R2(predictions, test.data$stars),
                      RMSE = RMSE(predictions, test.data$stars),
                      MAE = MAE(predictions, test.data$stars))
predictions <- model2 %>% predict(test.data)
results2<-data.frame( R2 = R2(predictions, test.data$stars),
                      RMSE = RMSE(predictions, test.data$stars),
                      MAE = MAE(predictions, test.data$stars))

#predictions <- model_r2 %>% predict(test.data)
#results3<-data.frame( R2 = R2(predictions, test.data$BODYFAT),
#                      RMSE = RMSE(predictions, test.data$BODYFAT),
#                      MAE = MAE(predictions, test.data$BODYFAT))
#predictions <- model_cp %>% predict(test.data)
#results4<-data.frame( R2 = R2(predictions, test.data$BODYFAT),
#                      RMSE = RMSE(predictions, test.data$BODYFAT),
#                      MAE = MAE(predictions, test.data$BODYFAT))
#predictions <- model_bic %>% predict(test.data)
#results5<-data.frame( R2 = R2(predictions, test.data$BODYFAT),
#                      RMSE = RMSE(predictions, test.data$BODYFAT),
#                      MAE = MAE(predictions, test.data$BODYFAT))
#results<-rbind(results1,results2,results3,results4,results5)

#lasso
x_var<-as.matrix(train.data[,-1])
y_var<-train.data$stars
lambda_seq <- 10^seq(2, -2, by = -.1)
c<-glmnet(x_var, y_var , standardize=TRUE, alpha=1)
plot(c)
cv_output <- cv.glmnet(x_var, y_var,
                       alpha = 1, lambda = lambda_seq) 
best_lam <- cv_output$lambda.min
best_lam
lasso_best <- glmnet(x_var, y_var, alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = as.matrix(test.data[,-1]))
final <- cbind(test.data$stars, pred)
coef(lasso_best)
rel<-data.frame( R2 = R2(pred, test.data$stars),
                 RMSE = RMSE(pred, test.data$stars),
                 MAE = MAE(pred, test.data$stars))
colnames(rel)<-c("R2","RMSE","MAE")

(results<-rbind(results1,results2,rel))
####By comparison, we chose our final model: BODYFAT ~ AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST + AGE:WRIST + ADIPOSITY:CHEST

#myfit<-lm(BODYFAT~AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST + AGE:WRIST + ADIPOSITY:CHEST,data=newbf[,-1])
#anova(m4,m1)
#anova(m1)
#summary(myfit)
#car::Anova(myfit)
#ci
#confint(myfit)


##########Diagnostics#########################
par(mfrow=c(1,1))
##check normality
qqnorm(rstandard(myfit),pch=19,cex=1.2,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)

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
# Outlying in X
n <- dim(model.matrix(myfit))[1] 
p <- dim(model.matrix(myfit))[2] 
plot(myinfluence$hat,type = "n") 
abline(h=2*p/n, col="red")
text(myinfluence$hat,label=newbf[,1],col="darkblue")
# 2. Identifying Influential Observations
# DFFITS
dffits(myfit)
plot(dffits(myfit),type = "n")
abline(h=1, col="red") 
abline(h=2*sqrt(p/n), col="green")
text(dffits(myfit),label=newbf[,1],col="darkblue")
# Cook's distance
cooks.distance(myfit)
plot(myfit, which = 4) 
plot(cooks.distance(myfit),type = "n") 
abline(h=1, col="red")
abline(h=qf(0.5, p, n-p), col="green") 
abline(h=4/n, col="blue") 
abline(h=4/(n-p-1-1), col="orange")
text(cooks.distance(myfit),label=newbf[,1],col="darkblue")
# DFBETAS
dfbetas(myfit)
plot(dfbetas(myfit)[,2],type = "n") # DFBETAS_{1(i)} 
abline(h=1, col="red")
abline(h=2/sqrt(n), col="blue")
text(dfbetas(myfit)[,2],label=newbf[,1],col="darkblue")

par(mfrow=c(2,2),mar=c(4,1,1,1))
qqnorm(rstandard(myfit),pch=19,cex=1.2,cex.lab=1.2,cex.main=1.2,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=3)
plot(myfit$fitted.values, rstandard(myfit),ylab = "standardized residual",xlab = "predicted body fat %",main = "standardized residual plot")
plot(myinfluence$hat,type = "n",ylab = "leverage",xlab = "index",main = "leverage plot") 
abline(h=2*p/n, col="red")
text(myinfluence$hat,label=newbf[,1],col="darkblue")
plot(myfit, which = 4) 









