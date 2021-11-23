library(readr)
library(glmnet)
library(tree)
library(ranger)
library(pROC)

bank<-bank[-c(1:3)]

n<-nrow(bank)

set.seed(19103)

sample <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.7, 0.3))

bank.train <- bank[sample, ]

bank.holdout <- bank[!sample, ]

#######

null <- glm(Exited ~ 1, data=bank, family = "binomial")
start_time <- Sys.time()
fwd.model.bank=step(null, direction = 'forward', scope=formula(full), keep = function(model, aic) list(model = model, aic = aic))
###q1.   Age

length(fwd.model.bank$coefficients)
###q2.   15

null <- glm(Exited ~ 1, data=bank.holdout, family = "binomial")
start_time <- Sys.time()
fwd.model=step(null, direction = 'forward', scope=formula(full), keep = function(model, aic) list(model = model, aic = aic))

M <- dim(fwd.model$keep)[2]

OOS=data.frame(R2=rep(NA,M), rank=rep(NA, M))

## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}  

for(k in 1:M){
  
  pred = predict(fwd.model$keep[["model",k]], newdata=bank.holdout, type = "response")
  
  OOS$R2[k]<-R2(y = bank.holdout$Exited,pred=pred, family="binomial")
  OOS$rank[k]<-fwd.model$keep[["model",k]]$rank
  
  
}
ax=c(1:max(OOS$rank))
par(mai=c(.9,.8,.2,.2))
plot(x=OOS$rank, y = OOS$R2, type="b", ylab=expression(paste("Out-of-Sample R"^"2")), xlab="# of model parameters estimated (rank)", xaxt="n")
axis(1, at=ax, labels=ax)

max.idx <- which.max(OOS$R2)

OOS$rank[max.idx]

abline(v=OOS$rank[max.idx], lty=3)

###q3.   10

# all the factor variables
factors<- model.matrix(xfactors<- model.matrix(Exited ~ Geography + Gender, data = bank.train))

# remove intercept
xfactors<-xfactors[,-1]

# all continuous variables
x<-as.matrix(data.frame(bank.train$CreditScore, bank.train$Age, bank.train$Tenure, bank.train$Balance, bank.train$NumOfProducts, bank.train$EstimatedSalary, xfactors))                        
head(x)

lasso.bank.train<-glmnet(x, y=as.factor(bank.train$Exited), alpha = 1, family = "binomial", nlambda = 100)       

par(mai=c(.9,.8,.8,.8))
par(mfrow=c(1,1))
plot(lasso.bank.train, xvar="lambda", label = TRUE, )

(dimnames(x)[2])

lasso.bank.cv<-cv.glmnet(x, y=bank.train$Exited, family = "binomial", type.measure = "deviance")
plot(lasso.bank.cv)
coef(lasso.bank.cv, s = "lambda.min")

###q4. Age

###q5.   9

print(lasso.bank.train)

###q6. ?????

tree<-tree(as.factor(Exited) ~ ., data=bank.train,mindev=0.01)
par(mfrow=c(1,1))
plot(tree, col=8, lwd=2)
# cex controls the size of the type, 1 is the default.  
# label="yprob" gives the probability
text(tree, label = "yprob", cex=.75, font=2, digits = 2, pretty=0)

###q7.   7

###q8.   0.87/0.13

tree_complex<-tree(as.factor(Exited) ~ . , data=bank.train, mindev=0, mincut=100)

par(mfrow=c(1,1))
par(mai=c(.8,.8,.2,.2))
plot(tree_complex, col=10, lwd=2)
text(tree_complex, cex=.5, label="yprob", font=2, digits = 2, pretty = 0)
title(main="Classification Tree: complex")

cv.tree_complex<-cv.tree(tree_complex, K=10)
cv.tree_complex$size

round(cv.tree_complex$dev)

par(mfrow=c(1,1))
plot(cv.tree_complex$size, cv.tree_complex$dev, xlab="tree size (complexity)", ylab="Out-of-sample deviance (error)", pch=20)

par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
tree_cut<-prune.tree(tree_complex, best = 9)
plot(tree_cut, col=10, lwd=2)
text(tree_cut, cex=1, label="yprob", font=2, digits = 2, pretty = 0)
title(main="A pruned tree")
summary(tree_cut)

###q9.   7

bank.training_rf<- ranger(Exited ~ ., data=bank.train, write.forest=TRUE, num.trees = 1000, min.node.size = 25, importance = "impurity", probability=TRUE, seed = 19103)
head(bank.training_rf)
###10.   Age