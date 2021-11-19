rm(list=ls())
install.packages("janitor")
install.packages("tree")
install.packages("ranger")
install.packages("pROC")
install.packages("glmnet")
install.packages("tree")
library(tree)
library(dplyr)
library(janitor)
library(car)
library(pROC)
library(ranger)
library(glmnet)


options("scipen"=200, "digits"=3)

# load ebeer, remove account number column
ebeer<-read.csv('data/ebeer.csv')
ebeer<-ebeer[-c(1)]

# drop the ID column, select customers that received a mailing only
ebeer_test<-subset(ebeer, mailing ==1)

# create ebeer rollout data
ebeer_rollout<-subset(ebeer, mailing ==0)

# rename ebeer_test ebeer
ebeer<-ebeer_test

# load telco
telco<-read.csv('data/telco.csv', stringsAsFactors = TRUE)

# drop ID column, divide Total charges by 1000
telco<-subset(telco, select=-ï..customerID)
telco$TotalCharges<-telco$TotalCharges/1000

# create 70% test and 30% holdout sample
set.seed(19103)
n<-nrow(telco)
sample <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.7, 0.3))
telco.test <- telco[sample, ]
telco.holdout <- telco[!sample, ]

#call test telco, and full data set telco.all
telco.all<-telco
telco<-telco.test



####   FORWARD SELECTION   ###

telco <- telco %>%
  mutate(Churn = ifelse(Churn == "No",0,1))
full <-glm(Churn ~ . + tenure:(.), data=telco, family = "binomial")

summary(full)


# intercept only
null <- glm(Churn ~ 1, data=telco, family = "binomial")
start_time <- Sys.time()
fwd.model=step(null, direction = 'forward', scope=formula(full), keep = function(model, aic) list(model = model, aic = aic))

fwd.model$anova

length(fwd.model$coefficients)

length(full$coefficients)


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
  
  pred = predict(fwd.model$keep[["model",k]], newdata=telco.holdout, type = "response")
  
  OOS$R2[k]<-R2(y = telco.holdout$Churn,pred=pred, family="binomial")
  OOS$rank[k]<-fwd.model$keep[["model",k]]$rank
  
  
}
ax=c(1:max(OOS$rank))
par(mai=c(.9,.8,.2,.2))
plot(x=OOS$rank, y = OOS$R2, type="b", ylab=expression(paste("Out-of-Sample R"^"2")), xlab="# of model parameters estimated (rank)", xaxt="n")
axis(1, at=ax, labels=ax)

max.idx <- which.max(OOS$R2)

OOS$rank[max.idx]

abline(v=OOS$rank[max.idx], lty=3)

model<-fwd.model$keep[["model",max.idx]]


model_full_data<-glm(model$formula, data = telco.all, family = binomial(link = "logit"))

summary(model_full_data)