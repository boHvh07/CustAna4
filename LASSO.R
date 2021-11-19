x<-seq(-20,20,1)
abs<-abs(x)
plot(x,abs,lwd=2, type="l", xlab = "beta", ylab = "abs(beta)")

# all the factor variables
xfactors<- model.matrix(Churn ~ SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod, data = telco)

# remove intercept
xfactors<-xfactors[,-1]

# all continuous variables
x<-as.matrix(data.frame(telco$tenure, telco$MonthlyCharges, telco$TotalCharges, xfactors))                        
head(x)

lasso_telco<-glmnet(x, y=as.factor(telco$Churn), alpha = 1, family = "binomial", nlambda = 100)       

par(mai=c(.9,.8,.8,.8))
par(mfrow=c(1,1))
plot(lasso_telco, xvar="lambda", label = TRUE, )

dimnames(x)[2]

print(lasso_telco)

plot(lasso_telco, xvar = "dev", label = TRUE)


###   chOOSING LAMBDA    ###
lasso_cv<-cv.glmnet(x, y=telco$Churn, family = "binomial", type.measure = "deviance")
plot(lasso_cv)

coef(lasso_cv, s = "lambda.min")


# use holdout telco data

xfactors<- model.matrix(Churn ~ SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod, data = telco.holdout)
head(xfactors)


# remove intercept
xfactors<-xfactors[,-1]

# all continuous variables

x<-as.matrix(data.frame(telco.holdout$tenure, telco.holdout$MonthlyCharges, telco.holdout$TotalCharges, xfactors))  

pred<-predict(lasso_cv, newx=x,  s = "lambda.min", type = "response")

churn<-as.numeric(telco.holdout$Churn)-1

head(cbind(churn, pred))

par(mfrow=c(1,1))
par(mai=c(.9,.8,.2,.2))
plot(roc(churn, pred), print.auc=TRUE, ylim=c(0,1), levels=c("Churn", "Stay"),
     col="black", lwd=1, main="ROC curve", xlab="Specificity: true negative rate",      ylab="Sensitivity: true positive rate", xlim=c(1,0), direction="<")
