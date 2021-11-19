# DV needs to be factor variable so it knows to use a classification tree
tree<-tree(as.factor(respmail) ~ ., data=subset(ebeer, select = c(respmail, F, student)),mindev=.005)
par(mfrow=c(1,1))
plot(tree, col=8, lwd=2)
# cex controls the size of the type, 1 is the default.  
# label="yprob" gives the probability
text(tree, label = "yprob", cex=.75, font=2, digits = 2, pretty=0)

tree$frame


#lets make a prediction for our data. It will create two columns, one with probability for response and no response.
pred_tree<-predict(tree,ebeer, type = "vector")

head(pred_tree)


# take probability that responds
prob_resp<-pred_tree[,2]

# there is no predicted probability over 0.5
sum(prob_resp>0.5)


confusion_matrix <- (table(ebeer$respmail, prob_resp > 0.5))
confusion_matrix <- as.data.frame.matrix(confusion_matrix)
colnames(confusion_matrix) <- c("No")

confusion_matrix


confusion_matrix[1]/sum(confusion_matrix)


summary(tree)



# mean two graphs side-by-side
par(mfrow=c(1,2), oma=c(0,0,2,0))
# same model as above

tree<-tree(as.factor(respmail) ~ ., data=subset(ebeer, select = c(respmail, F, student)),mindev=.005)

plot(tree, col=8, lwd=2)

# cex controls the size of the type, 1 is the default.  
# label="yprob" gives the probability
text(tree, cex=.75, label="yprob", font=2, digits = 2, pretty = 0)



par(mai=c(.8,.8,.2,.2))

# create an aggregate table of response by frequency and student
tbl<- ebeer %>% group_by(student, F) %>% summarise(mean=mean(respmail)) %>% data.frame()


pred<-predict(tree,tbl, type = "vector")[,2]

tbl<-tbl %>% mutate(pred = pred)

# plot it
par(mai=c(.8,.8,.2,.2))
plot(tbl$F[1:12],tbl$mean[1:12], col = "red", xlab="Frequency", ylab="mean response",ylim=c(-.05,0.5), pch=20)
points(tbl$F[13:24],tbl$mean[13:24], col = "blue", pch=20)
legend(7.5, 0.5, legend=c("Student = no", "Student= yes"), col=c("red", "blue"), pch=20, cex=0.8)

# create predictions from tree for every F x student combo
newF <- seq(1,12,length=12)
lines(tbl$F[1:12], tbl$pred[1:12], col=2, lwd=2)
lines(tbl$F[1:12], tbl$pred[13:24], col=4, lwd=2)
mtext("A simple tree",outer=TRUE,cex=1.5)



###   NONPARAMETRIC   ###
# mean two graphs side-by-side
par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
# same model as above
tree<-tree(as.factor(respmail) ~ ., data=subset(ebeer, select = c(respmail, F, student)),mindev=0, mincut=0)
plot(tree, col=8, lwd=2)
# cex controls the size of the type, 1 is the default.  
# label="yprob" gives the probability
text(tree, cex=.5, label="yprob", font=2, digits = 2, pretty = 0)

par(mai=c(.8,.8,.2,.2))

# create an aggregate table of response by frequency and student
tbl<- ebeer %>% group_by(student, F) %>% summarise(mean=mean(respmail)) %>% data.frame()

pred<-predict(tree,tbl, type = "vector")[,2]

tbl<-tbl %>% mutate(pred = pred)

# plot it
par(mai=c(.8,.8,.2,.2))
plot(tbl$F[1:12],tbl$mean[1:12], col = "red", xlab="Frequency", ylab="mean response",ylim=c(-.05,0.5), pch=20)
points(tbl$F[13:24],tbl$mean[13:24], col = "blue", pch=20)
legend(7.5, 0.5, legend=c("Student = no", "Student= yes"), col=c("red", "blue"), pch=20, cex=0.8)

# create predictions from tree for every F x student combo
newF <- seq(1,12,length=12)
lines(tbl$F[1:12], tbl$pred[1:12], col=2, lwd=2)
lines(tbl$F[1:12], tbl$pred[13:24], col=4, lwd=2)
mtext("A simple tree",outer=TRUE,cex=1.5)



###   OVERFITTING AND K/FOLD CROSS VALIDATION   ###
tree_complex<-tree(as.factor(respmail) ~ . , data=ebeer, mindev=0, mincut=100)

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
tree_cut<-prune.tree(tree_complex, best=4)
plot(tree_cut, col=10, lwd=2)
text(tree_cut, cex=1, label="yprob", font=2, digits = 2, pretty = 0)
title(main="A pruned tree")
summary(tree_cut)

pred_tree_ebeer<-predict(tree_cut, data=ebeer)[,2]

plot(roc(ebeer$respmail, pred_tree_ebeer), print.auc=TRUE,
     col="black", lwd=1, main="ROC curve", xlab="Specificity: true negative rate", ylab="Sensitivity: true positive rate", xlim=c(1,0))

# make a somewhat big tree
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
tree_telco<-tree(Churn ~ ., data=telco, mindev=0.005, mincut=0)
plot(tree_telco, col=10, lwd=2)
text(tree_telco, cex=.4, font=1, digits = 2, pretty = 0)

cv.tree_telco<-cv.tree(tree_telco, K=10)
cv.tree_telco

plot(cv.tree_telco$size, cv.tree_telco$dev, xlab="tree size (complexity)", ylab="Out-of-sample deviance (error)", pch=20)
mtext("Another example: telco",outer=TRUE,cex=1.5)

par(mfrow=c(1,1))
tree_cut<-prune.tree(tree_telco, best=6)
plot(tree_cut, col=10, lwd=2)
text(tree_cut, cex=1, font=1, digits = 2, pretty = 0, label="yprob")
