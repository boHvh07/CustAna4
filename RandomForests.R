ebeer_rf<- ranger(respmail ~ ., data=ebeer, write.forest=TRUE, num.trees = 1000, min.node.size = 25, importance = "impurity", probability=TRUE, seed = 19103)

par(mfrow=c(1,1))
par(mai=c(.9,.8,.2,.2))
sort(ebeer_rf$variable.importance, decreasing = TRUE)

barplot(sort(ebeer_rf$variable.importance, decreasing = TRUE), ylab = "variable importance")

head(ebeer_rf$predictions)

pred<-ebeer_rf$predictions[,2]

confusion_matrix <- (table(ebeer$respmail, pred > 0.5))
confusion_matrix <- as.data.frame.matrix(confusion_matrix)
colnames(confusion_matrix) <- c("No", "Yes")
confusion_matrix$Percentage_Correct <- confusion_matrix[1,]$No/(confusion_matrix[1,]$No+confusion_matrix[1,]$Yes)*100
confusion_matrix[2,]$Percentage_Correct <- confusion_matrix[2,]$Yes/(confusion_matrix[2,]$No+confusion_matrix[2,]$Yes)*100
print(confusion_matrix)

cat('Overall Percentage:', (confusion_matrix[1,1]+confusion_matrix[2,2])/nrow(ebeer)*100)

par(mfrow=c(1,1))
par(mai=c(.9,.8,.2,.2))
plot(roc(as.numeric(ebeer$respmail)-1, pred), print.auc=TRUE,
     col="black", lwd=1, main="ROC curve", xlab="Specificity: true negative rate", ylab="Sensitivity: true positive rate", xlim=c(1,0))

lines(roc(as.numeric(ebeer$respmail)-1, pred_tree_ebeer), print.auc=TRUE,  col="red", lwd=1)

legend('bottomright',legend=c("random forest", "decision tree"),col=c("black","red"), lwd=1)

ebeer_rollout$p_rf <- predict(ebeer_rf, ebeer_rollout)$predictions[,2]
head(ebeer_rollout)