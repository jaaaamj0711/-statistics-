# 데이터 불러오기
data <-read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv") 
data$rank<-factor(data$rank)#대학교의등급, factor로변환
data$admit<-factor(data$admit)
# 데이터 분리
set.seed(123456789)
n <-nrow(data)
idx<-1:n
training_idx<-sample(idx, n * .60)
idx<-setdiff(idx, training_idx)
validate_idx<-sample(idx, n * .20)
test_idx<-setdiff(idx, validate_idx)
training <-data[training_idx,]
validation <-data[validate_idx,]
test <-data[test_idx,]

## 로지스틱 ##
data_logit_full<-glm(admit ~ ., data = training, family = "binomial")   #모형적합
summary(data_logit_full)
library(caret)
install.packages("ROCR")
library(ROCR)
library(e1071)
# 학습데이터의 실제값
y_obs_t <- training$admit 
# 학습데이터 예측
pre_logit_t <- predict(data_logit_full, newdata = training, type = 'response')
#적합값, 실제값
pred_logit_t <- prediction(pre_logit_t, y_obs_t)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_logit_t>0.5,1,0)), y_obs_t) 
# ROC 커브
perf <- performance(pred_logit_t, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve Logistic", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC 
performance(pred_logit_t,"auc")@y.values[[1]] 
# 검증데이터의 실제값
y_obs_v <- validation$admit
pre_logit_v <- predict(data_logit_full, newdata = validation, type = 'response')
pred_logit_v <- prediction(pre_logit_v, y_obs_v)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_logit_v>0.5,1,0)), y_obs_v)
# ROC 커브
perf <- performance(pred_logit_v, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve Logistic", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC 
performance(pred_logit_v, "auc")@y.values[[1]]
# 테스트데이터의 실제값
y_obs_te <- test$admit
pre_logit_te <- predict(data_logit_full, newdata = test, type = 'response')
pred_logit_te <- prediction(pre_logit_te, y_obs_te)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_logit_te>0.5,1,0)), y_obs_te)
# ROC 커브
perf <- performance(pred_logit_te, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve Logistic", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC 
performance(pred_logit_te, "auc")@y.values[[1]]


## 나무 모형 ##
library(rpart)
# 모델 학습
data_tr <- rpart(admit ~ ., data=training)
# 나무 모형 시각화
opar <- par(mfrow = c(1,1), xpd=NA)
plot(data_tr)
text(data_tr, use.n = TRUE)
# 학습데이터 예측
pre_tr_t <- predict(data_tr, newdata = training) 
pred_tr_t <- prediction(pre_tr_t[, "1"], y_obs_t)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_tr_t[,"1"]>0.5,1,0)), y_obs_t)
# ROC 커브
perf <- performance(pred_tr_t, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve Decision tree", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC 
performance(pred_tr_t, "auc")@y.values[[1]]
# 검증데이터 예측
pre_tr_v <- predict(data_tr, newdata = validation)
pred_tr_v <- prediction(pre_tr_v[,"1"], y_obs_v)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_tr_v[,"1"]>0.5,1,0)), y_obs_v)
# ROC 커브
perf <- performance(pred_tr_v, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve Decision tree", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC
performance(pred_tr_v, "auc")@y.values[[1]] 
# 테스트데이터 예측
pre_tr_te <- predict(data_tr, newdata = test)
pred_tr_te <- prediction(pre_tr_te[,"1"], y_obs_te)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_tr_te[,"1"]>0.5,1,0)), y_obs_te)
# ROC 커브
perf <- performance(pred_tr_te, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve Decision tree", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC
performance(pred_tr_te, "auc")@y.values[[1]] 


## 랜덤 포레스트##
library(randomForest)
set.seed(123456789)
# 모델 학습
data_rf <- randomForest(admit ~ ., data=training)
varImpPlot(data_rf)
# 학습데이터 예측
pre_rf_t <- predict(data_rf, newdata = training, type='prob')[,'1']
pred_rf_t<- prediction(pre_rf_t, y_obs_t)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_rf_t>0.5,1,0)), y_obs_t)
# ROC 커브
perf <- performance(pred_rf_t, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve RandomForest", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC
performance(pred_rf_t, "auc")@y.values[[1]] 
# 검증데이터 예측
pre_rf_v <- predict(data_rf, newdata = validation, type="prob")[,'1']
pred_rf_v <- prediction(pre_rf_v, y_obs_v)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_rf_v>0.5,1,0)), y_obs_v)
# ROC 커브
perf <- performance(pred_rf_v, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve RandomForest", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC
performance(pred_rf_v, "auc")@y.values[[1]]
# 테스트데이터 예측
pre_rf_te <- predict(data_rf, newdata = test, type="prob")[,'1']
pred_rf_te <- prediction(pre_rf_te, y_obs_te)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_rf_te>0.5,1,0)), y_obs_te)
# ROC 커브
perf <- performance(pred_rf_v, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve RandomForest", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC
performance(pred_rf_te, "auc")@y.values[[1]]


## 부스팅 ##
library(gbm)
set.seed(123456789)
training$admit <- as.numeric(ifelse(training$admit=='0',0,1))
# 모델 학습
data_gbm <- gbm(admit ~ ., data=training, distribution = "bernoulli", n.trees = 500,
                  cv.folds=5, verbose=TRUE)
(best_iter = gbm.perf(data_gbm, method="cv"))
# 학습데이터 예측
pre_gbm_t <- predict(data_gbm, n.trees=best_iter, newdata = training, type =
                         'response')
pred_gbm_t <- prediction(pre_gbm_t, y_obs_t)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_gbm_t>0.5,1,0)), y_obs_t)
# ROC 커브
perf <- performance(pred_gbm_t, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve Boosting", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC
performance(pred_gbm_t, "auc")@y.values[[1]]
# 검증데이터 예측
pre_gbm_v <- predict(data_gbm, n.trees=best_iter, newdata = validation, type =
                       'response')
pred_gbm_v <- prediction(pre_gbm_v, y_obs_v)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_gbm_v>0.5,1,0)), y_obs_v)
# ROC 커브
perf <- performance(pred_gbm_v, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve Boosting", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
# AUC
performance(pred_gbm_v, "auc")@y.values[[1]]
# 테스트데이터 예측
pre_gbm_te <- predict(data_gbm, n.trees=best_iter, newdata = test, type =
                       'response')
pred_gbm_te <- prediction(pre_gbm_te, y_obs_te)
# 혼돈행렬
confusionMatrix(factor(ifelse(pre_gbm_te>0.5,1,0)), y_obs_te)
# ROC 커브
perf <- performance(pred_gbm_te, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve Boosting", col='blue', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
performance(pred_gbm_te, "auc")@y.values[[1]]


# ROC 커브(train)
perf <- performance(pred_logit_v, measure = "tpr", x.measure = "fpr")
plot(perf, main = "Train ROC curve", col='Blue 2', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
par(new=T)

perf <- performance(pred_tr_v, measure = "tpr", x.measure = "fpr")
plot(perf, col='Red 1', lwd=3)

par(new=T)
perf <- performance(pred_rf_v, measure = "tpr", x.measure = "fpr")
plot(perf, col='orange 1', lwd=3)


par(new=T)
perf <- performance(pred_gbm_v, measure = "tpr", x.measure = "fpr")
plot(perf, col='Green 1', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)

legend(0.55,0.2,c("Logistic AUC: 0.6848071","Decision tree: 0.7473539","RandomForest: 0.9780398", "Boosting: 0.7093901"),cex=0.9,col=c("Blue 2","Red 1","orange 1","Green 1"),lty=1) 

# ROC 커브(Validation)
perf <- performance(pred_logit_v, measure = "tpr", x.measure = "fpr")
plot(perf, main = "Validation ROC curve", col='Blue 2', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)
par(new=T)

perf <- performance(pred_tr_v, measure = "tpr", x.measure = "fpr")
plot(perf, col='Red 1', lwd=3)

par(new=T)
perf <- performance(pred_rf_v, measure = "tpr", x.measure = "fpr")
plot(perf, col='orange 1', lwd=3)


par(new=T)
perf <- performance(pred_gbm_v, measure = "tpr", x.measure = "fpr")
plot(perf, col='Green 1', lwd=3)
abline(a=0, b=1, lwd=2, lty=2)

legend(0.55,0.2,c("Logistic AUC: 0.6531593","Decision tree: 0.5786401","RandomForest: 0.6692995", "Boosting: 0.6177885"),cex=0.9,col=c("Blue 2","Red 1","orange 1","Green 1"),lty=1) 
