library(FNN)
library(caret)
library(e1071)
library(randomForest)

# Division into training (70%) and testing (30%):
dados = read.csv("MatrizTermos.csv", sep = ';')[,-1]
table(dados$status)
dados$status <- as.factor(dados$status)

trein = sample(1:nrow(dados), size=0.7*nrow(dados))
dados.trein = dados[trein,]
dados.test = dados[-trein,]
table(dados.trein$status)
table(dados.test$status)

a = dados.test[,-(names(dados.test) == 'status')]
sum(names(dados.test) == 'status')


# KNN---------------------------------------------------------------------------------------
mod.KNN=knn(train=dados.trein[,-2001], test=dados.test[,-2001], cl=dados.trein[,2001], k=7)

# Predictive performance (testing)
mc=table(mod.KNN, dados.test[,2001]); mc 
VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
ACC=(VP+VN)/(VP+VN+FN+FP)
MCC=(VP*VN-FP*FN)/(sqrt(VP+FP)*sqrt(VP+FN)*sqrt(VN+FP)*sqrt(VN+FN))
SEN=VP/(VP+FN)
VPP=VP/(VP+FP)
F1=2*VPP*SEN/(VPP+SEN)
ACC; MCC; SEN; VPP; F1


# SVM
#set.seed(123)
## tune kernel, cost

#tune.out=tune(svm ,status~.,data=dados ,kernel ="linear",
#              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))
#summary(tune.out)

#tune.out=tune(svm ,status~.,data=dados.trein , cost = 5,
#              ranges=list(kernel=c("linear", "polynomial",
#                                   "radial", "sigmoid")))
#summary(tune.out)

#bestmod=tune.out$best.model
#summary(bestmod)

df <- dados[sample(1:nrow(dados), size=nrow(dados)),]
#df$Status <- as.factor(df$Status)

modelo1 <- svm(status ~ . , data = dados.trein, kernel = 'sigmoid')
preditos1 <- predict(modelo1, dados.test)
mc = table(preditos1,dados.test$status)

VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
ACC = (VP+VN)/(VP+VN+FN+FP)
MCC = (VP*VN-FP*FN)/(sqrt(VP+FP)*sqrt(VP+FN)*sqrt(VN+FP)*sqrt(VN+FN))
SEN = VP/(VP+FN)
VPP = VP/(VP+FP)
F1 = 2*VPP*SEN/(VPP+SEN)

ACC;MCC;SEN;VPP;F1

# Naive Bayes------------------------------------------
mod.NB = naiveBayes(status ~ ., data = dados.trein)

# Predictive performance (testing)
pred=predict(mod.NB, dados.test, type="raw")[,2]
pred2=rep(0,length(pred))
pred2[pred>=0.5]=1
mc=table(pred2, dados.test[,2001]); mc
VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
ACC=(VP+VN)/(VP+VN+FN+FP)
MCC=(VP*VN-FP*FN)/(sqrt(VP+FP)*sqrt(VP+FN)*sqrt(VN+FP)*sqrt(VN+FN))
SEN=VP/(VP+FN)
VPP=VP/(VP+FP)
F1=2*VPP*SEN/(VPP+SEN)
ACC; MCC; SEN; VPP; F1


# Random Forest -------------------------------
ajuste=randomForest(status~., mtry=round(sqrt(ncol(dados.trein[,-2001]))), importance=TRUE, ntree=5000, data=dados.trein)	# Bagging: basta tomar mtry=4 (todas as var. preditoras)
#importance(ajuste)	# importancia de cada variavel
varImpPlot(ajuste)	# grafico de importancia

# Desempenho preditivo (teste):
predito=predict(ajuste, dados.test)
mc=table(predito, dados.test[,2001]); mc
VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
ACC=(VP+VN)/(VP+VN+FN+FP)
MCC=(VP*VN-FP*FN)/(sqrt(VP+FP)*sqrt(VP+FN)*sqrt(VN+FP)*sqrt(VN+FN))
SEN=VP/(VP+FN)
VPP=VP/(VP+FP)
F1=2*VPP*SEN/(VPP+SEN)
ACC; MCC; SEN; VPP; F1
