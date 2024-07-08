library(e1071)
library(FNN)
#----
#rm(a01);rm(a02);rm(aux);rm(df);rm(df_test);rm(valoresF1);rm(x)
#rm(ACUR);rm(AtualACC);rm(AtualF1);rm(atualizou);rm(desempenho);rm(i);rm(j);rm(Mod_AUX);rm(primeiroF1);rm(TamX);rm(variavel)
#rm(modelo)
#rm(tf);rm(idf)
#----

setwd('C:\\Users\\kleon\\OneDrive\\?rea de Trabalho\\TCCII')
dir()
dados <- read.csv('https://raw.githubusercontent.com/roneysco/Fake.br-Corpus/master/preprocessed/pre-processed.csv')
dados <- dados[,-1]

modelo <- function(df){ # modelo SVM
  trein=sample(1:nrow(df), size=0.7*nrow(df))
  dados.trein = df[trein,]
  dados.test = df[-trein,]
  
  modelo1 <- svm(status ~ . , data = dados.trein, kernel = 'sigmoid')
  preditos1 <- predict(modelo1, dados.test)
  mc = table(preditos1,dados.test$status); mc
  
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  ACC = (VP+VN)/(VP+VN+FN+FP)
  SEN = VP/(VP+FN)
  VPP = VP/(VP+FP)
  F1 = 2*VPP*SEN/(VPP+SEN)
  return(c(ACC,F1))
}

modelo <- function(df){ #modelo KNN
  trein=sample(1:nrow(df), size=0.7*nrow(df))
  dados.trein = df[trein,]
  dados.test = df[-trein,]
  
  mod.KNN=knn(train=dados.trein[,-1001], test=dados.test[,-1001], 
              cl=dados.trein[,1001], k=3)
  
  # Desempenho preditivo (teste):
  mc=table(mod.KNN, dados.test[,1001]); mc # classificou mtas not?cias verdadeiras como fakes
  
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  ACC = (VP+VN)/(VP+VN+FN+FP)
  SEN = VP/(VP+FN)
  VPP = VP/(VP+FP)
  F1 = 2*VPP*SEN/(VPP+SEN)
  return(c(ACC,F1))
}

modelo <- function(df){ # modelo Naive Bayes
  trein=sample(1:nrow(df), size=0.7*nrow(df))
  dados.trein = df[trein,]
  dados.test = df[-trein,]
  
  mod.NB = naiveBayes(status ~ ., data = dados.trein)
  
  # Desempenho preditivo (teste):
  pred=predict(mod.NB, dados.test, type="raw")[,2]
  pred2=rep(0,length(pred))
  pred2[pred>=0.5]=1
  mc=table(pred2, dados.test[,1001]); mc
  
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  ACC = (VP+VN)/(VP+VN+FN+FP)
  SEN = VP/(VP+FN)
  VPP = VP/(VP+FP)
  F1 = 2*VPP*SEN/(VPP+SEN)
  return(c(ACC,F1))
}


#df <- read.csv("MatrizTermos.csv", sep = ';')[,2:1001] #DF Uni-Freq
#df <- read.csv("Matriz_Termo_Bigrama.csv")[!duplicated(dados),-1] #DF Bi-Freq
df <- read.csv("Matriz_Termo_Trigrama.csv")[!duplicated(dados),-1] #DF Tri-Freq

df = as.data.frame(ifelse(df==0,0,1)) #DF X-bin

#------#tf-idf -> w{i,j} = tf{i,j}*log(N/df{i})
#tf <- as.matrix(df)
#idf <- matrix(rep(log(dim(df)[1]/apply(ifelse(tf>0,1,0),2,sum)),dim(df)[1]),nrow = dim(df)[1], byrow = T)
#df <- tf*idf
#df <- as.data.frame(df)

df$status <- as.factor(dados$label[!duplicated(dados)])

x <- read.csv("classe_palavra.csv")[,-1]
x <- x[!duplicated(dados),]
a01 <- read.csv("Polaridade")[,-1];a01$polaridade <- ifelse(a01$polaridade=='negativo',0,1)
a02 <- read.csv("Tamanhos.csv")[,-c(1,4)]; a02 <- a02[!duplicated(dados),]
x <- cbind(x,a01); x <- cbind(x,a02)
TamX <- dim(x)[2]

atualizou <- 'Primeiro'
variavel <- 'ZERO'
primeiroF1 <- modelo(df)
AtualF1 <- primeiroF1[2]

valoresF1 <- NULL
desempenho <- NULL
ACUR <- NULL
for (i in 1:TamX) {
  print(i)
  if(i>1){
    valoresF1[[i-1]] <- cbind(desempenho, names(x))
    if(AtualF1 < desempenho[order(desempenho, decreasing = T)[1]]){
      AtualF1 = desempenho[order(desempenho, decreasing = T)[1]]
      AtualACC = ACUR[order(ACUR, decreasing = T)[1]]
      aux <- as.data.frame(x[,order(desempenho, decreasing = T)[1]])
      names(aux) <- names(x)[order(desempenho, decreasing = T)[1]]
      df <- cbind(df,aux)
      atualizou[i] <- 'sim'
      variavel[i] <- names(x)[order(desempenho, decreasing = T)[1]]
      x <- x[,-(order(desempenho, decreasing = T)[1])]
      desempenho <- NULL
    }else{
      atualizou[i] <- 'nao'
      variavel[i] <- 'nenhuma'
      break
    }
  }
  for (j in 1:dim(x)[2]) {
    df_test <- cbind(df, x[,j])
    Mod_AUX <- modelo(df_test)
    desempenho[j] <- Mod_AUX[2]
    ACUR[j] <- Mod_AUX[1]
  }
}
valoresF1
atualizou; variavel

primeiroF1
AtualACC;AtualF1
