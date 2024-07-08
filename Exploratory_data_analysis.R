library(dplyr)
#library(hrbrthemes)
library(ggplot2)

#DADOS <- read.csv('https://raw.githubusercontent.com/roneysco/Fake.br-Corpus/master/preprocessed/pre-processed.csv')
##------------------------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------

tokens_rank = read.csv("Vocabulario.csv", sep = ';');tokens_rank = tokens_rank[,-1]

a = ggplot(tokens_rank[1:50,], aes(x=reorder(palavras, +n), y=n)) + 
  geom_bar(stat = "identity") + xlab("") +
  scale_y_continuous("Most frequent words", limits=c(0, 45000)) +
  coord_flip() + theme_bw()
b = ggplot(tokens_rank[951:1000,], aes(x=reorder(palavras,+n), y=n)) + 
  geom_bar(stat = "identity") + xlab("") + ylab("low frequency words")+
  coord_flip() + theme_bw()

gridExtra::grid.arrange(a,b, ncol = 2)


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
tokens_rank_F = read.csv("Vocabulario(FAKE).csv", sep = ';')[,-1]
tokens_rank_T = read.csv("Vocabulario(TRUE).csv", sep = ';')[,-1]

a = ggplot(tokens_rank_F[1:50,], aes(x=reorder(palavras,+n), y=100*n/sum(n))) + 
  geom_bar(stat = "identity") + xlab("Words (FAKE)") + ylab("Relative Frequency (%)")+
  coord_flip() + theme_bw()

b = ggplot(tokens_rank_T[1:50,], aes(x=reorder(palavras,+n), y=100*n/sum(n))) + 
  geom_bar(stat = "identity") + xlab("Words (TRUE)") + ylab("Relative Frequency (%)")+
  coord_flip() + theme_bw()


gridExtra::grid.arrange(a,b, ncol = 2)


## Considering the entire data set - The vocabulary of fake news is smaller than the vocabulary of real ones

# link to the function used for grammatical classification of words in the corpus
# https://universaldependencies.org/u/pos/all.html#al-u-pos/X
#library(udpipe)
#ud_model <- udpipe_download_model(language = "portuguese")
#ud_model$file_model
#udmodel_dutch <- udpipe_load_model(file = ud_model$file_model)

Contagem = read.csv("contagem.csv",sep = ';' )[,-1]
data = data.frame(Palavras = c(Contagem$N_palavrasF,Contagem$N_palavrasT),
                  Vocabulario = c(Contagem$N_Vocab_F,Contagem$N_Vocab_T),
                  Clase = c(rep("Fake",3600),rep("True",3600)))
a = ggplot(data, aes(x = Clase, y = Palavras)) + 
  geom_boxplot(outlier.size = 0.5)+ scale_y_continuous("Number of words", limits=c(0, 4500))+
  labs(x = "Type of news", title = "Comparison of the number of words")+
  theme_bw()

b = ggplot(data, aes(x = Clase, y = Vocabulario)) + 
  geom_boxplot(outlier.size = 0.5)+ scale_y_continuous("Vocabulary", limits=c(0, 4500))+
  labs(x = "Type of news", title = "Comparison of the number of words in vocabulary")+
  theme_bw()
gridExtra::grid.arrange(a,b)


data %>%
  ggplot( aes(x=Palavras, fill=Clase)) +
  geom_histogram(bins = 60,color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous("Words")+
  theme_bw() +
  labs(fill="")
data %>%
  ggplot( aes(x=Vocabulario, fill=Clase)) +
  geom_histogram(bins = 22,color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_x_continuous("Vocabulary")+
  theme_bw() +
  labs(fill="")



tokens_rank <- read.csv("Vocabulario.csv", sep = ';')[,-1]
## Bigramas
a = ggplot(tokens_rank[1:50,], aes(x=reorder(palavras, +n), y=n)) + 
  geom_bar(stat = "identity") + xlab("Unigram") +
  scale_y_continuous("", limits=c(0, 45000)) +
  coord_flip() + theme_bw()

BG = read.csv("Vbigramas.csv", sep=",")[,-1]
b = ggplot(BG[1:50,], aes(x=reorder(bigrama, +n), y=n)) + 
  geom_bar(stat = "identity") + xlab("Bigram") +
  scale_y_continuous("", limits=c(0, 5000)) +
  coord_flip() + theme_bw()

TG = read.csv("Vtrigramas.csv",sep = ",")[,-1]
c = ggplot(TG[1:50,], aes(x=reorder(trigrama, +n), y=n)) + 
  geom_bar(stat = "identity") + xlab("Trigram") +
  scale_y_continuous("", limits=c(0, 1500)) +
  coord_flip() + theme_bw()

gridExtra::grid.arrange(b,c,ncol = 2)
