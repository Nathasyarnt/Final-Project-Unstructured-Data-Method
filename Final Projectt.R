library(readxl)
library(tm)
library(readr)
library(tokenizers)
library(tibble)
library(tidytext)
library(SnowballC)

#baca data
dataeas <- read_xlsx('C:/Users/IKA ARIANTO/OneDrive - Institut Teknologi Sepuluh Nopember/College.6/Unstructured Data Method/Data.xlsx')
View(dataeas)


#pre process
#ubah data jadi corpus
corpusdata <- Corpus(VectorSource(dataeas$Tweet))
inspect(corpusdata[1:500])

#ubah huruf jadi kecil
casefolding <- tm_map(corpusdata,content_transformer(tolower))
inspect(casefolding[1:500])

#hapus url
removeURL <- function(x) gsub('http[^{:space:]]*','',x)
data_URL <- tm_map(casefolding,content_transformer(removeURL))
inspect(data_URL[1:500])

#hapus mention
removemention <- function(x)gsub("@\\S+","",x)
data_mention <- tm_map(data_URL, removemention)
inspect(data_mention[1:500])

#hapus hastag
removehastag <- function(x)gsub("#\\S+","",x)
data_hastag <- tm_map(data_URL, removehastag)
inspect(data_hastag[1:500])

#hapus tanda baca
data_punctuation <- tm_map (data_hastag, content_transformer(removePunctuation))
inspect(data_punctuation[1:500])

#hapus angka
data_nonumber <- tm_map (data_punctuation, content_transformer(removeNumbers))
inspect(data_nonumber[1:500])

removeEmot <- function(x) gsub("[^\x01-\x7F]", "", x)
data_emot <- tm_map(data_nonumber, content_transformer(removeEmot))
inspect(data_emot[1:500])

#import data stopword
datastopword <- read_xlsx('C:/Users/IKA ARIANTO/OneDrive - Institut Teknologi Sepuluh Nopember/College.6/Unstructured Data Method/EAS/stopwords_ind.xlsx')
View(datastopword)

#remove stopword
removestopword <- tm_map(data_emot,removeWords,datastopword$dictionary)
inspect(removestopword[1:500])

#remove spasi berlebihan
removespace <- tm_map(removestopword, content_transformer(stripWhitespace))
inspect(removespace[1:500])

#simpan clean data
data1 <- data.frame(text=unlist(sapply(removespace[1:"500"], '[')), StringAsFactors=F)
write.csv(data1$text,file="data1.csv")


#tokenizing
tokenize_words(data1$text[1:500])
tft_tokeen_ngram <- tokenize_ngrams(x = data1$text[1:500],
                                    lowercase = TRUE,
                                    n = 3L)
tft_tokeen_ngram

sample_vector <- c(data1$text[1:500])
sample_tibble <- tibble(text = sample_vector)
sample_tibble

token_sentence <- sample_tibble %>%
  unnest_tokens(word, text, token = "sentences", strip_punct=T)
token_sentence

token_words <- sample_tibble %>%
  unnest_tokens(word, text, token = "words", strip_punct=T)
token_words

token_ngram <- sample_tibble %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)
token_ngram

# stemming
SnowballC::wordStem
word <- token_sentence$word
sapply(word, wordStem, language = "english")
stemmed_words <- sapply(word, wordStem, language = "english")
data.frame(word, stemmed_words)

#wordcloud
library(wordcloud)
wordcloud(data1$text,max.words = 100, colors=brewer.pal(8,"Dark2"))

library(sentimentr)
library(data.table)
library(plyr)
data3 <- read.csv("data1.csv")

#analisis sentimen
sentiment(data1$text)
sentiments <- sentiment_by(data1$text)

sentiment_df <- setDF(sentiments)
get_sentiment_class <- function(sentiment_score){
  sentiment_class ="Positive"
  
  if (sentiment_score < -0.3) {
    sentiment_class = "Negative"
  }
  
  else if (sentiment_score < 0.3){
    sentiment_class = "Neutral"
  }
  sentiment_class
}

labeldata <-
  sapply(sentiment_df$ave_sentiment,get_sentiment_class)
labeldata

library(readr)
write.csv(labeldata,file ="labeldata.csv")
View(labeldata)

#naive bayes
library(readxl)
fix_data <- read_xlsx("C:/Users/IKA ARIANTO/OneDrive - Institut Teknologi Sepuluh Nopember/College.6/Unstructured Data Method/EAS/dataweslabel.xlsx")
view(fix_data)
sentimen_data <- data.frame(kelompok = fix_data$kelompok,text=fix_data$text)

library(caret)
library(e1071)

sampel<-sample(1:nrow(sentimen_data), 0.7*nrow(sentimen_data))
data_training<- data.frame(sentimen_data)[sampel,]
data_testing<-data.frame(sentimen_data)[-sampel,]
model<-naiveBayes(kelompok~.,data=data_training)
prediksi<-predict(model,data_testing)
hasil<-confusionMatrix(table(prediksi,data_testing$kelompok))
hasil