sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)

install.packages("tm")
library(tm)

sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)
## lower case
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
## remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
as.character(sms_corpus[[100]])
as.character(sms_corpus_clean[[100]])
## remove stopword "a, we, etc."
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
as.character(sms_corpus[[100]])
as.character(sms_corpus_clean[[100]])
## remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
as.character(sms_corpus[[100]])
as.character(sms_corpus_clean[[100]])

# create custom replacepunctuation
replacePunctuation <- function(x) { gsub("[[:punct:]]+", " ", x)}
replacePunctuation("hello...world")

install.packages("SnowballC")
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
as.character(sms_corpus[[77]])
as.character(sms_corpus_clean[[77]])

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

#sms before cleaning
as.character(sms_corpus[1:3])
#sms after cleaning
as.character(sms_corpus_clean[1:3])

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = 
  list(tolower = TRUE, removeNumbers = TRUE, stopwords = TRUE, removePunctuation = TRUE, stemming = TRUE))
sms_dtm
sms_dtm2

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5558,]

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5558, ]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
prop.table(table(sms_raw$type))

install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw,type == "ham")
wordcloud(spam$text, max.word = 40, scale = c(3,0.5))
wordcloud(ham$text, max.word = 40, scale = c(3,0.5))

sms_freq_words <- findFreqTerms(sms_dtm_train, 5) #word that appear more than 5 times in the corpus
str(sms_freq_words)

sms_dtm_freq_train <- sms_dtm_train[ ,sms_freq_words] #select column appears in sms_freq_words
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

convert_counts <- function(x){
  x <- ifelse(x>0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts) #numerical -> categorical
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_test_pred <- predict(sms_classifier, sms_test)
CrossTable(sms_test_labels, sms_test_pred, prop.chisq = FALSE, prop.t = FALSE, dnn = c('actual', 'predicted'))

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_labels, sms_test_pred2, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('actual', 'predicted'))