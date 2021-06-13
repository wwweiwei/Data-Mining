install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
set.seed(5432)

data <- read.csv("/Users/user/Desktop/clean_data.csv")
head(data)
colnames(data)
class(data)

## language
install.packages("textcat")
library("textcat")
data$lang <- textcat(data$App)
lang_count <- table(data$lang)
sort(lang_count, decreasing = TRUE)

## word cloud
docs <- Corpus(VectorSource(data$App)) # Load the data as a corpus
inspect(docs) # Inspect the content of the document

docs <- tm_map(docs, content_transformer(tolower))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 50)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## Plot word frequencies
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightgray", main ="Most frequent words",
        ylab = "Word frequencies")

