library(tm)
library(RWeka)

#data
corpus <- Corpus(DirSource(directory="data//final//en_US"),
                  readerControl = list(reader = readPlain,
                                       language = "en_US",

                                       load = TRUE))

#size
wc <- list()
wc[[1]] <- sapply(strsplit(corpus[[1]]$content, "\\s+"),length)
wc[[2]] <- sapply(strsplit(corpus[[2]]$content, "\\s+"),length)
wc[[3]] <- sapply(strsplit(corpus[[3]]$content, "\\s+"),length)

lc <- c(length(corpus[[1]]$content),
        length(corpus[[2]]$content),
        length(corpus[[3]]$content))

ss <- data.frame(document=c(meta(corpus[[1]])$id,meta(corpus[[2]])$id,meta(corpus[[3]])$id),
                 lines_count=lc,
                 words_count=sapply(wc,sum),
                 longest_line=sapply(wc,max),
                 avg_line=round(sapply(wc,mean))
                 )
names(ss) <- c("corpus", "line_cnt", "word_cnt", "max_line_words", "avg_line_words")


#cleaning
removeContraction <- function(x) {
    text <- gsub("'", " ", x$content)
    PlainTextDocument(text, id = meta(x)$id, language = meta(x)$language)
}


corpus.clear <- tm_map(corpus, content_transformer(tolower))
corpus.clear <- tm_map(corpus.clear, removeContraction)
corpus.clear <- tm_map(corpus.clear, removePunctuation)
corpus.clear <- tm_map(corpus.clear, removeNumbers)
corpus.clear <- tm_map(corpus.clear, stripWhitespace)


#sampling
sample_doc_lines <- function(x, ratio) {
    n <- length(x$content)
    text <- sample(x$content, n*ratio)
    PlainTextDocument(text, id = meta(x)$id, language = meta(x)$language)
}

sample.corpus <- tm_map(corpus.clear, function(x) sample_doc_lines(x, 0.2))
length(sample.corpus[[1]]$content)
length(sample.corpus[[2]]$content)
length(sample.corpus[[3]]$content)



#tokenization
tdmUni <- TermDocumentMatrix(sample.corpus)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdmBi <- TermDocumentMatrix(sample.corpus, control = list(tokenize = BigramTokenizer))

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdmTri <- TermDocumentMatrix(sample.corpus, control = list(tokenize = TrigramTokenizer))
