rm(list = ls())
setwd("~/Desktop/Data Science Specialization/P10 Data Science Capstone/data")
setwd("en_US")

# Load Libraries
library(Hmisc)
library(knitr)
library(tm)
library(tau)
library(SnowballC)
library(wordcloud)
library(rJava)
library(sqldf)
options(mc.cores = 1)
library(RWeka)

blogs <- readLines("en_US.blogs.txt", skipNul = TRUE)
news <- readLines("en_US.news.txt", skipNul = TRUE)
twitter <- readLines("en_US.twitter.txt", skipNul = TRUE)
sampleFrom <- 1
sampleRep <- 6
groups <- 20
grp1 <- cut2(1:length(blogs), g = groups, onlycuts = TRUE)
grp2 <- cut2(1:length(news), g = groups, onlycuts = TRUE)
grp3 <- cut2(1:length(twitter), g = groups, onlycuts = TRUE)

for(sampleNo in sampleFrom:sampleRep) {
        print(paste("Loading sample no.", sampleNo))
        if(sampleNo == sampleRep & FALSE) {
                blogsSub <- blogs[grp1[sampleNo]:length(blogs)]
                newsSub <- news[grp2[sampleNo]:length(news)]
                twitterSub <- twitter[grp3[sampleNo]:length(twitter)]
        }
        else {
                blogsSub <- blogs[grp1[sampleNo]:(grp1[sampleNo + 1] - 1)]
                newsSub <- news[grp2[sampleNo]:(grp2[sampleNo + 1] - 1)]
                twitterSub <- twitter[grp3[sampleNo]:(grp3[sampleNo + 1] - 1)]
        }
        print("Writing files...")
        write.csv(blogsSub, paste0("sample/blogs.txt"), row.names = FALSE)
        write.csv(newsSub, paste0("sample/news.txt"), row.names = FALSE)
        write.csv(twitterSub, paste0("sample/twitter.txt"), row.names = FALSE)
        rm(blogsSub, newsSub, twitterSub)
        # Load the Sample Data into a Corpus
        print("Reading sample into corpus...")
        docs <- Corpus(DirSource("sample"),
                         readerControl = list(reader = readPlain, language = "en_US", load = TRUE))
        # PreProcessing
        print("Preprocessing the corpus...")
        # Convert Special Characters, URLs to Spaces
        toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
        docs <- tm_map(docs, toSpace, pattern = "#|/|@|\\|")
        docs <- tm_map(docs, toSpace, pattern = "(f|ht)tp(s?)://(.*)[.][a-z]+")
        docs <- tm_map(docs, toSpace, pattern = "www.(.*)[.][a-z]+")
        # Transform Words to Lowercase
        docs <- tm_map(docs, tolower)
        # Remove Profanity Words Before Filtering Numbers
        profanity <- read.csv("../en_profanity_words", header = FALSE,
                              stringsAsFactors = FALSE)
        profanity <- profanity$V1
        docs <- tm_map(docs, removeWords, words = profanity)
        # Remove Numbers
        docs <- tm_map(docs, removeNumbers)
        # Remove Punctuations
        docs <- tm_map(docs, removePunctuation)
        # Strip White Spaces
        docs <- tm_map(docs, stripWhitespace)
        # Convert the Corpus Back to PlainTextDocument
        docs <- tm_map(docs, PlainTextDocument)
        
        # Get 1-gram Words
        print("Collecting 1-gram words...")
        docs1gTDM <- TermDocumentMatrix(docs)
        docs1gMat <- as.matrix(docs1gTDM)
        docs1gCount <- data.frame(term = rownames(docs1gMat),
                                  freq = rowSums(docs1gMat),
                                  row.names = 1:dim(docs1gMat)[1])
        docs1gCount <- docs1gCount[order(docs1gCount$freq, decreasing = TRUE), ]
        rm(docs1gTDM, docs1gMat)
        # Get 2-gram Words
        print("Collecting 2-gram words...")
        twoGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
        docs2gTDM <- TermDocumentMatrix(docs, control = list(tokenize = twoGramTokenizer))
        docs2gMat <- as.matrix(docs2gTDM)
        docs2gCount <- data.frame(term = rownames(docs2gMat),
                                  freq = rowSums(docs2gMat),
                                  row.names = 1:dim(docs2gMat)[1])
        docs2gCount <- docs2gCount[order(docs2gCount$freq, decreasing = TRUE), ]
        rm(docs2gTDM, docs2gMat)
        # Get 3-gram Words
        print("Collecting 3-gram words...")
        threeGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
        docs3gTDM <- TermDocumentMatrix(docs, control = list(tokenize = threeGramTokenizer))
        docs3gMat <- as.matrix(docs3gTDM)
        docs3gCount <- data.frame(term = rownames(docs3gMat),
                                  freq = rowSums(docs3gMat),
                                  row.names = 1:dim(docs3gMat)[1])
        docs3gCount <- docs3gCount[order(docs3gCount$freq, decreasing = TRUE), ]
        rm(docs3gTDM, docs3gMat)
        # Get 4-gram words
        print("Collecting 4-gram words...")
        fourGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
        docs4gTDM <- TermDocumentMatrix(docs, control = list(tokenize = fourGramTokenizer))
        docs4gMat <- as.matrix(docs4gTDM)
        docs4gCount <- data.frame(term = rownames(docs4gMat),
                                  freq = rowSums(docs4gMat),
                                  row.names = 1:dim(docs4gMat)[1])
        docs4gCount <- docs4gCount[order(docs4gCount$freq, decreasing = TRUE), ]
        rm(docs4gTDM, docs4gMat)
        
        if(sampleNo == 1) {
                words1g <- docs1gCount
                words2g <- docs2gCount
                words3g <- docs3gCount
                words4g <- docs4gCount
        }
        else {
                load(file = "../../model/words1g.Rda")
                load(file = "../../model/words2g.Rda")
                load(file = "../../model/words3g.Rda")
                load(file = "../../model/words4g.Rda")
                words1g <- rbind(words1g, docs1gCount)
                words2g <- rbind(words2g, docs2gCount)
                words3g <- rbind(words3g, docs3gCount)
                words4g <- rbind(words4g, docs4gCount)
        }
        print("Saving training data...")
        save(words1g, file = "../../model/words1g.Rda")
        save(words2g, file = "../../model/words2g.Rda")
        save(words3g, file = "../../model/words3g.Rda")
        save(words4g, file = "../../model/words4g.Rda")
        rm(docs1gCount, docs2gCount, docs3gCount, docs4gCount)
        rm(words1g, words2g, words3g, words4g)
        print(paste("Sampling no.", sampleNo, "completed!"))
}

test1 <- sqldf("SELECT term, sum(freq) AS freq FROM words1g GROUP BY term ORDER BY freq DESC")
test2 <- sqldf("SELECT term, sum(freq) AS freq FROM words2g GROUP BY term ORDER BY freq DESC")
test3 <- sqldf("SELECT term, sum(freq) AS freq FROM words3g GROUP BY term ORDER BY freq DESC")
test4 <- sqldf("SELECT term, sum(freq) AS freq FROM words4g GROUP BY term ORDER BY freq DESC")
save(test1, file = "../../model/words1g.Rda")
save(test2, file = "../../model/words2g.Rda")
save(test3, file = "../../model/words3g.Rda")
save(test4, file = "../../model/words4g.Rda")
