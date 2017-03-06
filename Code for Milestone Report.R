rm(list = ls())
setwd("~/Desktop/Data Science Specialization/P10 Data Science Capstone/data")
setwd("en_US")

# Load Libraries
library(knitr)
library(tm)
library(tau)
library(SnowballC)
library(wordcloud)
library(rJava)
options(mc.cores = 1)
library(RWeka)
# library(devtools)
# install_github("mannau/tm.plugin.sentiment")

# Check if all sample data already exists
existsSample <- sum(setdiff(dir(), "sample") == dir("sample")) == length(setdiff(dir(), "sample"))

# Sample the data
reSample <- TRUE
if(!existsSample | reSample) {
        set.seed(12345)
        for(doc in setdiff(dir(), "sample")) {
                con <- file(doc, "r")
                all <- readLines(con, skipNul = TRUE)
                close(con)
                sub <- all[rbinom(n = length(all) * .1, size = length(all), prob = .5)]
                write.csv(sub, paste0("sample/", gsub(".txt$", "", doc), ".txt"), row.names = FALSE)
        }
        rm(con, all, sub, doc)
}

# Load the Sample Data into a Corpus
sample <- Corpus(DirSource("sample"),
                 readerControl = list(reader = readPlain, language = "en_US", load = TRUE))

# Explore the Corpus
sample # DMetadata
sample[1:3]
summary(sample) # More information
inspect(sample) # More information

sample[[1]]
length(sample[[1]]) # Number of elements in the first text document
length(sample[[1]][[1]]) # Number of lines/posts in the text document

meta(sample[[1]]) # Metadata of the first text document
sample[[1]][[1]] # First element is the whole text document
sample[[1]][[1]][2] # Second line/post of the text document
sample[[1]]$content[2] # Equivalent to sample[[1]][[1]][2]
sample[[1]][[2]] # Second element is the metadata, equivalent to meta(sample[[1]])
sample[[1]]$meta # Equivalent to sample[[1]][[2]]

info <- system("wc -c -l -w sample/en_US.blogs.txt sample/en_US.twitter.txt sample/en_US.news.txt", intern = TRUE)
info <- read.table(textConnection(stripWhitespace(info)), sep = " ")[, c(5, 4, 2, 3)]
names(info) <- c("File", "Size", "Lines", "Words" )
kable(info) # Gives better display in Rmd

# PreProcessing
docs <- sample
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
# Remove Stopwords (for initial analysis, before removing punctuations)
docsKWIC <- tm_map(docs, removeWords, stopwords("english"))
# Remove Punctuations
docs <- tm_map(docs, removePunctuation)
docsKWIC <- tm_map(docsKWIC, removePunctuation)
# Stemming (Erasing Word Suffixes)
#docs <- tm_map(docs, stemDocument, language = "english")
#docsKWIC <- tm_map(docsKWIC, stemDocument, language = "english")
# Strip White Spaces
docs <- tm_map(docs, stripWhitespace)
docsKWIC <- tm_map(docsKWIC, stripWhitespace)
# Convert the Corpus Back to PlainTextDocument
docs <- tm_map(docs, PlainTextDocument)
docsKWIC <- tm_map(docsKWIC, PlainTextDocument)

# Sample Results Before and After Filtering
sample[[3]]$content[50:60]
docs[[3]]$content[50:60]
docsKWIC[[3]]$content[50:60]
sample[[3]]$content[80:90]
docs[[3]]$content[80:90]
docsKWIC[[3]]$content[80:90]

# Term Document Matrix
# 1-gram Analysis (without stopwords)
docs1gTDM <- TermDocumentMatrix(docsKWIC)
docs1gMat <- as.matrix(docs1gTDM)
docs1gCount <- data.frame(term = rownames(docs1gMat),
                          freq = rowSums(docs1gMat),
                          row.names = 1:dim(docs1gMat)[1])
docs1gCount <- docs1gCount[order(docs1gCount$freq, decreasing = TRUE), ]

# 1-gram Analysis (with stopwords)
docs1gTDM2 <- TermDocumentMatrix(docs)
docs1gMat2 <- as.matrix(docs1gTDM2)
docs1gCount2 <- data.frame(term = rownames(docs1gMat2),
                           freq = rowSums(docs1gMat2),
                           row.names = 1:dim(docs1gMat2)[1])
docs1gCount2 <- docs1gCount2[order(docs1gCount2$freq, decreasing = TRUE), ]

# 2-gram Analysis
twoGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
docs2gTDM <- TermDocumentMatrix(docs, control = list(tokenize = twoGramTokenizer))
docs2gMat <- as.matrix(docs2gTDM)
docs2gCount <- data.frame(term = rownames(docs2gMat),
                          freq = rowSums(docs2gMat),
                          row.names = 1:dim(docs2gMat)[1])
docs2gCount <- docs2gCount[order(docs2gCount$freq, decreasing = TRUE), ]

# 3-gram Analysis
threeGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
docs3gTDM <- TermDocumentMatrix(docs, control = list(tokenize = threeGramTokenizer))
docs3gMat <- as.matrix(docs3gTDM)
docs3gCount <- data.frame(term = rownames(docs3gMat),
                          freq = rowSums(docs3gMat),
                          row.names = 1:dim(docs3gMat)[1])
docs3gCount <- docs3gCount[order(docs3gCount$freq, decreasing = TRUE), ]

# Summary
summary <- rbind(summary(docs1gCount$freq), summary(docs2gCount$freq), summary(docs3gCount$freq))
summary <- cbind(c("Unigram", "Bigram", "Trigram"),
                 c(length(docs1gTDM$i), length(docs2gTDM$i), length(docs3gTDM$i)), summary)
colnames(summary)[1:2] <- c("Model", "Total Terms")
kable(summary)

# Distributions of Word Frequency
par(mfrow = c(1, 3))
hist(docs1gCount$freq, breaks = 400, xlim = c(1, 200), col = "red",
     main = "Unigram", xlab = "Number of Occurrence")
hist(docs2gCount$freq, breaks = 400, xlim = c(1, 200), col = "cyan",
     main = "Bigram", xlab = "Number of Occurrence")
hist(docs3gCount$freq, breaks = 30, xlim = c(1, 200), col = "lightgreen",
     main = "Trigram", xlab = "Number of Occurrence")

# Top 5 Most-Frequently Occurred Word(s)
topWords <- cbind(1:5, head(docs1gCount, 5), head(docs2gCount, 5), head(docs3gCount, 5))
colnames(topWords) <- c("Rank", "Unigram", "Freq", "Bigram", "Freq", "Trigram", "Freq")
row.names(topWords) <- NULL
kable(topWords)

# Word Cloud
par(mfrow = c(1, 3))
wordcloud(docs1gCount$term, docs1gCount$freq,
          scale = c(3, 0.3), colors = brewer.pal(8, "Accent"),
          random.order = FALSE, min.freq = 100, max.words = 100)
wordcloud(docs2gCount$term, docs2gCount$freq,
          scale = c(3, 0.5), colors = brewer.pal(8, "Accent"),
          random.order = FALSE, min.freq = 100, max.words = 100)
wordcloud(docs3gCount$term, docs2gCount$freq,
          scale = c(3, 0.3), colors = brewer.pal(8, "Accent"),
          random.order = FALSE, min.freq = 100, max.words = 100)

# Coverage Plot Functions
wordCoverage <- function(freq, p) {
        num <- 1
        cumSums <- cumsum(sort(freq, decreasing = TRUE))
        total <- sum(freq)
        pcts <- round(cumSums[1] / total, 4)
        while(pcts < p) {
                num <- num + 1
                pcts <- round(cumSums[num] / total, 4)
        }
        num
}
# Coverage Plots
par(mfrow = c(1, 3))
docs1g50cov <- wordCoverage(docs1gCount2$freq, 0.5)
docs1g90cov <- wordCoverage(docs1gCount2$freq, 0.9)
with(docs1gCount2, plot(1:nrow(docs1gCount2), round(100 * cumsum(freq) / sum(freq), 2),
                        main = "Unigram Model", type = "l",
                        xlab = "No of Words", ylab = "Coverage (%)"))
lines(rep(docs1g50cov, 2), c(0, 50), col = "red", lty = 2)
lines(c(0, docs1g50cov), rep(50, 2), col = "red", lty = 2)
lines(rep(docs1g90cov, 2), c(0, 90), col = "green", lty = 2)
lines(c(0, docs1g90cov), rep(90, 2), col = "green", lty = 2)
points(c(docs1g50cov, docs1g90cov), c(50, 90), type = "p", cex = 0.5, col = c("red", "green"))
text(x = docs1g50cov + 2500, y = 50, docs1g50cov)
text(x = docs1g90cov + 2500, y = 90 - 2, docs1g90cov)

docs2g50cov <- wordCoverage(docs2gCount$freq, 0.5)
docs2g90cov <- wordCoverage(docs2gCount$freq, 0.9)
with(docs2gCount, plot(1:nrow(docs2gCount), round(100 * cumsum(freq) / sum(freq), 2),
                       main = "Bigram Model", type = "l",
                       xlab = "No of Words", ylab = "Coverage (%)"))
lines(rep(docs2g50cov, 2), c(0, 50), col = "red", lty = 2)
lines(c(0, docs2g50cov), rep(50, 2), col = "red", lty = 2)
lines(rep(docs2g90cov, 2), c(0, 90), col = "green", lty = 2)
lines(c(0, docs2g90cov), rep(90, 2), col = "green", lty = 2)
points(c(docs2g50cov, docs2g90cov), c(50, 90), type = "p", cex = 0.5, col = c("red", "green"))
text(x = docs2g50cov + 15000, y = 50, docs2g50cov)
text(x = docs2g90cov + 15000, y = 90 - 2, docs2g90cov)

docs3g50cov <- wordCoverage(docs3gCount$freq, 0.5)
docs3g90cov <- wordCoverage(docs3gCount$freq, 0.9)
with(docs3gCount, plot(1:nrow(docs3gCount), round(100 * cumsum(freq) / sum(freq), 2),
                       main = "Trigram Model", type = "l",
                       xlab = "No of Words", ylab = "Coverage (%)"))
lines(rep(docs3g50cov, 2), c(0, 50), col = "red", lty = 2)
lines(c(0, docs3g50cov), rep(50, 2), col = "red", lty = 2)
lines(rep(docs3g90cov, 2), c(0, 90), col = "green", lty = 2)
lines(c(0, docs3g90cov), rep(90, 2), col = "green", lty = 2)
points(c(docs3g50cov, docs3g90cov), c(50, 90), type = "p", cex = 0.5, col = c("red", "green"))
text(x = docs3g50cov + 22000, y = 50, docs3g50cov)
text(x = docs3g90cov + 22000, y = 90 - 2, docs3g90cov)

# Store the Words and Required Information into "model" Folder for Model Building
setwd("../..")
if(!file.exists("model")) mkdir("model")
setwd("model")
save(docs1gCount2, file = "words1g.Rda")
save(docs2gCount, file = "words2g.Rda")
save(docs3gCount, file = "words3g.Rda")
save(profanity, file = "profaneWords.Rda")

# Get 4-gram words
fourGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
docs4gTDM <- TermDocumentMatrix(docs, control = list(tokenize = fourGramTokenizer))
docs4gMat <- as.matrix(docs4gTDM)
docs4gCount <- data.frame(term = rownames(docs4gMat),
                          freq = rowSums(docs4gMat),
                          row.names = 1:dim(docs4gMat)[1])
docs4gCount <- docs4gCount[order(docs4gCount$freq, decreasing = TRUE), ]
save(docs4gCount, file = "words4g.Rda")

# Store 90% Coverage Results
coverage90 <- data.frame(model = c("Unigram", "Bigram", "Trigram", "Quadgram"),
                         n = 1:4,
                         words = c(docs1g90cov, docs2g90cov, docs3g90cov,
                                   wordCoverage(docs4gCount$freq, 0.9)))
save(coverage90, file = "coverage90.rda")
