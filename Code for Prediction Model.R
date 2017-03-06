rm(list = ls())
setwd("~/Desktop/Data Science Specialization/P10 Data Science Capstone/model")
for (obj in dir()[grep("Rda$", dir())]) load(obj)

library(stringr)
library(tm)
library(tau)
library(dplyr)

# Choose the Number of n-gram Words that Covers 90% of All Word Counts
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
covPct <- 0.9
no1gWords <- wordCoverage(test1$freq, covPct)
no2gWords <- wordCoverage(test2$freq, covPct)
no3gWords <- wordCoverage(test3$freq, covPct)
no4gWords <- wordCoverage(test4$freq, covPct)
test1 <- head(test1, no1gWords) # 10461 words
test2 <- head(test2, no2gWords) # 3109605 words
test3 <- head(test3, no3gWords) # 13376624 words
test4 <- head(test4, no4gWords) # 19732154 words

# Considers only n-gram words that occurred more than once
# Performance will be significantly enhanced by sacrificing minor accuracies
test1 <- subset(test1, freq > 1) # 10461 words
test2 <- subset(test2, freq > 1) # 1609979 words
test3 <- subset(test3, freq > 1) # 2257769 words
test4 <- subset(test4, freq > 1) # 1346841 words

# Extract n - 1 words from All Terms of n-gram Models
test2$cond <- sub(" \\w+$", "", test2$term)
test3$cond <- sub(" \\w+$", "", test3$term)
test4$cond <- sub(" \\w+$", "", test4$term)
# Extract the last words from All Terms of n-gram Models
test1$pred <- as.character(test1$term)
test2$pred <- str_extract(test2$term, "\\w+$")
test3$pred <- str_extract(test3$term, "\\w+$")
test4$pred <- str_extract(test4$term, "\\w+$")
# Have a look at the dictionaries
head(test2)
head(test3)
head(test4)

example <- "A computer once beat me at chess, but it was no match for me at kickboxing"
extractLastNWords <- function(string, words = 3) {
        if(is.null(string)) NULL
        string <- string %>% paste("") %>% tolower %>% removeWords(words = profanity) %>%
                removeNumbers %>% removePunctuation #%>%
        #stemDocument(language = "english")
        tokens <- tokenize(string)
        tokens <- tokens[-grep(" ", tokens)]
        tail(tokens, words)
}
extractLastNWords(example)

nextWord <- function(string, rank = 5, nWords = 3) {
        words <- extractLastNWords(string, nWords)
        if (length(words) == 3 & paste(words, collapse = " ") %in% test4$cond)
                test4 %>% filter(paste(words, collapse = " ") == test4$cond) %>%
                arrange(desc(freq)) %>% select(pred) %>% head(n = rank) 
        else if (length(words) >= 2 & paste(tail(words, 2), collapse = " ") %in% test3$cond)
                test3 %>% filter(paste(tail(words, 2), collapse = " ") == test3$cond) %>%
                arrange(desc(freq)) %>% select(pred) %>% head(n = rank)
        else if (length(words) >= 1 & tail(words, 1) %in% test2$cond)
                test2 %>% filter(tail(words, 1) == test2$cond) %>%
                arrange(desc(freq)) %>% select(pred) %>% head(n = rank)
        else
                test1 %>% select(pred) %>% head(n = rank)
}
nextWord(example)

nextWordList <- function(string, rank = 5, nWords = 3) {
        if(nchar(string) == 0) NULL
        else {
                models <- "4-gram"
                result <- nextWord(string)
                if(nrow(result) >= rank) head(result, rank)
                else {
                        models <- append(models, "3-gram")
                        newResult <- nextWord(string, nWords = 2)
                        result <- rbind(result, setdiff(newResult, result))
                        if(nrow(result) >= rank) head(result, rank)
                        else {
                                models <- append(models, "2-gram")
                                newResult <- nextWord(string, nWords = 1)
                                result <- rbind(result, setdiff(newResult, result))
                                if(nrow(result) >= rank) head(result, rank)
                                else {
                                        models <- append(models, "1-gram")
                                        newResult <- data.frame(pred = test1$pred[1:rank])
                                        result <- rbind(result, setdiff(newResult, result))
                                        head(result, rank)
                                }
                        }
                }
        }
}

# Test Data from Quiz 3
# q1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
# q2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
# q3 <- "I'd give anything to see arctic monkeys this"
# q4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
# q5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
# q6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
# q7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
# q8 <- "Every inch of you is perfect from the bottom to the"
# q9 <- "I’m thankful my childhood was filled with imagination and bruises from playing"
# q10 <- "I like how the same people are in almost all of Adam Sandler's"
# nextWordList(q1)
# nextWordList(q2)
# nextWordList(q3)
# nextWordList(q4)
# nextWordList(q5)
# nextWordList(q6)
# nextWordList(q7)
# nextWordList(q8)
# nextWordList(q9)
# nextWordList(q10)

test1 <- test1[, c("pred", "freq")]
test2 <- test2[, c("cond", "pred", "freq")]
test3 <- test3[, c("cond", "pred", "freq")]
test4 <- test4[, c("cond", "pred", "freq")]
if(!file.exists("../deployment")) dir.create("../deployment")
saveRDS(test1, file = "../deployment/words1g.rds")
saveRDS(test2, file = "../deployment/words2g.rds")
saveRDS(test3, file = "../deployment/words3g.rds")
saveRDS(test4, file = "../deployment/words4g.rds")
saveRDS(profanity, file = "../deployment/profaneWords.rds")
