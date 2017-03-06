extractLastNWords <- function(string, words = 3) {
        if(is.null(string)) NULL
        string <- string %>% paste("") %>% tolower %>% removeWords(words = profanity) %>%
                removeNumbers %>% removePunctuation #%>%
        #stemDocument(language = "english")
        tokens <- tokenize(string)
        tokens <- tokens[-grep(" ", tokens)]
        tail(tokens, words)
}

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


profanity <- readRDS(file = "./model/profaneWords.rds")
test1 <- readRDS(file = "./model/words1g.rds")
test2 <- readRDS(file = "./model/words2g.rds")
test3 <- readRDS(file = "./model/words3g.rds")
test4 <- readRDS(file = "./model/words4g.rds")