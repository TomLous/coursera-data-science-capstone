source("utils.R")

locale <- "en_US"



# load
twitter <- readLines(paste0("data/final/",locale,"/",locale,".twitter.txt"))
news <- readLines(paste0("data/final/",locale,"/",locale,".news.txt"))
blogs <- readLines(paste0("data/final/",locale,"/",locale,".blogs.txt"))

# combine
#corpusEn <- c(twitterEN$sample.data, newsEN$sample.data, blogsEN$sample.data)
corpus <- c(twitter, news, blogs)

# preprocess

corpusPreprocessed <- preProcessData(corpus)

corpusTokenized <-  tokenize(corpusPreprocessed)
corpusTokenized <- profanityFilter(corpusTokenized, locale)

# ngram
uniGram <- unlist(corpusTokenized)
biGram <- transformNGram(corpusTokenized, 2)
triGram <- transformNGram(corpusTokenized, 3)
quadGram <- transformNGram(corpusTokenized, 4)

# ngram
uniGram <- ngram(corpusEnPreprocessed[str_count(corpusEnPreprocessed, " ") >= 0], n=1, sep = " ")
biGram <- ngram(corpusEnPreprocessed[str_count(corpusEnPreprocessed, " ") >= 1], n=2, sep = " ")
triGram <- ngram(corpusEnPreprocessed[str_count(corpusEnPreprocessed, " ") >= 2], n=3, sep = " ")
quadGram <- ngram(corpusEnPreprocessed[str_count(corpusEnPreprocessed, " ") >= 3], n=4, sep = " ")

#grams <- c(uniGram, biGram, triGram, quadGram)

#generate usefull tables for lookup
genPhraseTable <- function(gram){
  n <- gram@n
  phraseTable <- get.phrasetable(gram)
  terms <- str_split(trimws(phraseTable$ngrams),"\\s")
  phraseTable$suggest <- sapply(terms, tail, n=1)
  
  nTerms <- n-1
  phraseTable$lookup <-  sapply(lapply(terms, head, n=nTerms), paste, collapse=" ")
  phraseTable$n <- n
  
  return(phraseTable)
}

uniGramL <- genPhraseTable(uniGram)
biGramL <- genPhraseTable(biGram)
triGramL <- genPhraseTable(triGram)
quadGramL <- genPhraseTable(quadGram)

twitterENTokenized <- tokenize(twitterEN$sample.data)
twitterENTokenized <- profanityFilter(twitterENTokenized, twitterEN$locale)
#twitterENTokenized <- stopwordFilter(twitterENTokenized, twitterEN$locale)
#twitterENTokenized <- shortTermFilter(twitterENTokenized, 2)

twitterENBiGrams <- transformNGram(twitterENTokenized, 2)



#ngramLookupTables <- lapply(grams, genPhraseTable)

ngramLookupTables <- rbind(uniGramL, biGramL, triGramL, quadGramL)

#store 
save(ngramLookupTables, file="ngramLookupTables.RData")

