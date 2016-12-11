source("utils.R")
load("ngramLookupTables.RData")

predict <- function(phrase, phraseTables, alpha=0.4, top=10){
  terms <- unlist(strsplit(preProcessData(phrase), "\\s")) #split in list of unigrams
  startGram <- min(length(phraseTables), length(terms)+1) # determine the starting n-gram model
  
  suggestions <- rec(startGram, startGram, terms, phraseTables, alpha, NULL)
  
  suggestions <- setNames(aggregate(score ~ suggest,suggestions,max), c("suggest","score"))
  
  suggestions <- suggestions[order(-suggestions$score),]
  
  rownames(suggestions) <- 1:nrow(suggestions)
  
  top <- min(nrow(suggestions), top)
  
  suggestions <- suggestions[1:top, ]
  
  return(suggestions) 
}


rec <- function(nIter, nMax, terms, phraseTables, alpha, suggestions){
  if(nIter <= 1) return(suggestions)
  
  lookupTerms <- tail(terms, n=nIter-1)
  lookup <- paste(lookupTerms, collapse = " ")
  
  
  phraseTable <- phraseTables[[nIter]]
  phraseTable <- phraseTable[phraseTable$lookup == lookup, ]
  phraseTable$score <- phraseTable$prop * alpha^(nMax-nIter)
  
  maxRows <- min(nrow(phraseTable), 1000)
  if(maxRows > 0){
    phraseTable$n <- nIter
    phraseTable <- phraseTable[1:maxRows,c("suggest","score","n")]
  }
  
  if(nrow(phraseTable)>0){
    # return(phraseTable)
  }
  
  suggestions <- rbind(suggestions, phraseTable)
  
  
  return(rec(nIter-1, nMax, terms, phraseTables, alpha, suggestions))
}







checks <- rbind(
  c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "beer", "cheese", "pretzels", "soda"), 
  c( "You're the reason why I smile everyday. Can you follow me please? It would mean the", "world", "most", "universe", "best"), 
  c( "Hey sunshine, can you follow me and make me the", "smelliest", "saddest", "bluest", "happiest"), 
  c( "Very early observations on the Bills game: Offense still struggling but the", "referees", "players", "defense", "crowd"), 
  c( "Go on a romantic date at the", "movies", "mall", "grocery", "beach"), 
  c( "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", "motorcycle", "way", "phone", "horse"), 
  c( "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", "time", "weeks", "thing", "years"), 
  c( "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "eyes", "fingers", "toes", "ears"), 
  c( "Be grateful for the good times and keep the faith during the", "sad", "bad", "hard", "worse"), 
  c( "If this isn't the cutest thing you've ever seen, then you must be", "callous", "insane", "insensitive", "asleep"))


for (x in 1:nrow(checks)) {
  res <- predict(checks[x, 1], ngramLookupTables, top=10000)
  matches <- res[res$suggest %in% checks[x,2:5], ]
  
  suggest <- NA
  if(nrow(matches)>0){
    suggest <- matches[1,"suggest"]
  }
  
  print(matches)
  
  print(paste(x,checks[x, 1],":",suggest))
}
