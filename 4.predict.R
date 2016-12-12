source("utils.R")
loadVar("ngramLookupTables", "en_US")

predict <- function(phrase, phraseTables, alpha=0.4, top=10){
  terms <- unlist(tokenize(preProcessData(phrase)))
  #terms <- unlist(strsplit(preProcessData(phrase), "\\s")) #split in list of unigrams
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
  if(nIter < 1) return(suggestions)
  
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
  
  
  suggestions <- rbind(suggestions, phraseTable)
  
  if(nrow(phraseTable)>0){
    return(suggestions)
  }
  
  
  
  
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
  c( "If this isn't the cutest thing you've ever seen, then you must be", "callous", "insane", "insensitive", "asleep"),
  c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd","die","give","sleep","eat"),
  c("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his","horticultural","spiritual","financial","marital"),
  c("I'd give anything to see arctic monkeys this","month","morning","weekend","decade"),
  c("Talking to your mom has the same effect as a hug and helps reduce your","sleepiness","happiness","stress","hunger"),
  c("When you were in Holland you were like 1 inch away from me but you hadn't time to take a","look","minute","picture","walk"),
  c("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the","incident","case","account","matter"),
  c("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each","finger","hand","arm","toe"),
  c("Every inch of you is perfect from the bottom to the","center","middle","top","side"),
  c("I’m thankful my childhood was filled with imagination and bruises from playing","outside","daily","inside","weekly"),
  c("I like how the same people are in almost all of Adam Sandler's","pictures","novels","movies","stories")
  )






for (x in 1:nrow(checks)) {
  res <- predict(checks[x, 1], ngramLookupTables, top=10000)
  matches <- res[res$suggest %in% checks[x,2:5], ]
  
  suggest <- NA
  if(nrow(matches)>0){
    suggest <- matches[1,"suggest"]
  }
  
  cat(paste("\n > ",x,". ",checks[x, 1],": ",suggest,"\n", sep = ""))
  print(head(res))
}
