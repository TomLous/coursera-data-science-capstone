library(data.table)
library(ngram)
library(stringr)
library(plyr)

log <- function(..., obj1=NULL, obj2=NULL) {
  #cat(file=stderr(), exists("verbose"))
  #cat(file=stderr(), verbose)
  if(exists("verbose") && verbose){
    cat(file=stderr(),"-------------------------------")
    cat(file=stderr(),"\n> ", ..., "\n", sep=" ")
    if(!is.null(obj1)){
      print(obj1)
    }
    if(!is.null(obj2)){
      print(obj2)
    }
  }
}





sampleFile <- function(filePath, sampleFactor=0.1, recreate=FALSE){
  log("Sampling `",filePath,"` with factor: ", sampleFactor)
  
  # define target sample file
  sampleFilePath <- gsub(".txt",".sample.txt",gsub("final", "sample", filePath))
  metaSampleFilePath <- paste0(sampleFilePath,".meta")
  locale <- gsub(".*([a-z]{2}_[A-Z]{2}).*", "\\1",filePath)
  
  # as long as sample != final
  if(sampleFilePath == filePath) stop(paste0("Invalid filepath: ",filePath))
  if(sampleFactor < 0 || sampleFactor > 1) stop(paste0("Invalid sampleFactor [0 < s < 1] : ",sampleFactor))
  
  if(file.exists(sampleFilePath) && file.exists(metaSampleFilePath) && !recreate) {
    log("Already exists `",sampleFilePath, "` & `",metaSampleFilePath,"`")
    load(metaSampleFilePath)
    sampleLines <- readLines(sampleFilePath)
  }
  else{
    log("Creating `",sampleFilePath, "` & `",metaSampleFilePath,"`")
    dir.create(dirname(sampleFilePath), showWarnings = TRUE, recursive = TRUE, mode = "0777")
    
    dirName <- dirname(filePath)
    fileName <- basename(filePath)
    
    orgLines <- readLines(filePath)
    orgLinesLength <- nchar(orgLines)
    orgLinesLengthSummary <- summary(orgLinesLength)
    orgLinesCount <- length(orgLines)
    
    sampleLines <- orgLines[rbinom(orgLinesLength, 1,  sampleFactor)==1]
    writeLines(sampleLines, sampleFilePath)
    save(orgLinesLength, orgLinesLengthSummary, orgLinesCount, file=metaSampleFilePath)
  }
  
  retData <- list(sample.data=sampleLines, 
                  org.lines.length=orgLinesLength,
                  org.lines.summary=orgLinesLengthSummary,
                  org.lines.count=orgLinesCount, 
                  org.file.path=filePath, 
                  sample.file.path=sampleFilePath, 
                  sample.meta.file.path=metaSampleFilePath,
                  locale=locale)
  
  return(retData)
}


createAllSamples <- function(startDir, sampleFactor, recreate=FALSE){
  if(sampleFactor < 0 || sampleFactor > 1) stop(paste0("Invalid sampleFactor [0 < s < 1] : ",sampleFactor))
  
  orgFiles <- list.files(startDir,pattern=".txt",full.names=TRUE,recursive=TRUE)
  fileData <- lapply(orgFiles, sampleFile, sampleFactor=sampleFactor, recreate=recreate)
  
  retData = list()

  for(i in 1:length(fileData)){
    log("Assign to info$",basename(fileData[[i]]$org.file.path))
    retData[basename(fileData[[i]]$org.file.path)] <- fileData[i]
  }
  
  return(retData)
}

tokenize <- function(dataset, flatten=FALSE){
  dataset <- unlist(strsplit(dataset, "[\\.\\,!\\?\\:]+"))
  dataset <- tolower(dataset)
  dataset <- gsub("[^a-z'\\s]", " ", dataset)
  dataset <- gsub("\\s+", " ", dataset)
  dataset <- trimws(dataset)
  dataset <- strsplit(dataset, "\\s")
  if(!flatten){
    return(dataset)
  }else{
    terms <- unlist(dataset)
    indexes <- which(terms == "")
    if(length(terms) > 0){
      terms <- terms[-indexes]
    } 
    return(terms)
  }
}

profanityFilter <- function(termList, locale){
  #https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
  profanities <- readLines(paste0("data/config/",locale,"/profanity.txt"))
  lapply(termList, setdiff, y=profanities)
}

stopwordFilter <- function(termList, locale){
  #http://www.ranks.nl/stopwords/
  stopwords <- readLines(paste0("data/config/",locale,"/stopwords.txt"))
  lapply(termList, setdiff, y=stopwords)
}

filterShort <- function(vec, minLength){
  vec[nchar(vec) >=minLength]
}

shortTermFilter <- function(termList, minLength){
  lapply(termList, filterShort, minLength=minLength)
}

createNgram <- function(vec, n=2){
  l <- length(vec) 
  if(l < n){
    return(c())
  }else if(l == n){
    return(paste(vec, collapse=" "))
  }else{
    numNgrams <- l-n+1
    mtrx <- matrix(nrow=numNgrams, ncol=n)
    for(i in 1:n){
      m <- l - n + i
      mtrx[,i] <- vec[i:m]
    }
    ngrams <- apply(mtrx, 1, paste, collapse=" ")
    return(ngrams)
  }
} 

transformNGram <- function(termList, n=2){
  if(n <= 1) unlist(termList)
  else unlist(lapply(termList, createNgram, n=n))
}

#createNgram <- function(vec, n=2){
#  l <- length(vec) 
#  if(l < n){
#    return(c())
#  }else if(l == n){
#    return(list(vec))
#  }else{
#    numNgrams <- l-n+1
#    mtrx <- matrix(nrow=numNgrams, ncol=n)
#    for(i in 1:n){
#      m <- l - n + i
#      mtrx[,i] <- vec[i:m]
#    }
#    ngrams <- unname(split(mtrx, c(row(mtrx))))
#    return(ngrams)
#  }
#} 

#transformNGram <- function(termList, n=2){
#  unlist(sapply(termList, createNgram, n=n), recursive = FALSE)
#}

frequencyTable <- function(termList, n){
  grouped <- as.data.frame(table(termList), stringsAsFactors=FALSE)
  rm(termList); gc(verbose=FALSE)
  
  grouped <- rename(grouped, c("Freq"="freq"))
  colnames(grouped)[1] <- "ngrams"
  
  phraseTable <- grouped[order(-grouped$freq),]
  rm(grouped); gc(verbose=FALSE)
  
  rownames(phraseTable) <- 1:nrow(phraseTable)
  
  total <- sum(phraseTable$freq)
  fraction <- 1 / total
  
  phraseTable$prop <- phraseTable$freq * fraction
  #phraseTable$cumfreq <- cumsum(phraseTable$freq)
  #phraseTable$coverage <- phraseTable$cumfreq/total
  phraseTable$n <- n
  
  #terms <- str_split(trimws(phraseTable$ngrams),"\\s")
  #phraseTable$suggest <- sapply(terms, tail, n=1)
  if(n > 1){
    phraseTable$idx <- regexpr(" [^ ]*$", phraseTable$ngrams)
    phraseTable$suggest <- substr(phraseTable$ngrams, phraseTable$idx+1, nchar(phraseTable$ngrams))
    phraseTable$lookup <-  substr(phraseTable$ngrams, 1, phraseTable$idx-1)
  }else{
    phraseTable$idx <- NA
    phraseTable$suggest <- phraseTable$ngrams
    phraseTable$lookup <- ""
  }
  
  #rm(terms); gc(verbose=FALSE)
  
  return(phraseTable)
}

filterFrequencyTable <- function(freqTable, binCoverageShift=0.01){
  shiftTotal <- 0
  
  freqTable$keep <- FALSE
  
  for (n in 1:nrow(freqTable)) {
    shiftTotal <- shiftTotal + freqTable$CovarageShift[n]
    
    if(shiftTotal >= binCoverageShift){
      freqTable$keep[n] <- TRUE
      shiftTotal <- 0
    }
  }
  
  freqTable[freqTable$keep == TRUE,]
  
}


lookupTable <- function(freqTable, minFreq=2){
  lookup <- freqTable[freqTable$freq >= minFreq,]
  a <- lookup[!duplicated(lookup$lookup),]
  #other <- lookup[duplicated(lookup$lookup),]
  #b <- other[!duplicated(other$lookup),]
  #other <- other[duplicated(other$lookup),]
  #c <- other[!duplicated(other$lookup),]
  #other <- other[duplicated(other$lookup),]
  #d <- other[!duplicated(other$lookup),]
  lookup <- a #rbind(a)
  lookup <- lookup[,c("lookup","suggest","prop","n","freq")]
  return(lookup)
}

coverageFactor <- function(freqTable, coverage){
  pos <- nrow(freqTable[freqTable$Coverage < coverage,])
  pos / nrow(freqTable) 
}


preProcessData <- function(datasetIn){
  dataset <- sapply(datasetIn, preprocess, case = "lower", remove.punct = FALSE, remove.numbers = TRUE, fix.spacing = TRUE)
  names(dataset) <- NULL
  dataset <- unlist(strsplit(dataset, "[\\.\\!\\?:;]+"))
  dataset <- gsub("[`'’‘']", "'", dataset)
  dataset <- gsub("[^a-z0-9\\'\\-_\\\\\\s]", " ", dataset)
  dataset <- gsub("\\s+", " ", dataset)
  dataset <- trimws(dataset)
  dataset <- dataset[!nchar(dataset) == 0]
  return(dataset)
}


preProcessString <- function(str){
  paste(preProcessData(str), collapse = ' ')
}

missingVar <- function(varname, locale, postfix=NULL){
  if(!exists(varname, envir=globalenv())){
    loadVar(varname, locale, postfix)
  }
  return(!exists(varname, envir=globalenv()))
}

loadVar  <- function(varname, locale, postfix=NULL){
    fname <- paste0("data/cache/",varname,".",locale,postfix,".RData")
    if(file.exists(fname)){
      log("Loading var from file", varname, fname)
      try(load(fname, envir=globalenv()), silent = TRUE)
    }
}



storeVar <- function(var, locale, postfix=NULL, force=FALSE){
  varname = deparse(substitute(var))
  
  log("Storing var: ", varname)
  
  fname <- paste0("data/cache/",varname,".",locale,postfix,".RData")
  if(!file.exists(fname) || force){
    log("Storing var to file", varname, " => ", fname)
    save(list=c(varname), file=fname)
  }else{
    log("Already exists", fname)
  }
}




cleanup <- function(envvar){
  suppressWarnings(try(rm(list=c(deparse(substitute(envvar))), envir=globalenv()), silent = TRUE))
  suppressWarnings(try(rm(list=c(deparse(substitute(envvar))),  envir=parent.frame()), silent = TRUE))
  invisible(gc(verbose = FALSE))
}

predictSuggest <- function(phrase, phraseTables, completeWord=TRUE, alpha=0.4){
  predictTop(phrase, phraseTables, completeWord, alpha)[1, ]
}

predictTop <- function(phrase, phraseTables, completeWord=TRUE, alpha=0.4, top=10){
    
    terms <- unlist(tokenize(preProcessData(phrase)))
    #terms <- unlist(strsplit(preProcessData(phrase), "\\s")) #split in list of unigrams
    startGram <- min(length(phraseTables), length(terms)+1) # determine the starting n-gram model
    
    suggestions <- recursiveLookup(startGram, startGram, terms, phraseTables, alpha, NULL, completeWord)
    
    suggestions <- setNames(aggregate(score ~ suggest + lookup + n,suggestions,max), c("suggest","lookup","n", "score"))
    
    suggestions <- suggestions[order(-suggestions$n, -suggestions$score),]
    
    rownames(suggestions) <- 1:nrow(suggestions)
    
    top <- min(nrow(suggestions), top)
    
    suggestions <- suggestions[1:top, ]
    
    return(suggestions) 
    
}


recursiveLookup <- function(nIter, nMax, terms, phraseTables, alpha, suggestions, completeWord=TRUE){
  if(nIter < 1) return(suggestions)
  
  lookupTerms <- tail(terms, n=nIter-1)
  lookup <- paste(lookupTerms, collapse = " ")
  
  
  phraseTable <- phraseTables[[nIter]]
  if(completeWord){
    phraseTable <- phraseTable[phraseTable$lookup == lookup, ]
  }else{
    phraseTable <- phraseTable[startsWith(phraseTable$lookup,lookup), ]
  }
  phraseTable$score <- phraseTable$prop * alpha^(nMax-nIter)
  
  maxRows <- min(nrow(phraseTable), 1000)
  if(maxRows > 0){
    phraseTable$n <- nIter
    phraseTable <- phraseTable[1:maxRows,c("suggest","score","n","lookup")]
  }
  
  
  suggestions <- rbind(suggestions, phraseTable)
  
  if(nrow(phraseTable)>0){
   # return(suggestions)
  }
  
  
  return(recursiveLookup(nIter-1, nMax, terms, phraseTables, alpha, suggestions, completeWord))
}

concatWithOverlap<- function(str1, str2){
  clean1 <- preProcessString(str1)
  clean2 <- preProcessString(str2)
  minI <- min(nchar(clean1),nchar(clean2))
  
  #cat(paste0("Testing `",clean1,"` `",str2,"`\n"))
  repeat{
    left <- str_sub(clean2,1,minI)
    right <- str_sub(clean1,-minI)
    if(left == right){
      #cat(paste("Found",minI,"\n"))
      loop <- FALSE 
    }
    else
    {
      #cat(paste0(" `",left,"`!=`",right,"`\n"))
      minI <- minI - 1 
      loop <- minI > 0
    }
    if(!loop){
      break
    }
  }
  
  paste0(str1,str_sub(str2,minI+1))#, " [",minI,"]")
}
