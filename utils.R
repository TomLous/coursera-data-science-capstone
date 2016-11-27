log <- function(...) {
  cat("[preprocess.R] ", ..., "\n", sep="")
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
  dataset <- gsub("[^a-z\\s]", " ", dataset)
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
    return(paste(vec, collapse = " "))
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
  lapply(termList, createNgram, n=n)
}

frequencyTable <- function(termList){
  term <- data.frame(unlist(termList))
  grouped <- as.data.frame(table(term))
  freq <- grouped[order(-grouped$Freq),]
  rownames(freq) <- 1:nrow(freq)
  
  total <- sum(freq$Freq)
  freq->CumFreq <- cumsum(freq$Freq)
  freq->Coverage <- freq->CumFreq/total
  
  return(freq)
}
