source("utils.R")

locale <- "en_US"
folder <- "sample"
filePostfix <- if(folder == "sample") ".sample" else ""
grams <- 7
minFreq <- 2
verbose <- TRUE
debug <- TRUE

# load the corpus and tokenize it
if(missingVar("corpusTokenized",locale)){
  if(missingVar("corpusPreprocessed",locale)){
    if(missingVar("corpus",locale)){
      
      # load source data
      log("Loading data", folder, locale)
      twitter <- readLines(paste0("data/",folder,"/",locale,"/",locale,".twitter",filePostfix,".txt"))
      news <- readLines(paste0("data/",folder,"/",locale,"/",locale,".news",filePostfix,".txt"))
      blogs <- readLines(paste0("data/",folder,"/",locale,"/",locale,".blogs",filePostfix,".txt"))
      
      # combine into 1 big var
      corpus <- c(twitter, news, blogs)
      
      # remove old vars (memory management)
      cleanup(twitter)
      cleanup(news)
      cleanup(blogs)
      
      # cache file 
      storeVar(corpus, locale)
    }
    
    # preprocess raw text files into seperate lines
    log("Preprocessing corpus, # lines:", length(corpus))
    corpusPreprocessed <- preProcessData(corpus)
    
    # cache preprocessed
    storeVar(corpusPreprocessed, locale)
    cleanup(corpus)
  }
  
  
  # Tokenize all string
  log("Tokenizing corpus, # lines:", length(corpusPreprocessed))
  corpusTokenized <-  tokenize(corpusPreprocessed)
  
  # remove profanity
  corpusTokenized <- profanityFilter(corpusTokenized, locale)
  
  # remove old vars (memory management)
  storeVar(corpusTokenized, locale)
  cleanup(corpusPreprocessed)
}

# Here we should have tokenized data for creating the n-grams
log("Tokenized corpus, # lines:", length(corpusTokenized))


# create filal datastructure for all n-gram Lookup Tables
ngramLookupTables <- list()

# loop over all gram n's

for(gram in 1:grams){
  
  log("Creating ngram", gram)
  gramName <- paste0("gram",gram)
  postfix <- paste0(".",gramName)
  
  # Create or load the n-gram lookup (list of all unique n-gram lookups)
  if(missingVar("gramLookupTable",locale,postfix)){
    
    # Create or load the n-gram frequancy (list of all aggregated & sorted n-gram's )
    if(missingVar("gramFrequencyTable",locale,postfix)){
      
      # Create or load the n-gram table (list of all n-gram)
      if(missingVar("gramTable",locale,postfix)){
       
            log("Generating n-gram table",gram)
            gramTable <- transformNGram(corpusTokenized, gram)
            log("# rows", length(gramTable))
      
            storeVar(gramTable, locale, postfix)
      }
      
      log("Generating n-gram Frequency Table",gram)
      gramFrequencyTable <- frequencyTable(gramTable, gram)
      log("# rows", nrow(gramFrequencyTable))
  
      storeVar(gramFrequencyTable, locale, postfix)
      cleanup(gramTable)
    }
    
    log("Generating n-gram Lookup Table",gram)
    gramLookupTable <- lookupTable(gramFrequencyTable, minFreq)
    log("# rows", nrow(gramLookupTable))
      
    storeVar(gramLookupTable, locale, postfix)
    cleanup(gramFrequencyTable)
  }
      
  if(debug){
    gramLookupTableName <- paste0(gramName,"LookupTable")
    log("Assign lookuptable, ",gramLookupTableName)
    assign(gramLookupTableName, gramLookupTable, envir=globalenv())
  }
  
  log("Adding to ngramLookupTables list",gram)
  ngramLookupTables[[gram]] <- gramLookupTable
    
  cleanup(gramLookupTable)
}

log("Storing lookup tables",gram)
storeVar(ngramLookupTables, locale, force=TRUE)


