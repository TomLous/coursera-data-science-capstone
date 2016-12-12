missingVar <- function(varname, locale){
  if(!exists(varname, envir=globalenv())){
    fname <- paste0("data/cache/",varname,".",locale,".RData")
    if(file.exists(fname)){
      try(load(fname, envir=globalenv()), silent = TRUE)
    }
  }
  return(!exists(varname, envir=globalenv()))
}

storeVar <- function(varname, locale){
  fname <- paste0("data/cache/",varname,".",locale,".RData")
  save(list=c(varname), file=fname)
}