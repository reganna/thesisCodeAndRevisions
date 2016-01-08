ngramsFromList <- function(wordList){
  library(ngramr)
  ngramList <- list()
  for(word in wordList){
    ngrams <- ngrami(word, corpus="spa_2012", year_start=1500, year_end=1900)
    Sys.sleep(15)
    if (!is.na(ngrams$Phrase[1])){
      ngramList[[word]] <- ngrams
    }
    else{
      ngramList[[word]] <- NA
    }
  }
  return(ngramList)
}

firstAppearances <- function(ngrams){
  #Takes ngram list returned by the ngramr package and finds the date of first
  #occurence for each word
  dates <- list()
  for(i in 1:length(ngrams)){
    if(!is.na(ngrams[[i]])){
      word <- ngrams[[i]]
      start <- which(word[4] > 0)
      freqs <- word[start,]
      dates[[i]] <- freqs[1,1]
    }
    else{
      dates[[i]] <- NA
    }
  }
  names(dates) <- names(ngrams)
  return(dates)
}

wordsFromDir <- function(directoryName, pos, exclude=""){
  files_v <- dir(path=directoryName, pattern="*") 
  files_v <- files_v[which(files_v != exclude)]
  
  novel_words <- character()
  for(i in 1:length(files_v)){
    file_tagged <- read.table(file.path(directoryName, files_v[i]), sep="\t", fill = TRUE, quote="", stringsAsFactors=FALSE)
    if(pos == "all"){
      words <- file_tagged[1]
    }
    else{
      words <- file_tagged[which(file_tagged$V2 == pos),]
    }
    words_list <- words$V1
    words_list <- gsub("[[:punct:]]", "", words_list)
    novel_words <- c(novel_words, words_list)
  }
  #words_list <- names(table(words_list))
  
return(tolower(novel_words))
}

wordsFromFile <- function(fileName, pos){
  novel_nouns <- character()
  file_tagged <- read.table(fileName, sep="\t", fill = TRUE, quote="", stringsAsFactors=FALSE)
  words <- file_tagged[which(file_tagged$V2 == pos),]
  words_list <- words$V1
  words_list <- gsub("[[:punct:]]", "", words_list)
  #words_list <- names(table(words_list))
  return(tolower(words_list))
}

compareNouns <- function(directoryName){
  files_v <- dir(path=directoryName, pattern="*") 
  directory_nouns <- list()
  for(i in 1:length(files_v)){
    fileNouns <- nounsFromFile(file.path(directoryName, files_v[i]))
    directory_nouns[[i]] <- fileNouns
  }
  names(directory_nouns) <- files_v
  return(directory_nouns)
}
  
