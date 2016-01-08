library(e1071)

getgetTEIWordTableList <- function(doc.object){
  paras <- getNodeSet(doc.object, "/tei:TEI/tei:text/tei:body//tei:p", 
                      c(tei = "http://www.tei-c.org/ns/1.0"))
  words <- paste(sapply(paras, xmlValue), collapse = " ")
  words.lower <- tolower(words)
  words.l <- strsplit(words.lower, "\\W")
  word.v <- unlist(words.l)
  book.freqs.t <- table(word.v[which(word.v!="")])
  book.freqs.rel.t <- 100*(book.freqs.t/sum(book.freqs.t))
  return(book.freqs.rel.t)
}

#
# scan file and returns either raw frequency table, relative frequency table or word vector
getTxtFreqs <- function(txtfile, raw=FALSE, rel=TRUE, vec=FALSE){
  text.v <- scan(txtfile, what="character", sep="\n")
  text.v <- gsub('([0-9])([[:alpha:]])', '\\1 \\2', text.v)
  novel.v <- paste(text.v, collapse=" ")
  novel.lower.v <- tolower(novel.v)
  novel.words.l <- strsplit(novel.lower.v, "\\W")
  novel.words.v <- unlist(novel.words.l)
  novel.freqs.t <- table(novel.words.v[which(novel.words.v!="")])
  novel_rel_freqs_table <- (novel.freqs.t/sum(novel.freqs.t))
  if(raw==TRUE)  {
    return(novel.freqs.t) 
  }
  else if(vec==TRUE){
    return(novel.words.v[which(novel.words.v!="")])
  }
  else if(rel==TRUE){
    return(novel_rel_freqs_table)
  }
  return(novel_rel_freqs_table)
}

ttrDirectory <- function(directoryName, mn=FALSE){
  files_v <- dir(path=directoryName, pattern="*") 
  ttr_df <- data.frame()
  for(i in 1:length(files_v)){
    raws.l <- getTxtFreqs(file.path(directoryName, files_v[i]), raw = TRUE)
    ttr_df <- rbind(ttr_df, c(length(raws.l), sum(raws.l)))
  }
  if(mn==TRUE){
    type <- ttr_df[,1]
    token <- ttr_df[,2]
    ratios <- type/token
    ttrMean <- mean(ratios)
    rownames(ttr_df) <- getAuthorNames(directoryName)
    return(ttrMean)
  }
  else{
    rownames(ttr_df) <- getAuthorNames(directoryName)
    return(ttr_df)
    
  }
}

getAuthorNames <- function(directoryName){
  authors <- list()
  files_v <- dir(path=directoryName, pattern="*")
  names <- sapply(files_v, strsplit, '_')
  names <- sapply(names, "[[", 1)
  for (i in files_v){
    authors <- c(authors, names[[i]])
  }
  authors <- unlist(authors)
  return(authors)
}

#ttr <- lapply(t, function(x) {length(x)/sum(x)*100})

clusterTxtFiles <- function(directoryName){
    
  #list of filenames  
  files_v <- dir(path=directoryName, pattern="*") 
  
  #loop to get all text freqs
  text_freqs_list <- list() #a list object to hold the results
  for (i in 1:length(files_v)){ 
    wordinfo <- getTxtFreqs(file.path(directoryName, files_v[i]), raw = TRUE)
    text_freqs_list[[files_v[i]]] <- wordinfo
  }
  
   freqs_list <- mapply(data.frame,
                       ID=seq_along(text_freqs_list),
                       text_freqs_list, SIMPLIFY=FALSE,
                       MoreArgs=list(stringsAsFactors=FALSE))
  
  freqs_df <- do.call(rbind, freqs_list)
  resultT <- xtabs(Freq ~ ID+Var1, data=freqs_df)
  final_m <- apply(resultT, 2, as.numeric)
  smaller_m <- final_m[, apply(final_m, 2, mean) >= .15]

  #Create a distance object
  dmT <- dist(smaller_m)
  #Perfrom a cluster analysis on the distance object
  clusterT <- hclust(dmT)
  # Get the book file names to use as labels
  clusterT$labels <- names(text_freqs_list)
  # Plot the results as a dendogram for inspection
  
  #plot(clusterT)
  return(clusterT)
}

#texts.pr <- prcomp(smaller_m)


textSegmentation <- function(txtfile, chunk_size=10){
  text_v <- scan(txtfile, what="character", sep="\n")
  novel_v <- paste(text_v, collapse=" ")
  novel_lower_v <- tolower(novel_v)
  novel_words_l <- strsplit(novel_lower_v, "\\W")
  novel_words_v <- unlist(novel_words_l)
  max_length <- length(novel_words_v)/chunk_size
  x <- seq_along(novel_words_v)
  chunks_list <- split(novel_words_v, ceiling(x/max_length))
  chunks_list <- lapply(chunks_list, removeBlanks)
  freq_chunks_list <- lapply(chunks_list, table)
  rel_freq_chunk_list <- lapply(freq_chunks_list, prop.table)
  return(rel_freq_chunk_list)
}

uniqueHapax <- function(directoryName){
  words <- character()
  files_v <- dir(path=comparisonDirectory, pattern="*") 
  novel.words <- names(getTxtFreqs(novel.a, raw=TRUE))
  #novel.b.words <- names(getTxtFreqs(novel.b, raw=TRUE))
  #words <- setdiff(novel.a.words, novel.b.words)
  #for (word in novel.a.words){
   # if (word %in% novel.b.words){
    #  words <- c(words, word)
    
    return(test_files)
}

treeTagger <- function(directoryName){
  files_v <- dir(path=directoryName, pattern="*") 
  for(i in 1:length(files_v)){
    wordinfo <- getTxtFreqs(file.path(directoryName, files_v[i]))
    
    text <- system(sprintf("corpus/treeTagger/cmd/tree-tagger-spanish %s", file.path(directoryName, files_v[i])), intern=TRUE)
    write(text, file.path(directoryName, files_v[i]))
    #words <- scan("tagged_words", what="character", sep=" ")
  }
    #return(words)
}

splitText <- function(text){
  unlist(strsplit(text, " "))
}

selectTaggedWords <- function(tagged_words, target_tag){
  tagged_words[grep(target_tag, tagged_words)]
}

removeTags <- function(word_pos){
  sub("/[A-Z]{2,3}", "", word_pos)
}

getWordSegmentTable <- function(word_v, chunk_size=10){
  max_length <- length(word_v)/chunk_size
  x <- seq_along(word_v)
  chunks_list <- split(word_v, ceiling(x/max_length))
  chunks_list <- lapply(chunks_list, removeBlanks)
  freq_chunks_list <- lapply(chunks_list, table)
  rel_freq_chunk_list <- lapply(freq_chunks_list, prop.table)
  return(rel_freq_chunk_list)
}

#chunkDirectory <- function(directoryName, excludeName, category){
  
svmDirectory <- function(directoryName){
  files_v <- dir(path=directoryName, pattern="*") 
  book_freqs_list <- list()
  for(i in 1:length(files_v)){
    words_v <- getTxtFreqs(file.path(directoryName, files_v[i]), vec = TRUE)
    chunk_data_list <- getWordSegmentTable(words_v, 10)
    book_freqs_list[[files_v[i]]] <- chunk_data_list
  }
  freqs_l <- lapply(book_freqs_list, my_mapply)
  freqs_df <- do.call(rbind, freqs_l)
  bookids_v <- gsub("\\..*", "", rownames(freqs_df))
  book_chunk_ids <- paste(bookids_v, freqs_df$ID, sep="_")
  freqs_df$ID <- book_chunk_ids
  result_t <- xtabs(Freq ~ ID+Var1, data=freqs_df)
  final_df <- as.data.frame.matrix(result_t)
  metacols_m <- do.call(rbind, strsplit(rownames(final_df), "_"))
  colnames(metacols_m) <- c("author", "title", "country", "sampleChunk")
  author_v <- gsub("\\d+$", "", metacols_m[,"author"])
  authorship_df <- cbind(author_v, metacols_m, final_df)
  freq_means_v <- colMeans(authorship_df[6:ncol(authorship_df)])
  keepers_v <- which(freq_means_v >= .00005)
  smaller_df <- authorship_df[,names(keepers_v)]
  smaller_df <- cbind(author_v, metacols_m, smaller_df)
  column <- smaller_df[,"sampleChunk"]
#  anon_v <- which(column == excludeName)
  anon_v <- which(column  %in% c(1:7))
  
  train <- smaller_df[-anon_v, 6:ncol(smaller_df)]
#  train <- smaller_df[an,6:ncol(smaller_df)]
  
  class_f <- smaller_df[-anon_v, "author_v"]
#  class_f <- smaller_df[5:10, "author_v"]
  
  model_svm <- svm(train, class_f)
  pred_svm <- predict(model_svm, train, type="decision")
  test_data <- smaller_df[anon_v, 6:ncol(smaller_df)]
  final_result <- predict(model_svm, test_data)
  
  return(final_result)
  #return(as.data.frame(final_result))
}


my_mapply <- function(x){
  my_list <- mapply(data.frame, ID=seq_along(x),
                    x, SIMPLIFY=FALSE,
                    MoreArgs=list(stringsAsFactors=FALSE))
  my_df <- do.call(rbind, my_list)
  return(my_df)
}

removeBlanks <- function(x) {
  x[which(x!="")]
}

splitCorpus <- function(directoryName, chunk_size=10){
  
  files_v <- dir(path=directoryName, pattern="*") 
  text_freqs_list <- list()
  for (i in 1:length(files_v)){ 
    wordinfo <- getTxtFreqs(file.path(directoryName, files_v[i]), vec = TRUE)
    max_length <- length(wordinfo)/chunk_size
    x <- seq_along(wordinfo)
    chunks_list <- split(wordinfo, ceiling(x/max_length))
    chunks_list[i] <- lapply(chunks_list, removeBlanks)
    name_list <- list()
    for(chunkNumber in 1:chunk_size){
      name_list <- c(name_list, paste(files_v[i], chunkNumber, sep="_"))
    }
    names(chunks_list) <- name_list
    text_freqs_list <- append(text_freqs_list, chunks_list)
    #text_freqs_list[[files_v[i]]] <- wordinfo
  }
  
#   chunks_list <- list()
#   files_v <- dir(path=directoryName, pattern="*") 
#   for (i in 1:length(files_v)) {
#     textWords <- getTxtFreqs(file.path(directoryName, files_v[i]), vec = TRUE)
# 

#     chunks_list[files_v[[i]]] <- textWords
#   }
  return(text_freqs_list)
}
