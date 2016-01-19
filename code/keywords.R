library(tm)
library(qdap)
library(data.table)

getTxtFreqs <- function(txtfile, raw=FALSE, rel=TRUE, vec=FALSE){
  text.v <- scan(txtfile, what="character", sep="\n")
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

pvalueList <- function(){
  
  texta <- getTxtFreqs("corpus/corpus/isaacs_maria_colombia.txt", raw = TRUE)
  textb <- getTxtFreqs("corpus/corpus/marmol_amalia_argentina.txt", raw = TRUE)
  text_df <- as.data.frame(texta, stringsAsFactors = FALSE)
  ref_df <- as.data.frame(textb, stringsAsFactors = FALSE)

  wordList <- text_df[which(text_df$Freq>2),]
  
  pvalues <- data.frame(word=character(), p=numeric())
  
  for (i in 1:length(wordList$Var1)){
    word <- wordList[i, 1]
    word_ref <- ref_df[which(ref_df$Var1 == word),]
    word_text <- text_df[which(text_df$Var1 == word),]
    conTbl <-  rbind(c(word_ref[1, 2], sum(ref_df$Freq)), c(word_text[1, 2], sum(text_df$Freq)))
    conTbl[is.na(conTbl)] <- 0
    test <- chisq.test(conTbl)
    remove(conTbl)
    pvalues <- rbind(pvalues, data.frame(word, test$p.value))
  }
  colnames(pvalues) <- c("word", "p")
  keywords <- pvalues[which(pvalues$p < .0001),]
  return(keywords)
}


keywords <- function(directoryName){
  novelKeywordList <- list()
  corp <- Corpus(DirSource(directoryName))
  corpora_wfm <- as.wfm(corp)
  
  for(n in 1:length(corp)){
    text_v <- corpora_wfm[,n]
    ref_v <- rowSums(corpora_wfm[,-n])
    wordList <- names(which(text_v>0))
    keywordList <- list()
    pvalues <- list()

      for (i in 1:length(wordList)){
        textword <- wordList[i]
        conTbl <-  rbind(c(ref_v[textword], sum(ref_v)), c(text_v[textword], sum(text_v)))
        conTbl[is.na(conTbl)] <- 0
        test <- chisq.test(conTbl)
        remove(conTbl)
        pvalues <- rbind(pvalues, data.frame(textword, test$p.value))
      }
    
    keywordList <- pvalues[which(pvalues$test.p.value < .05),]
    novelKeywordList[[names(corp[n])]] <- keywordList[,1]
    remove(pvalues)
  }
  
  return(novelKeywordList)
  
}

KwordRatio <- function(directoryName){
  
  Ksum <- as.numeric(summary(Kwords))
  novTotals <- colSums(corpora_wfm)

}

# galdos <- as.vector(corpKeywords[[6]])
# stylo(gui = FALSE, mfw.min=1000, mfw.max=1000, corpus.lang="Spanish", features = galdos)
# 
# classify()
# Reduce(intersect, list(maria, amalia, galdos))

