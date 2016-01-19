sumalibrary(zipfR)

createTfls <- function(directoryName){
  files_v <- dir(path=directoryName, pattern="*") 
  for(i in 1:length(files_v)){
    raws.l <- as.data.frame(getTxtFreqs(file.path(directoryName, files_v[i]), raw = TRUE))
    name <- paste(files_v[i], "tfl", sep="." )
    name <- paste("tfls/", name, sep="")
    names(raws.l) <- c("type", "f")
    write.table(raws.l, file = name, sep="\t", col.names = NA)
  }
}
# 
# colors = c("red", "green", "blue", "black", "gray", "orange", "yellow", "purple", "brown", "darkgreen", "cyan", "chocolate4", "magenta")
# plot(testVgc[[1]], testVgc[[2]], testVgc[[3]], testVgc[[4]], testVgc[[5]], testVgc[[6]], testVgc[[7]], 
#      testVgc[[8]], testVgc[[9]], testVgc[[10]], legend = leg, col = colors, conf.level = NA)

compareVgc <- function(a_path, b_path){
  library(zipfR)
  a_tfl <- read.tfl(a_path)
  b_tfl <- read.tfl(b_path)
  b_spc <- tfl2spc(b_tfl)
  a_spc <- tfl2spc(a_tfl)
  b.fzm <- lnre("fzm", b_spc, exact=FALSE)
  a.fzm <- lnre("fzm", a_spc, exact=FALSE)
  #a.fzm.vgc <- lnre.vgc(a.fzm, (1:100) * 230000, variances=TRUE)
  a.fzm.vgc <- lnre.vgc(a.fzm, (1:230000), variances=TRUE)
  b.fzm.vgc <- lnre.vgc(b.fzm, (1:230000), variances=TRUE)
  comparisonList <- list("a" = a.fzm.vgc, "b" = b.fzm.vgc)
  return(comparisonList)
}

## Takes a directory of TFL files
compareVgcDir <- function(directoryName){
  comparisonList <- list()
  files_v <- dir(path=directoryName, pattern="*") 
  for(i in 1:length(files_v)){
    a_tfl <- read.tfl(file.path(directoryName, files_v[i]))
    a_spc <- tfl2spc(a_tfl)
    a.fzm <- lnre("fzm", a_spc, exact=FALSE)
    a.fzm.vgc <- lnre.vgc(a.fzm, (1:230000), variances=TRUE)
    comparisonList[i] <- list(a.fzm.vgc)
    
  }
  name <- gsub("\\_.*", "", files_v)
  names(comparisonList) <- name
  return(comparisonList)
}

compareSpcDir <- function(directoryName){
  comparisonList <- list()
  files_v <- dir(path=directoryName, pattern="*") 
  for(i in 1:length(files_v)){
    a_tfl <- read.tfl(file.path(directoryName, files_v[i]))
    a_spc <- tfl2spc(a_tfl)
    #a.fzm <- lnre("fzm", a_spc, exact=FALSE)
    #a.fzm.vgc <- lnre.vgc(a.fzm, (1:230000), variances=TRUE)
    comparisonList[i] <- list(a_spc)
    
  }
  name <- gsub("\\_.*", "", files_v)
  names(comparisonList) <- name
  return(comparisonList)
}

hapaxRatio <- function(directoryName){
  spcList <- compareSpcDir(directoryName)
  hapaxRatioDf <- data.frame(Hapax=as.numeric(), Total=as.numeric())
  for (i in 1:length(spcList)){
    hapaxRatioDf <- rbind(hapaxRatioDf, c(N(spcList[[i]]), Vm(spcList[[i]], 1)))
  }
  rownames(hapaxRatioDf) <- names(spcList)
  return(hapaxRatioDf)
}