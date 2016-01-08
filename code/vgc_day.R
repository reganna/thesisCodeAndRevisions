(1/3)*(1/.5)
(1/3)*(1/(1/2))
((1/3)*(1/(1/2)))*.333
((1/2)/(3))*.333
(3/14)*.333
.222+.0555+.07135714
.333+.222+.071357
.5/2
.25+.333
source("http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/coll.analysis.r")
100000
source("http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/coll.analysis.r")
source("http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/coll.analysis.r")
source("http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/coll.analysis.r")
source('~/Documents/thesis/code/corpusFunctions.R')
source('~/Documents/thesis/code/ngram_lists.R')
setwd("~/thesis")
traf <- getTxtFreqs("corpus/corpus/galdos_trafalgar_spain.txt")
traf <- getTxtFreqs("corpus/corpus/galdos_trafalgar_spain.txt", raw=TRUE)
head(traf)
head(traf, 100)
ref <- wordsFromDir("corpus/tagged/", pos="NC")
traf_not_ref <- setdiff(traf, ref)
traf_not_ref
ref
taf
traf
traf <- getTxtFreqs("corpus/corpus/galdos_trafalgar_spain.txt", vec=TRUE)
traf <- getTxtFreqs("corpus/corpus/galdos_trafalgar_spain.txt", vec=TRUE)
traf_not_ref <- setdiff(traf, ref)
ref <- read.csv("gut_unique_types.csv", sep = ",")
ref <- wordsFromDir("corpus/gutenberg_utf8_tagged/", pos="NC")
traf_not_ref <- setdiff(traf, ref)
traf <- wordsFromFile("corpus/galdos_trafalgar_spain_tagged.txt", pos="NC")
traf_not_ref <- setdiff(traf, ref)
traf_ngrams <- ngramsFromList(traf_not_ref)
first_traf <- firstAppearances(traf_ngrams)
plot(first_traf)
plot(names(first_traf), first_traf)
first_traf
traf <- getTxtFreqs("corpus/corpus/galdos_trafalgar_spain.txt", raw=TRUE)
maria <- getTxtFreqs("corpus/corpus/isaacs_maria_colombia.txt")
m_t_d <- setdiff(traf, maria)
traf
m_t_d <- setdiff(names(traf), names(maria))
m_t_d
write.csv(traf, file = trafalgar_freq.csv, sep = "\t")
write.tsv(traf, file = trafalgar_freq.csv, sep = "\t")
write.table(traf, file = trafalgar_freq.csv, sep = "\t")
write.table(traf, file = "trafalgar_freq.csv", sep = "\t")
?tfl
library(zipfR)
?tfl
traf_tfl <- read.tfl("trafalgar_freq.csv")
plot(traf_tfl)
maria
maria <- getTxtFreqs("corpus/corpus/isaacs_maria_colombia.txt", raw=TRUE)
write.table(maria, file = "maria_freq.csv", sep = "\t")
maria_tfl <- read.tfl("maria_freq.csv")
maria_tfl <- read.tfl("maria_freq.csv")
plot(maria_tfl)
maria_spc <- tfl2spc(maria_tfl)
traf_spc <- tfl2spc(traf_tfl)
plot(maria_spc)
plot(maria_spc, traf_spc)
?vgc
?spc
> maria.fzm <- lnre("fzm", maria_spc, exact=FALSE)
maria.fzm <- lnre("fzm", maria_spc, exact=FALSE)
?lnre.vgc
maria.fzm
maria.fzm.vgc <- lnre.vgc(maria.fzm,(1:9000), variances=TRUE)
plot(maria.fzm.vgc)
plot(maria.fzm.vgc, add.m=1)
maria.fzm.vgc
maria.fzm.vgc <- lnre.vgc(maria.fzm, N(traf_spc), variances=TRUE)
plot(maria.fzm.vgc)
traf.fzm <- lnre("fzm", traf_spc, exact=FALSE)
traf.fzm.vgc <- lnre.vgc(traf.fzm,(1:9000), variances=TRUE)
plot(maria.fzm.vgc, traf.fzm.vgc)
plot(traf.fzm.vgc)
traf.fzm <- lnre("fzm", traf_spc, exact=FALSE)
plot(traf.fzm)
traf.fzm
maria.fzm.vgc <- lnre.vgc(maria.fzm, N(traf_spc), variances=TRUE)
maria.fzm.vgc
maria.fzm.vgc <- lnre.vgc(maria.fzm, N(traf.fzm.vgc), variances=TRUE)
traf.fzm.vgc <- lnre.vgc(traf.fzm, variances=TRUE)
traf.fzm.vgc <- lnre.vgc(traf.fzm, N(traf.fzm), variances=TRUE)
traf.fzm.vgc <- lnre.vgc(traf.fzm, N(traf.fzm), variances=TRUE)
traf_spc
traf_spc(N)
N(traf_spc)
traf.fzm.vgc <- lnre.vgc(traf.fzm, N(traf_spc), variances=TRUE)
traf.fzm.vgc
traf.fzm.vgc <- lnre.vgc(traf.fzm, (1:100) * 50000, variances=TRUE)
traf.fzm.vgc <- lnre.vgc(traf.fzm, (1:50000), variances=TRUE)
maria.fzm.vgc <- lnre.vgc(maria.fzm, (1:50000), variances=TRUE)
plot(maria.fzm.vgc, traf.fzm.vgc)
savehistory("~/Dropbox/laptop/Documents/thesis/code/vgc_day.R")
