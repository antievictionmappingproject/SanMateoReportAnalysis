#useful functions for dealing with frequencies:
sortedfreq <- function(x, decreasing = T, n = NULL) {
  if (is.null(n)) {
    sort(table(x), decreasing)
  }
  else {
    head(sort(table(x), decreasing), n)
  }
}

freq2pct <- function(freqtable) {
  total <- sum(freqtable)
  sapply(freqtable, function(n) {n/total})
}