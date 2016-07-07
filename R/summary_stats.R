if(!exists("SM_Evictions")){
  source('R/read_clean_expand_data.R')
}
source('R/frequency_functions.R')

summary_stats <- function(x, outputfile = NULL) {
  x_freq <- sortedfreq(x)
  summary_frame <- data.frame(frequency = x_freq,
                              pct = freq2pct(x_freq))
  rownames(summary_frame) <- names(x_freq)
  if (!is.null(outputfile)) {
    write.csv(summary_frame, outputfile)
  }
  summary_frame
}


setwd("outputs/frequency_summaries/")
# generate easy summary stats for logical and character columns:
for (col in colnames(SM_Evictions)) {
  if (is.character(SM_Evictions[,col]) || is.logical(SM_Evictions[,col])) {
    summary_stats(SM_Evictions[,col], 
                  outputfile = sprintf("%s_frequency_summary.csv", col))
  }
}
setwd("../../")