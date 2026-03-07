library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

process_text <- function(file_path, custom_stopwords = NULL) {
  text <- readLines(file_path, encoding = "UTF-8", warn = FALSE)
  text <- paste(text, collapse = " ")
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("en"))
  text <- stripWhitespace(text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]
  
  if (!is.null(custom_stopwords)) {
    words <- words[!words %in% custom_stopwords]
  }
  
  return(words)
}

word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq), stringsAsFactors = FALSE)
  freq_df <- freq_df[order(-freq_df$freq), ]
  rownames(freq_df) <- NULL
  return(freq_df)
}

plot_wordcloud <- function(freq_df, plot_title, color_palette = "Dark2") {
  wordcloud(
    words = freq_df$word,
    freq = freq_df$freq,
    min.freq = 16,
    colors = brewer.pal(8, color_palette)
  )
  title(plot_title)
}

file_path <- "Biden2021.txt"
custom_stopwords <- c("—", "–", "’s", "’re")

words <- process_text(file_path, custom_stopwords)
freq_df <- word_frequency(words)

plot_wordcloud(freq_df, "Chmura słów - Biden2021.txt", "Dark2")
print(head(freq_df, 10))

file_paths <- c("Biden2021.txt", "Biden2024.txt")
results_list <- list()

for (file_path in file_paths) {
  words <- process_text(file_path, custom_stopwords)
  freq_df <- word_frequency(words)
  
  results_list[[file_path]] <- freq_df
  
  plot_wordcloud(freq_df, paste("Chmura słów -", file_path), "Dark2")
  
  cat("Najczęściej występujące słowa w pliku", file_path, ":\n")
  print(head(freq_df, 10))
  cat("\n")
}