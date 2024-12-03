#--------------------------------------------------------
# Load Libraries
#--------------------------------------------------------
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(textdata)
library(tm)

#--------------------------------------------------------
# Read and Clean RTF Files
#--------------------------------------------------------
# File paths for the RTF files
file_paths <- c(
  "/Users/junheau/Documents/Classes/FA24/MGSC_310/Final Project/I Aint Worried.rtf",
  "/Users/junheau/Documents/Classes/FA24/MGSC_310/Final Project/Under the Influence.rtf",
  "/Users/junheau/Documents/Classes/FA24/MGSC_310/Final Project/Im Good.rtf",
  "/Users/junheau/Documents/Classes/FA24/MGSC_310/Final Project/Unholy.rtf"
)

# Function to process RTF files
read_rtf_file <- function(file_path) {
  text <- readLines(file_path, warn = FALSE)
  text <- paste(text, collapse = " ") # Combine all lines into one text
  text <- str_replace_all(text, "\\\\[a-zA-Z0-9]+", "") # Remove RTF formatting codes
  text <- str_replace_all(text, "[^a-zA-Z\\s]", "") # Remove non-alphabetic characters
  text <- tolower(text) # Convert to lowercase
  return(text)
}

# Create a data frame of all songs and their cleaned text
lyrics <- tibble(
  song = c("I Ain't Worried", "Under the Influence", "I'm Good", "Unholy"),
  text = map_chr(file_paths, read_rtf_file)
)

# Combine all texts for collective analysis
combined_lyrics <- lyrics %>%
  summarise(text = paste(text, collapse = " ")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z]+$"))

#--------------------------------------------------------
# Generate Word Cloud
#--------------------------------------------------------
# Count word frequencies
word_frequencies <- combined_lyrics %>%
  count(word, sort = TRUE)

# Generate word cloud
wordcloud(
  words = word_frequencies$word,
  freq = word_frequencies$n,
  min.freq = 2,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

#--------------------------------------------------------
# Perform Sentiment Analysis
#--------------------------------------------------------
# Load NRC sentiment lexicon
if (!"textdata" %in% installed.packages()) {
  install.packages("textdata")
}
nrc_lexicon <- textdata::lexicon_nrc()

# Perform sentiment analysis
sentiment <- combined_lyrics %>%
  inner_join(nrc_lexicon, by = "word") %>%
  count(sentiment, sort = TRUE)

# Plot sentiment analysis results
ggplot(sentiment, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Collective Sentiment Analysis of Lyrics",
    x = "Sentiment",
    y = "Frequency"
  ) +
  theme_minimal()