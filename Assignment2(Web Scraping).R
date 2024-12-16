install.packages("tm")  
install.packages("textclean") 
install.packages("tidyverse")
install.packages("hunspell")
install.packages("textstem")
install.packages("SnowballC")
install.packages("english")
install.packages("stringr")

library(rvest)
library(tm)
library(textclean)
library(tidyverse) # data manipulation and visualization.
library(hunspell) # checks spelling
library(textstem) # Lemmatization of words (running to run)
library(SnowballC) # Also for Lemmatization 
library(english)
library(stringr)

# Scrape Links
base_url <- "https://www.thedailystar.net/business"
limit <- 10 # Specifies a limit of 10 pages 
all_links <- c() # An empty vector to store extracted links.
page_num <- 1 #initial page= 1

#web scraping loop that collects article links
while (length(all_links) < limit) {
  current_page_url <- paste0(base_url, "?page=", page_num) #Construct Current Page URL
  main_page <- read_html(current_page_url) #downloads and ready the html to be processed
  
  #Extracting all links from main page 
  links <- main_page %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    unique() %>% 
    .[grepl("/business/news/", .)] #Filters for links that include /business/news/
  
  links <- paste0("https://www.thedailystar.net", links)
  all_links <- unique(c(all_links, links)) #Combines new links with those already collected 
  
#Find the Next Page
  next_page <- main_page %>% 
    html_nodes(".pager-show-more-next a") %>% #Selects the "Next" button
    html_attr("href")
  
  if (length(next_page) == 0) break #loop breaks when no next pg found
  page_num <- page_num + 1
}

#final output having all the collected links
article_links <- head(all_links, limit)

# Scrape Contents
articles <- list() #Initialize an Empty List to Store Articles
for (url in article_links) {
  article_page <- read_html(url)
  content <- article_page %>% # <article> tag
    html_nodes("article p") %>%  #selects all <p> (paragraph) elements 
    html_text(trim = TRUE)
  
  if (length(content) == 0) {
    content <- article_page %>% 
      html_nodes("div.content p") %>% #Alternative Selector <div> when no <article> tag found
      html_text(trim = TRUE)
  }
  
  articles[[url]] <- paste(content, collapse = " ") #Adds the content of the current article to the articles list
}

# Convert the articles List to a Data Frame
articles_df <- data.frame(content = unlist(articles), stringsAsFactors = FALSE)
write.csv(articles_df, "DailyStar_Business_Unprocessed.csv", row.names = FALSE) #download the unprocessed csv

# Text processing (conv numbers to words)
convert_number_to_word_fp <- function(text) {
  str_replace_all(text, "\\d+(\\.\\d+)?", function(x) {
    num <- as.numeric(x)
    int_part <- floor(num)
    dec_part <- num - int_part
    
    int_words <- as.character(as.english(int_part))
    dec_words <- ifelse(dec_part > 0, paste("point", as.character(as.english(dec_part * 100))), "") #for floating numbers
    
    paste(int_words, dec_words)
  })
}
# calling text cleaning function
clean_text <- function(text) {
text %>%
    
# Convert to ASCII and remove HTML tags
    iconv(to = "ASCII", sub = "") %>% #removing special symbols
    gsub("<[^>]+>", "", .) %>% #removes all HTML tags
    
# Remove non-printable characters and normalize whitespace (only 1 space between each words)
str_replace_all("[[:cntrl:]]", " ") %>%
str_squish() %>% #Cleans up any excessive whitespace
    
# Convert numbers to words and remove punctuation
str_remove_all("[[:punct:]]") %>%
    
# Convert to lowercase and trim whitespace
tolower() %>%
str_trim()
}
#removing all punctuations except the $ 
last_clean <- function(text) {
  str_remove_all("[[:punct:]&&[^\\$]]")
}

# splits a given text into individual tokens (words) based on spaces
tokenize_text <- function(text) {
  unlist(strsplit(text, "\\s+")) #split a string into substrings (tokens)
}


for (i in 1:ncol(articles_df)) {
  
  # Convert numbers to words
  articles_df[[i]] <- sapply(articles_df[[i]], convert_number_to_word_fp)
  
# Clean and preprocess text
cleaned_text <- sapply(articles_df[[i]], clean_text)
cleaned_text <- replace_contraction(cleaned_text) #contracted words replaced,ex: doesn't - does not
cleaned_text <- replace_emoji(cleaned_text)
cleaned_text <- gsub("\\s{2,}", " ", cleaned_text) #removes extra spaces
  
# Remove stopwords (e.g., "and", "the", "is") 
  stop_words <- stopwords("en")
  no_stopwords <- removeWords(cleaned_text, stop_words)
  spell_checked_text <- sapply(no_stopwords, function(x) {
    words <- unlist(strsplit(x, " "))
    corrected_words <- sapply(words, function(word) {
      suggestions <- hunspell(word) #checks spelling and returns a list of suggestions for correction
      if (length(suggestions) > 0) word else suggestions[[1]][1] #replaces the error word with 1st suggested word
    })
    paste(corrected_words, collapse = " ")
  })
  
spell_checked_text <- gsub("\\s{2,}", " ", no_stopwords)
  
stemmed_text <- wordStem(spell_checked_text) #stemming: better - bett (not always helpful)
final_text <- lemmatize_words(stemmed_text) # lemmatize: better - good
  
  final_text <- gsub("\\s{2,}", " ", trimws(final_text))
  final_text <- sapply(final_text, last_clean)
  articles_df[[i]] <- final_text
}

write.csv(articles_df, "DailyStar_Business_Processed.csv", row.names = FALSE)

write.csv(article_links, "aricle_links.csv", row.names = FALSE)



