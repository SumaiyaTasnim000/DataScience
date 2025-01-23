library(tm)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(RColorBrewer)
library(wordcloud)
library(reshape2)
library(LDAvis)
library(servr)
library(forcats)
textdata <- read.csv("E:\\10th Sem\\Data Science\\Final Lab\\Final Project\\LASTLY\\processed_text.csv", stringsAsFactors = FALSE)
processedCorpus <- Corpus(VectorSource(textdata[[1]]))
processedCorpus 

DTM <- DocumentTermMatrix(processedCorpus)
DTM
TFIDF <- weightTfIdf(DTM)
TFIDF
# Apply TF-IDF weighting to the DTM
tfidf_matrix <- as.matrix(TFIDF)
tfidf_threshold <- 0.1 # Set a threshold to keep important words
filtered_terms <- apply(tfidf_matrix, 2, max) > tfidf_threshold
filtered_DTM <- DTM[, filtered_terms]
topicModel <- LDA(filtered_DTM, control = list(seed=123), k = 10)
tmResult <- posterior(topicModel)  # Extract posterior distributions (topics and terms) from the LDA model
inspect(DTM)
inspect(filtered_DTM)
top_terms <- terms(topicModel, 10) #The model does LDA with 10 topics
topic_terms <- as.data.frame(top_terms)

# Display topic weights for each word in each topic (Weight high means Association of word with topic is high)
beta_matrix <- tmResult$terms
num_top_words <- 10  
for (topic in 1:nrow(beta_matrix)) {
  cat("\nTOPIC", topic, "WEIGHTS\n")
  top_words <- names(sort(beta_matrix[topic, ], decreasing = TRUE))[1:num_top_words]
  top_weights <- sort(beta_matrix[topic, ], decreasing = TRUE)[1:num_top_words]
  for (i in 1:num_top_words) {
    cat(sprintf('"%s" %.4f\n', top_words[i], top_weights[i] * 100))
  }
}

# Function to calculate word frequencies
get_word_frequencies <- function(corpus) {
  tdm <- TermDocumentMatrix(corpus)
  word_freqs <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  word_freq_df <- data.frame(
    word = names(word_freqs),
    frequency = word_freqs
  )
  write.csv(word_freq_df, "E:\\10th Sem\\Data Science\\Final Lab\\Final Project\\LASTLY\\word_frequencies.csv", row.names = FALSE)
  return(word_freq_df)
}

word_freq_df <- get_word_frequencies(processedCorpus)
word_freq_df[2]

#Frequency Table
datatable(
  word_freq_df, 
  options = list(
    scrollX = TRUE, 
    pageLength = 100  
  ),
  caption = "Word Frequency Table"
)

topic_terms

#Top 20 frequent Words
p1 <- ggplot(head(word_freq_df, 20), aes(x = reorder(word, frequency), y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Most Freq Words",
       x = "Word",
       y = "Freq")
p1

#Topic Proportions (Y axis proportion = 0.05 means 5%... 0.10 means 10% of Topic x prevails in entire document)
topic_proportions <- colSums(tmResult$topics) / nrow(tmResult$topics)
topic_prop_df <- data.frame(
  topic = 1:length(topic_proportions),
  proportion = topic_proportions
)

p2 <- ggplot(topic_prop_df, aes(x = factor(topic), y = proportion)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  labs(title = "Topic Proportions",
       x = "Topic",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2

#Word Cloud Visualized
wordcloud(
  words = word_freq_df$word,          
  freq = word_freq_df$frequency,     
  min.freq = 2,                       
  max.words = 100,                   
  random.order = FALSE,              
  rot.per = 0.3,                     
  colors = brewer.pal(8, "Dark2"),   
  scale = c(4, 0.5)                   
)

#Heatmap
top_terms_matrix <- as.matrix(top_terms)
top_terms_df <- melt(top_terms_matrix)
colnames(top_terms_df) <- c("Topic", "Rank", "Term")

top_terms_df <- as.data.frame(top_terms)
colnames(top_terms_df) <- paste0("Topic ", 1:ncol(top_terms_df))

top_terms_df_long <- top_terms_df %>%
  pivot_longer(cols = everything(), names_to = "Topic", values_to = "Term")

top_terms_df_long$Topic <- factor(top_terms_df_long$Topic, levels = colnames(topic_terms))

top_terms_df_long <- top_terms_df_long %>%
  group_by(Topic) %>%
  mutate(Rank = row_number())

top_terms_df_long$Rank <- as.numeric(top_terms_df_long$Rank)

p3 <- ggplot(top_terms_df_long, aes(x = Topic, y = Term, fill = Rank)) +
  geom_tile() +
  scale_fill_gradient(low = "gray", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap of Top Terms", x = "Topic", y = "Term", fill = "Weight")

p3

phi <- posterior(topicModel)$terms
theta <- posterior(topicModel)$topics
vocab <- Terms(DTM)
valid_terms <- match(colnames(phi), vocab) 
vocab_filtered <- vocab[valid_terms]  
DTM_filtered <- as.matrix(DTM)[, valid_terms, drop = FALSE] 
doc.length <- rowSums(DTM_filtered) 
term.frequency <- colSums(DTM_filtered) 
json_lda <- createJSON(phi = phi, theta = theta, vocab = vocab_filtered, 
                       doc.length = doc.length, term.frequency = term.frequency)
serVis(json_lda, open.browser = TRUE)


#labeling the 10 different topics
beta_matrix <- tmResult$terms
num_top_words <- 10  
topic_words <- data.frame()
labels <- c(
  "Agriculture & Commodity Prices",
  "Global Business & Digital Economy",
  "Stock Market & Investments",
  "Economic Development & Policy",
  "Corporate Finance & Taxation",
  "Banking & Financial Institutions",
  "Macroeconomics & Monetary Policy",
  "Economic Growth & Trade Data",
  "China & Global Trade Relations",
  "Labor & Industrial Sectors"
)

for (topic in 1:nrow(beta_matrix)) {
  top_words <- names(sort(beta_matrix[topic, ], decreasing = TRUE))[1:num_top_words]
  top_weights <- sort(beta_matrix[topic, ], decreasing = TRUE)[1:num_top_words]
  topic_df <- data.frame(Topic = rep(labels[topic], num_top_words),
                         Word = top_words,
                         Weight = top_weights * 100)
  topic_words <- rbind(topic_words, topic_df)
}
topic_words <- topic_words %>%
  group_by(Topic) %>%
  arrange(desc(Weight), .by_group = TRUE) %>%
  ungroup()
p4 <- ggplot(topic_words, aes(x = fct_reorder(Word, Weight), y = Weight, fill = Topic)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~Topic, scales = "free", ncol = 2) +
  coord_flip() +
  theme_minimal() +
  labs( x = "Term", y = "Beta")

p4
