library(ggplot2)
library(tm)
library(translateR)
library(dplyr)
library(gridExtra)

# Function to transform the data and chart the frequency 
plotItemCountByCategory <- function(data) {
  categoryCounts <- as.data.frame(table(data$product_category_name_english))
  categoryCounts <- categoryCounts[which(categoryCounts$Freq != 0),]
  categoryCounts$Var1 <- as.factor(categoryCounts$Var1)
  categoryCounts <- categoryCounts[order(-categoryCounts$Freq),]
  categoryCounts$Var1 <- factor(categoryCounts$Var1, levels = categoryCounts$Var1[order(categoryCounts$Freq)])
  print(ggplot(categoryCounts, aes(x = Var1, y = Freq)) + geom_bar(stat = 'identity') + coord_flip())
}

reviewData <- read.csv("d:\\data\\Marketing_Project\\olist_order_reviews_dataset.csv")
orderData <- read.csv("d:\\data\\Marketing_Project\\olist_order_items_dataset.csv")
productData <- read.csv("d:\\data\\Marketing_Project\\olist_products_dataset.csv")
productTranslation <- read.csv("d:\\data\\Marketing_Project\\product_category_name_translation.csv")
productTranslation$product_category_name = productTranslation$ï..product_category_name

mergedData <- merge(x = reviewData, y = orderData, by = "order_id")
mergedData <- merge(x = mergedData, y = productData, by = "product_id")
mergedData <- merge(x = mergedData, y = productTranslation, by = "product_category_name")
# Category avg price
mergedData <- mergedData %>% group_by(product_category_name_english) %>% mutate(scaledPrice = scale(price))

# Overall review score histogram
ggplot(mergedData, aes(x = review_score)) + geom_histogram(bins = 5) + labs(title = "Overall Review Score Count")

# Items sold by category
plotItemCountByCategory(mergedData)

# Review scores by category
ggplot(mergedData, aes(x = review_score)) + geom_histogram(bins = 5) + facet_wrap(~product_category_name_english)

# Look for terms by review score
termData <- mergedData[which(mergedData$review_comment_title != "" | mergedData$review_comment_message != ""),]
termData <- subset(termData, select = c(review_comment_title, review_comment_message, review_score))
termData$review_text = paste(termData$review_comment_title, termData$review_comment_message, sep = " ")

#termCorpusPerReviewScore <- termData %>% group_by(review_score) %>% summarise(reviewCorpus = paste0(review_text, sep = " "))
termCorpusPerReviewScore <- aggregate(review_text ~ review_score, data = termData, paste)

tdms = list()
plots = list()

for (rating in 1:5) {
  corpus <- SimpleCorpus(VectorSource(toString(termCorpusPerReviewScore$review_text[rating])), control = list(language = "pt-BR"))
  tdms[[rating]] <- TermDocumentMatrix(corpus)
  print(findMostFreqTerms(tdms[[rating]], 10))
  
  terms <- termFreq(toString(termCorpusPerReviewScore$review_text[rating]))
  terms.df <- data.frame(terms)
  terms.df$term <- names(terms)
  terms.df$counts <- as.numeric(terms.df$terms)
  terms.df <- terms.df[order(-terms.df$counts),]
  
  topTerms <- head(terms.df, 20)
  topTerms$term <- factor(topTerms$term, levels = topTerms$term[order(topTerms$counts)])
  
  plots[[rating]] <- ggplot(topTerms, aes(x = term, y = counts)) + geom_bar(stat = 'identity') + coord_flip() + labs(title = paste("Terms for Rating ", rating))
}

grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]])

# Top 100 selling items
items <- sort(table(mergedData$product_id))
items <- tail(items, 100)
items <- rownames(items)
topSelling <- mergedData[which(mergedData$product_id %in% items),]

# Total count of sales per category for the top 100 selling items
plotItemCountByCategory(topSelling)

topSellingItems <- subset(topSelling, select = c(product_id, product_category_name_english))
topSellingItems <- unique(topSellingItems)

# Count of items per category for the top 100 selling items
plotItemCountByCategory(topSellingItems)

# cool_stuff exploration
coolStuffData <- topSelling[which(topSelling$product_category_name_english == "cool_stuff"),]
hist(coolStuffData$price)
hist(coolStuffData$review_score)

# Quick linear regression on sales vs. price + review score
linearData <- mergedData %>% group_by(product_id) %>% summarize(numSales = n(), avgPrice = mean(scaledPrice), avgReview = mean(review_score))
# Scope to products with at least 100 sales
linearData <- linearData[which(linearData$numSales >= 10),]

model <- lm(numSales ~ avgReview + avgPrice, data = linearData)
summary(model)
