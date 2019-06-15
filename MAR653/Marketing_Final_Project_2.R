library(dplyr)
library(ggplot2)

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

# Top 100 selling items
items <- sort(table(mergedData$product_id))
items <- tail(items, 100)
topSelling <- mergedData[which(mergedData$product_id %in% rownames(items)),]

# Total count of sales per category for the top 100 selling items
plotItemCountByCategory(topSelling)


productData <- mergedData %>% group_by(product_id) %>% summarize(totalSales = n(), price = mean(price), sellers = n_distinct(seller_id))
multipleSellers <- productData[which(productData$sellers > 1),]

multipleSellers <- multipleSellers[which(multipleSellers$totalSales > 10),]
multipleSellersProducts <- mergedData[which(mergedData$product_id %in% multipleSellers$product_id),]

example <- multipleSellersProducts[which(multipleSellersProducts$product_id == "36f60d45225e60c7da4558b070ce4b60"),]
examplePerSeller <- example %>% group_by(seller_id, price) %>% summarize(sales = n(), avgPrice = mean(price), rating = mean(review_score), avgShipping = mean(freight_value))

multipleSellersProducts <- multipleSellersProducts %>% group_by(product_id) %>% mutate(totalSales = n())
priceData <- multipleSellersProducts %>% group_by(product_id, seller_id, price) %>% summarize(percSales = n() / mean(totalSales), rating = mean(review_score), numPics = mean(product_photos_qty))

mergedDataWithTotal <- mergedData %>% group_by(product_id) %>% mutate(totalSales = n())
priceData <- mergedDataWithTotal %>% group_by(product_id, seller_id, price) %>% summarize(percSales = n() / mean(totalSales), rating = mean(review_score), numPics = mean(product_photos_qty))
priceData <- priceData[which(priceData$percSales != 1),]
print(summary(lm(percSales ~ price + numPics, data = priceData)))
