library(dplyr)
library(stats)
library(gridExtra)
library(ggplot2)

reviewData <- read.csv("d:\\data\\Marketing_Project\\olist_order_reviews_dataset.csv")
orderData <- read.csv("d:\\data\\Marketing_Project\\olist_order_items_dataset.csv")
productData <- read.csv("d:\\data\\Marketing_Project\\olist_products_dataset.csv")
productTranslation <- read.csv("d:\\data\\Marketing_Project\\product_category_name_translation.csv")
productTranslation$product_category_name = productTranslation$ï..product_category_name

mergedData <- merge(x = reviewData, y = orderData, by = "order_id")
mergedData <- merge(x = mergedData, y = productData, by = "product_id")
mergedData <- merge(x = mergedData, y = productTranslation, by = "product_category_name")

sellerData <- mergedData %>% group_by(seller_id) %>% summarize(totalSales = n(), totalRevenue = sum(price), totalProducts = n_distinct(product_id), 
                                                               avgShipping = mean(freight_value), avgReview = mean(review_score),
                                                               numCategories = n_distinct(product_category_name), avgPhoto = mean(product_photos_qty))
sellerData <- sellerData[order(sellerData$totalSales, decreasing = TRUE),]
sellerData$salesPerProduct <- sellerData$totalSales / sellerData$totalProducts

sellerData <- sellerData[order(sellerData$totalRevenue, decreasing = TRUE),]
sellerData$avgPrice <- sellerData$totalRevenue / sellerData$totalSales
sellerData$productsPerCategory <- sellerData$totalProducts / sellerData$numCategories

top100Sellers <- head(sellerData, 100)

corData <- subset(sellerData, select = -c(seller_id))

clusterData <- subset(top100Sellers, select = c(avgPrice, productsPerCategory, numCategories))
clusterData <- scale(clusterData)

clusters <- kmeans(clusterData, 4)
top100Sellers$cluster <- as.factor(clusters$cluster)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
plot1 <- ggplot(top100Sellers, aes(x = avgPrice, y = productsPerCategory, color = cluster)) + geom_point(aes(size = 1.5)) + scale_color_manual(values=cbPalette) + theme(legend.position = "none")
plot2 <- ggplot(top100Sellers, aes(x = avgPrice, y = numCategories, color = cluster)) + geom_point(aes(size = 1.5)) + scale_color_manual(values=cbPalette) + guides(size = FALSE)

grid.arrange(plot1, plot2, ncol = 2)

ggplot(top100Sellers, aes(x = productsPerCategory, y = numCategories, color = cluster)) + geom_point(aes(size = avgPrice)) + scale_color_manual(values=cbPalette) + guides(size = FALSE)

revenueByCluster <- top100Sellers %>% group_by(cluster) %>% summarize(totalRev = sum(totalRevenue))
clusterSellers <- subset(top100Sellers, select = c(seller_id, cluster))
table(clusterSellers$cluster)

mergedWithCluster <- merge(mergedData, clusterSellers, by = "seller_id")

thisCluster <- mergedWithCluster[which(mergedWithCluster$cluster == 4),]
thisClusterCat <- subset(thisCluster, select = c(seller_id, product_id, product_category_name_english))
thisClusterCat <- unique(thisClusterCat)
topCats <- table(thisClusterCat$product_category_name_english)
sort(topCats, decreasing = TRUE)
