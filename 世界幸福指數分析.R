# 讀取資料
world_happiness <- read.csv("/Users/alan/world-happiness-report-2021.csv")

# 檢查資料框中是否有 NA
anyNA(world_happiness)  # 檢查是否有 NA
any(is.nan(world_happiness)) # 檢查是否有 NaN
any(is.infinite(world_happiness))  # 檢查是否有 Inf

# 替換 NaN 和 Inf 為 NA
world_happiness[is.nan(world_happiness)] <- NA
world_happiness[is.infinite(world_happiness)] <- NA

# 用均值填補 NA（對於數值型欄位）
world_happiness$Logged.GDP.per.capita[is.na(world_happiness$Logged.GDP.per.capita)] <- mean(world_happiness$Logged.GDP.per.capita, na.rm = TRUE)
world_happiness$Social.support[is.na(world_happiness$Social.support)] <- mean(world_happiness$Social.support, na.rm = TRUE)
world_happiness$Healthy.life.expectancy[is.na(world_happiness$Healthy.life.expectancy)] <- mean(world_happiness$Healthy.life.expectancy, na.rm = TRUE)
world_happiness$Freedom.to.make.life.choices[is.na(world_happiness$Freedom.to.make.life.choices)] <- mean(world_happiness$Freedom.to.make.life.choices, na.rm = TRUE)
world_happiness$Generosity[is.na(world_happiness$Generosity)] <- mean(world_happiness$Generosity, na.rm = TRUE)
world_happiness$Perceptions.of.corruption[is.na(world_happiness$Perceptions.of.corruption)] <- mean(world_happiness$Perceptions.of.corruption, na.rm = TRUE)

# 檢查是否還有 NA
anyNA(world_happiness)  # 應該返回 FALSE，表示沒有 NA

# 只保留數值型資料（刪除非數值型欄位）
world_happiness_numeric <- world_happiness[, sapply(world_happiness, is.numeric)]

# 去除包含NA的行
world_happiness_numeric <- na.omit(world_happiness_numeric)

# 顯示處理後的數據
head(world_happiness_numeric)

# 計算數值型變數的相關矩陣
cor_matrix <- cor(world_happiness_numeric)

# 顯示相關矩陣
print(cor_matrix)

# 找出與其他變數最相關的四個變數
# 可以從相關矩陣中選擇相關性最強的四個變數
# 例如，根據對角線的值（自己與自己關聯是1）和其他變數之間的相關性，選擇最相關的

# 假設我們選擇 'Logged.GDP.per.capita', 'Social.support', 'Healthy.life.expectancy', 和 'Freedom.to.make.life.choices' 作為最相關的變數
world_happiness_selected <- world_happiness_numeric[, c("Logged.GDP.per.capita", "Social.support", "Healthy.life.expectancy", "Freedom.to.make.life.choices")]

# K-Means 聚類分析
kmeans.result = kmeans(world_happiness_selected, 3)
kmeans.result

# 顯示聚類結果與 Regional.indicator 的關聯
table(world_happiness$Regional.indicator, kmeans.result$cluster)

# 畫出K-Means結果
plot(world_happiness_selected, col=kmeans.result$cluster)

# 檢查 K-Means 結果的群聚中心
kmeans.result$centers
centers = kmeans.result$centers[kmeans.result$cluster,]
head(centers)
distances = sqrt(rowSums((world_happiness_selected - centers)^2))
outliers = order(distances, decreasing = TRUE)[1:5]
outliers
world_happiness_selected[outliers,]
# 繪製 Outliers 的圖形
# 獲取每個資料點對應的聚類中心
centers <- kmeans.result$centers[kmeans.result$cluster,]

# 計算每個點到聚類中心的距離
distances <- sqrt(rowSums((world_happiness_selected - centers)^2))

# 找出距離最大的 5 個 Outliers
outliers <- order(distances, decreasing = TRUE)[1:5]

# 繪製 Logged.GDP.per.capita 與 Social.support 的散佈圖
plot(world_happiness_selected[, c("Logged.GDP.per.capita", "Social.support")], 
     col = kmeans.result$cluster, 
     main = "K-Means Clustering with Outliers Highlighted", 
     xlab = "Logged GDP per Capita", 
     ylab = "Social Support")

# 標記聚類中心
points(kmeans.result$centers[, c("Logged.GDP.per.capita", "Social.support")], 
       col = 1:3, 
       pch = 4, 
       cex = 2)

# 標記 Outliers
points(world_happiness_selected[outliers, c("Logged.GDP.per.capita", "Social.support")], 
       col = "red", 
       pch = 8, 
       cex = 2)

# 顯示 Outliers 的詳細資料
print("Outliers details:")
print(world_happiness_selected[outliers,])





# 加入 factoextra 库來使用手肘法
library(factoextra)

# 手肘法（Elbow Method）來確定最佳的 K 值
# K-Means 手肘法
fviz_nbclust(world_happiness_selected, 
             FUNcluster = kmeans, 
             method = "wss",     
             k.max = 12) + 
  labs(title = "Elbow Method for K-Means") + 
  geom_vline(xintercept = 3, linetype = 2)

# K-Medoid 手肘法
fviz_nbclust(world_happiness_selected, 
             FUNcluster = pam,   
             method = "wss",     
             k.max = 12) + 
  labs(title = "Elbow Method for K-Medoid") + 
  geom_vline(xintercept = 3, linetype = 2)

# Hierarchical Clustering 手肘法（如果需要）
fviz_nbclust(world_happiness_selected, 
             FUNcluster = hcut,  
             method = "wss",     
             k.max = 12) + 
  labs(title = "Elbow Method for HC") + 
  geom_vline(xintercept = 3, linetype = 2)

