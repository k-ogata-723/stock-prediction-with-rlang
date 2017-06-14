
stock <- read.csv("stock-sony-250.csv")


# 1行目を削除
stockData <- stock[2:240, 2:7]
print(stockData)

# 終値をyとしてデータにする
y <- stock[ ,5]

# 終値以外のデータを取り出し
dataRegressionTrain <- stockData[ , -5]
dataRegressionTrain <- stockData[ , -1]

print(length(dataRegressionTrain[ , 1]))
print(length(dataRegressionTrain[ , 2]))
print(length(dataRegressionTrain[ , 3]))
print(length(dataRegressionTrain[ , 4]))
print(length(dataRegressionTrain[ , 5]))
# print(dataRegressionTrain)

# それぞれのデータを取り出し
lowPrice <- stock[ , 4]
highPrice <- stock[ , 3]
date <- stock[ , 1]

# 確認用にlowPriceをプロット
plot(date, lowPrice, col="blue", main="plot row stock data", xlab="date", ylab="price")

modelLm <- lm(
  y ~ .,
  data = dataRegressionTrain
)

modelLm <- step(modelLm)
