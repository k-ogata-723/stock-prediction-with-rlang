stock <- read.csv("stock-sony-250.csv")

# 1行目を削除
stockData <- stock[2:240, 2:7]
# print(stockData)

# 終値をyとしてデータにする
y <- stock[ ,5]
# print(y)

dataRegressionTrain <- stockData[ , -1]

# それぞれのデータを取り出し
lowPrice <- stock[ , 4]
highPrice <- stock[ , 3]
date <- stock[ , 1]

# 確認用にlowPriceをプロット
# plot(date, lowPrice, col="blue", main="plot row stock data", xlab="date", ylab="price")

# data.frame型にまとめていく
# 始値と出来高と売買代金を利用する
dataRegressionTrain <- data.frame(
  y = stock[ ,5],
  x1 = stock[ ,2],
#   x2 = stock[ ,3],
#   x3 = stock[ ,4],
  x4 = stock[ ,6],
  x5 = stock[ ,7]
)


modelLm <- lm(
  y ~(.)^2,
  data = dataRegressionTrain
)

sum <- summary(modelLm)
sum

modelLm <- step(modelLm)
