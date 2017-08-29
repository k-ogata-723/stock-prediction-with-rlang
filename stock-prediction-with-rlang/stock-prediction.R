stock <- read.csv("stock-sony-250.csv")
stock_2015 <- read.csv("stocks-sony-2015.csv")
stock_2016 <- read.csv("stocks-sony-2016.csv")
stock_2017 <- read.csv("stocks-sony-2017.csv")


# 1行目を削除
stockData <- stock[2:240, 2:7]
# print(stockData)

# 終値をyとしてデータにする
# y <- stock[ ,5]
# print(y)

# trainデータ
stock_train <- rbind(stock_2017, stock_2016)
# print(stock_train)

# testデータ
stock_test <- stock_2017

# # それぞれのデータを取り出し
# # 学習には利用していない
# lowPrice <- stock[ , 4]
# highPrice <- stock[ , 3]
# date <- stock[ , 1]
#
#
# # 確認用にlowPriceをプロット
# # plot(date, lowPrice, col="blue", main="plot row stock data", xlab="date", ylab="price")
#
# # data.frame型にまとめていく
# # 始値と出来高と売買代金を利用する
#

## train データをdata.frame型に変換
dataRegressionTrain <- data.frame(
  end = stock_train[ ,5],
  start = stock_train[ ,2],
  yield = stock_train[ ,6],
  buy-sell = stock_train[ ,7]
)

# print(dataRegressionTrain)
# print(stock_train[,1])

# ## test データ
# dataRegressionTest <- data.frame(
# y = stock_test[ ,5],
# x1 = stock_test[ ,2],
# x4 = stock_test[ ,6],
# x5 = stock_test[ ,7]
# )


#
#
# modelLm <- lm(
#   y ~(.)^2,
#   data = dataRegressionTrain
# )
#
# print(modelLm)
#
# sum <- summary(modelLm)
# print(sum)
#
# modelLm <- step(modelLm)
#
# # データと重回帰結果をプロットしていく
# par(mfrow=c(2,2))
# plot(modelLm)
# lines(lowPrice, fitted(modelLm))
# predict(modelLm)

# cor関数を使って、各データの相関関係を出す
cor.data <- cor(dataRegressionTrain)
print(cor.data)

pairs.data <- pairs(dataRegressionTrain, pch=21, bg='red', cex=1)
print(pairs.data)

# model.lm.train <- ggplot(dataRegressionTrain, aes(x = "日付", y = "終値")) +
#   geom_point() +
#   geom_smooth(method = 'lm', se = FALSE)
#
# print(model.lm.train)
