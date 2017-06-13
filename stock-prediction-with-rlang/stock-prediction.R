
stock <- read.csv("stock-sony-250.csv")
lowPrice <- stock[ , 4]
highPrice <- stock[ , 5]
date <- stock[ , 1]

plot(date, lowPrice, col="blue", main="plot row stock data", xlab="date", ylab="price")
# 図を上書き
# par(new=T)
# plot(date, highPrice,
#       col = 'red')
