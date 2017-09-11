library('plotly')
library("devtools")

p <- ggplotly(username="koki.ogata", key="••••••••••")

#データの作成
x0 = rnorm(500)
x1 = rnorm(500)+1

#データを表示方法を一緒に記述
data0 = list(x=x0,type='histogramx',opacity=0.8)
data1 = list(x=x1,type='histogramx',opacity=0.8)
layout = list(barmode='overlay')
#
response = p$ggplotly(data0, data1, kwargs=list(layout=layout))
#ブラウザーの表示
browseURL(response$url)
