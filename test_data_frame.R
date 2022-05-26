state <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/state.csv")
mean(state[["Population"]])
mean(state[["Population"]], trim=0.1)
median(state[["Population"]])
# 使用加权均值或中位数
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])
library("matrixStats")
weightedMedian(state[["Murder.Rate"]], w=state[["Population"]])
# 下面使用 R 语言自带的函数计算标准偏差、四分位距和中位数绝对偏差。
sd(state[["Population"]])
IQR(state[["Population"]])
mad(state[["Population"]])
# 可使用 quantile 函数生成百分位数。
quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95))
# 箱线图是由图基提出的一种快速可视化绘图
boxplot(state[["Population"]]/1000000, ylab="Population (millions)")
# 可使用下面命令计算美国人口按州分布的频数表
breaks <- seq(from=min(state[["Population"]]),
              to=max(state[["Population"]]), length=11)
pop_freq <- cut(state[["Population"]], breaks=breaks,
                right=TRUE, include.lowest = TRUE)
table(pop_freq)
# 直方图是频数表的一种可视化方法，其中 x 轴为组距，y 轴为数据的计数。
hist(state[["Population"]], breaks=breaks)
# 在 R 语言中，可以使用 density 函数计算密度估计。
hist(state[["Murder.Rate"]], freq=FALSE)
lines(density(state[["Murder.Rate"]]), lwd=3, col="blue")
# 条形图是在各大媒体上常用的一种可视化工具，它可显示单个分类变量的总体情况。
# barplot(as.matrix(dfw)/6, cex.axis=.5)

