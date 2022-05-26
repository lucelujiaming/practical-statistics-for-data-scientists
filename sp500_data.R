sp500_px <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/sp500_data.csv")
sp500_sym <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/sp500_sectors.csv")
etfs <- sp500_px[row.names(sp500_px)>"2012-07-01",
                 sp500_sym[sp500_sym$sector=="etf", 'symbol']]
library(corrplot)
corrplot(cor(etfs), method = "ellipse")

telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_services", 'symbol']]
telecom <- telecom[row.names(telecom)>"2012-07-01", ]
plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ")

nflx <- sp500_px[,'NFLX']
nflx <- diff(log(nflx[nflx>0]))
qqnorm(nflx)
abline(a=0, b=1, col='grey')


dbinom(x=2, n=5, p=0.1)
pbinom(2, 5, 0.1)

# 下面的代码对雪佛龙公司（CVX）和埃克森美孚公司（XOM）的股票收益做了主成分分析。
oil_px <- sp500_px[, c('CVX', 'XOM')] 
pca <- princomp(oil_px)
pca$loadings
# 绘制数据与主成分，也有一定的指导意义。
loadings <- pca$loadings 
ggplot(data=oil_px, aes(x=CVX, y=XOM)) +
  geom_point(alpha=.3) + 
  stat_ellipse(type='norm', level=.99) +
  geom_abline(intercept = 0, slope = loadings[2,1]/loadings[1,1]) +
  geom_abline(intercept = 0, slope = loadings[2,2]/loadings[1,2])
# 一些标准的可视化展示有助于我们深入了解主成分的相关信息。
# 一种可视化方法就是陡坡图（Screeplot）。
syms <- c( 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 
           'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_sp <- sp500_px[row.names(sp500_px)>='2005-01-01', syms] 
sp_pca <- princomp(top_sp)
screeplot(sp_pca)
# 如果绘制出前几个主成分的权重，那么绘图将更有启示作用。
# 一种绘图方法是在 ggplot 中结合使用 tidyr 包中的 gather 函数。
library(tidyr)
loadings <- sp_pca$loadings[,1:5] 
loadings$Symbol <- row.names(loadings)
loadings <- gather(loadings, "Component", "Weight", -Symbol) 
ggplot(loadings, aes(x=Symbol, y=Weight)) +
  geom_bar(stat='identity') + 
  facet_grid(Component ~ ., scales='free_y')


# 下面的代码使用了 XOM 和 CVX 股票收益这两个变量，划分出 4 个类。
df <- sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM', 'CVX')] 
km <- kmeans(df, centers=4)
# 在代码的输出结果中，每个记录的聚类分配情况在 cluster 项中。
df$cluster <- factor(km$cluster)
head(df)
# 代码还返回了类均值。
centers <- data.frame(cluster=factor(1:4), km$centers)
centers
# 在本例中，只有两个变量，所以可以直观地查看各个类及其含义。
ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) + 
  geom_point(alpha=.3) +
  geom_point(data=centers, aes(x=XOM, y=CVX), size=3, stroke=2)
# 下面的代码使用 10 个不同的初始类均值运行 K-Means，以找出 5 个类。
syms <- c( 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP',
           'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
df <- sp500_px[row.names(sp500_px)>='2011-01-01', syms]
km <- kmeans(df, centers=5, nstart=10)
km$size
# 我们可以在 ggplot 中一并使用 gather 函数绘制类中心。
centers <- as.data.frame(t(centers))
names(centers) <- paste("Cluster", 1:5) 
centers$Symbol <- row.names(centers)
centers <- gather(centers, "Cluster", "Mean", -Symbol) 
centers$Color = centers$Mean > 0
ggplot(centers, aes(x=Symbol, y=Mean, fill=Color)) + 
  geom_bar(stat='identity', position = "identity", width=.75) + 
  facet_grid(Cluster ~ ., scales='free_y')
# 但是肘部法则很容易应用于kmeans 函数的输出，如下所示。
pct_var <- data.frame(pct_var = 0,
                      num_clusters=2:14)
totalss <- kmeans(df, centers=14, nstart=50, iter.max = 100)$totss 
for(i in 2:14){
  pct_var[i-1, 'pct_var'] <- kmeans(df, centers=i, nstart=50, iter.max = 100)
  $betweenss/totalss
}

# 下面的代码对一组公司的股票收益数据应用层次聚类。
syms1 <- c('GOOGL', 'AMZN', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX',
           'XOM', 'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 
           'WMT', 'TGT', 'HD', 'COST')
# 下面执行转置操作。因为要按照公司聚类，所以需要股票数据按行排列
df <- t(sp500_px[row.names(sp500_px)>='2011-01-01', syms1])
d <- dist(df)
hcl <- hclust(d)
plot(hcl)
cutree(hcl, k=4)

# 使用该软件包实现基于模型的聚类。
library(mclust)
df <- sp500_px[row.names(sp500_px)>='2011-01-01', c('XOM', 'CVX')]
mcl <- Mclust(df)
summary(mcl)
# 下面使用 predict 函数给出聚类情况，并绘图显示。
cluster <- factor(predict(mcl)$classification)
ggplot(data=df, aes(x=XOM, y=CVX, color=cluster, shape=cluster)) + 
  geom_point(alpha=.8)
# 我们可以使用 summary 函数提取正态分布的参数。
summary(mcl, parameters=TRUE)$mean
# 我们可以使用 hclust 软件包中的函数，绘制不同类数的 BIC 值。
plot(mcl, what='BIC', ask=FALSE)

# 下面，我们将 Alphabet（GOOGL）和亚马逊（AMZN）的股票添加到分析中。
syms <- c('AMZN', 'GOOGL', 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 
          'SLB', 'COP', 'JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_sp1 <- sp500_px[row.names(sp500_px)>='2005-01-01', syms] 
sp_pca1 <- princomp(top_sp1)
screeplot(sp_pca1)
round(sp_pca1$loadings[,1:2], 3)
# 


