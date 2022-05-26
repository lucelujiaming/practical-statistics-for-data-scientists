# 列出的数据存储在一个名为 house 的 data.frame 中。
house <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/house_sales.csv", sep='\t')
# house <- house[house$ZipCode > 0, ]
head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", 
               "Bedrooms", "BldgGrade")])
# 我们还要设置函数的参数na.action=na.omit，使得模型可以丢弃那些有缺失值的记录。
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade,
               data=house, na.action=na.omit)
# 打印 house_lm 对象将产生输出。
house_lm
# 可以使用 R 语言的 summary 函数计算一个模型的标准残差等度量。
summary(house_lm)
# 要预测一处房屋的价值，可以使用房屋面积或建造年份等变量。
house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                   Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                   SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                 data=house, na.action=na.omit)
# 逐步回归计算函数stepAIC通过连续地添加并丢弃预测因子，发现可降低 AIC 的模型。
library(MASS)
step <- stepAIC(house_full, direction="both")
step
# Call:
# lm(formula = AdjSalePrice ~ SqFtTotLiving + Bathrooms + Bedrooms +
#    BldgGrade + PropertyType + SqFtFinBasement + YrBuilt, data = house0,
#    na.action = na.omit)

# 我们可以计算自 2005 年（数据的开始年份）以来的年份数，以此作为 Weight 变量。
library(lubridate)
house$Year = year(house$DocumentDate)
house$Weight = house$Year - 2005
# 下面，我们使用设置了 weight 参数的 lm 函数计算加权回归。
house_wt <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade,
               data=house, weight=Weight)
round(cbind(house_lm=house_lm$coefficients,
            house_wt=house_wt$coefficients), digits=3)

head(house[, 'PropertyType'])

library(MASS)
step_lm <- stepAIC(house_full, direction="both")
step_lm
# 下面查看一下 4.2.4 节中拟合的 step_lm 模型的回归系数。
step_lm$coefficients
# 我们在回归方程中移除了 SqFtTotLiving、SqFtFinBasement 和 Bathrooms 变量，
# 拟合了另一个回归。
update(step_lm, . ~ . -SqFtTotLiving - SqFtFinBasement - Bathrooms)
# 为了对地段情况建模，我们加入了变量 ZipGroup。
# 该变量将邮政编码分到 5 个组中的一个，从房价最便宜的组 1 到房价最贵的组 5。
lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot +
     Bathrooms + Bedrooms +
     BldgGrade + PropertyType + ZipGroup,
   data=house, na.action=na.omit)
# 下面的代码使用金县房屋数据拟合了 SqFtTotLiving 和 ZipGroup 间的交互作用。
lm(AdjSalePrice ~ SqFtTotLiving*ZipGroup + SqFtLot +
     Bathrooms + Bedrooms + BldgGrade + PropertyType,
   data=house, na.action=na.omit)

# 下面，我们使用邮政编码 98105 区域的所有金县房屋销售数据拟合一个回归模型。
house_98105 <- house[house$ZipCode == 98105,]
lm_98105 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms +
                 Bedrooms + BldgGrade, data=house_98105)
# 我们使用 rstandard 函数抽取出标准残差，并使用 order 函数获得最小残差。
sresid <- rstandard(lm_98105)
idx <- order(sresid)
# 与该离群值对应的原始数据记录如下：
house_98105[idx[1], c('AdjSalePrice', 'SqFtTotLiving', 'SqFtLot',
                      'Bathrooms', 'Bedrooms', 'BldgGrade')]
# 影响图，也被称为气泡图，在单个绘图中展示了标准残差、帽值和库克距离。\
std_resid <- rstandard(lm_98105)
cooks_D <- cooks.distance(lm_98105)
hat_values <- hatvalues(lm_98105)
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty=2)
# 使用lm_98105 模型，绘制了残差绝对值与预测值的对比情况。
library(ggplot2)
df <- data.frame(
  resid = residuals(lm_98105),
  pred = predict(lm_98105))
ggplot(df, aes(pred, abs(resid))) +
  geom_point() +
  geom_smooth()
# R 中的 predict 函数提供了返回单个回归项 的选项。
terms <- predict(lm_98105, type='terms')
partial_resid <- resid(lm_98105) + terms
# 使用 ggplot2，很容易实现在已有绘图上叠加偏残差的平滑绘图，代码如下：
df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 'SqFtTotLiving'],
                 PartialResid = partial_resid[, 'SqFtTotLiving'])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) +
  geom_line(aes(SqFtTotLiving, Terms))

# 下面的代码使用金县房屋数据，对SqFtTotLiving 拟合了一个二项式回归。
lm(AdjSalePrice ~ poly(SqFtTotLiving, 2) + SqFtLot +
     BldgGrade + Bathrooms + Bedrooms,
   data=house_98105)
# 下面的代码在金县房屋回归模型中添加了一个 b 样条项。
library(splines)
knots <- quantile(house_98105$SqFtTotLiving, p=c(.25, .5, .75))
lm_spline <- lm(AdjSalePrice ~ bs(SqFtTotLiving, knots=knots, degree=3) +
                  SqFtLot + Bathrooms + Bedrooms + BldgGrade, data=house_98105)
# 可使用 R 的 gam 软件包，拟合金县房屋数据的广义加性模型。
library(mgcv)
lm_gam <- gam(AdjSalePrice ~ s(SqFtTotLiving) + SqFtLot +
                Bathrooms + Bedrooms + BldgGrade,
              data=house_98105)

