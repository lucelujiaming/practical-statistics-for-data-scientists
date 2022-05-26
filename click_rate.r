# 我们可以直接使用lmPerm 软件包提供的 aovp 函数实现置换检验的计算。
library(lmPerm)
summary(aovp(Time ~ Page, data=four_sessions))

# 使用 aov 函数计算 ANOVA 表。
summary(aov(Time ~ Page, data=four_sessions))

# 使用 R 语言的 chisq.test 函数，就可以计算重抽样的卡方统计量。
click_rate <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/click_rates.csv")
clicks <- matrix(click_rate$Rate, nrow=3, ncol=2, byrow=TRUE)
chisq.test(clicks, simulate.p.value=TRUE)

# 用 R 语言实现基本的费舍尔精确检验非常简单。
fisher.test(clicks)


