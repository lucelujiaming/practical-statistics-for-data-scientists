loans_income <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/loans_income.csv")

library(ggplot2)
# 做一次简单随机抽样
# samp_data <- data.frame(income=sample(loans_income, 1000), 
#                         type='data_dist')
# 对5个数据的均值做抽样
samp_mean_05 <- data.frame( 
  income = tapply(sample(loans_income, 1000*5),
                  rep(1:1000, rep(5, 1000)), FUN = mean),
  type = 'mean_of_5')
# 对20个数据的均值做抽样
samp_mean_20 <- data.frame(
  income = tapply(sample(loans_income, 1000*20),
                  rep(1:1000, rep(20, 1000)), FUN = mean),
  type = 'mean_of_20')
# 将抽样结果绑定到一个data.frames对象，并转化为因子类型
income <- rbind(samp_data, samp_mean_05, samp_mean_20)
income$type = factor(income$type, 
                     levels=c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels=c('Data', 'Mean of 5', 'Mean of 20'))

x <- seq(from=-3, to=3, length=300)
library(boot)
stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R = 1000, statistic = stat_fun)

loan_data <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/loan_data.csv")
loan_data$outcome <- ordered(loan_data$outcome, levels=c('paid off', 'default'))
# 下面的代码就使用了 klaR 软件包去拟合模型。
library(klaR)
naive_model <- NaiveBayes(outcome ~ purpose_ + home_ + emp_len_,
                          data = na.omit(loan_data))
naive_model$table
# 可以使用该模型预测一笔新贷款的结果。
new_loan <- loan_data[147, c('purpose_', 'home_', 'emp_len_')]
new_loan
# 在本例中，模型预测了一次贷款拖欠。
predict(naive_model, new_loan)



