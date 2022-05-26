# 我们使用 ggplot 实现了箱线图的并排绘制。
session_times <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/web_page_data.csv")
ggplot(session_times, aes(x=Page, y=Time)) +
  geom_boxplot()
# 所示。图中显示了页面 B 具有比页面 A 更长的会话时间。
mean_a <- mean(session_times[session_times['Page']=='Page A', 'Time'])
mean_b <- mean(session_times[session_times['Page']=='Page B', 'Time'])
mean_b - mean_a

# 我们实现了一个进行置换检验的函数。
# 该函数的工作原理是，无放回地抽样 n2 次，并分配给 B 组，余下的 n1 次抽样分配给 A组。
perm_fun <- function(x, n1, n2)
{
  n <- n1 + n2
  idx_b <- sample(1:n, n1)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return(mean_diff)
}

# 绘制所生成的会话时间差异分布情况的直方图。
perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = perm_fun(session_times[,'Time'], 21, 15)
hist(perm_diffs, xlab='Session time differences (in seconds)')
abline(v = mean_b - mean_a)


four_sessions <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/four_sessions.csv")
click_rate <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/click_rates.csv")
imanishi <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/imanishi_data.csv")
