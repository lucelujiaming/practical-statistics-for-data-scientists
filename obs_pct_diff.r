# 创建随机置换转换率差异的直方图。
obs_pct_diff <- 100*(200/23739 - 182/22588)
conversion <- c(rep(0, 45945), rep(1, 382))
perm_diffs <- rep(0, 1000)
for(i in 1:1000)
  perm_diffs[i] = 100*perm_fun(conversion, 23739, 22588 )
hist(perm_diffs, xlab='Session time differences (in seconds)')
abline(v = obs_pct_diff)

# 在估计置换检验的 p 值时，我们可以采用置换检验中生成
# 大于或等于观测差异值的检验次数所占的比例。
mean(perm_diffs > obs_pct_diff)
# 根据二项分布，我们可以使用正态分布近似估计 p 值。
# 在使用 R 语言编程时，函数 prop.test 执行该操作。
prop.test(x=c(200,182), n=c(23739,22588), alternative="greater")

# 在 R 语言中，我们可以使用函数 t.test。
t.test(Time ~ Page, data=session_times, alternative='less' )
