loan_data <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/loan_data.csv")
loan_data$outcome <- ordered(loan_data$outcome, levels=c('paid off', 'default'))
glm(formula = outcome ~ payment_inc_ratio + purpose_ + home_ + 
      emp_len_ + borrower_score, family = "binomial", data = loan_data)
# 下面的代码拟合了一个逻辑回归。
# 这时需要将函数的参数 family 设置为 binomial。
logistic_model <- glm(outcome ~ payment_inc_ratio + purpose_ + 
                        home_ + emp_len_ + borrower_score,
                      data=loan_data, family='binomial')
logistic_model
# 下面我们查看 logistic_model 模型的预测值。
pred <- predict(logistic_model)
prob <- 1/(1 + exp(-pred))
summary(prob)
# 除了估计的回归系数，R 还会将回归系数的标准误差、z 值和 p 值等一并给出。
summary(logistic_model)
# 我们可以使用 mgcv 软件包拟合广义添加模型
library(mgcv)
logistic_gam <- gam(outcome ~ s(payment_inc_ratio) + purpose_ +
                      home_ + emp_len_ + s(borrower_score),
                    data=loan_data, family='binomial')
logistic_gam

# 可以直接计算偏残差。
terms <- predict(logistic_gam, type='terms')
partial_resid <- resid(logistic_model) + terms
df <- data.frame(payment_inc_ratio = loan_data[, 'payment_inc_ratio'],
                 terms = terms[, 's(payment_inc_ratio)'],
                 partial_resid = partial_resid[, 's(payment_inc_ratio)'])
ggplot(df, aes(x=payment_inc_ratio, y=partial_resid, solid = FALSE)) +
  geom_point(shape=46, alpha=.4) +
  geom_line(aes(x=payment_inc_ratio, y=terms),
            color='red', alpha=.5, size=1.5) +
  labs(y='Partial Residual')

idx <- order(-pred)
test_set <- loan_data[idx,]
train_set <- loan_data[-idx,]
# 对于使用整个训练集（即非平衡数据）训练得到的 logistic_gam 模型，
# 下面的 R 代码计算了其混淆矩阵。
pred <- predict(logistic_gam, newdata=train_set)
pred_y <- as.numeric(pred > 0)
true_y <- as.numeric(train_set$outcome=='default')
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)
conf_mat <- matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)), 2, 2)
colnames(conf_mat) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat) <- c('Y = 1', 'Y = 0')
conf_mat

# 在 R 语言中，计算 ROC 曲线很简单。下面的代码计算了贷款数据的 ROC 曲线：
idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0))/sum(true_y==0)
roc_df <- data.frame(recall = recall, specificity = specificity)
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(color='blue') +
  scale_x_reverse(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x),
            linetype='dotted', color='red')
# 可以使用数值积分计算 AUC 的值。
sum(roc_df$recall[-1] * diff(1-roc_df$specificity))



full_train_set <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/full_train_set.csv")
full_train_set$outcome <- ordered(full_train_set$outcome, levels=c('paid off', 'default'))
# 预测值也给出了类似的结果，有一半预测的概率小于 0.5，另一半预测的概率大于 0.5。
mean(loan_data$outcome == "default")
# 在完整的数据集中，只有约 18.9% 的贷款被拖欠。
mean(full_train_set$outcome=='default')

# 下面的命令使用 glm 函数的 weight 参数，为贷款数据添加了权重向量。
wt <- ifelse(full_train_set$outcome=='default', 
             1/mean(full_train_set$outcome == 'default'), 1)
# full_model <- glm(outcome ~ payment_inc_ratio + purpose_ +
#                     home_ + emp_len_+ dti + revol_bal + revol_util,
#                   data=train_set, family='binomial')
# pred <- predict(full_model)

