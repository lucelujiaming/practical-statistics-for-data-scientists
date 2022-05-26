PSDS_PATH <- file.path('~', 'statistics-for-data-scientists')

loan200 <- read.csv(file.path(PSDS_PATH, 'data', 'loan200.csv'))
loan200$outcome <- ordered(loan200$outcome, levels=c('paid off', 'default'))

loan3000 <- read.csv(file.path(PSDS_PATH, 'data', 'loan3000.csv'))
loan3000$outcome <- ordered(loan3000$outcome, levels=c('paid off', 'default'))

loan_data <- read.csv(file.path(PSDS_PATH, 'data', 'loan_data.csv'))
loan_data <- select(loan_data, -X, -status)
outcome <- loan_data[-1,1]

# 我们在 R 中计算在 dti=22.5 和 payment_inc_ratio=9 的情况下，
# 对要预测的新贷款 newloan 的 KNN 估计值。
newloan <- loan200[1, 2:3, drop=FALSE]
library(FNN)
knn_pred <- knn(train=loan200[-1,2:3], test=newloan, cl=loan200[-1,1], k=20)
knn_pred == 'paid off'
newloan
# 下面使用 nn.index 显示 load_df 中最近的 5 行数据。
loan_df <- model.matrix(~ -1 + payment_inc_ratio + dti + revol_bal + 
                          revol_util, data=loan_data) 
knn_pred <- knn(train=loan_df, test=newloan, cl=outcome, k=5) 
loan_df[attr(knn_pred,"nn.index"),]
# 下面，我们使用 R 语言的 scale 函数对数据做标准化，
# 该函数计算每个变量的 z 分数。然后对标准化后的数据应用 KNN。
#  loan_std <- scale(loan_df)
#  knn_pred <- knn(train=loan_std, test=newloan_std, cl=outcome, k=5)
#  loan_df[attr(knn_pred,"nn.index"),]

# 下面的命令将构建一个表示借款者信誉的特征。
borrow_df <- model.matrix(~ -1 + dti + revol_bal + revol_util + open_acc +
                            delinq_2yrs_zero + pub_rec_zero, data=loan_data)
borrow_knn <- knn(borrow_df, test=borrow_df, cl=loan_data[, 'outcome'],
                  prob=TRUE, k=10) 
prob <- attr(borrow_knn, "prob")
borrow_feature <- ifelse(borrow_knn=='default', prob, 1-prob) 
summary(borrow_feature)


# 下面的代码使用 rpart 软件包，根据变量 payment_inc_ratio 和 borrower_score 
# 对贷款数据拟合了一个树模型。所使用的数据是一个具有 3000 条记录的样本。
library(rpart)
loan_tree <- rpart(outcome ~ borrower_score + payment_inc_ratio, 
                   data=loan_data, control = rpart.control(cp=.005))
plot(loan_tree, uniform=TRUE, margin=.05) 
text(loan_tree)

# 下面的代码对贷款数据应用该软件包
library(randomForest)
rf <- randomForest(outcome ~ borrower_score + payment_inc_ratio,
                     data=loan3000)
# 使用模型的输出，可以绘制出 OOB 误差与随机森林中树模型的数量。
error_df = data.frame(error_rate = rf$err.rate[,'OOB'],
                      num_trees = 1:rf$ntree) 
ggplot(error_df, aes(x=num_trees, y=error_rate)) +
  geom_line()
# 我们可以使用如下代码，从 predict 函数得到预测值并绘图。
pred <- predict(rf, prob=TRUE)
rf_df <- cbind(loan3000, pred = pred)

pred <- predict(loan_lda)
rf_df <- cbind(loan3000, pred_default=pred[,'default']>.5) 
ggplot(data=rf_df, aes(x=borrower_score, y=payment_inc_ratio,
                       color=pred_default, shape=pred_default)) + 
  geom_point(alpha=.6, size=2) +
  scale_shape_manual( values=c( 46, 4))



# 在下面的代码中，我们对只有两个预测变量的贷款数据应用 xgboost。
library(xgboost)
predictors <- data.matrix(loan3000[, c('borrower_score',
                                       'payment_inc_ratio')]) 
label <- as.numeric(loan3000[,'outcome'])-1
xgb <- xgboost(data=predictors, label=label, 
               objective = "binary:logistic", 
               params=list(subsample=.63, eta=0.1), nrounds=100)
# 由于在本例中只使用了两个变量，因此只绘制预测变量和预测值。
pred <- predict(xgb, newdata=predictors)
xgb_df <- cbind(loan3000, pred_default=pred>.5, prob_default=pred) 
ggplot(data=xgb_df, aes(x=borrower_score, y=payment_inc_ratio,
                        color=pred_default, shape=pred_default)) + 
  geom_point(alpha=.6, size=2)
# 下面的代码在训练数据上使用 xgboost 拟合贷款数据，模型中考虑了全部变量。
seed <- 400820
predictors <- data.matrix(loan_data[,-which(names(loan_data) %in%
                                              'outcome')])
label <- as.numeric(loan_data$outcome)-1
test_idx <- sample(nrow(loan_data), 10000)
xgb_default <- xgboost(data=predictors[-test_idx,],
                       label=label[-test_idx],
                       objective = "binary:logistic", nrounds=250)
pred_default <- predict(xgb_default, predictors[test_idx,])
error_default <- abs(label[test_idx] - pred_default) > 0.5
xgb_default$evaluation_log[250,]

# 下面的代码探索了将 lambda 设置为 1000 的情况。
xgb_penalty <- xgboost(data=predictors[-test_idx,],
                       label=label[-test_idx],
                       params=list(eta=.1, subsample=.63, lambda=1000), 
                       objective = "binary:logistic", nrounds=250)
pred_penalty <- predict(xgb_penalty, predictors[test_idx,])
error_penalty <- abs(label[test_idx] - pred_penalty) > 0.5
xgb_penalty$evaluation_log[250,] 
mean(error_penalty)

# 结合样本外误差，我们就可以绘制误差与迭代次数的关系。
errors <- rbind(xgb_default$evaluation_log,
                xgb_penalty$evaluation_log,
                data.frame(iter=1:250, train_error=error_default), 
                data.frame(iter=1:250, train_error=error_penalty))
errors$type <- rep(c('default train', 'penalty train',
                     'default test', 'penalty test'), rep(250, 4))
ggplot(errors, aes(x=iter, y=train_error, group=type)) +
geom_line(aes(linetype=type, color=type))

# 为了介绍这一技术，我们将其应用于 xgboost 函数的参数选择。
# 首先，我们设置折数和参数列表。
N <- nrow(loan_data)
fold_number <- sample(1:5, N, replace = TRUE)
params <- data.frame(eta = rep(c(.1, .5, .9), 3),
                     max_depth = rep(c(3, 6, 12), rep(3,3)))
# 然后我们应用前面介绍的算法，使用 5 折计算每个模型和每折的误差。
error <- matrix(0, nrow=9, ncol=5)
for(i in 1:nrow(params)){
  for(k in 1:5){
    fold_idx <- (1:N)[fold_number == k]
    xgb <- xgboost(data=predictors[-fold_idx,], label=label[-fold_idx], 
                     params = list(eta = params[i, 'eta'],
                                   max_depth = params[i, 'max_depth']), 
                     objective = "binary:logistic", nrounds=100, verbose=0)
    pred <- predict(xgb, predictors[fold_idx,])
    error[i, k] <- mean(abs(label[fold_idx] - pred) >= 0.5)
    }
  }

# 使用函数 rowMeans 比较各个参数集的错误率。
avg_error <- 100 * rowMeans(error)
cbind(params, avg_error)

# 例如，下面代码不对贷款拖欠数据做归一化处理，就应用了 kmeans 函数。
defaults <- loan_data[loan_data$outcome=='default',]
df <- defaults[, c('loan_amnt', 'annual_inc', 'revol_bal', 'open_acc', 
                   'dti', 'revol_util')]
km <- kmeans(df, centers=4, nstart=10)
centers <- data.frame(size=km$size, km$centers)
round(centers, digits=2)
# 下面，我们将 kmeans 应用于归一化的数据，再次查看类所发生的变化。
df0 <- scale(df)
km0 <- kmeans(df0, centers=4, nstart=10) 
centers0 <-scale(km0$centers, center=FALSE, 
                 scale=1/attr(df0, 'scaled:scale'))
centers0 <- scale(centers0, center=-attr(df0, 'scaled:center'), scale=F) 
data.frame(size=km0$size, centers0)

# 为了更好地解释高氏距离，我们以贷款数据为例，从数据中取几行。
defaults
x = defaults[1:5, c('dti', 'payment_inc_ratio')] # , 'home', 'purpose'
library(cluster)
daisy(x, metric='gower')
# 下面对 daisy 函数的输出调用 hclust，对所生成的距离矩阵应用层次聚类
df <- defaults[sample(nrow(defaults), 250),
               c('dti', 'payment_inc_ratio')] # , 'home', 'purpose'
d = daisy(df, metric='gower')
hcl <- hclust(d)
dnd <- as.dendrogram(hcl)
plot(dnd, leaflab='none', ylab='distance')
# 下面我们将 kmeans 函数应用于具有因子变量 home 和 pub_rec_zero 的贷款拖欠数据。
df <- model.matrix(~ -1 + dti + payment_inc_ratio + home_ + pub_rec_zero,
                   data = defaults)
df0 <- scale(df)
km0 <- kmeans(df0, centers=4, nstart=10)
centers0 <-scale(km0$centers, center=FALSE,
                 scale=1/attr(df0, 'scaled:scale'))
round(scale(centers0, center=-attr(df0, 'scaled:center'), scale=FALSE), 2)


