loan3000 <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/loan3000.csv")
library(MASS)
loan_lda <- lda(outcome ~ borrower_score + payment_inc_ratio,
                data=loan3000)
loan_lda$scaling

pred <- predict(loan_lda) 
head(pred$posterior)

# 从库上代码里面摘出来一段。用于给lda_df0赋值。
x <- seq(from=.33, to=.73, length=100)
y <- seq(from=0, to=20, length=100)
newdata <- data.frame(borrower_score=x, payment_inc_ratio=y)
pred <- predict(loan_lda, newdata=newdata)
lda_df0 <- cbind(newdata, outcome=pred$class)
lda_df <- cbind(loan3000, prob_default=pred$posterior[,'default']) 

library(ggplot2)
ggplot(data=lda_df,
       aes(x=borrower_score, y=payment_inc_ratio, color=prob_default)) + 
  geom_point(alpha=.6) +
  scale_color_gradient2(low='white', high='blue') + 
  geom_line(data=lda_df0, col='green', size=2, alpha=.8) 



