lc_loans <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/lc_loans.csv")
library(descr)
x_tab <- CrossTable(lc_loans$grade, lc_loans$status,
                    prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)
