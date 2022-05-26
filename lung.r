lung <- read.csv(file="/Users/lucelu/statistics-for-data-scientists/data/LungDisease.csv")
model <- lm(PEFR ~ Exposure, data=lung)
model
fitted <- predict(model) 
resid <- residuals(model)
