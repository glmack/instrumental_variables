setwd('/Users/lee/Documents/code/instrumental_variables')

install.packages('ivpack')
library(ivpack)

# install Card dataset from ivpack
data = data(card.data)

# inspect instrumentor, nearc4 (near 4 year college)
summary(card.data$nearc4)

# inspect dep var, lwage (log wage)
summary(card.data$lwage)

# inspect treatment, educ (years of education)
summary(card.data$educ)

par(mfrow=c(1,2))

hist(card.data$lwage)

hist(card.data$educ)
