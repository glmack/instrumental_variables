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

# inspect iv association with treatment
mean(card.data$educ[card.data$nearc4==1])
mean(card.data$educ[card.data$nearc4==0])

# convert educ to boolean
educ12 <- card.data$educ>12
educ12

# proportion of compliers
prop_compliers <- mean(educ12[card.data$nearc4==1] -
                         mean(educ12[card.data$nearc4==0]))
prop_compliers

# intent to treat 
# causal effect of encouragement (nearc4)
itt <- mean(card.data$lwage[card.data$nearc4==1] -
              mean(card.data$lwage[card.data$nearc4==0]))
itt


# calculate complier average causal effect (CACE)
# impact of # of years of education among compliers
cace = itt/prop_compliers
cace

# consider no defiers assumption

# Calculate via two-stage least squares (2SLS) method

# stage 1 - regress A on Z
s1 <- lm(educ12~card.data$nearc4)
s1

# get predicted val of A given Z for each subject
pred_tx <- predict(s1, type='response')
pred_tx

# stage 2 - regress Y on predicted val of A
lm(card.data$lwage~pred_tx)

lm(formula = card.data$lwage ~ pred_tx)

# results show that coef of pred_tx == cace in model1 above

# Calculcate 2sls using ivpack
ivmodel1=ivreg(lwage ~ educ12, ~ nearc4, x=TRUE, data=card.data)
ivmodel

# calculate with robust SE
robust.se(ivmodel)

# calculate 2SLS with covariates
ivmodel2 = ivreg(lwage ~ educ12 + exper + reg661 + reg662 + reg663 +
                 reg664 + reg665 + reg666 + reg667 + reg668,
                 ~ nearc4 + exper + reg661+ reg662 + reg663 + reg664
                 + reg665 + reg666 + reg667 + reg668, x= TRUE, data = card.data)
ivmodel2

robust.se(ivmodel2)

