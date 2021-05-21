setwd('/Users/lee/Documents/code/instrumental_variables')
install.packages('AER')
library('AER')
data("CigarettesSW", package="AER")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSWtdiff <- with(CigarettesSW, (taxs - tax)/cpi)

# model
fm <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome + tdiff + I(tax/cpi), data = CigarettesSW, subset = year == "1995")

summary(fm)
summary(fm, vcov = sandwich, df = Inf, diagnostics = TRUE)

# ANOVA
fm2 <- ivreg(log(packs) - log(rprice) | tdiff, data = CigarettesSW, subset = year == "1995")
anova(fm, fm2)


# -----------------------
library(AER)

# load panel data set from 1976
data("PSID1976")

# summarize data set
summary(PSID1976)

# family background variables, such as father's education ('feducation') and mother's education ('meducation') are
# possible candidates for IVs

# regress log of wage on education, produces error
model1 <- lm(log(wage) ~ education, data = PSID1976)

#
model2 <- lm(log(wage) ~ education, data = subset(PSID1976, participation=="yes"]

summary(model2)

plot(PSID1976$education[PSID1976$participation == "yes"],
     log(PSID1976$wage[PSID1976$participation == "yes"],
     col = "red", main = log(wage) vs education",
     xlab = "education (years of schooling)",
     ylab = "log(wage)")
     
abline(model2)

# model 3
model3 <- lm(education  ~ feducation,
          data = subset(PSID1976, participation == 'yes))
          
summary(model3)








