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









