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

# model 4
model4 <- lm(log(wage) - fitted.values(model3),
    data = subset(PSID1976, participation == "yes"))
summary(model4)

model5 <- ivreg(log(wage) ~ education | feducation,
    data = subset(PSID1976, participation == "yes"))
summary(model5)

library(devtools)

# observe a variable, the instrument, that is correlated with the outcome variable
# assume that instrument does not have a causal effect on the treatment variable
# assume that instrument has a causal effect on the treatment
# assume that instrument as if randomly assigned
# b/c instrument assumed to be as if randomly assigned, causal effect of instrument on explanatory variable is their correlation with the data
# since instrument is randomly assigned, it is not correlated with other confounders, except the treatment

# if instrument shows clear correlation with outcome, it is because the treatment did have an effect

# pretreatment variables are fixed before treatment is assigned
# pretreatment variables may be observed and unobserved variables/confounding variables

# instrument - treatment - outcome

# instrument provides an exogenouse source of variation

#---------
library(data.table)

# read
ak91 <- fread('http://masteringmetrics.com/wp-content/uploads/2015/01/Data.zip


This posting includes three data sets. 
The first is a minimal 1980 ASCII extract without covariates.  This data set was used in Angrist and Krueger (1995) and Angrist, Imbens, and Krueger (1999). 

# load minimal 1980 ASCII file from Angrist and Krueger 1991
ak91_1 <- fread('https://economics.mit.edu/files/397')
ak91_2 <- fread('https://economics.mit.edu/files/390')

# load ASCII file with 1980 census extract from Angrist and Krueger 1991
# ASCII data set in the file QOB.rar, which contains the 1980 census extract from Angrist and Krueger (1991) with covariates (men born 1930-39 and 1940-49)

ak91_3 <- fread('https://economics.mit.edu/files/2853')
ak91_4 <- fread('https://economics.mit.edu/files/5354') # descriptive statistics file explaining above

# load Stata file with original 1970 and 1980 census data, including all cohorts and covariates
Third, NEW7080.rar, a larger Stata data set with the complete original Angrist and Krueger 1970 and 1980 extracts, and all cohorts (men born 1920-29 in 1970, men born 1930-39 in 1980, and men born 1940-49 in 1980).
ak91_5 <- fread('https://economics.mit.edu/files/2854')


