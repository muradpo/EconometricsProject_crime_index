library(AER)

data <- read.csv("crim_index_filledgaps.csv")
colnames(data) <- gsub("\\.", " ", colnames(data))
# vcov = sandwich gives robust errors
# diagnostics = T makes summary to return 3 basic tests:
# Weak instruments (how good are the instruments = relevancy). H_0: instruments are not relevant
# Wu-Hausman (whether we had endogeneity or not). H_0: no endogeneity, OLS estimates are "better"
# Sargan (validity). H_0: instruments are valid
#2мнк модель
mIV1 <- ivreg(log(`Criminality`) ~ log(`Unemployment Rate`)+(`Inflation`)+(`Incarceration rate`) + log(`Corruption Perception Index`) + (`GDP per capita`)  +
                (`Urban Population`)+log(`Gini Rate`)+`Part of the world` +  log(`Alcohol consumption per capita`)+ (`Political Stability and Absence of Violence`)  +log(`Mafia style groups`) + (`Gun Ownership Rate`)+log(`Poverty rate`)| 
                (`Inflation`)+log(`Unemployment Rate`)+`Part of the world`+ (`Literacy Rate`)+log(`Gini Rate`)  + (`Population density`) + (`GDP per capita`) +log (`Corruption Perception Index`) + 
                (`Urban Population`) +(`Political Stability and Absence of Violence`) + log(`Alcohol consumption per capita`)+ (`Gun Ownership Rate`)+log(`Mafia style groups`)+log(`Poverty rate`) , data = data) 

summary(mIV1, vcov = sandwich, diagnostics = TRUE)


#обычный мнк
mOLS2 <- lm(log(`Criminality`) ~ log(`Unemployment Rate`)+(`Inflation`)+(`Incarceration rate`) + log(`Corruption Perception Index`) + (`GDP per capita`)  +
              (`Urban Population`)+log(`Gini Rate`)+`Part of the world` +  log(`Alcohol consumption per capita`)+ (`Political Stability and Absence of Violence`)  +log(`Mafia style groups`) + (`Gun Ownership Rate`)+log(`Poverty rate`), data=data)
summary(mOLS2, vcov = sandwich, diagnostics = TRUE)


install.packages("stargazer")
library(stargazer)
stargazer(mIV1, mOLS2, digits = 2, title = "Regression Results", 
          no.space = TRUE, ci.level = 0.90, type = "html", out = "mytable.doc",
          single.row = TRUE, row.sep = "")
getwd()
