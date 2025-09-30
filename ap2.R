PackageNames <- c("tidyverse", "gplots", "plm", "readxl", "lmtest", "foreign")
for (i in PackageNames) {
  if (!require(i, character.only = T)) {
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

hc_simple <- read.csv("C:/Users/Raluk/Desktop/Econometrie2/WorldHappiness_Corruption_2015_2020.csv")
# Statistici descriptive
summary(hc_simple)

#Declarare set date tip panel 
pd.df <- pdata.frame(hc_simple, index = c("Country", "Year"), drop.index = TRUE)

# Explorarea heterogeneității
# Grafic pe țări
plotmeans(cpi_score ~ Country, main = "Heterogeneitate intre tari", data = hc_simple)


# Grafic pe ani
plotmeans(cpi_score ~ Year, main = "Heterogeneitate in timp", data = hc_simple)

# Model OLS (regresie liniară simplă)
ols <- lm( cpi_score ~ happiness_score + gdp_per_capita + freedom + generosity, data = hc_simple)
summary(ols)


yhat <- ols$fitted # valori estimate

ggplot(hc_simple, aes(x = happiness_score, y = cpi_score)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_bw()


# Model FE (Fixed Effects)
fe <- plm(cpi_score ~ happiness_score + gdp_per_capita + freedom + generosity, data = hc_simple, index = c("Country", "Year"), model = "within")
summary(fe)


# Comparare OLS vs FE
pFtest(fe, ols) # Testul F pentru alegea intre OLS si FE


# Model RE (Random Effects)
re <- plm(cpi_score ~ happiness_score + gdp_per_capita + freedom + generosity, data = hc_simple, index = c("Country", "Year"), model = "random")
summary(re)



# Testul Hausman pentru alegerea intre FE si RE
phtest(fe, re) # Hausman test


# Testarea efectelor fixe in timp
fixed.time <- plm(cpi_score ~ happiness_score + factor(Year), data = hc_simple, index = c("Country", "Year"), model = "within")
pFtest(fixed.time, fe) # Test F pentru efecte fixe in timp


# Testarea efectelor aleatorii (Breusch-Pagan)
plmtest(re, type = c("bp"))

# Testarea dependentei transversale (Breusch-Pagan LM si Parasan CD)
pcdtest(fe, test = 'lm') # LM test
pcdtest(fe, test = 'cd') # CD test

pbgtest(fe)

# Testarea heteroschedasticitatii
bptest(cpi_score ~ happiness_score + factor(Country), data = hc_simple, studentize = FALSE)


plmtest(re, c('time'), type = 'bp') # p-value > 0.05 => nu se recomanda efecte random


