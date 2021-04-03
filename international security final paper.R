# International Security - Final Paper

# Are Democracies More Prone to Conduct Covert Operations 
# Than Their Autocratic Counterparts?

library(readxl)
library(tidyverse)
library(jtools)
library(stargazer)
library(gridExtra)
library(plm)

covert <- read_excel("C:/Users/atmas/Downloads/covert.xlsx")

# 2020 codebook indexes below: 

www.idea.int/gsod-indices/sites/default/files/global-state-of-democracy-indices-codebook-v4.pdf

# Representative Government (C_A1)

# Fundamental Rights (C_A2)

# Checks on government (C_A3)

# Civil Society Participation (C_SD51)

model_1 <- glm(covert ~ C_A2 + C_A3 + C_A1 + C_SD51, data = covert, family = binomial(link = "probit"))

model_2 <- glm(covert ~ C_A2 + C_A3 + C_A1 + C_SD51, data = covert, family = binomial(link = "logit"))

stargazer(
  model_1,
  model_2,
  type = "html",
  out = "covert_table.html",
  title = "Statistical Results",
  dep.var.labels = "Covert Activity",
  notes.label = "Significance Levels",
  covariate.labels = c("Fundamental Rights", "Checks on Government", "Representative Government", " Civil Society Participation"))

