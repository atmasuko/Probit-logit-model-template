# Devolution

# loading required packages

library(readxl)
library(tidyverse)
library(jtools)
library(stargazer)
library(gridExtra)

# Importing dataset
devolution <- read_excel("C:/Users/atmas/Downloads/dev.xlsx")

# constructing probit model
model_1 <- glm(dev ~ bra, data = devolution, family = binomial(link = "probit"))

# adding control variables
model_2 <- glm(dev ~ bra + ira + idn + zas, data = devolution, family = binomial(link = "probit"))

# constructing logit model  
model_3 <- glm(dev ~ bra, data = devolution, family = binomial(link = "logit"))

#adding control variables
model_4 <- glm(dev ~ bra + ira + idn +zas, data = devolution, family = binomial(link = "logit"))


# Running the model
stargazer(
          model_2,
          model_4,
          type = "html",
          out = "dev.html",
          title = "Statistical Results",
          dep.var.labels = "Devolution",
          notes.label = "Significance Levels",
          covariate.labels = c("Brazil", "Iran", "Indonesia", "South Africa"))


bra <- ggplot(data = devolution) + 
  geom_smooth(mapping = aes(x = bra, y = dev), method = "glm") +
  labs(x = "Brazil",
       y = " ") +
  theme_minimal()

iran <- ggplot(data = devolution) + 
  geom_smooth(mapping = aes(x = ira, y = dev), method = "glm") +
  labs(x = "Iran",
       y = " ") +
  theme_minimal()

idn <- ggplot(data = devolution) + 
  geom_smooth(mapping = aes(x = idn, y = dev), method = "glm") +
  labs(x = "Indonesia",
       y = " ") +
  theme_minimal()

zas <- ggplot(data = devolution) + 
  geom_smooth(mapping = aes(x = zas, y = dev), method = "glm") +
  labs(x = "South Africa",
       y = " ") +
  theme_minimal()

plots <- grid.arrange(bra, idn, iran, zas, 
             nrow = 2, ncol = 2, 
             left = "Devolution",
             top = "The Impact of a Country on Devolution")

ggsave(plots, file = "plots.pdf")

lm_bra <- lm(dev ~ bra, data = devolution)
lm_idn <- lm(dev ~ idn, data = devolution)
lm_zas <- lm(dev ~ zas, data = devolution)
lm_iran <- lm(dev ~ ira, data = devolution)

plots2 <- plot_summs(lm_bra, lm_idn, lm_zas, lm_iran,
           scale = TRUE,
           coefs = c("Brazil" = "bra", "Indonesia" = "idn", "South Africa" = "zas", "Iran" = "ira"),
           model.names = c("Brazil", "Indonesia", "South Africa", "Iran"))

ggsave(plots2, file = "plots_2.pdf")           
