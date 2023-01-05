
# Preço de alimentos em países democratas e autocratas -------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data do script: 04/01/23 -----------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/food-prices ---------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------


# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

preco <- read.csv("food-prices.csv")
view(preco)
names(preco)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

preco <- preco %>%
  select(-Code) %>%
  rename(prec_alim = healthy_unaffordable_share) %>%
  view()

prec1 <- preco %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China")) %>%
  group_by(Entity) %>%
  summarise(media = mean(prec_alim),
            sd = sd(prec_alim), n = n(),
            se = sd/sqrt(n)) %>%
  view()
