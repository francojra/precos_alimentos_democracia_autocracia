
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
  filter(Entity %in% c("United States", "Germany", "Japan", "China")) %>%
  group_by(Entity) %>%
  summarise(media = mean(prec_alim),
            sd = sd(prec_alim), n = n(),
            se = sd/sqrt(n)) %>%
  view()

prec2 <- preco %>%
  filter(Entity %in% c("United States", "Germany", "Japan", "China")) %>%
  view()

prec3 <- preco %>%
  filter(Entity %in% c("United States", "Brazil", "China")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 4)

ggplot(prec1, aes(x = fct_reorder(Entity, media), y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.9) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Alemanha", "Estados Unidos",
                              "Japão", "China")) +
  labs(x = "Países", y = "População sem recursos\n para alimentação (%)") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none", axis.text = element_text(color = "black"))

ggplot(prec2, aes(x = Year, y = prec_alim,
                  group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                                "#DDCC77", "#117733"),
                                labels = c("China", "Alemanha",
                                           "Japão", "Estados Unidos")) +
  labs(x = "Tempo (anos)", 
       y = "População sem recursos\n para alimentação (%)",
       color = "Países") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(axis.text = element_text(color = "black"))

ggplot(prec3, aes(x = Year, y = prec_alim,
                  group = Entity, color = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", 
       y = "População sem recursos\n para alimentação (%)",
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
