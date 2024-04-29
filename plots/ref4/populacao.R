# https://www.sciencedirect.com/science/article/pii/S0169207021001394
# https://population.un.org/wpp/

rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)

# 1950-2021: estimativas
# 2022-2100: mediana das estimativas

# MUNDO ========================================================================
# Pop está em milhares, passando para bilhões
df = read_excel('./2024/pop_forecast.xlsx', sheet = 'world') %>% 
  mutate(Total = Total*0.000001,
         `Lower 95 PI` = `Lower 95 PI`*0.000001,
         `Lower 80 PI` = `Lower 80 PI`*0.000001,
         `Upper 80 PI` = `Upper 80 PI`*0.000001,
         `Upper 95 PI` = `Upper 95 PI`*0.000001)

# As áreas sombreadas claras/escuras mostram os intervalos de previsão pontuais 
# de 80%/95%

ggplot(df, aes(x = Year, y = Total)) +
  geom_ribbon(aes(ymin = `Upper 95 PI`, ymax = `Lower 95 PI`), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymin = `Upper 80 PI`, ymax = `Lower 80 PI`), fill = "blue", alpha = 0.3) +
  geom_line(color='red', size=1) +
  scale_x_continuous(breaks = seq(1950, 2100, by = 10)) +
  labs(title = "World Population Projection",
       x = "Year",
       y = "Total Population (billions)") +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black") # Adicione a linha vertical pontilhada em 2021



# Continentes ==================================================================
dfc = read_excel('./2024/pop_forecast.xlsx', sheet = 'continent') %>% 
  mutate(Total = Total*0.000001,
         `Lower 95 PI` = `Lower 95 PI`*0.000001,
         `Lower 80 PI` = `Lower 80 PI`*0.000001,
         `Upper 80 PI` = `Upper 80 PI`*0.000001,
         `Upper 95 PI` = `Upper 95 PI`*0.000001) %>% 
  rename(Continent=2)


ggplot(dfc, aes(x = Year, y = Total, color = Continent, fill = Continent)) +
  geom_ribbon(aes(ymin = `Upper 95 PI`, ymax = `Lower 95 PI`), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `Upper 80 PI`, ymax = `Lower 80 PI`), alpha = 0.3, color = "transparent") +
  geom_line(size=1) +
  scale_x_continuous(breaks = seq(1950, 2100, by = 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Continental Population Projections",
       x = "Year",
       y = "Total Population (billions)") +
  #theme_minimal() +
  theme(legend.position = "right") +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black")+
  theme(legend.position = c(.02, .98), legend.justification = c(0, 1),
        legend.background = element_blank(),  # Remove o fundo branco da legenda
        legend.title = element_blank())  # Remove o título da legenda


# tirando mais populosos =======================================================
ggplot(dfc %>% filter(!Continent %in% c('Asia', 'Africa')), 
       aes(x = Year, y = Total, color = Continent, fill = Continent)) +
  geom_ribbon(aes(ymin = `Upper 95 PI`, ymax = `Lower 95 PI`), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `Upper 80 PI`, ymax = `Lower 80 PI`), alpha = 0.3, color = "transparent") +
  geom_line(size=1) +
  scale_x_continuous(breaks = seq(1950, 2100, by = 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Continental Population Projections",
       x = "Year",
       y = "Total Population (billions)") +
  #theme_minimal() +
  theme(legend.position = "right") +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "black")+
  theme(legend.position = c(.02, .98), legend.justification = c(0, 1),
        legend.background = element_blank(),  # Remove o fundo branco da legenda
        legend.title = element_blank())  # Remove o título da legenda

#==============================================================================#