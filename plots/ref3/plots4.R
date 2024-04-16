rm(list = ls()) 

getwd()

# Perspnaliza tema
ref_theme <- theme(
  text = element_text(family = "Arial", size = 12, color = "black"),
  axis.text = element_text(size = 9, color = "black"),
  axis.title = element_text(size = 12, face = "bold"),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "lightgray", size = 0.5),
  panel.grid.minor = element_blank(),
  legend.position = "bottom",
  legend.text = element_text(size = 8),
  legend.title = element_text(size = 9, face = "bold"),
  plot.subtitle = element_text(family = "Arial",
                               size = 11, 
                               color = "grey"#, 
                               #face = "italic"
                               )
)


require(readxl) # ou library()
require(tidyverse)
library(ggplot2)

# Abrindo df -------------------------------------------------------------------
df = read_excel('data.xlsx', range='A4:AE23') %>% 
  drop_na()


df2 <- df %>% 
  filter(!`Country Name` %in% c('World', 'Latin America & Caribbean')) %>% 
  pivot_longer(!`Country Name`, names_to = 'Year') %>% 
  select(2,1,3) %>% 
  rename(`Forest Area` = value) %>% 
  mutate(`Forest Area` = `Forest Area`*0.000001,
         `Country Name` = ifelse(`Country Name`=='Africa Eastern and Southern',
         'Africa \nEastern and \nSouthern', 
         ifelse(`Country Name`=='Africa Western and Central',
                'Africa \nWestern and \nCentral', 
                ifelse(`Country Name`=='Latin America & Caribbean (Excluding Brazil)',
                       'Latin \nAmerica & \nCaribbean \n(Excluding Brazil)',`Country Name`)))) 
df2


# Plots ========================================================================
# Criando paleta personalizada
paleta <- colorRampPalette(c("#006400","#DAA520", "#90EE90", "#b2b578"))


# Texto c/ comentário
nota = expression(bold('An combined area of 2,572,911 sq.km, (Almost the size or Argentina) of forest was lost in \nthose countries from 1992 to 2021. A 1/3 of it in Brazil alone.'))

p1 = ggplot(df2, aes(fill=Year, y=`Forest Area`, x=`Country Name`)) +
         geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Forest Area (1992-2021)", 
       subtitle = "millions sq. km",
       y = "",
       x = "") + 
  ref_theme +
  scale_fill_manual(values = paleta(30)) +
  guides(fill = guide_legend(nrow = 2)) + # Linhas da legenda
  scale_x_discrete(limits = unique(df2$`Country Name`)) 

p1


# coordenadas da caixa para texto
xmin <- 7
xmax <- 13.3
ymin <- 7
ymax <- 8

# Adicionando comentário ao gráfico
p1_1 = p1 +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "#7FA653", # cor da caixa
            color = "#7FA653", # cor da borda
            alpha = 0.5) + # Transparência
  annotate("text", x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, 
           label = nota, color = "white", size = 3.0, vjust = 1) + 
  theme(legend.title = element_blank())

p1_1

#ggsave("area_2.png", p1_1, width = 15, height = 7, dpi = 300)


# Gráfico da variação do ano 1 para ano 0

df3 <- df %>% 
  filter(!`Country Name` %in% c('World', 'Latin America & Caribbean')) %>% 
  select(`Country Name`, `1992`, `2021`) %>% 
  mutate(variation = (`2021`/`1992`-1)*100) %>% 
  pivot_longer(!c(`Country Name`, variation), names_to = 'Year', values_to = 'Forest Area') %>% 
  mutate(`Forest Area` = `Forest Area`*0.000001,
         #`Country Name` = ifelse(`Country Name`=='Africa Eastern and Southern',
         #                        'Africa \nEastern and \nSouthern', 
         #                        ifelse(`Country Name`=='Africa Western and Central',
         #                               'Africa \nWestern and \nCentral', 
         #                               ifelse(`Country Name`=='Latin America & Caribbean (Excluding Brazil)',
         #                                      'Latin \nAmerica & \nCaribbean \n(Excluding Brazil)',`Country Name`)))
         ) %>% 
  mutate(variation = ifelse(Year==1992, NA, variation))
df3


# Fixando área
p2 = ggplot(df3, aes(x=`Forest Area`, y=`Country Name`, color = Year)) +
  geom_line(size = 2, color = '#e4e4e5') +
  geom_point(size = 5) +
  labs(title = "Evolution of the Forest Area in 2021 compared to 1992", 
       subtitle = "millions sq. km",
       y = "",
       x = "") + 
  ref_theme +
  geom_text(data = subset(df3, Year=='2021'),
            aes(label = paste0(sprintf("%.1f", variation), '%'), 
                               y = as.factor(`Country Name`)), 
            vjust = -1, hjust = 0.5, color = "#008080", size = 3) + 
  theme(legend.title = element_blank())

p2

#ggsave("eixo_area.png", p2, width = 15, height = 7, dpi = 300)


# Fixando porcentagem

p3 = ggplot(df3 %>% mutate(variation=ifelse(Year==1992,0,variation)), 
       aes(x=variation, y=`Country Name`, group = `Country Name`, color = Year)) +
  geom_line(size = 2, color = '#e4e4e5') +
  geom_point(size = 5) +
  labs(title = "Evolution of the Forest Area in 2021 compared to 1992", 
       subtitle = "%",
       y = "",
       x = "") + 
  ref_theme +
  #geom_text(aes(label = paste0(sprintf("%.1f", `Forest Area`), 'M. sq. km'), 
  #              y = as.factor(`Country Name`)), 
  #          vjust = -1, hjust = 0.5, color = "#008080", size = 2) + 
  theme(legend.title = element_blank()) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text(data = subset(df3, Year=='2021'),
            aes(label = paste0(sprintf("%.1f", variation), '%'), 
                y = as.factor(`Country Name`)), 
            vjust = -1, hjust = 0.5, color = "#008080", size = 3) 

p3

#ggsave("eixo_porc.png", p3, width = 15, height = 7, dpi = 300)


# Treepmap
library(plotly)

plot_ly(
  data = df3 %>% filter(Year==2021),
  labels = ~`Country Name`,
  parents = ~"",
  values = ~`Forest Area`,
  type = "treemap",
  text = ~paste0(round(`Forest Area`,1),' M sq. km'),
  marker = list(
    colorscale = "YlGnBu",
    colorbar = list(title = "Values") 
  )
) %>%
  layout(title = "Forest Area in 2021")
