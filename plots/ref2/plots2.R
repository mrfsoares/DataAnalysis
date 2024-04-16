# pacotes necessários
require(readxl)
require(tidyverse)
require(ggplot2)
require(plotly) 
require(ggthemes)
require(hrbrthemes)
require(zoo)

getwd()

# Ler excel com dados
df = read_excel('country.xlsx')

# Dataframe modeficado
df2 <- df %>% 
  filter(CTYNAME %in% c('Mexico', 'China', 'Canada')) %>% # seleciona apenas países de interesse
  select(!starts_with('E')) %>% # seleciona dados que não são de exportação
  select(-c(CTY_CODE, IYR)) %>% # remove outras duas colunas sem utilidade
  pivot_longer(!c(year, CTYNAME), names_to = 'MONTH', values_to='IMPORT VALUE') # pivota dataframe

# Converte coluna de mês para o tipo numérico para criar, posteriormente, coluna de dada
df2$MONTH[df2$MONTH=='IJAN'] = 01
df2$MONTH[df2$MONTH=='IFEB'] = 02
df2$MONTH[df2$MONTH=='IMAR'] = 03
df2$MONTH[df2$MONTH=='IAPR'] = 04
df2$MONTH[df2$MONTH=='IMAY'] = 05
df2$MONTH[df2$MONTH=='IJUN'] = 06
df2$MONTH[df2$MONTH=='IJUL'] = 07
df2$MONTH[df2$MONTH=='IAUG'] = 08
df2$MONTH[df2$MONTH=='ISEP'] = 09
df2$MONTH[df2$MONTH=='IOCT'] = 10
df2$MONTH[df2$MONTH=='INOV'] = 11
df2$MONTH[df2$MONTH=='IDEC'] = 12

# criando coluna de data (format: YYYY-MM-DD)
df2$DATE <- paste0(df2$year, '-', df2$MONTH, '-', 01)
df2$DATE = as.Date(df2$DATE) # converte coluna para o tipo data

# seleciona só colunas de interesse e remove dados sem divulgação
df2 = df2 %>% 
  select(-c(year, MONTH)) %>% 
  filter(DATE<'2023-10-01') 

# criando coluna com média móvel de 7 dias
df3 = df2 %>%
  arrange(CTYNAME, DATE) %>% 
  group_by(CTYNAME) %>% 
  mutate(MM = rollapply(`IMPORT VALUE`, width = 7, FUN = mean, align = "right", fill = NA)) %>% 
  ungroup()

# Plots
g = ggplot(df3, aes(x=DATE, y=`MM`, color = CTYNAME)) +
  geom_line(size=1) +
  labs(title = "Seven-month moving average of imports by country of interest",
       y = "",
       x = "") +
  theme_ipsum()  +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "9 months", 
               date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("#3498db", "#95a5a6", "#d62728"))

g

# Gráfico interativo
ggplotly(g)

