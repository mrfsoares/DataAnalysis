
require(readxl)
require(tidyverse)
require(ggplot2) 
require(plotly)
library(zoo) 
require(ggthemes) 

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



graf = df3 %>% 
  #filter(DATE>='2019-01-01') %>% 
  select(-MM) %>% 
  select(DATE, everything())


# Default: position_stack(); total agrupado ------------------------------------
ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
  geom_area() +
  theme_minimal()


# position_dodge(); respeita o total de cada, sem agrupar ----------------------
ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
  geom_area(position = position_dodge(), alpha=0.4) +
  theme_minimal()


# position_fill; participação percentual ---------------------------------------
ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
  geom_area(position = position_fill()) +
  theme_minimal()


ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
  geom_area(position = position_fill()) +
  theme_minimal() +
  scale_y_continuous(labels=scales::percent)



# Trabalhando design e temas ---------------------------------------------------

#====> Posição de legendas: “left”,“top”, “right”, “bottom”, “bottom left”, etc...

# The Economist
ge = ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
  geom_area(position=position_dodge(), alpha=0.4) +
  geom_line() +
  theme_pander() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "7 months", 
               date_labels = "%b %Y", 
               expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Value of imports by country of interest",
       y = "",
       x = "",
       subtitle = 'US$ billions') 

ge


# The Wall Street Journal
gs = ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
  geom_area(position=position_dodge(), alpha=0.4) +
  geom_line() +
  theme_wsj() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "7 months", 
               date_labels = "%b %Y", 
               expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Value of imports by country of interest",
       y = "",
       x = "",
       subtitle = 'US$ billions')

gs


gs = ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, color=`CTYNAME`)) +
  #geom_area(position=position_dodge(), alpha=0.4) +
  geom_line(size=1) +
  theme_wsj() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "7 months", 
               date_labels = "%b %Y", 
               expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Value of imports by country of interest",
       y = "",
       x = "",
       subtitle = 'US$ billions')

gs



# Minimalista
gm = ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
  geom_area(position=position_dodge(), alpha=0.4) +
  geom_line() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "7 months", 
               date_labels = "%b %Y", 
               expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Value of imports by country of interest",
       y = "",
       x = "",
       subtitle = 'US$ billions') +
  theme(legend.position="top")

gm

ggplotly(ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
           geom_area(position=position_dodge(), alpha=0.4) +
           geom_line() +
           theme_wsj() +
           #theme(legend.title = element_blank()) +
           scale_x_date(date_breaks = "7 months", 
                        date_labels = "%b %Y", 
                        expand=c(0,0)) +
           theme(axis.text.x = element_text(angle = 90)) +
           labs(title = "Value of imports by country of interest",
                y = "",
                x = "",
                subtitle = 'US$ billions'))



# Linhas diagonais, verticais e horizontais: https://ggplot2.tidyverse.org/reference/geom_abline.html

gline = ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
  geom_area(position=position_dodge(), alpha=0.4) +
  geom_line() +
  geom_hline(aes(yintercept=mean(`IMPORT VALUE`)), 
             colour="red", 
             linetype = "dashed", 
             size=1) +
  theme_economist() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "7 months", 
               date_labels = "%b %Y", 
               expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Value of imports by country of interest",
       y = "",
       x = "",
       subtitle = 'US$ billions',
       caption = 'Nome da fonte') +
  geom_text(aes(x = as.Date('1985-08-01'), y = 20000, label = "Mean"),
           color = "red", angle = 0, vjust = 1.2) 

gline


# porcentagem
# The Wall Street Journal
gp = ggplot(graf, aes(x=DATE, y=`IMPORT VALUE`, fill=`CTYNAME`)) +
  geom_area(position=position_fill(), alpha=0.4) +
  #geom_line() +
  theme_economist() +
  theme(legend.title = element_blank()) +
  scale_x_date(date_breaks = "7 months", 
               date_labels = "%b %Y", 
               expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Value of imports by country of interest",
       y = "",
       x = "",
       subtitle = '% of the total') +
  scale_y_continuous(labels=scales::percent)

gp


library(patchwork)


ge + gs + gline + gm
ge + gm + gline + gp


