getwd() 

require(readxl)
require(tidyverse)

# Abrindo df
df = read_excel('comex_export.xlsx', sheet = 'Resultado')
df

sapply(df, class)


# Alguns gráficos --------------------------------------------------------------

require(ggplot2)

# Plot normal - exportações de UF's por mês
ggplot(df, aes(y=`UF do Produto`, x=`Valor FOB (US$)`, fill=Mês)) + 
  geom_bar(stat = "identity") # fornece o valores de y

# Ordenando barras pelo valor exportado (menor para maior)
ggplot(df, aes(y=reorder(`UF do Produto`, -`Valor FOB (US$)`), x=`Valor FOB (US$)`, fill=Mês)) + 
  geom_bar(stat = "identity")

# Ordenando barras pelo valor exportado (maior para menor)
ggplot(df, aes(y=reorder(`UF do Produto`, -desc(`Valor FOB (US$)`)), x=`Valor FOB (US$)`, fill=Mês)) + 
  geom_bar(stat = "identity")


# usando alguns temas
require(viridis) # paleta
require(hrbrthemes) # pacote de temas
require(ggthemes)

# Gráfico do maior para o menor com temas novos
ggplot(df, aes(y=reorder(`UF do Produto`, -desc(`Valor FOB (US$)`)), x=`Valor FOB (US$)`, fill=Mês)) + 
  geom_bar(stat = "identity") + 
  scale_fill_viridis(discrete = T) + # paleta
  ggtitle("Exportações por unidade da federação em 2022") +
  theme_ipsum() + # tema
  xlab("") +
  ylab("")


# Fazendo ajuste em eixo de dados e labels
# Passando valor para milhões
ggplot(df %>% mutate(`Valor FOB (US$)`=`Valor FOB (US$)`/1000000), 
       aes(y=reorder(`UF do Produto`, -desc(`Valor FOB (US$)`)), x=`Valor FOB (US$)`, fill=Mês)) + 
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = T) +
  labs(title="Exportações por unidade da federação em 2022", 
       subtitle = 'US$ milhões',
       y='',
       x='') +
  theme_ipsum() 


# Trabalhando com dados mais próximos ao exemplo desejado

uf <- df %>% 
  filter(`UF do Produto` %in% c('São Paulo', 
                                'Rio de Janeiro', 
                                'Minas Gerais',
                                'Mato Grosso',
                                'Rio Grande do Sul')) %>% 
  bind_rows(df %>%  filter(!`UF do Produto` %in% c('São Paulo', 'Rio de Janeiro', 
                                                   'Minas Gerais', 'Mato Grosso',
                                                   'Rio Grande do Sul')) %>%  # seleciona UF's que não são as da lista
              select(-Ano) %>% 
              mutate(`UF do Produto` = 'Outros') %>% 
              group_by(Mês, `UF do Produto`) %>% 
              summarise(`Valor FOB (US$)` = sum(`Valor FOB (US$)`))) %>%  # agrega as outras UF's em "outros"
  mutate(`Valor FOB (US$ milhões)` = `Valor FOB (US$)`/1000000) %>% # passa para milhares
  select(-c(Ano,`Valor FOB (US$)`)) %>% 
  arrange(Mês)


# Plotando gráficos
ggplot(uf, aes(x=reorder(`Mês`, -desc(`Valor FOB (US$ milhões)`)), 
               y=`Valor FOB (US$ milhões)`, 
               fill=`UF do Produto`)) + 
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = T) +
  labs(title="Exportações por unidade da federação em 2022", 
       subtitle = 'US$ milhões',
       y='',
       x='') +
  theme_ipsum() 


# personalizando legenda
# contorno caixinha
ggplot(uf, aes(x = `Mês`, 
               y = `Valor FOB (US$ milhões)`, 
               fill = `UF do Produto`)) + 
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Exportações por unidade da federação em 2022", 
       subtitle = "US$ milhões",
       y = "",
       x = "") +
  theme_ipsum()  +
  theme(legend.background = element_rect(color = "yellow", size = 1))


# remove título de legendas
ggplot(uf, aes(x = `Mês`, 
               y = `Valor FOB (US$ milhões)`, 
               fill = `UF do Produto`)) + 
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Exportações por unidade da federação em 2022", 
       subtitle = "US$ milhões",
       y = "",
       x = "") +
  theme_ipsum()  +
  theme(legend.background = element_rect(color = "yellow", size = 1)) +
  theme(legend.title = element_blank()) 


# Valor total no topo da barra
ggplot(uf, aes(x = `Mês`, y = `Valor FOB (US$ milhões)`, fill = `UF do Produto`)) + 
  geom_bar(stat = "identity") + 
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Exportações por unidade da federação em 2022", 
       subtitle = "US$ milhões",
       y = "",
       x = "") +
  theme_ipsum()  +
  theme(legend.background = element_rect(color = "yellow", size = 1)) +
  theme(legend.title = element_blank()) +
  geom_text(
    aes(label = after_stat(round(y,1)), group = `Mês`), 
    stat = 'summary', fun = sum, vjust = -1
  )


# Usando outro tema
ggplot(uf, aes(x = Mês, y = `Valor FOB (US$ milhões)`, fill = `UF do Produto`)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(title = "Exportações por unidade da federação em 2022", 
       subtitle = "US$ milhões",
       y = "",
       x = "") +
  theme_economist() + # usando outro tema
  scale_fill_economist() + # paleta de cores
  theme(legend.background = element_rect(color = "yellow", size = 1)) +
  theme(legend.title = element_blank()) +
  geom_text(
    aes(label = after_stat(round(y,1)), group = `Mês`), 
    stat = 'summary', fun = sum, vjust = -1
  )


# gráficos interativos
library(plotly)

# Grafico de linhas
ggplotly(ggplot(data = uf, aes(x = Mês, y = `Valor FOB (US$ milhões)`, color = `UF do Produto`, group = `UF do Produto`)) +
           geom_point(size=1.5) +
           geom_line(size=1.3) +
           labs(title = "Exportações por unidade da federação em 2022", 
                subtitle = "US$ milhões",
                y = "Milhões",
                x = "") +
           theme_economist(), 
         dynamicTicks = TRUE)

# Gráfico de barras
ggplotly(ggplot(uf, aes(x = `Mês`, 
                        y = `Valor FOB (US$ milhões)`, 
                        fill = `UF do Produto`)) + 
           geom_bar(stat = "identity") + 
           scale_fill_viridis(discrete = TRUE) +
           labs(title = "Exportações por unidade da federação em 2022", 
                subtitle = "US$ milhões",
                y = "",
                x = "") +
           theme_ipsum()  +
           theme(legend.background = element_rect(color = "yellow", size = 1)) +
           theme(legend.title = element_blank())
)



# Gráfico de área
# Fonte: https://r-graph-gallery.com/136-stacked-area-chart.html
# Fonte 2: https://t-redactyl.io/blog/2015/12/creating-plots-in-r-using-ggplot2-part-2-area-plots.html

ggplot(uf, aes(x = Mês, y = `Valor FOB (US$ milhões)`, fill = `UF do Produto`, group = `UF do Produto`)) +
  geom_area(position = "stack") + 
  labs(title = "Exportações por unidade da federação em 2022", 
       subtitle = "US$ milhões",
       y = "Milhões",
       x = "") +
  theme_economist() + # usando outro tema
  scale_fill_economist() + # paleta de cores
  theme(legend.title = element_blank())

# Interativo
ggplotly(ggplot(uf, aes(x = Mês, y = `Valor FOB (US$ milhões)`, fill = `UF do Produto`, group = `UF do Produto`)) +
           geom_area(position = "stack") + 
           labs(title = "Exportações por unidade da federação em 2022", 
                subtitle = "US$ milhões",
                y = "Milhões",
                x = "") +
           theme_economist() + # usando outro tema
           scale_fill_economist() + # paleta de cores
           theme(legend.title = element_blank())
)

