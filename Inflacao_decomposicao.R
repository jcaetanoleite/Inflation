#pacotes necessários
library(tidyverse)
library(sidrar)
library(xts)
library(reshape2)
library(RColorBrewer)

# Chamando os dados de inflação
ipca = get_sidra(api = '/t/1737/n1/all/v/2266/p/all/d/v2266%2013') %>%
  mutate(date = parse_date(`Mês (Código)`, format = "%Y%m")) %>%
  mutate(inflacao_mensal = (Valor/lag(Valor,1)-1)*100,
         inflacao_anual = (Valor/lag(Valor,12)-1)*100) %>%
  dplyr::rename(indice = Valor) %>%
  select(date, indice, inflacao_mensal, inflacao_anual) %>%
  as_tibble()
tail(ipca)

#Tabela inicial
ipca %>%
  tail() %>%
  kable(digits = 2)

#Gráficos de 2020 para cá
ipca %>% 
  gather(variavel, valor, -date) %>%
  filter(date > '2020-01-01') %>%
  ggplot(aes(x=date, y=valor, colour = variavel))+
  geom_line(size=.8)+
  geom_point()+
  ggthemes::theme_tufte()+
  facet_wrap(~variavel, scales='free')+
  theme(legend.position = 'none')

#Decomposição do índice
variacao =
  '/t/7060/n1/all/v/63/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202' %>%
  get_sidra(api=.) %>%
  mutate(date = parse_date(`Mês (Código)`, format='%Y%m')) %>%
  select(date, "Geral, grupo, subgrupo, item e subitem", Valor) %>%
  pivot_wider(names_from = "Geral, grupo, subgrupo, item e subitem",
              values_from = Valor)

variacao

peso =
  '/t/7060/n1/all/v/66/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v66%204' %>%
  get_sidra(api=.) %>%
  mutate(date = parse_date(`Mês (Código)`, format='%Y%m')) %>%
  select(date, "Geral, grupo, subgrupo, item e subitem", Valor) %>%
  pivot_wider(names_from = "Geral, grupo, subgrupo, item e subitem",
              values_from = Valor)
peso

contribuicao = (variacao[,-1]*peso[,-1]/100) %>%
  mutate(date = variacao$date) %>%
  select(date, everything())

contribuicao

View(contribuicao)


contribuicao_long = contribuicao %>%
  gather(variavel, valor, -date)

inflacao = round(rowSums(contribuicao[,-1]),2)
inflacao = xts(inflacao, order.by = contribuicao$date)
inflacao = data.frame(time = index(contribuicao_long$date),
                      melt(as.data.frame(inflacao)))




# Generate a vector of colors using the "Set3" palette from RColorBrewer
n_colors <- 9  # Number of unique variables
colours <- brewer.pal(n_colors, "Set3")

# Use the colours vector in your code
contribuicao_long %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = valor, fill = variavel, color = variavel)) +
  geom_line(data = inflacao, aes(x = contribuicao_long$date, y = value), size = 0.9, color = 'black') +
  geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed') +
  ggthemes::theme_tufte()
  scale_color_manual(values = colours) +
  scale_fill_manual(values = colours) +
  theme(legend.title = element_blank(), legend.position = 'right', legend.key.size = unit(0.1, "cm")) +
  labs(x = '', y = '', title = "Contribuição dos Grupos do IPCA para a Inflação Mensal")

