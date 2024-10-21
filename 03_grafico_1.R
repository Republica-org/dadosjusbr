# Script para o gráfico 1

# Criar a variável teto constitucional sem a correção pela inflação
contracheque_filtrado <- contracheque_filtrado %>%
  mutate(
    # Definir o teto constitucional com base no ano e mês (sem correção)
    teto_constitucional = case_when(
      ano <= 2018 & mes <= 10 ~  33763.00,
      ano < 2023  ~  39293.32,
      ano <= 2023 & mes <= 3 ~ 39293.32,
      ano == 2023 & mes > 3 ~ 41650.92,
      ano == 2024 & mes <= 1 ~ 41650.92,
      ano == 2024 & mes > 1 ~ 44008.52
    )
  )


table(contracheque_filtrado$ano,contracheque_filtrado$teto_constitucional)




# Criar a variável dummy comparando a remuneração com o teto constitucional (sem correção)
contracheque_filtrado <- contracheque_filtrado %>%
  mutate(acima_do_teto = ifelse(remuneracao > teto_constitucional, 1, 0))

table(contracheque_filtrado$acima_do_teto)


# Agrupar e calcular porcentagens por ano
servidores_acima_do_teto <- contracheque_filtrado %>%
  group_by(nome,orgao,,ano) %>%
  summarise(teto=sum(acima_do_teto, na.rm=TRUE))

servidores_acima_do_teto$flag_teto = ifelse(servidores_acima_do_teto$teto>0,1,0)

prop.table(table(servidores_acima_do_teto$flag_teto,servidores_acima_do_teto$ano),2)


# Calcular a porcentagem de servidores por ano
porcentagem_servidores <- servidores_acima_do_teto %>%
  group_by(ano) %>%
  summarise(
    total_servidores = n(), 
    ultrapassaram_teto = sum(flag_teto == 1, na.rm = TRUE),
    porcentagem = (ultrapassaram_teto / total_servidores) * 100
  ) %>%
  ungroup()

# Gráfico 1: Exibir remuneração sem correção pela inflação
ggplot(contracheque_filtrado, aes(x = as.factor(ano), y = remuneracao)) +
  geom_boxplot(fill = "blue", color = "black", outlier.color = "black", outlier.size = 1, alpha = .6, width = .3) +
  coord_flip(ylim = c(10000, 80000)) +
  geom_text(data = porcentagem_servidores, aes(x = as.factor(ano), y = 10000, 
                                               label = paste0(round(porcentagem, 2), "%")),
            vjust = -0.5, hjust = -0.2, size = 5, color = "black") +  # Ajustar o alinhamento horizontal e tamanho das labels
  xlab("") +
  ylab(TeX("Remuneração Mensal (Sem Correção pela Inflação) $\\rightarrow$")) + 
  scale_y_continuous(
    breaks = c(10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000), 
    labels = c('R$ 10 Mil', 'R$ 20 Mil', 'R$ 30 Mil', 'R$ 40 Mil', 'R$ 50 Mil', 'R$ 60 Mil', 'R$ 70 Mil', 'R$ 80 Mil')
  ) +
  labs(
    title = "Porcentagem de Servidores com Remuneração Acima do Teto (2018-2024)",
    subtitle = "Sem correção pela inflação",
    caption = "Fonte: Dados JusBR, base de contracheque disponível no site."
  ) +
  theme_economist() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 14),  # Aumentar o tamanho das labels dos eixos
    axis.title.x = element_text(size = 16),  # Aumentar o tamanho do título do eixo X
    plot.title = element_text(size = 18, face = "bold"),  # Aumentar o tamanho do título
    plot.subtitle = element_text(size = 16),  # Aumentar o tamanho do subtítulo
    plot.caption = element_text(size = 14),  # Aumentar o tamanho do texto de rodapé
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom"
  )


# Remuneração corrigida pela inflação -------------------------------------

# Criar um dataframe com os índices IPCA acumulados em relação a 2024
indice_ipca <- data.frame(
  ano = c(2018, 2019, 2020, 2021, 2022, 2023, 2024),
  ipca_acumulado = c(1.37, 1.32, 1.2704, 1.215, 1.1008, 1.0407, 1.00)
)



# Juntar os índices IPCA com o dataframe de contracheques e calcular o teto constitucional atualizado
contracheque_atualizado <- contracheque_filtrado %>%
  left_join(indice_ipca, by = "ano") %>%
  mutate(
    # Atualizar os valores de remuneração e teto constitucional para o ano base 2024
    remuneracao_atualizada = remuneracao * ipca_acumulado,
    teto_constitucional_atualizado = teto_constitucional * ipca_acumulado
  )

# Gráfico 2: Exibir remunerações corrigidas pela inflação
ggplot(contracheque_atualizado, aes(x = as.factor(ano), y = remuneracao_atualizada)) +
  geom_boxplot(fill = "blue", color = "black", outlier.color = "black", outlier.size = 1, alpha = .6, width = .3) +
  coord_flip(ylim = c(10000, 80000)) +
  # Adicionar porcentagens de servidores acima do teto (sem correção pela inflação)
  geom_text(data = porcentagem_servidores, aes(x = as.factor(ano), y = 10000, label = paste0(round(porcentagem, 2), "%")),
            vjust = -0.5, size = 6, color = "black") +  # Aumentar o tamanho das labels de porcentagem
  xlab("") +
  ylab(TeX("Remuneração Mensal Atualizada (Base 2024) $\\rightarrow$")) + 
  scale_y_continuous(
    breaks = c(10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000), 
    labels = c('R$ 10 Mil', 'R$ 20 Mil', 'R$ 30 Mil', 'R$ 40 Mil', 'R$ 50 Mil', 'R$ 60 Mil', 'R$ 70 Mil', 'R$ 80 Mil')
  ) +
  labs(
    title = "Distribuição das Remunerações Mensais Atualizadas (2018-2024)",
    subtitle = "Valores corrigidos para o Ano Base 2024",
    caption = "Fonte: Dados JusBR, base de contracheque disponível no site."
  ) +
  theme_economist() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 14),  # Aumentar o tamanho das labels dos eixos
    axis.title.x = element_text(size = 16),  # Aumentar o tamanho do título do eixo X
    plot.title = element_text(size = 18, face = "bold"),  # Aumentar o tamanho do título
    plot.subtitle = element_text(size = 16),  # Aumentar o tamanho do subtítulo
    plot.caption = element_text(size = 14),  # Aumentar o tamanho do texto de rodapé
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom"
  )
