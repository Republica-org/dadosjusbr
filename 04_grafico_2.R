# Agrupar os dados por ano e rubrica, somando os valores corrigidos
rubricas_agregadas <- dadosjusbr_empilhado_filtrado %>%
  group_by(ano, rubrica_desambiguada) %>%
  summarise(custo_acumulado = sum(valor_corrigido, na.rm = TRUE)) %>%
  arrange(rubrica_desambiguada, ano) %>%
  group_by(rubrica_desambiguada) %>%
  mutate(
    crescimento_percentual = (custo_acumulado / lag(custo_acumulado) - 1) * 100  # Calcular o crescimento percentual
  ) %>%
  ungroup()

# Ajustar os valores para milhões de reais
rubricas_agregadas <- rubricas_agregadas %>%
  mutate(custo_acumulado = custo_acumulado / 1e6)  # Convertendo para milhões de reais

# Selecionar as rubricas da parte 2
rubricas_parte1 <- rubricas_agregadas %>%
  filter(rubrica_desambiguada %in% c("auxilio-alimentacao", "auxilio-saude", "ferias", "gratificacao-natalina"))

# Criar o gráfico de barras com crescimento percentual, ajustando ainda mais a linha de tendência
ggplot(rubricas_parte1, aes(x = ano)) +
  geom_bar(aes(y = custo_acumulado), stat = "identity", fill = "blue", alpha = 0.6, width = 0.4) +
  geom_line(aes(y = custo_acumulado * 0.7, group = rubrica_desambiguada), color = "black", size = 1) +  # Linha de evolução na metade do gráfico
  geom_point(aes(y = custo_acumulado * 0.7), color = "black", size = 2) +  # Pontos da linha de evolução
  geom_text(aes(y = custo_acumulado, label = ifelse(ano == 2018, "", paste0(round(crescimento_percentual, 1), "%"))), 
            vjust = -0.5, hjust = 0.5, size = 3, color = "black") +  # Reduzir o tamanho dos rótulos
  scale_y_continuous(
    name = "Custo Real Acumulado (R$ milhões)"
  ) +
  facet_wrap(~ rubrica_desambiguada, scales = "free_y", strip.position = "top") +  # Facetar por rubrica
  labs(
    title = "Custo Real Acumulado e Crescimento Percentual por Rubrica de Benefícios (2018-2024)",
    x = "Ano",
    caption = "Fonte: Dados JusBR. Valores em milhões de reais."
  ) +
  theme_economist() +  # Manter o tema consistente com gráficos anteriores
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 14),  # Tamanho das labels dos eixos
    axis.title.x = element_text(size = 16),  # Tamanho do título do eixo X
    plot.title = element_text(size = 18, face = "bold"),  # Tamanho do título
    plot.subtitle = element_text(size = 16),  # Tamanho do subtítulo
    plot.caption = element_text(size = 14),  # Tamanho do texto de rodapé
    plot.margin = margin(1, 1, 1, 1, "cm"),
    strip.text = element_text(size = 12),  # Tamanho das labels das facetas
    panel.spacing = unit(2, "lines"),  # Aumenta o espaçamento entre os gráficos para evitar sobreposição
    plot.background = element_rect(size = 1, color = "white")  # Ajustar a margem
  ) +
  coord_cartesian(clip = "off")  # Impedir o corte dos números grandes

# Selecionar as rubricas da parte 2
rubricas_parte2 <- rubricas_agregadas %>%
  filter(rubrica_desambiguada %in% c("indenizacao-de-ferias", "licenca-compensatoria", "licenca-premio", "outros"))

# Criar o gráfico de barras com crescimento percentual para as rubricas da parte 2
ggplot(rubricas_parte2, aes(x = ano)) +
  geom_bar(aes(y = custo_acumulado), stat = "identity", fill = "blue", alpha = 0.6, width = 0.4) +
  geom_line(aes(y = custo_acumulado * 0.7, group = rubrica_desambiguada), color = "black", size = 1) +  # Linha de evolução na metade do gráfico
  geom_point(aes(y = custo_acumulado * 0.7), color = "black", size = 2) +  # Pontos da linha de evolução
  geom_text(aes(y = custo_acumulado, label = ifelse(ano == 2018, "", paste0(round(crescimento_percentual, 1), "%"))), 
            vjust = -0.5, hjust = 0.5, size = 3, color = "black") +  # Reduzir o tamanho dos rótulos
  scale_y_continuous(
    name = "Custo Real Acumulado (R$ milhões)"
  ) +
  facet_wrap(~ rubrica_desambiguada, scales = "free_y", strip.position = "top") +  # Facetar por rubrica
  labs(
    title = "Custo Real Acumulado e Crescimento Percentual por Rubrica de Benefícios (2018-2024)",
    x = "Ano",
    caption = "Fonte: Dados JusBR. Valores em milhões de reais."
  ) +
  theme_economist() +  # Manter o tema consistente com gráficos anteriores
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 14),  # Tamanho das labels dos eixos
    axis.title.x = element_text(size = 16),  # Tamanho do título do eixo X
    plot.title = element_text(size = 18, face = "bold"),  # Tamanho do título
    plot.subtitle = element_text(size = 16),  # Tamanho do subtítulo
    plot.caption = element_text(size = 14),  # Tamanho do texto de rodapé
    plot.margin = margin(1, 1, 1, 1, "cm"),
    strip.text = element_text(size = 12),  # Tamanho das labels das facetas
    panel.spacing = unit(2, "lines"),  # Aumenta o espaçamento entre os gráficos para evitar sobreposição
    plot.background = element_rect(size = 1, color = "white")  # Ajustar a margem
  ) +
  coord_cartesian(clip = "off")  # Impedir o corte dos números grandes
