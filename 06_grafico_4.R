library(tidyverse)

# Calcular a mediana de cada rubrica por ano, excluindo valores iguais a zero
medianas_por_ano <- dadosjusbr_agrupados %>%
  group_by(ano) %>%
  summarise(
    mediana_alimentacao = median(auxilio_alimentacao[auxilio_alimentacao != 0], na.rm = TRUE),
    mediana_saude = median(auxilio_saude[auxilio_saude != 0], na.rm = TRUE),
    mediana_licenca_premio = median(licenca_premio[licenca_premio != 0], na.rm = TRUE),
    mediana_gratificacao_natalina = median(gratificacao_natalina[gratificacao_natalina != 0], na.rm = TRUE),
    mediana_ferias = median(ferias[ferias != 0], na.rm = TRUE),
  mediana_indenizacao_ferias = median(indenizacao_de_ferias[indenizacao_de_ferias != 0], na.rm = TRUE),
  mediana_licenca_compensatoria = median(licenca_compensatoria[licenca_compensatoria != 0], na.rm = TRUE),
  mediana_nao_identificado_despadronizado = median(nao_identificado_despadronizado[nao_identificado_despadronizado != 0], na.rm = TRUE)
    
  )


# Calcular a media de cada rubrica por ano, excluindo valores iguais a zero
medias_por_ano <- dadosjusbr_agrupados %>%
  group_by(ano) %>%
  summarise(
    media_alimentacao = mean(auxilio_alimentacao[auxilio_alimentacao != 0], na.rm = TRUE),
    media_saude = mean(auxilio_saude[auxilio_saude != 0], na.rm = TRUE),
    media_licenca_premio = mean(licenca_premio[licenca_premio != 0], na.rm = TRUE),
    media_gratificacao_natalina = mean(gratificacao_natalina[gratificacao_natalina != 0], na.rm = TRUE),
    media_ferias = mean(ferias[ferias != 0], na.rm = TRUE),
   # media_nao_especificado = mean(nao_especificado[nao_especificado != 0], na.rm = TRUE),
   media_indenizacao_ferias = mean(indenizacao_de_ferias[indenizacao_de_ferias != 0], na.rm = TRUE),
   media_licenca_compensatoria = mean(licenca_compensatoria[licenca_compensatoria != 0], na.rm = TRUE),
   media_nao_identificado_despadronizado = mean(nao_identificado_despadronizado[nao_identificado_despadronizado != 0], na.rm = TRUE)
  )

## Coluna de tipo de operação

medianas_por_ano$tipo = "mediana"
medias_por_ano$tipo = "medias"


# Supondo que 'medias_por_ano' já foi criado, agora vamos pivotar os dados para facilitar a criação dos gráficos
medianas_por_ano_long <- medianas_por_ano %>%
  pivot_longer(cols = starts_with("mediana_"), 
               names_to = "rubrica", 
               values_to = "valor")

medias_por_ano_long <- medias_por_ano %>%
  pivot_longer(cols = starts_with("media_"), 
               names_to = "rubrica", 
               values_to = "valor")

por_ano_long = rbind(medianas_por_ano_long,medias_por_ano_long)

### Correção ipca e 0 no NA
por_ano_long$valor = ifelse(is.na(por_ano_long$valor),0,por_ano_long$valor)

por_ano_long <- por_ano_long %>%
  left_join(indice_ipca, by = "ano") %>%
  mutate(
    valor_corrigido = valor * ipca_acumulado #,
    # teto_constitucional_corrigido = teto_constitucional * fator_correcao
  )




## Tratando a variável de rubrica

por_ano_long$rubrica2 <-ifelse(startsWith(por_ano_long$rubrica,"mediana_"),gsub("mediana_","", por_ano_long$rubrica),por_ano_long$rubrica)

por_ano_long$rubrica2 <-ifelse(startsWith(por_ano_long$rubrica,"media_"),gsub("media_","", por_ano_long$rubrica),por_ano_long$rubrica2)



# Filtrar as rubricas para a primeira parte
rubricas_parte1 <-por_ano_long %>%
  filter(rubrica2 %in% c("alimentacao","saude", "ferias", "gratificacao_natalina"))

# Criar o gráfico para a primeira parte
ggplot(rubricas_parte1, aes(x = ano, y = valor_corrigido, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, width = 0.7) +  # Barras para as medianas
  geom_text(aes(label = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")(valor_corrigido)),  # Rótulos como moeda
            position = position_dodge(width = 0.7), 
            vjust = -1.5, size = 2.5)+ # Rótulos com os valores
  facet_wrap(~rubrica2, scales = "free_y", ncol = 1,strip.position = "bottom",labeller = labeller(tipo = c("alimentacao"  = "Auxílio alimentação" ,
                                                                                                           "ferias"  = "Férias",
                                                                                                           "gratificacao_natalina"      =           "Gratificação natalina")))+ 
  #facet_wrap(~rubrica2)+ 
  # Dividido em uma coluna por rubrica
  labs(title = "Mediana e média por rubrica de benefícios recebidos por cada servidor (Parte 1)",
       x = "Ano",
       y = "Valores médios e medianos dos benefícios",
       fill = "Rubrica") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),  
    axis.title.x = element_text(size = 12),  
    plot.title = element_text(size = 14, face = "bold"),  
    plot.margin = margin(3, 1, 1, 1, "cm"),
    strip.text = element_text(size = 12, face = "bold"),  # Ajustar o tamanho do texto das facetas
    panel.spacing = unit(2, "lines"),  # Espaçamento maior entre as facetas
    strip.placement = "outside",  # Colocar o título da faceta fora do gráfico
    #strip.text = element_blank(),  # Remover os títulos das facetas
    #panel.spacing = unit(1, "lines"),
    plot.background = element_rect(size = 1, color = "white")
  ) +
  coord_cartesian(clip = "off")+
  
  scale_fill_manual(values = c("mediana" = "#B2D33E", "medias" = "#6633FF"),
                    labels = c("Mediana", "Média")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ","))

### Parte 2



rubricas_parte2 <-por_ano_long %>%
  filter(rubrica2 %in% c("licenca_compensatoria", "licenca_premio","indenizacao_ferias","nao_identificado_despadronizado"))

# Criar o gráfico para a primeira parte
ggplot(rubricas_parte2, aes(x = ano, y = valor_corrigido, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, width = 0.7) +  # Barras para as medianas
  geom_text(aes(label = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")(valor_corrigido)),  # Rótulos como moeda
            position = position_dodge(width = 0.7), 
            vjust = -1.5, size = 2.5)+ # Rótulos com os valores
  facet_wrap(~rubrica2, scales = "free_y", ncol = 1,strip.position = "bottom")+ 
  #facet_wrap(~rubrica2)+ 
  # Dividido em uma coluna por rubrica
  labs(title = "Mediana e média por rubrica de benefícios recebidos por cada servidor (Parte 1)",
       x = "Ano",
       y = "Valores médios e medianos dos benefícios",
       fill = "Rubrica") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),  
    axis.title.x = element_text(size = 12),  
    plot.title = element_text(size = 14, face = "bold"),  
    plot.margin = margin(3, 1, 1, 1, "cm"),
    strip.text = element_text(size = 12, face = "bold"),  # Ajustar o tamanho do texto das facetas
    panel.spacing = unit(2, "lines"),  # Espaçamento maior entre as facetas
    strip.placement = "outside",  # Colocar o título da faceta fora do gráfico
    #strip.text = element_blank(),  # Remover os títulos das facetas
    #panel.spacing = unit(1, "lines"),
    plot.background = element_rect(size = 1, color = "white")
  ) +
  coord_cartesian(clip = "off")+
  
  scale_fill_manual(values = c("mediana" = "#B2D33E", "medias" = "#6633FF"),
                    labels = c("Mediana", "Média")) +
  scale_y_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ","))







