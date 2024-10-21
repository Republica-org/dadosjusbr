# Limpar e empilhar as bases ----------------------------------------------

# Criar o dataframe com os índices IPCA acumulados em relação a 2024
indice_ipca <- data.frame(
  ano = c(2018, 2019, 2020, 2021, 2022, 2023, 2024),
  ipca_acumulado = c(1.37, 1.32, 1.2704, 1.215, 1.1008, 1.0407, 1.00)
)

# Calcular o fator de correção, que é o inverso do IPCA acumulado
#indice_ipca <- indice_ipca %>%
 # mutate(fator_correcao = 1 / ipca_acumulado)

# Juntar os índices IPCA com os dados de cada ano e aplicar o fator de correção

###### Teto remuneratório diferente por mês em 2018. até nov/18 era de R$ 33,7 mil




# 2018
dadosjusbr_2018 <- dadosjusbr_2018 %>%
  left_join(indice_ipca, by = "ano") %>%
  mutate(
    teto_constitucional = case_when(
      ano <= 2018 & mes <= 10 ~  33763.00,
      ano < 2023  ~  39293.32,
      ano <= 2023 & mes <= 3 ~ 39293.32,
      ano == 2023 & mes > 3 ~ 41650.92,
      ano == 2024 & mes <= 1 ~ 41650.92,
      ano == 2024 & mes > 1 ~ 44008.52
    ),
    valor_corrigido = valor * ipca_acumulado #,
   # teto_constitucional_corrigido = teto_constitucional * fator_correcao
  )

# 2019
dadosjusbr_2019 <- dadosjusbr_2019 %>%
  left_join(indice_ipca, by = "ano") %>%
  mutate(
    teto_constitucional = case_when(
      ano <= 2018 & mes <= 10 ~  33763.00,
      ano < 2023  ~  39293.32,
      ano <= 2023 & mes <= 3 ~ 39293.32,
      ano == 2023 & mes > 3 ~ 41650.92,
      ano == 2024 & mes <= 1 ~ 41650.92,
      ano == 2024 & mes > 1 ~ 44008.52
    ),
    valor_corrigido = valor * ipca_acumulado #,
    #teto_constitucional_corrigido = teto_constitucional * fator_correcao
  )

# 2020
dadosjusbr_2020 <- dadosjusbr_2020 %>%
  left_join(indice_ipca, by = "ano") %>%
  mutate(
    teto_constitucional = case_when(
      ano <= 2018 & mes <= 10 ~  33763.00,
      ano < 2023  ~  39293.32,
      ano <= 2023 & mes <= 3 ~ 39293.32,
      ano == 2023 & mes > 3 ~ 41650.92,
      ano == 2024 & mes <= 1 ~ 41650.92,
      ano == 2024 & mes > 1 ~ 44008.52
    ),
    valor_corrigido = valor * ipca_acumulado #,
    #teto_constitucional_corrigido = teto_constitucional * fator_correcao
  )

# 2021
dadosjusbr_2021 <- dadosjusbr_2021 %>%
  left_join(indice_ipca, by = "ano") %>%
  mutate(
    teto_constitucional = case_when(
      ano <= 2018 & mes <= 10 ~  33763.00,
      ano < 2023  ~  39293.32,
      ano <= 2023 & mes <= 3 ~ 39293.32,
      ano == 2023 & mes > 3 ~ 41650.92,
      ano == 2024 & mes <= 1 ~ 41650.92,
      ano == 2024 & mes > 1 ~ 44008.52
    ),
    valor_corrigido = valor * ipca_acumulado #,
    #teto_constitucional_corrigido = teto_constitucional * fator_correcao
  )

# 2022
dadosjusbr_2022 <- dadosjusbr_2022 %>%
  left_join(indice_ipca, by = "ano") %>%
  mutate(
    teto_constitucional = case_when(
      ano <= 2018 & mes <= 10 ~  33763.00,
      ano < 2023  ~  39293.32,
      ano <= 2023 & mes <= 3 ~ 39293.32,
      ano == 2023 & mes > 3 ~ 41650.92,
      ano == 2024 & mes <= 1 ~ 41650.92,
      ano == 2024 & mes > 1 ~ 44008.52
    ),
    valor_corrigido = valor * ipca_acumulado #,
   # teto_constitucional_corrigido = teto_constitucional * fator_correcao
  )

# 2023
dadosjusbr_2023 <- dadosjusbr_2023 %>%
  left_join(indice_ipca, by = "ano") %>%
  mutate(
    teto_constitucional = case_when(
      ano <= 2018 & mes <= 10 ~  33763.00,
      ano < 2023  ~  39293.32,
      ano <= 2023 & mes <= 3 ~ 39293.32,
      ano == 2023 & mes > 3 ~ 41650.92,
      ano == 2024 & mes <= 1 ~ 41650.92,
      ano == 2024 & mes > 1 ~ 44008.52
    ),
    valor_corrigido = valor * ipca_acumulado #,
   # teto_constitucional_corrigido = teto_constitucional * fator_correcao
  )

# 2024 (não precisa de correção, pois é o ano base)
dadosjusbr_2024 <- dadosjusbr_2024 %>%
  left_join(indice_ipca, by = "ano") %>%
  mutate(
    teto_constitucional = case_when(
      ano <= 2018 & mes <= 10 ~  33763.00,
      ano < 2023  ~  39293.32,
      ano <= 2023 & mes <= 3 ~ 39293.32,
      ano == 2023 & mes > 3 ~ 41650.92,
      ano == 2024 & mes <= 1 ~ 41650.92,
      ano == 2024 & mes > 1 ~ 44008.52
    ),
    valor_corrigido = valor #,
   # teto_constitucional_corrigido = teto_constitucional
  )

# Empilhar os dataframes de 2018 a 2024
dadosjusbr_empilhado <- rbind(dadosjusbr_2018, 
                              dadosjusbr_2019, 
                              dadosjusbr_2020, 
                              dadosjusbr_2021, 
                              dadosjusbr_2022, 
                              dadosjusbr_2023, 
                              dadosjusbr_2024)


### Filtrar os tribunais eleitorais

table(dadosjusbr_empilhado$orgao)

dadosjusbr_empilhado_sem_TRE = dadosjusbr_empilhado %>%  filter(!startsWith(orgao, "tre"))



# Especificar um pouco mais as rubricas desambiguadas para diminuir a quantidade de missings
# Definir as principais rubricas corretas que você mencionou
rubricas_principais <- c("auxilio-saude", "licenca-premio", "auxilio-alimentacao", 
                         "ferias 13", "ferias indenizada", "gratificacao natalina")

table(dadosjusbr_empilhado$rubrica_desambiguada)


# Preencher rubrica_desambiguada por aproximação de escrita usando amatch()
dadosjusbr_empilhado <- dadosjusbr_empilhado %>%
  mutate(
    rubrica_desambiguada = case_when(
      is.na(rubrica_desambiguada) ~ rubricas_principais[amatch(item_sanitizado, rubricas_principais, maxDist = 3)],  # Atribuir por similaridade
      TRUE ~ rubrica_desambiguada  # Manter os valores já existentes
    )
  )

tabyl(dadosjusbr_empilhado$rubrica_desambiguada)

## Criar variável de outros especifiado e não especificado.  




# Contando os valores únicos e ordenando pelo count em ordem decrescente

## para ver o que sempre são enviados e diferencias dos que são enviados por apenas um tribunal iremos ver quais


mp = dadosjusbr_empilhado %>% filter(startsWith(orgao,"mp"))

# Contar com filtro de valores que começam com "mp"
contagem <- dadosjusbr_empilhado %>%
  group_by(item_sanitizado) %>%  # Agrupar por item_sanitizado
  summarise(
    contagem_itens = n(),  # Contar quantas linhas (itens) existem para cada item_sanitizado
    categorias_unicas_mp = n_distinct(orgao[grepl("^mp", orgao, ignore.case = TRUE)]),  # Contar categorias únicas que começam com "mp",
    categorias_unicas = n_distinct(orgao) 
  ) %>%
  arrange(desc(contagem_itens)) 





summary(contagem$categorias_unicas)

quantile(contagem$categorias_unicas, probs = 0.99)

### quase 95% das rubricas são provenientes de apenas 1 órgão, ou seja, são enviadas daquela forma por apenas 1 órgão
# apenas 137 rubricas
137/3211 #4,3%

acima_1 = contagem %>%  filter(categorias_unicas>1)

### Verificando no CNJ a rubrica de cada um detalhado: 

rubricas_cnj <- data.frame(
  item_sanitizado = c(
"subsidio", #77 orgaos
"previdencia publica", # 88 orgaos
"imposto de renda", #107
"descontos diversos", # 86
"retencao por teto constitucional", # 77
"remuneracao do orgao de origem",# 19
"diarias", # 89
"abono de permanencia", # 79
"auxilioalimentacao", #71
"auxilio preescolar",#51
"auxilio saude", #58
"auxilio natalidade", #33
"auxilio moradia", #70  # se quiser padronizar tem um "auxiliomoradia"
"ajuda de custo", # 58
"abono constitucional de 13 de ferias",#68
"indenizacao de ferias", #65
"antecipacao de ferias",#43
"gratificacao natalina", #78
"antecipacao de gratificacao natalina",#61
"substituicao", #60
"gratificacao por exercicio cumulativo",#61
"gratificacao por encargo cursoconcurso", #55
"pagamentos retroativos", #75
"jeton" #31
),#24 categorias
categoria_cnj = c(rep("contracheque",7),rep("direitos_pessoais",1),rep("indenizacoes",5),rep("direitos_eventuais",11))
)


contagem2 = left_join(contagem,rubricas_cnj,"item_sanitizado")

## rubricas para o mp

connta_mp = contagem2 %>% filter(categorias_unicas_mp>0)

df= dadosjusbr_empilhado_sem_TRE %>% filter(item_sanitizado=="representacao magistrado presidente")

## O que achamos do MP: https://www.cnmp.mp.br/portaldatransparencia/contracheque/remuneracao-de-todos-os-servidores-ativos

rubricas_MP <- data.frame(
  item_sanitizado = c(
"remuneracao do cargo efetivo",
"outras verbas remuneratorias legais ou judiciais",
"funcao de confianca ou cargo em comissao",
"gratificacao natalinaferias",
"13 constitucional",
"abono de permanencia",
"indenizacoes",
"outras remuneracoes temporarias",
"contribuicao previdenciaria",
"imposto de renda",
"retencao por teto constitucional"
  ),#24 categorias
  categoria_mp = c(rep("remuneração",6),"indenizações","remuneração",rep("descontos",3))
)

contagem3 = left_join(contagem2,rubricas_MP,"item_sanitizado")
contagem3$flag = ifelse(!is.na(contagem3$categoria_cnj) | !is.na(contagem3$categoria_mp),1,0)

flag= contagem3 %>% dplyr::select("item_sanitizado","flag")

write.csv2(contagem3,"rubricas_especificadas.csv")


dadosjusbr_empilhado_sem_TRE1 = left_join(dadosjusbr_empilhado_sem_TRE,flag,"item_sanitizado")


## Criar coluna comm rubrica_desambiguada + especificado cnj + especificado mp

dadosjusbr_empilhado_sem_TRE2= dadosjusbr_empilhado_sem_TRE1 %>%
  mutate(rubrica_desambiguada = ifelse(!is.na(rubrica_desambiguada), rubrica_desambiguada, ifelse(
    is.na(rubrica_desambiguada) & flag==1, item_sanitizado,"não especificado ou despadronizado"
    
  )))
table(dadosjusbr_empilhado_sem_TRE2$rubrica_desambiguada)



# Criar um dataframe que conta a quantidade de meses por ano para cada órgão
orgaos_anos_meses <- dadosjusbr_empilhado_sem_TRE2 %>%
  group_by(orgao, ano) %>%
  summarise(meses_por_ano = n_distinct(mes),
            possui_novembro = any(mes == 11),
            possui_dezembro = any(mes == 12)) %>%
  ungroup()

# Filtrar órgãos que enviaram dados por pelo menos 9 meses por ano e possuem dados para novembro e dezembro
orgaos_validos_ano <- orgaos_anos_meses %>%
  filter(meses_por_ano >= 9 & possui_novembro & possui_dezembro) %>%
  group_by(orgao) %>%
  summarise(anos_validos = n_distinct(ano)) %>%
  ungroup()

# Filtrar apenas os órgãos com pelo menos 6 anos válidos
orgaos_validos_filtrados <- orgaos_validos_ano %>%
  filter(anos_validos >= 6) #69 órgãos cumprem os requisitos desse novo filtro

# Filtrar os dados do dataframe empilhado para incluir apenas os órgãos válidos
dadosjusbr_empilhado_filtrado <- dadosjusbr_empilhado_sem_TRE2 %>%
  filter(orgao %in% orgaos_validos_filtrados$orgao)

summary(dadosjusbr_empilhado_filtrado$valor)



# Substituir os valores negativos por seus valores absolutos no dataframe original
dadosjusbr_empilhado_filtrado <- dadosjusbr_empilhado_filtrado %>%
  mutate(
    valor = if_else(valor < 0, abs(valor), valor),
    valor_corrigido = if_else(valor_corrigido < 0, abs(valor_corrigido), valor_corrigido)
  )

# Filtrar valores acima de 1.000.000 no valor corrigido
dadosjusbr_empilhado_filtrado <- dadosjusbr_empilhado_filtrado %>%
  filter(valor_corrigido < 1000000)



# Analisando quais são os tipos mais frequentes em rendimentos outros que não foram especificados
#ro <- dadosjusbr_empilhado_filtrado %>%
#  filter(tipo == "R/O") %>%
#  filter(rubrica_desambiguada == "outros")
#
## Verificar frequência dos itens que não costam nas rubricas principais
## Gerar a tabela de frequência com tabyl
#tabela_sanitizado <- tabyl(ro$item_sanitizado)
#
# Exportar a tabela gerada para um arquivo Excel
write_xlsx(tabela_sanitizado, "tabela_item_sanitizado.xlsx")



# Preparando a base de contracheque ---------------------------------------
# Na base de contracheque os resultados de servidores acima do teto são sempre os mesmos, utilizar outra base para o gráfico 1
# Filtrar o DataFrame de contracheques para incluir apenas os órgãos válidos

summary(contracheque$remuneracao)

contracheque_filtrado <- contracheque %>%
  filter(orgao %in% orgaos_validos_filtrados$orgao  & !startsWith(orgao, "tre" ))


contracheque_filtrado <- contracheque_filtrado %>%
  filter(salario >= 0, remuneracao >= 0)

summary(contracheque_filtrado$remuneracao)

summary(contracheque_filtrado$remuneracao)

