



# Adicionar a coluna 'rubrica_desambiguada' com base nos valores de 'item_sanitizado'
dadosjusbr_empilhado_filtrado <- dadosjusbr_empilhado_filtrado %>%
  mutate(rubrica_desambiguada = 
           case_when(item_sanitizado == "pagamentos retroativos" ~ "Pagamentos Retroativos",
                                          item_sanitizado == "auxilio moradia" ~ "Auxílio Moradia",
                                          TRUE ~ rubrica_desambiguada))



status_rubrica = read.csv2("rubricas_especificadas.csv", encoding = "latin1")

status_rubrica = status_rubrica %>% filter(flag==1) %>% dplyr::select(item_sanitizado,status)


## soma de valores por rubrica para ver a mais significativa. 

soma_rub = dadosjusbr_empilhado_filtrado %>% group_by(rubrica_desambiguada) %>% summarise(sum(valor))

# Puxar info do status rubrica

dadosjusbr_empilhado_filtrado = left_join(dadosjusbr_empilhado_filtrado,status_rubrica,"item_sanitizado")

dadosjusbr_empilhado_filtrado$status= ifelse(is.na(dadosjusbr_empilhado_filtrado$status) & dadosjusbr_empilhado_filtrado$tipo=="D", "Desconto",
                                             ifelse(is.na(dadosjusbr_empilhado_filtrado$status) & dadosjusbr_empilhado_filtrado$tipo=="R/B","Remuneração",
                                                    ifelse(is.na(dadosjusbr_empilhado_filtrado$status) & dadosjusbr_empilhado_filtrado$tipo=="R/O", "Não identificado",dadosjusbr_empilhado_filtrado$status)))
                                                    



## testando tipo e rubrica
table(dadosjusbr_empilhado_filtrado$rubrica_desambiguada,dadosjusbr_empilhado_filtrado$status)

## Como o auxilio alimentacao ja era desambiguado,temos que colocar como indenização, Inserir: férias, auxílio ali

indeniza_fora = c("auxilio-alimentacao","auxilio-saude","ferias","indenizacao-de-ferias")
dadosjusbr_empilhado_filtrado$status = ifelse(dadosjusbr_empilhado_filtrado$rubrica_desambiguada %in%indeniza_fora,"Indenização PL",dadosjusbr_empilhado_filtrado$status)

## Preciso incluir também a licença-prêmio e a licença-compensatória. 


dadosjusbr_empilhado_filtrado$status = ifelse(dadosjusbr_empilhado_filtrado$rubrica_desambiguada %in%c("licenca-compensatoria","licenca-premio"),"Indenização PL",dadosjusbr_empilhado_filtrado$status)







# Criando colunas de D, R/B e cada categoria de R/O, incluindo Pagamentos Retroativos e Auxílio Moradia
dadosjusbr_agrupados <- dadosjusbr_empilhado_filtrado %>%
  group_by(id_contracheque, nome, ano, mes, local_trabalho) %>%
  summarise(
    descontos = sum(valor[status == "Desconto"], na.rm = TRUE),
    remuneracao_basica = sum(valor[status == "Remuneração"], na.rm = TRUE),
    nao_identificado_padronizado = sum(valor[status == "Não identificado" & rubrica_desambiguada != "não especificado ou despadronizado"], na.rm = TRUE),
    nao_identificado_despadronizado = sum(valor[status == "Não identificado" & rubrica_desambiguada == "não especificado ou despadronizado"], na.rm = TRUE),
    pl_supersalarios = sum(valor[status == "Indenização PL"], na.rm = TRUE),
    abono_permanencia = sum(valor[rubrica_desambiguada == "abono de permanencia"], na.rm = TRUE),
    ajuda_custo = sum(valor[rubrica_desambiguada == "ajuda de custo"], na.rm = TRUE),
    auxilio_alimentacao = sum(valor[rubrica_desambiguada == "auxilio-alimentacao"], na.rm = TRUE),
    auxilio_saude = sum(valor[rubrica_desambiguada == "auxilio-saude"], na.rm = TRUE),
    auxilio_moradia = sum(valor[rubrica_desambiguada == "Auxílio Moradia"], na.rm = TRUE),
    auxilio_natalidade = sum(valor[rubrica_desambiguada == "auxilio natalidade"], na.rm = TRUE),
    auxilio_preescolar = sum(valor[rubrica_desambiguada == "auxilio preescolar"], na.rm = TRUE),
    diarias = sum(valor[rubrica_desambiguada == "diarias"], na.rm = TRUE),
    ferias = sum(valor[rubrica_desambiguada == "ferias"], na.rm = TRUE),
    gratificacao_natalina = sum(valor[rubrica_desambiguada == "gratificacao-natalina"], na.rm = TRUE),
    gratificacao_cursoconcurso = sum(valor[rubrica_desambiguada == "gratificacao por encargo cursoconcurso"], na.rm = TRUE),
    gratificacao_exercicio_cumulativo = sum(valor[rubrica_desambiguada == "gratificacao por exercicio cumulativo"], na.rm = TRUE),
    indenizacao_de_ferias = sum(valor[rubrica_desambiguada == "indenizacao-de-ferias"], na.rm = TRUE),
    indenizacoes = sum(valor[rubrica_desambiguada == "indenizacoes"], na.rm = TRUE),
    licenca_premio = sum(valor[rubrica_desambiguada == "licenca-premio"], na.rm = TRUE),
    licenca_compensatoria = sum(valor[rubrica_desambiguada == "licenca-compensatoria"], na.rm = TRUE)
  ) %>%
  ungroup()    # Desagrupar os dados após o summarise


## vendo se bate o liquido do contracheque com os das rubricas:
#criando cariavel
dadosjusbr_agrupados$total_liquido = dadosjusbr_agrupados$remuneracao_basica+dadosjusbr_agrupados$nao_identificado_padronizado+
  dadosjusbr_agrupados$nao_identificado_despadronizado+dadosjusbr_agrupados$pl_supersalarios-dadosjusbr_agrupados$descontos

## estao com linhas diferentes
# Juntar as informações de rubricas desagregadas ao df contracheque
dados_completo <- contracheque_filtrado %>%
  left_join(dadosjusbr_agrupados, by = c("id_contracheque", "nome", "ano", "mes", "local_trabalho"))

dados_completo$teste = round(dados_completo$remuneracao,0)==round(dados_completo$total_liquido,0)
table(dados_completo$teste)
## Retirar os que ficaram negativos

dados_completo=dados_completo %>% filter(total_liquido>0 &!is.na(total_liquido)  )

summary(dados_completo$total_liquido) #
prop.table(table(dados_completo$teste)) # 27% não batem exatamente
# Vou trabalhar com as rubricas detalhadas






# Adicionar coluna do teto constitucional
dados_completo <- dados_completo %>%
  mutate(
    teto_constitucional = case_when(
      ano <= 2018 & mes <= 10 ~  33763.00,
      ano < 2023  ~  39293.32,
      ano <= 2023 & mes <= 3 ~ 39293.32,
      ano == 2023 & mes > 3 ~ 41650.92,
      ano == 2024 & mes <= 1 ~ 41650.92,
      ano == 2024 & mes > 1 ~ 44008.52
    ))

# Criar variável para excedente do teto
dados_completo <- dados_completo %>%
  mutate(
    excedente_teto = if_else(total_liquido > teto_constitucional, 
                             total_liquido - teto_constitucional, 
                             0)
  )

dados_completo$teste2 = round(dados_completo$salario,0)==round(dados_completo$remuneracao_basica,0)
prop.table(table(dados_completo$teste2)) # 4,3% não batem exatamente



## Criando a remuneração

dados_completo$total_bruto = dados_completo$remuneracao_basica+dados_completo$nao_identificado_padronizado+
  dados_completo$nao_identificado_despadronizado+dados_completo$pl_supersalarios



## Parte elaborada por Paula Frias
# Calcular as proporções de cada rubrica em relação à remuneração
dados_completo <- dados_completo %>%
  mutate(
    
    nao_identificado_padronizado_prop     =  nao_identificado_padronizado/(total_bruto-remuneracao_basica)  ,
    nao_identificado_despadronizado_prop  =  nao_identificado_despadronizado/(total_bruto-remuneracao_basica) , 
    abono_permanencia_prop                =  abono_permanencia/(total_bruto-remuneracao_basica)   ,   
    ajuda_custo_prop                      =  ajuda_custo / (total_bruto-remuneracao_basica),
    auxilio_alimentacao_prop              =  auxilio_alimentacao/(total_bruto-remuneracao_basica),  
    auxilio_saude_prop                    =  auxilio_saude/(total_bruto-remuneracao_basica)  ,
    auxilio_moradia_prop                  =  auxilio_moradia/(total_bruto-remuneracao_basica)  ,
    auxilio_natalidade_prop               =  auxilio_natalidade/(total_bruto-remuneracao_basica)  ,
    auxilio_preescolar_prop               =  auxilio_preescolar/(total_bruto-remuneracao_basica)  ,
    diarias_prop                          =  diarias/(total_bruto-remuneracao_basica)  ,
    ferias_prop                           =  ferias/(total_bruto-remuneracao_basica)  ,
    gratificacao_natalina_prop            =  gratificacao_natalina/(total_bruto-remuneracao_basica),  
    gratificacao_cursoconcurso_prop       =  gratificacao_cursoconcurso/(total_bruto-remuneracao_basica)  ,
    gratificacao_exercicio_cumulativo_prop=  gratificacao_exercicio_cumulativo/(total_bruto-remuneracao_basica),  
    indenizacao_de_ferias_prop            =  indenizacao_de_ferias/(total_bruto-remuneracao_basica)  ,
    indenizacoes_prop                     =  indenizacoes/(total_bruto-remuneracao_basica)  ,
    licenca_premio_prop                   =  licenca_premio/(total_bruto-remuneracao_basica)  ,
    licenca_compensatoria_prop           =  licenca_compensatoria/(total_bruto-remuneracao_basica)  
  
  )


## agora é seguir aqui: 



# Calcular a proporção dos valores que excederam o teto
dados_completo <- dados_completo %>%
  mutate(
  nao_identificado_padronizado_teto          =excedente_teto*nao_identificado_padronizado_prop,     
  nao_identificado_despadronizado_teto       =excedente_teto*nao_identificado_despadronizado_prop,  
  abono_permanencia_teto                     =excedente_teto*abono_permanencia_prop      ,          
  ajuda_custo_teto                          =excedente_teto*ajuda_custo_prop         ,             
  auxilio_alimentacao_teto                   =excedente_teto*auxilio_alimentacao_prop  ,            
  auxilio_saude_teto                      =excedente_teto*auxilio_saude_prop          ,          
  auxilio_moradia_teto                     =excedente_teto*auxilio_moradia_prop       ,           
  auxilio_natalidade_teto                  =excedente_teto*auxilio_natalidade_prop    ,           
  auxilio_preescolar_teto                 =excedente_teto*auxilio_preescolar_prop     ,          
  diarias_teto                            =excedente_teto*diarias_prop                ,          
  ferias_teto                                =excedente_teto*ferias_prop              ,             
  gratificacao_natalina_teto                 =excedente_teto*gratificacao_natalina_prop    ,        
  gratificacao_cursoconcurso_teto            =excedente_teto*gratificacao_cursoconcurso_prop ,      
  gratificacao_exercicio_cumulativo_teto     =excedente_teto*gratificacao_exercicio_cumulativo_prop,
  indenizacao_de_ferias_teto              =excedente_teto*indenizacao_de_ferias_prop ,           
  indenizacoes_teto                         =excedente_teto*indenizacoes_prop,
  licenca_premio_teto                     =excedente_teto*licenca_premio_prop ,           
  licenca_compensatoria_teto               =excedente_teto*licenca_compensatoria_prop   
  )

# Agrupar e somar os valores excedentes por rubrica
gastos_extra_teto <- dados_completo %>%
  summarise(
    
  nao_identificado_padronizado       = sum(nao_identificado_padronizado_teto      ,na.rm=TRUE)  ,
  nao_identificado_despadronizado    = sum(nao_identificado_despadronizado_teto   ,na.rm=TRUE),
  abono_permanencia                 = sum(abono_permanencia_teto                 ,na.rm=TRUE),
  ajuda_custo                       = sum(ajuda_custo_teto                       ,na.rm=TRUE),
  auxilio_alimentacao                = sum(auxilio_alimentacao_teto               ,na.rm=TRUE),
  auxilio_saude                     = sum(auxilio_saude_teto                     ,na.rm=TRUE),
  auxilio_moradia                  = sum(auxilio_moradia_teto                   ,na.rm=TRUE),
  auxilio_natalidade                = sum(auxilio_natalidade_teto                ,na.rm=TRUE),
  auxilio_preescolar                 = sum(auxilio_preescolar_teto                ,na.rm=TRUE),
  diarias                            = sum(diarias_teto                           ,na.rm=TRUE),
  ferias                             = sum(ferias_teto                            ,na.rm=TRUE),
  gratificacao_natalina              = sum(gratificacao_natalina_teto             ,na.rm=TRUE),
  gratificacao_cursoconcurso         = sum(gratificacao_cursoconcurso_teto        ,na.rm=TRUE),
  gratificacao_exercicio_cumulativo  = sum(gratificacao_exercicio_cumulativo_teto ,na.rm=TRUE),
  indenizacao_de_ferias             = sum(indenizacao_de_ferias_teto             ,na.rm=TRUE),
  indenizacoes                       = sum(indenizacoes_teto                      ,na.rm=TRUE),
  licenca_premio             = sum(licenca_premio_teto             ,na.rm=TRUE),
  licenca_compensatoria                       = sum(licenca_compensatoria_teto                      ,na.rm=TRUE)
    
) %>%
  gather(key = "rubrica", value = "gasto_extra_teto") %>%
  mutate(proporcao = gasto_extra_teto / sum(gasto_extra_teto))





# Criar o gráfico com as proporções, incluindo "Pagamentos Retroativos" e "Auxílio Moradia"
ggplot(gastos_extra_teto, aes(x = reorder(rubrica, proporcao), y = proporcao)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = .6, width = .3) +
  coord_flip() +
  geom_text(aes(label = paste0(round(proporcao * 100, 2), "%")), 
            hjust = -0.1, size = 6, color = "black") +
  xlab("") +
  ylab("Proporção dos Gastos Extra-Teto (%)") + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(gastos_extra_teto$proporcao) * 1.2)) +
  scale_x_discrete(labels = c(
    "auxilio_alimentacao" = "Auxílio Alimentação",
    "auxilio_saude" = "Auxílio Saúde",
    "ferias" = "Férias",
    "gratificacao_natalina" = "Gratificação Natalina",
    "indenizacao_ferias" = "Indenização de Férias",
    "licenca_premio" = "Licença-Prêmio",
    "outros" = "Outros",
    "pagamentos_retroativos" = "Pagamentos Retroativos",  # Adicionar Pagamentos Retroativos
    "auxilio_moradia" = "Auxílio Moradia"  # Adicionar Auxílio Moradia
  )) + 
  labs(
    title = "Proporção dos Gastos Extra-Teto por Tipo de Rubrica",
    subtitle = "",
    caption = "Fonte: Dados JusBR, base de contracheque"
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
    legend.position = "none"
  )


# Fazendo para 2023 filtrando para órgão que enviaram os dados por mais de nove meses incluindo nov e dez --------

# Criar as colunas agregadas
dados_completo_2023 <- dados_completo %>%
  filter(ano == "2023")

#Gastos extra teto 2023
gastos_extra_teto_2023 <- dados_completo_2023 %>%
  summarise(
    
    nao_identificado_padronizado       = sum(nao_identificado_padronizado_teto      ,na.rm=TRUE)  ,
    nao_identificado_despadronizado    = sum(nao_identificado_despadronizado_teto   ,na.rm=TRUE),
    abono_permanencia                 = sum(abono_permanencia_teto                 ,na.rm=TRUE),
    ajuda_custo                       = sum(ajuda_custo_teto                       ,na.rm=TRUE),
    auxilio_alimentacao                = sum(auxilio_alimentacao_teto               ,na.rm=TRUE),
    auxilio_saude                     = sum(auxilio_saude_teto                     ,na.rm=TRUE),
    auxilio_moradia                  = sum(auxilio_moradia_teto                   ,na.rm=TRUE),
    auxilio_natalidade                = sum(auxilio_natalidade_teto                ,na.rm=TRUE),
    auxilio_preescolar                 = sum(auxilio_preescolar_teto                ,na.rm=TRUE),
    diarias                            = sum(diarias_teto                           ,na.rm=TRUE),
    ferias                             = sum(ferias_teto                            ,na.rm=TRUE),
    gratificacao_natalina              = sum(gratificacao_natalina_teto             ,na.rm=TRUE),
    gratificacao_cursoconcurso         = sum(gratificacao_cursoconcurso_teto        ,na.rm=TRUE),
    gratificacao_exercicio_cumulativo  = sum(gratificacao_exercicio_cumulativo_teto ,na.rm=TRUE),
    indenizacao_de_ferias             = sum(indenizacao_de_ferias_teto             ,na.rm=TRUE),
    indenizacoes                       = sum(indenizacoes_teto                      ,na.rm=TRUE),
    licenca_premio             = sum(licenca_premio_teto             ,na.rm=TRUE),
    licenca_compensatoria       = sum(licenca_compensatoria_teto                      ,na.rm=TRUE)
    
  ) %>%
  gather(key = "rubrica", value = "gasto_extra_teto") %>%
  mutate(proporcao = gasto_extra_teto / sum(gasto_extra_teto))




# Criar o gráfico para o ano de 2023
ggplot(gastos_extra_teto_2023, aes(x = reorder(rubrica, proporcao), y = proporcao)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = .6, width = .3) +
  coord_flip() +
  geom_text(aes(label = paste0(round(proporcao * 100, 2), "%")), 
            hjust = -0.1, size = 6, color = "black") +
  xlab("") +
  ylab("Proporção dos Gastos Extra-Teto (%)") + 
  scale_y_continuous(labels = percent_format(), limits = c(0, max(gastos_extra_teto_2023$proporcao) * 1.2)) +
  scale_x_discrete(labels = c(
    "nao_identificado_padronizado"  = "Rubricas padronizadas sem identificação" ,
    "nao_identificado_despadronizado"  = "Rubricas despadronizadas sem identificação",
    "abono_permanencia"      =           "Abono permanência",
    "ajuda_custo"           =            "Ajuda de custo",
    "auxilio_alimentacao"   =            "Auxílio alimentação",
    "auxilio_saude"          =           "Auxílio saúde",
    "auxilio_moradia"         =         "Auxílio moradia",
    "auxilio_natalidade"     =           "Auxílio natalidade",
    "auxilio_preescolar"      =          "Auxílio pré-escolar",
    "diarias"                =           "Diárias",
    "ferias"                   =         "Férias",
    "gratificacao_natalina"    =         "Gratificação natalina",
    "gratificacao_cursoconcurso"  =      "Gratificação por curso ou concurso",
    "gratificacao_exercicio_cumulativo" = "Gratificação por execício cumulativo",
    "indenizacao_de_ferias"         =    "Indenização de férias",
    "indenizacoes"               =       "Indenizações",
    "licenca_compensatoria" = "Licença compensatória",
    "licenca_premio" = "Licença prêmio"

  )) + 
  labs(
    title = "Proporção dos Gastos Extra-Teto por Tipo de Rubrica (2023)",
    subtitle = "",
    caption = "Fonte: Dados JusBR, base de contracheque"
  ) +
  theme_economist() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 14),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "none"
  )


#salvando bases:

save(dadosjusbr_empilhado_filtrado, contracheque_filtrado,dados_completo,dadosjusbr_agrupados,file='dados_filtrados.Rdata')


### No não identificado padronizados temos as seguintes rubricas: jetons, pagamentos retroativos e substituição.