#Importar arquivos

#Definir diretório
#setwd("C:/Users/aaman/OneDrive/Documentos/Projeto_Super_Salários")

# 2018
dadosjusbr_2018 <- read_delim("extracao-17-10-2024\\extracao-dadosjusbr-republica-ponto-org-2018.csv", 
                              delim = ";", 
                              escape_double = FALSE, 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE) 

# 2019
dadosjusbr_2019 <- read_delim("extracao-17-10-2024\\extracao-dadosjusbr-republica-ponto-org-2019.csv", 
                              delim = ";", 
                              escape_double = FALSE, 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE) 

# 2020
dadosjusbr_2020 <- read_delim("extracao-17-10-2024\\extracao-dadosjusbr-republica-ponto-org-2020.csv", 
                              delim = ";", 
                              escape_double = FALSE, 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE) 

#2021
dadosjusbr_2021 <- read_delim("extracao-17-10-2024\\extracao-dadosjusbr-republica-ponto-org-2021.csv", 
                              delim = ";", 
                              escape_double = FALSE, 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE) 

# 2022
dadosjusbr_2022 <- read_delim("extracao-17-10-2024\\extracao-dadosjusbr-republica-ponto-org-2022.csv", 
                              delim = ";", 
                              escape_double = FALSE, 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE) 
# 2023
dadosjusbr_2023 <- read_delim("extracao-17-10-2024\\extracao-dadosjusbr-republica-ponto-org-2023.csv", 
                              delim = ";", 
                              escape_double = FALSE, 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE) 

# 2024
dadosjusbr_2024 <- read_delim("extracao-17-10-2024\\extracao-dadosjusbr-republica-ponto-org-2024.csv", 
                              delim = ";", 
                              escape_double = FALSE, 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE)

#Contracheque
contracheque <- read_delim("contracheque.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                           trim_ws = TRUE)

## Checando até que mês vai o contracheque e até em que mês vai o especifico:

names(contracheque)

max(contracheque[contracheque$ano == "2024",]['mes'])
max(dadosjusbr_2024['mes'])

# Vai até agosto/2024
