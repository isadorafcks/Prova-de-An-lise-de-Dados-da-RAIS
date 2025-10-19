# Prova Pr†tica: An†lise de Dados da RAIS (2010-2018)
# Chamada 049/2025 - Atlas do Estado Brasileiro (IPEA)

# Isadora Russo Friedericks

###### BASE DE DADOS BRUTA #####################

### 1 etapa: baixar bibliotecas necess†rias para os pr¢ximos passos

library(googledrive)
library(data.table)
library(ggplot2)

# 2 etapa: Por ser um a base gigantesca, n∆o Ç poss°vel baixar os dados no pr¢prio computador. 
# Por isso, usamos o link p£blico da pasta do Drive criada por mim que contÇm os arquivos da RAIS, separados por estado e ano.

url_pasta <- "https://drive.google.com/drive/folders/1R0Rb9_ffNVipDFvocq-X7esPHPVpQuHM"
arquivos <- drive_ls(as_id(url_pasta))
dir.create("dados_rais", showWarnings = FALSE)


# 3 etapa: O c¢digo vai criar uma pasta local chamada "dados_rais" e fazer um loop que percorre todos os arquivos da pasta do Drive, baixando um por um.
# Assim, garantimos que todos os dados necess†rios estejam dispon°veis localmente, sem precisar fazer o download manualmente.

for (i in seq_len(nrow(arquivos))) {
  nome <- arquivos$name[i]
  destino <- file.path("dados_rais", paste0(nome, ".csv"))
  if (!file.exists(destino)) {
    message("baixando: ", nome)
    drive_download(as_id(arquivos$id[i]), path = destino, overwrite = TRUE)
  }
}

# Vamos criar uma lista com o caminho de todos os arquivos que est∆o na pasta "dados_rais".

arquivos_locais <- list.files("dados_rais", full.names = TRUE)

# E ler o arquivo com funá‰es espec°ficas para bases grandes

ler_arquivo <- function(caminho) {
  tryCatch(fread(caminho, encoding = "Latin-1", showProgress = FALSE), error = function(e) NULL)
}


# 4 etapa:A partir de agora, temos que organizar os dados por ano.
# Para isso, vamos criar uma funá∆o que junta todos os arquivos referentes a um mesmo per°odo.
# Ela identifica, com o comando grep(), quais arquivos pertencem a determinado ano (por exemplo, "10" para 2010)


montar_base <- function(ano) {
  arqs <- grep(ano, arquivos_locais, value = TRUE)
  rbindlist(lapply(arqs, ler_arquivo), fill = TRUE)
}

# 5 etapa: Por fim, basta juntar todos os anos numa mesma base para que possamos realizar o que foi pedido

rais2010 <- montar_base("10")
rais2014 <- montar_base("14")
rais2018 <- montar_base("18")Ô

rais_todas <- rbindlist(list(rais2010, rais2014, rais2018), fill = TRUE)


############# BASE DE DADOS AGREGADA COM UMA UNIDADE DE OBSERVAÄ«O "MUNIC÷PIO ANO"  ###################


#Nesta fase vamos criar a base agregada "munic°pio-ano", resumindo as informaá‰es da RAIS em n°vel local e temporal que foram pedidos. 
# Nela, calcularems o total de empregos, os v°nculos CLT e p£blicos, alÇm dos sal†rios mÇdios gerais, por tipo de v°nculo e por raáa. 
# TambÇm inclu°mos a raz∆o entre os sal†rios de brancos e negros.

# vamos criar uma base agregada por municipio-ano e fazer os recortes por raáa, salario e v°nculo.

# Como devemos separar os indiv°duos entre CLT e P£blico e n∆o temos nenhuma coluna pronta que apresente essa denominaá∆o, vamos descobrir todas 
# respostas poss°veis na coluna subsetor_ibge e assim utilizar o dicion†rio via subsetor de atividade do IBGE a fim de criar uma nova coluna que 
#defina o que Ç publico ou nao.


# Usamos essa funá∆o para descobrir quais s∆o os tipos de respostas poss°veis
unique(rais_todas$subsetor_ibge)


# Para assim, definir os subsetores que correspondem ao setor p£blico

subsetores_publico <- c(
  "Administraá∆o p£blica direta e aut†rquica",
  "Ensino",
  "Serviáos industriais de utilidade p£blica"
)


# Criar a coluna booleana setor_publico
rais_todas[, setor_publico := subsetor_ibge %in% subsetores_publico]

head(rais_todas)


# Criando base agregada munic°pio-ano 
rais_agregada <- rais_todas[, .(
  # Empregos
  emprego_total = .N,
  emprego_clt = sum(grepl("^CLT", tipo_vinculo, ignore.case = TRUE), na.rm = TRUE),
  emprego_setor_publico = sum(setor_publico, na.rm = TRUE),
  
  # Sal†rios mÇdios gerais
  salario_medio_total = mean(valor_remuneracao_media, na.rm = TRUE),
  salario_medio_clt = mean(valor_remuneracao_media[grepl("^CLT", tipo_vinculo, ignore.case = TRUE)], na.rm = TRUE),
  salario_medio_publico = ifelse(sum(setor_publico, na.rm = TRUE) > 0,
                                 mean(valor_remuneracao_media[setor_publico == TRUE], na.rm = TRUE),
                                 0),
  
  # Sal†rios mÇdios brancos
  salario_brancos_total = mean(valor_remuneracao_media[raca_cor == "Branca"], na.rm = TRUE),
  salario_brancos_clt = mean(valor_remuneracao_media[raca_cor == "Branca" & grepl("^CLT", tipo_vinculo, ignore.case = TRUE)], na.rm = TRUE),
  salario_brancos_publico = ifelse(sum(setor_publico[raca_cor == "Branca"], na.rm = TRUE) > 0,
                                   mean(valor_remuneracao_media[raca_cor == "Branca" & setor_publico == TRUE], na.rm = TRUE),
                                   0),
  
  # Sal†rios mÇdios negros 
  salario_negros_total = mean(valor_remuneracao_media[raca_cor %in% c("Preta","Parda")], na.rm = TRUE),
  salario_negros_clt = mean(valor_remuneracao_media[raca_cor %in% c("Preta","Parda") & grepl("^CLT", tipo_vinculo, ignore.case = TRUE)], na.rm = TRUE),
  salario_negros_publico = ifelse(sum(setor_publico[raca_cor %in% c("Preta","Parda")], na.rm = TRUE) > 0,
                                  mean(valor_remuneracao_media[raca_cor %in% c("Preta","Parda") & setor_publico == TRUE], na.rm = TRUE),
                                  0),
  
  # Raz‰es brancos / negros
  razao_total = mean(valor_remuneracao_media[raca_cor == "Branca"], na.rm = TRUE) /
    mean(valor_remuneracao_media[raca_cor %in% c("Preta","Parda")], na.rm = TRUE),
  razao_clt = mean(valor_remuneracao_media[raca_cor == "Branca" & grepl("^CLT", tipo_vinculo, ignore.case = TRUE)], na.rm = TRUE) /
    mean(valor_remuneracao_media[raca_cor %in% c("Preta","Parda") & grepl("^CLT", tipo_vinculo, ignore.case = TRUE)], na.rm = TRUE),
  razao_publico = ifelse(sum(setor_publico[raca_cor %in% c("Branca","Preta","Parda")], na.rm = TRUE) > 0,
                         mean(valor_remuneracao_media[raca_cor == "Branca" & setor_publico == TRUE], na.rm = TRUE) /
                           mean(valor_remuneracao_media[raca_cor %in% c("Preta","Parda") & setor_publico == TRUE], na.rm = TRUE),
                         0)
), by = .(ano, id_municipio)]

#Visualizar e salvar

head(rais_agregada)
fwrite(rais_agregada, "rais_agregado_municipio_ano.csv")


####### VIZUALIZAÄ«O DE DADOS #############

# 1.Crie uma visualizaá∆o que permita captar de forma clara a dispers∆o da raz∆o entre os sal†rios de brancos e
#negros no territ¢rio nacional para cada categoria (Total, clt p£blico)


####  DISPERS«O DOS SALµRIOS MêDIOS: Brancos x Negros ( TOTAL)

ggplot(rais_agregada, aes(x = salario_negros_total, y = salario_brancos_total)) +
  geom_point(alpha = 0.3) +
  labs(title = "Dispers∆o dos sal†rios mÇdios: brancos x negros (Total)",
       x = "Sal†rio mÇdio trabalhadores negros",
       y = "Sal†rio mÇdio trabalhadores brancos") +
  theme_minimal()

#Podemos observar uma tendància geral em que a maior parte dos pontos est† acima da linha imagin†ria de 45¯,
#o que indica que, em quase todos os munic°pios, os trabalhadores brancos ganham, em mÇdia, mais do que os trabalhadores negros. 

#### DISPERS«O DOS SALµRIOS MêDIOS: Brancos X Negros (CLT)

ggplot(rais_agregado1, aes(x = salario_negros_clt, y = salario_brancos_clt)) +
  geom_point(alpha = 0.3, color = "blue") +
  labs(title = "Dispers∆o dos sal†rios mÇdios: brancos x negros (CLT)",
       x = "Sal†rio mÇdio trabalhadores negros",
       y = "Sal†rio mÇdio trabalhadores brancos") +
  theme_minimal()

#TambÇm conseguimos observar essa tendància em que a maior parte dos pontos est† acima da linha imagin†ria de 45¯,
#o que indica que, em quase todos os munic°pios, os trabalhadores brancos ganham, em mÇdia, mais do que os trabalhadores negros. 


#### DISPERS«O DOS SALµRIOS MêDIOS: Brancos X Negros (SETOR PÈBLICO)

ggplot(rais_agregada, aes(x = salario_negros_publico, y = salario_brancos_publico)) +
  geom_point(alpha = 0.3, color = "green") +
  labs(title = "Dispers∆o dos sal†rios mÇdios: brancos x negros (Setor P£blico)",
       x = "Sal†rio mÇdio trabalhadores negros",
       y = "Sal†rio mÇdio trabalhadores brancos") +
  theme_minimal()

#TambÇm conseguimos observar essa tendància em que a maior parte dos pontos est† acima da linha imagin†ria de 45¯,
#o que indica que, em quase todos os munic°pios, os trabalhadores brancos ganham, em mÇdia, mais do que os trabalhadores negros tambÇm no setor publico. 



# 2.Crie uma visualizaá∆o que mostre como evolu°ram essas raz‰es ao longo destes 8 anos para cada uma das categorias


##### EVOLUÄ«O DA RAZ«O TOTAL ENTRE SALµRIOS DE BRANCOS E NEGROS ( TOTAL) #####

# Garantir que a coluna ano seja numÇrica
rais_agregada$ano <- as.numeric(as.character(rais_agregada$ano))

# Agregar por ano (mÇdia da raz∆o total)
razao_total_evolucao <- aggregate(razao_total ~ ano,
                                  data = rais_agregada,
                                  FUN = mean, na.rm = TRUE)

# Plot ajustado
ggplot(razao_total_evolucao, aes(x = ano, y = razao_total)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, color = "red") +
  labs(
    title = "Evoluá∆o da raz∆o entre sal†rios de brancos e negros (Total)",
    x = "Ano",
    y = "Raz∆o mÇdia (Brancos / Negros)"
  ) +
  scale_x_continuous(breaks = c(2010, 2014, 2018)) +
  scale_y_continuous(
    limits = c(min(razao_total_evolucao$razao_total)*0.95, 
               max(razao_total_evolucao$razao_total)*1.05),
    breaks = scales::pretty_breaks(n = 5)  # define 5 quebras "bonitas"
  ) +
  theme_minimal()


##### EVOLUÄ«O DA RAZ«O TOTAL ENTRE SALµRIOS DE BRANCOS E NEGROS ( CLT ) #####

# Garantir que a coluna ano seja numÇrica
rais_agregada$ano <- as.numeric(as.character(rais_agregada$ano))

# Agregar por ano a raz∆o CLT
razao_clt_evolucao <- aggregate(razao_clt ~ ano,
                                data = rais_agregada,
                                FUN = function(x) mean(x, na.rm = TRUE))

# Gr†fico CLT
ggplot(razao_clt_evolucao, aes(x = ano, y = razao_clt)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, color = "red") +
  labs(
    title = "Evoluá∆o da raz∆o entre sal†rios de brancos e negros (CLT)",
    x = "Ano",
    y = "Raz∆o mÇdia (Brancos / Negros)"
  ) +
  scale_x_continuous(breaks = c(2010, 2014, 2018)) +
  scale_y_continuous(
    limits = c(min(razao_clt_evolucao$razao_clt, na.rm = TRUE)*0.95, 
               max(razao_clt_evolucao$razao_clt, na.rm = TRUE)*1.05),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  theme_minimal()


##### EVOLUÄ«O DA RAZ«O TOTAL ENTRE SALµRIOS DE BRANCOS E NEGROS ( SETOR PÈBLICO ) #####


# Agregar por ano a raz∆o do setor p£blico
razao_publico_evolucao <- aggregate(razao_publico ~ ano,
                                    data = rais_agregada,
                                    FUN = function(x) mean(x, na.rm = TRUE))

# Gr†fico setor p£blico
ggplot(razao_publico_evolucao, aes(x = ano, y = razao_publico)) +
  geom_line(size = 1, color = "green") +
  geom_point(size = 2, color = "red") +
  labs(
    title = "Evoluá∆o da raz∆o entre sal†rios de brancos e negros (Setor P£blico)",
    x = "Ano",
    y = "Raz∆o mÇdia (Brancos / Negros)"
  ) +
  scale_x_continuous(breaks = c(2010, 2014, 2018)) +
  scale_y_continuous(
    limits = c(min(razao_publico_evolucao$razao_publico, na.rm = TRUE)*0.95,
               max(razao_publico_evolucao$razao_publico, na.rm = TRUE)*1.05),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  theme_minimal()




# A an†lise da RAIS entre 2010 e 2018 mostra diferenáas consistentes nos sal†rios mÇdios de trabalhadores brancos e negros, tanto no setor privado quanto no setor p£blico. Observando os gr†ficos de dispers∆o, nota-se que, na maioria dos munic°pios, os trabalhadores brancos recebem mais do que os negros, sendo essa diferenáa mais pronunciada no setor privado formal (CLT). No setor p£blico, embora ainda existam disparidades, os sal†rios s∆o mais equilibrados, indicando que este setor atua como um moderador da desigualdade salarial racial.
# Ao longo do per°odo analisado, a raz∆o entre os sal†rios mÇdios de brancos e negros permanece relativamente est†vel. 

#Para o total de trabalhadores, os sal†rios dos brancos foram, em mÇdia, entre 1,1 e 1,25 vezes maiores que os dos negros.
#No setor CLT, a desigualdade persiste de forma semelhante, mostrando que a formalizaá∆o do emprego n∆o elimina a diferenáa racial nos rendimentos. J† no setor p£blico, a raz∆o Ç mais pr¢xima de 1, demonstrando uma distribuiá∆o mais equitativa de sal†rios, embora haja munic°pios com grandes disparidades.
# Conclui-se que  os resultados indicam que infelizmente a desigualdade salarial racial entre os anos de 2010 e 2018 Ç uma caracter°stica persistente, independente do contrato e trabalho. 

