##################################################################################
#                       CURSO FAXINA DE DADOS - CURSO R                          #
##################################################################################

# A ideia é mostrar todo o processo como ocorreu.
# Este não é um script limpo, mas uma documentação do processo de limpeza.

# Instalação de Pacotes --------------------------------------------------------

library(readxl)
library(tidyverse)
library(lubridate) #manipulação de datas
library(janitor)

# Leitura dos dados -------------------------------------------------------

#Como o arquivo é .xls, a função do pacote readxl deveria funcionar.

readxl::read_excel("dados/dados_bo_2021_roubodeveiculos_completa.xls")

#Mas não funciona. =( Não podemos desistir. 
#O arquivo pode estar com a extensão errada.
#Tentar view file, abrir com o excel, abrir com bloco de notas.
#Este arquivo abre com excel e no bloco de notas, ou seja, deve ser texto
#No bloco de notas parece um arquivo de texto delimitado por tab.
#Se o arquivo for muito grande, não será possivel abrir no bloco de notas. Tentar:

readLines("dados/dados_bo_2021_roubodeveiculos_completa.xls", n = 10)
#não dá certo, provavelmente por causa do encoding.

# Usando o read_delim do tidyverse
base_bruta <- readr::read_delim("dados/dados_bo_2021_roubodeveiculos_completa.xls", delim = "\t")
# a base carregou com todos os valores Na

#Tentar o read.delim da base do R
base_bruta <- read.delim("dados/dados_bo_2021_roubodeveiculos_completa.xls", sep = "\t", stringsAsFactors = FALSE)
# a base carregou com somente uma coluna

#Encoding: o jeito que o computador entende os caracteres. Como descobrir o encoding?
readr::guess_encoding("dados/dados_bo_2021_roubodeveiculos_completa.xls", threshold = 0, n_max = 1000)
#Se o arquivo for muito grande, usar o argumento n_max.
#Devolve a probabilidade do arquivo ter determinado encoding.

base_bruta <- readr::read_delim("dados/dados_bo_2021_roubodeveiculos_completa.xls",
                                delim = "\t", locale = readr::locale(encoding = "UTF-16LE"))

#Com a versão mais recente do readr, funciona. Esta é a melhor solução neste caso.
#Outra opção que funciona neste caso:

base_bruta <- utils::read.delim("dados/dados_bo_2021_roubodeveiculos_completa.xls",
                                sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-16LE")

#É melhor descobrir o encoding no início do processo.


# Experimentos - Manipulação  -------------------------------------------------

base_bruta %>%
  tibble::view()

base_bruta %>%
  dplyr::glimpse()

#Existem muitas colunas totalmente vazias, dados pessoais foram omitidos.
#Identificamos repetição de dados e outros problemas.
#Várias vitimas na mesma ocorrência?
#No caso das repetições, o que não está repetido? Vamos limpar todas as repetições.

#Qual unidade que irei considerar? No final tem que ter 1 linha da base para cada unidade.
#Preciso encontrar a chave.Perguntar para alguém, consultar documentação.
#Ou tentar entender a base para achar a chave por tentativa e erro.
#Neste caso, existe documentação para consultar no site da SSP.

#A unidade é a ocorrência?
base_bruta %>%
  count(ANO_BO, NUM_BO,DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME)

#Casos particulares:

base_bruta %>%
  filter(
    NUM_BO == 23, ANO_BO == 2021, DELEGACIA_NOME == "DEIC-5ª DELEGACIA DA DISCCPAT"
  ) %>%
  tibble::view()


#A unidade é o carro roubado?
base_bruta %>%
  count(ANO_BO, NUM_BO,DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO, ESPECIE, RUBRICA) %>%
  filter(n > 1)

#Ainda há repetições. Vamos descobrir o motivo:

base_bruta %>%
  filter(ANO_BO == 2021, NUM_BO == 1347, DELEGACIA_NOME == "16º D.P. VILA CLEMENTINO") %>%
  tibble::view()

# Parece que a coluna status também gera as repetições. Vamos incluí-la.

base_bruta %>%
  count(ANO_BO, NUM_BO,DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO, STATUS, ESPECIE, RUBRICA) %>%
  filter(n > 1)


#Ainda há repetições. Vamos descobrir o motivo:

base_bruta %>%
  filter(ANO_BO == 2021, NUM_BO == 1692, DELEGACIA_NOME == "69º D.P. TEOTONIO VILELA") %>%
  tibble::view()

# Parece que a coluna marca e cor do veículo também geram repetições:

base_bruta %>%
  count(ANO_BO, NUM_BO,DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO, DESCR_MARCA_VEICULO, DESCR_COR_VEICULO, STATUS, ESPECIE, RUBRICA, DESDOBRAMENTO) %>%
  filter(n > 1)

#Ainda há repetições. Vamos descobrir o motivo:

base_bruta %>%
  filter(ANO_BO == 2021, NUM_BO == 437009, DELEGACIA_NOME == "DELEGACIA ELETRONICA") %>%
  tibble::view()

# Parece que a cidade do veículo também gera repetições

base_brut5a %>%
  count(ANO_BO, NUM_BO,DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO, DESCR_MARCA_VEICULO, DESCR_COR_VEICULO, CIDADE_VEICULO, STATUS, ESPECIE, RUBRICA, DESDOBRAMENTO) %>%
  filter(n > 1)

#Agora encontramos a chave e não há mais repetições. Vamos usar carro como unidade.


# Arrumar o nome das colunas ----------------------------------------------

#Verificando as possibilidades em para filtrar:

base_bruta %>%
  count(PLACA_VEICULO) %>%
  tibble::view()

base_bruta %>%
  count(ANO_BO, NUM_BO) %>%
  tibble::view()

base_bruta %>%
  count(ESPECIE, DESDOBRAMENTO, RUBRICA, STATUS) %>%
  tibble::view()


#Criando a base arrumada

base_nomes_arrumados_preenchida <- base_bruta %>%
  # tira espaços e troca por "_", tira acento, simbolos, passa para minúsculo,
  # se tiver repetição de colunas, ela coloca um índice após a coluna.
  janitor::clean_names() %>%
  #remove os carros com id vazio
  filter(!is.na(placa_veiculo), placa_veiculo != "",
         !(placa_veiculo %in% c("SEMPLA", "SEMPLAC", "000000", "0000000", "ZZZ0000", "TAMPADA"))) %>%
  filter(status == "Consumado")


#Vamos verificar se parece ok:
base_nomes_arrumados_preenchida %>%
  count(placa_veiculo) %>%
  tibble::view()

# Separando as bases ------------------------------------------------------

# Estratégia:
# Separar as três bases e montar a base final fazendo joins (left_join)
# a partir da base de unidade que eu escolher(no caso, veículo)
# as bases vão ser ocorrências, crimes e carros.
# dentro de cada base, escolher colunas e remover duplicações
# juntar as bases pelas chaves correspondentes

# Ocorrências

ocorrencias <- base_nomes_arrumados_preenchida %>%
  dplyr::select(ano_bo:delegacia_circunscricao,
                placa_veiculo:ano_fabricacao) %>%
  dplyr::distinct()

ocorrencias %>% tibble::view()

# Veículos

veiculos <- base_nomes_arrumados_preenchida %>%
  dplyr::select(placa_veiculo:ano_fabricacao) %>%
  dplyr::distinct()

veiculos %>% tibble::view()

# Crimes

crimes_passo1 <- base_nomes_arrumados_preenchida %>%
  dplyr::select(num_bo, ano_bo, delegacia_nome, delegacia_circunscricao,
                especie:desdobramento) %>%
  dplyr::distinct() %>%
  # Cria uma nova coluna e une a info de outras colunas:
  tidyr::unite(crime_completo, especie, rubrica, desdobramento, sep = " @@@ ")

crimes_passo1 %>% tibble::view()

# Preciso garantir 1 conjunto de crimes só por ecorrência:
# Opção A - sumarização

crimes_passo2a <- crimes_passo1 %>%
  dplyr::group_by(num_bo, ano_bo, delegacia_nome, delegacia_circunscricao) %>%
  dplyr::summarise(
    todos_os_crimes = paste0(crime_completo, collapse = ", ")
  ) %>%
  dplyr::ungroup()

crimes_passo2a %>% tibble::view()

# Opção B - pivot_wider, neste caso não foi uma boa opção.

crimes_passo2b <- crimes_passo1 %>%
  dplyr::group_by(num_bo, ano_bo, delegacia_nome, delegacia_circunscricao) %>%
  dplyr::mutate(
    nome_da_coluna = paste0("crime_", 1:n())
  ) %>%
  tidyr::pivot_wider(names_from = nome_da_coluna, values_from = crime_completo)

crimes_passo2b %>% tibble::view()

#Opção C - nest: transformar as linhas da tabela de crimes em uma list-column

crimes_passo2c <- crimes_passo1 %>%
  dplyr::group_by(num_bo, ano_bo, delegacia_nome, delegacia_circunscricao) %>%
  tidyr::nest()

crimes_passo2c$data[[1]] %>% tibble::view()


# A info de crimes vira uma tibble


# Construção da Tabela Final--------------------------------------------------


base_final_tidy <- veiculos %>%
  dplyr::left_join(ocorrencias, by = c('placa_veiculo', 'uf_veiculo', 'cidade_veiculo',
                                       'descr_cor_veiculo', 'descr_marca_veiculo',
                                       'ano_fabricacao')) %>%
  dplyr::left_join(crimes_passo2c)

base_final_tidy %>% tibble::view()


# Limpeza de repetições e finalização -----------------------------------------


veiculos %>%
  left_join(ocorrencias) # resulta em 3151 obs; repetição de carro em ocorrências


ocorrencias %>%
  count(across(placa_veiculo:ano_fabricacao)) %>%
  count(n) # retorna o número de repetições



  
  


