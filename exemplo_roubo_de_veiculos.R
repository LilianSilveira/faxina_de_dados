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

#Com a versão mais recente do readr, funciona.
#Outra opção que funciona neste caso:

base_bruta <- read.delim("dados/dados_bo_2021_roubodeveiculos_completa.xls",
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
#Ou tentar entender a base para achar a chave.
#Neste caso, existe documentação para consultar.

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

# Parece que a coluna status também gera as repetições

base_bruta %>%
  count(ANO_BO, NUM_BO,DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO, STATUS, ESPECIE, RUBRICA) %>%
  filter(n > 1)


#Ainda há repetições. Vamos descobrir o motivo:

base_bruta %>%
  filter(ANO_BO == 2021, NUM_BO == 1692, DELEGACIA_NOME == "69º D.P. TEOTONIO VILELA") %>%
  tibble::view()

# Parece que a coluna status também gera as repetições

base_bruta %>%
  count(ANO_BO, NUM_BO,DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO, DESCR_MARCA_VEICULO, DESCR_COR_VEICULO, STATUS, ESPECIE, RUBRICA, DESDOBRAMENTO) %>%
  filter(n > 1)

#Ainda há repetições. Vamos descobrir o motivo:

base_bruta %>%
  filter(ANO_BO == 2021, NUM_BO == 437009, DELEGACIA_NOME == "DELEGACIA ELETRONICA") %>%
  tibble::view()

# Parece que a coluna status também gera as repetições

base_brut5a %>%
  count(ANO_BO, NUM_BO,DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO, DESCR_MARCA_VEICULO, DESCR_COR_VEICULO, CIDADE_VEICULO, STATUS, ESPECIE, RUBRICA, DESDOBRAMENTO) %>%
  filter(n > 1)

#Agora encontramos a chave e não há mais repetições. Vamos usar carro como unidade.


# Arrumar o nome das colunas ----------------------------------------------

base_nomes_arrumados <- 
