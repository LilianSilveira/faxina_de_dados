##################################################################################
#                       CURSO FAXINA DE DADOS - CURSO R                          #
##################################################################################

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

#Identificamos repetição de dados e outros problemas
#No caso das repetições, o que não está repetido? Parei 2h:28m









