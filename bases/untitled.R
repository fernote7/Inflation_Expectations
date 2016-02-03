## Carregando funções necessárias
nomes = list("ingrid.oliveira", "fernando.teixeira")
for (nome in nomes)
{
  direc = paste0("C:\\Users\\", nome, "\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\")
  try(setwd(direc), silent = TRUE)
  
}
source(paste0(direc, "\\Funcoes\\Modelos\\bases\\base_modelo_tweet.R"), encoding = c("utf8"))
##
##Carregando Pacotes
require(corrgram)


codace=read.csv2("Dados\\Microdados_Filtrados\\codace.csv")
folha = read.csv2("Dados\\Twitter\\twitter_cont\\folha\\variavel_midia_folha.csv")
controlados = read.csv("Dados\\Microdados_Filtrados\\IPCA\\preco_monitorado.csv")



