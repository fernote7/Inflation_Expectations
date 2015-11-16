## Carregando pacotes
require(chron)
require(lubridate)
require(e1071)
require(dummies)
require(plyr)
require(MASS)
require(dynlm)
require(car)
require(xtable)

setwd(direc)

## Lendo os microdados
  microdados_modelos = read.csv2("Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")

  microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                       format = "d/m/y", out.format = "d/m/y")
  microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])

# Obtendo IPCA de um mês passado
  inflacao_def = unique(microdados_modelos[,c("Mes_Ano", "IPCA")])
  inflacao_def[2:nrow(inflacao_def),"IPCA_lag2"] = inflacao_def[1:(nrow(inflacao_def) - 1),"IPCA"]
  inflacao_def = inflacao_def[,c("Mes_Ano", "IPCA_lag2")]
  microdados_modelos = merge(microdados_modelos, inflacao_def)

##Lendo Dados Twitter
  tweet=read.csv2(paste0(direc,"\\Dados\\Twitter\\base\\tweetglobo.csv"))
  colnames(tweet) <- c("Mes_Ano", "ano", "mes", "contagem")
  tweet = tweet[1:53,]
  tweet[,"Mes_Ano"] = chron(as.character(tweet[,"Mes_Ano"]),
                            format = "d/m/y", out.format = "d/m/y")
  keeps <- c("Mes_Ano","contagem")
  tweet=tweet[keeps]
  microdados_modelos = merge(microdados_modelos, tweet)

##Montando Base
  microdados_modelo1 = microdados_modelos[complete.cases(microdados_modelos),
                                          c("Mes_Ano","Resposta", 
                                            "Renda_2", "Renda_3", "Renda_4",
                                            "Escolaridade_2", "Escolaridade_3", "Escolaridade_4", 
                                            "Cidade_2", "Cidade_3", "Cidade_4", "Cidade_5", "Cidade_6", "Cidade_7", 
                                            "Idade_2", "Idade_3", "Idade_4", 
                                            "Sexo_2", 
                                            "Pergunta_1177_2", "Pergunta_1177_3", 
                                            "Pergunta_1178_2", "Pergunta_1178_3", 
                                            "Pergunta_1147_2", "Pergunta_1147_3", 
                                            "Pergunta_1149_2", "Pergunta_1149_3", 
                                            "Pergunta_1182_2", "Pergunta_1182_3", 
                                            "Pergunta_1183_2", "Pergunta_1183_3",
                                            "Pergunta_1189_2", "Pergunta_1189_3",
                                            "Pergunta_1194_2", "Pergunta_1194_3", 
                                            "IPCA", "IPCA_lag2", "Previsao_Focus", "Learn_4em8", "contagem")]
  
