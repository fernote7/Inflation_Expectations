## Carregando funções necessárias
nomes = list("ingrid.oliveira", "fernando.teixeira")
for (nome in nomes)
{
  direc = paste0("C:\\Users\\", nome, "\\Dropbox\\10 Expectativas de infla��o - Brasil\\ProgramasTD64\\")
  try(setwd(direc), silent = TRUE)
  
}
# source("Funcoes\\Modelos\\TD64_Modelos.R")


## Pacotes necessários
# require(dynlm)
# require(quantmod)
# require(tseries)
require(chron)
require(lubridate)
# require(xtable)
# require(plyr)


## Importando dados
  
  # MICRODADOS
  microdados_modelos = read.csv2("Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
  microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                         format = "d/m/y", out.format = "d/m/y")
  microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])
  dados = microdados_modelos
  microdados_modelos2 = read.csv2("Dados\\Microdados_Filtrados\\IPCA\\Pasta3.csv")
  microdados_modelos2[,"Mes_Ano"] = chron(as.character(microdados_modelos2[,"Mes_Ano"]),
                                          format = "d/m/y", out.format = "d/m/y")

  # TWITTER
  #   tweet=read.csv2("C:/Users/ingrid.oliveira/Dropbox/10 Expectativas de inflação - Brasil/ProgramasTD64/Dados/Twitter/tweetglobo.csv")
  #   colnames(tweet) <- c("Mes_Ano", "ano", "mes", "contagem")
  #   tweet = tweet[1:53,]
  #   tweet[,"Mes_Ano"] = chron(as.character(tweet[,"Mes_Ano"]),
  #                            format = "d/m/y", out.format = "d/m/y")
  #   keeps <- c("Mes_Ano","contagem")
  #   tweet=tweet[keeps]
  #   microdados_modelos = merge(microdados_modelos, tweet)

  
## Expectativa de inflação mensal
  aggdata <-aggregate(microdados_modelos$Resposta, 
                      by=list(microdados_modelos$Mes_Ano,
                              microdados_modelos$Cidade, 
                              microdados_modelos$Renda), 
                      FUN=mean, na.rm=TRUE)
  colnames(aggdata) = c("Mes_Ano", "Cidade", "Renda", "Resp_Agg")

  # São Paulo
  aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 1),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 1),"Resp_Agg"]*0.1033
  aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 2),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 2),"Resp_Agg"]*0.1058
  aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 3),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 3),"Resp_Agg"]*0.1008
  aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 4),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 4),"Resp_Agg"]*0.1179

  # Rio de Janeiro
  aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 1),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 1),"Resp_Agg"]*0.0626
  aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 2),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 2),"Resp_Agg"]*0.0688
  aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 3),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 3),"Resp_Agg"]*0.0651
  aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 4),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 4),"Resp_Agg"]*0.0626

  # Belo Horizonte
  aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 1),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 1),"Resp_Agg"]*0.0160
  aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 2),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 2),"Resp_Agg"]*0.0164
  aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 3),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 3),"Resp_Agg"]*0.0164
  aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 4),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 4),"Resp_Agg"]*0.0172

  # Brasília
  aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 1),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 1),"Resp_Agg"]*0.0224
  aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 2),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 2),"Resp_Agg"]*0.0257
  aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 3),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 3),"Resp_Agg"]*0.0311
  aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 4),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 4),"Resp_Agg"]*0.0328
  
  # Salvador
  aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 1),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 1),"Resp_Agg"]*0.0179
  aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 2),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 2),"Resp_Agg"]*0.0137
  aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 3),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 3),"Resp_Agg"]*0.0116
  aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 4),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 4),"Resp_Agg"]*0.0100
  
  # Porto Alegre
  aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 1),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 1),"Resp_Agg"]*0.0116
  aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 2),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 2),"Resp_Agg"]*0.0137
  aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 3),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 3),"Resp_Agg"]*0.0145
  aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 4),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 4),"Resp_Agg"]*0.0145
  
  # Recife
  aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 1),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 1),"Resp_Agg"]*0.0071
  aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 2),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 2),"Resp_Agg"]*0.0071
  aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 3),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 3),"Resp_Agg"]*0.0067
  aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 4),"Resp_Agg"] =
    aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 4),"Resp_Agg"]*0.0067
  
  # Agregando por mês
  expinf_mensal = aggregate(aggdata[,"Resp_Agg"], by = list(aggdata[,"Mes_Ano"]),
                            FUN = sum, na.rm = TRUE)
 
  
## Ajuste do modelo em duas partes
  
  # Parte 1 - Variáveis
  IPCA = aggregate(microdados_modelos[,"IPCA"], 
                   by = list(microdados_modelos[,"Mes_Ano"]),
                   FUN = mean, na.rm = TRUE)
  Focus = aggregate(microdados_modelos[,"Previsao_Focus"], 
                    by = list(microdados_modelos[,"Mes_Ano"]),
                    FUN = mean, na.rm = TRUE)
  Meta = aggregate(microdados_modelos[,"Meta"], 
                   by = list(microdados_modelos[,"Mes_Ano"]),
                   FUN = mean, na.rm = TRUE)
  # Tweet = aggregate(microdados_modelos[,"Tweet"], 
  #                   by = list(microdados_modelos[,"Mes_Ano"]),
  #                   FUN = mean, na.rm = TRUE)
  dados_parte1 = data.frame(Mes_Ano = expinf_mensal[-1,"Group.1"],
                            Resposta = expinf_mensal[-1,"x"],
                            IPCA = IPCA[-nrow(expinf_mensal),"x"],
                            Focus = Focus[-1,"x"],
                            Meta = Meta[-1,"x"])
  
  # Parte 1 - Regressão (out/05 a dez/13)
  # ajuste_parte1 = lm(Resposta ~ IPCA + Focus + Meta - 1, data = dados_parte1)
  # res_parte1 = ajuste_parte1$residuals
  ajuste_parte1 = lm(Resposta ~ IPCA + Focus - 1, data = dados_parte1)
  res_parte1 = ajuste_parte1$residuals
  
  
  # Parte 2 - Variáveis
  dados_parte2 = data.frame(Mes_Ano = dados_parte1[-1,"Mes_Ano"],
                            dif_Resposta = diff(dados_parte1[,"Resposta"],1),
                            dif_IPCA = diff(dados_parte1[,"IPCA"],1),
                            dif_Focus = diff(dados_parte1[,"Focus"],1),
                            dif_Meta = diff(dados_parte1[,"Meta"],1),
                            resid_parte1 = res_parte1[-length(res_parte1)])
  
  
  # Parte 2 - Correção de erro em t (dez/05 a dez/13)
  ajuste_parte2 = lm(dif_Resposta ~ dif_IPCA + dif_Focus + resid_parte1 - 1,
                     data = dados_parte2)
  

## ADL
dados_adl = data.frame(dados_parte2, dados_parte1[-nrow(dados_parte1),])
dados_adl = dados_adl[,-(6:7)]
# dados_adl[,"IPCA"] = -dados_adl[,"IPCA"]
# dados_adl[,"Focus"] = -dados_adl[,"Focus"]

ajuste_adl = lm(dif_Resposta ~ dif_IPCA + dif_Focus + Resposta +
                               IPCA + Focus - 1, data = dados_adl)

