## Carregando funções necessárias
nomes = list("ingrid.oliveira", "fernando.teixeira")
for (nome in nomes)
{
  direc = paste0("C:\\Users\\", nome, "\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\")
  try(setwd(direc), silent = TRUE)
  
}
rm(nome, nomes, direc)
# source("Funcoes\\Modelos\\TD64_Modelos.R")


## Pacotes necessários
remove.packages("MSBVAR")
# require(dynlm)
# require(quantmod)
# require(tseries)
require(chron)
require(lubridate)
# require(xtable)
# require(plyr)
require(xts)
require(urca)
require(tseries)


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
  expinf_cons = ts(expinf_mensal$x, start = c(2005, 09), freq = 12)
  plot(expinf_cons, ylab = "% acumulado em 12 meses", xlab = "", 
       # main = "Índice de Preços ao Consumidor Amplo", 
       lty = "dashed", col = "darkblue", lwd = 1.6)
  
  
## Covariáveis

  # Índice de Preços ao Consumidor Amplo (IBGE)
  IPCA = aggregate(microdados_modelos[,"IPCA"], 
                   by = list(microdados_modelos[,"Mes_Ano"]),
                   FUN = mean, na.rm = TRUE)
  ipca = ts(IPCA$x, start = c(2005, 09), freq = 12)
  plot(ipca, ylab = "% acumulado em 12 meses", xlab = "", 
       # main = "Índice de Preços ao Consumidor Amplo", 
       lty = "dashed", col = "darkblue", lwd = 1.6)

  # Expectativa de Inflação do Mercado (Relatório Focus)
  Focus = aggregate(microdados_modelos[,"Previsao_Focus"], 
                    by = list(microdados_modelos[,"Mes_Ano"]),
                    FUN = mean, na.rm = TRUE)
  focus = ts(Focus$x, start = c(2005, 09), freq = 12)
  plot(focus, ylab = "% acumulado em 12 meses", xlab = "", 
       # main = "Previsão Focus", 
       lty = "dashed", col = "darkblue", lwd = 1.6)
  
  # Centro da Meta de Inflação
  Meta = aggregate(microdados_modelos[,"Meta"], 
                   by = list(microdados_modelos[,"Mes_Ano"]),
                   FUN = mean, na.rm = TRUE)
  meta = ts(Meta$x, start = c(2005, 09), freq = 12)
  # plot(meta, ylab = "% acumulado em 12 meses", xlab = "", 
  #      main = "Meta de Inflação", lty = "dashed", 
  #      col = "darkblue", lwd = 1.6)
  
  dados = xts(data.frame(Exp_Cons = round(expinf_mensal$x,2), IPCA = IPCA$x, Focus = Focus$x), 
              order.by = seq(as.Date("2005/09/01"), as.Date("2013/12/01"), by = "month"),
              frequency = 12)
  
## Testando a estacionariedade das séries
  
  # Expectativa de Inflação
  adf_expinf = ur.df(expinf_cons, type = "none", lags = 13, selectlags = "AIC"); summary(adf_expinf) 
  # adf_expinf = ur.df(expinf_cons, type = "none", lags = 1); summary(adf_expinf)
  acf(adf_expinf@res)

  adf2_expinf = ur.df(expinf_cons, type = "trend", lags = 13, selectlags = "AIC"); summary(adf2_expinf)
  # adf2_expinf = ur.df(expinf_cons, type = "trend", lags = 0); summary(adf2_expinf)
  acf(adf2_expinf@res)
  
  adf3_expinf = ur.df(expinf_cons, type = "drift", lags = 36, selectlags = "AIC"); summary(adf3_expinf)
  acf(adf3_expinf@res)
  
  adf4_expinf = adf.test(expinf_cons, alternative = "stationary"); adf4_expinf
  kpss_expinf <- ur.kpss(expinf_cons, type="mu", lags="short"); summary(kpss_expinf)
  
  # PP.test(expinf_cons, lshort = TRUE)
  
  # tendencia_expinf = seq(1, length(expinf_cons), by = 1)
  # ajuste_expinf = lm(expinf_cons ~ tendencia_expinf); summary(ajuste_expinf)
  # res_ajusteexpinf = ajuste_expinf$residuals; plot(res_ajusteexpinf, t="l")
  # adf_resexpinf = ur.df(res_ajusteexpinf, type = "none", lags = 0); summary(adf_resexpinf)
  # acf(adf_resexpinf@res)
  
  # IPCA
  adf_ipca = ur.df(ipca, type = "none", lags = 36, selectlags = "AIC"); summary(adf_ipca) 
  # adf_ipca = ur.df(ipca, type = "none", lags = 1); summary(adf_ipca)
  acf(adf_ipca@res)
  
  adf2_ipca = ur.df(ipca, type = "trend", lags = 36, selectlags = "AIC"); summary(adf2_ipca)
  # adf2_ipca = ur.df(ipca, type = "trend", lags = 0); summary(adf2_ipca)
  acf(adf2_ipca@res)
  
  adf3_ipca = ur.df(ipca, type = "drift", lags = 36, selectlags = "AIC"); summary(adf3_ipca)
  acf(adf3_ipca@res)
  
  adf4_ipca = adf.test(ipca, alternative = "stationary"); adf4_ipca
  kpss_ipca <- ur.kpss(ipca, type="tau", lags="short"); summary(kpss_ipca)
  
  tendencia_ipca = seq(1, length(ipca), by = 1)
  ajuste_ipca = lm(ipca ~ tendencia_ipca); summary(ajuste_ipca)
  res_ajusteipca = ajuste_ipca$residuals; plot(res_ajusteipca)
  adf_resipca = ur.df(res_ajusteipca, type = "none", lags = 1); summary(adf_resipca)

  # Focus
  adf_focus = ur.df(focus, type = "none", lags = 36, selectlags = "AIC"); summary(adf_focus) 
  # adf_focus = ur.df(focus, type = "none", lags = 1); summary(adf_focus)
  acf(adf_focus@res)
  
  adf2_focus = ur.df(focus, type = "trend", lags = 36, selectlags = "AIC"); summary(adf2_focus)
  # adf2_focus = ur.df(focus, type = "trend", lags = 0); summary(adf2_focus)
  acf(adf2_focus@res)
  
  adf3_focus = ur.df(focus, type = "drift", lags = 36, selectlags = "AIC"); summary(adf3_focus)
  acf(adf3_focus@res)
  
  adf4_focus = adf.test(focus, alternative = "stationary"); adf4_focus
  kpss_focus <- ur.kpss(focus, type="tau", lags="short"); summary(kpss_focus)
  
  tendencia_focus = seq(1, length(focus), by = 1)
  ajuste_focus = lm(focus ~ tendencia_focus); summary(ajuste_focus)
  res_ajustefocus = ajuste_focus$residuals; plot(res_ajustefocus, t="l")
  adf_ajustefocus = ur.df(res_ajustefocus, type = "none", lags = 2); summary(adf_ajustefocus)


## Testando se as séries são cointegradas

  # Expectativa de Inflação x IPCA
  ajuste_coin1 = lm(expinf_cons ~ ipca); summary(ajuste_coin1)
  adf_coin1 = ur.df(ajuste_coin1$residuals, "none", lags = 12, selectlags = "AIC")
  summary(adf_coin1)
  acf(adf_coin1@res)
  
  # Expectativa de Inflação x Focus
  ajuste_coin2 = lm(expinf_cons ~ focus); summary(ajuste_coin2)
  adf_coin2 = ur.df(ajuste_coin2$residuals, "none", lags = 36, selectlags = "AIC")
  summary(adf_coin2)
  
  # Expectativa de Inflação x IPCA + Focus
  ajuste_coin3 = lm(expinf_cons ~ ipca + focus); summary(ajuste_coin3)
  adf_coin3 = ur.df(ajuste_coin3$residuals, "none", lags = 36, selectlags = "AIC")
  summary(adf_coin3)
  kpss_coin3 = ur.kpss(ajuste_coin3$residuals, type = "mu", lags = "short")
  summary(kpss_coin3)
  
    
## Testando causalidade de Granger entre séries -
## A hipótese nula é a de que X1 não causa X2, ou seja, um p-valor pequeno
## (< 0.05, considerando 5% de significância) significa que a hipótese nula
## é rejeitada e X1 causa X2
  
  # IPCA, Focus e Expectativa de Inflação
  install.packages("MSBVAR")
  require(MSBVAR)
  dados = cbind(expinf_cons, ipca, focus)
  granger.test(dados, p = 1)
  

## Cointegração
  
  require(egcm)
  teste_coin = egcm(expinf_mensal$x, IPCA$x)
  

## Estimação do modelo
  
  require(dynlm)
  
  # Procedimento em duas etapas
  reg1 = lm(expinf_mensal$x ~ IPCA$x - 1)
  res = ts(reg1$residuals, start = c(2005,09), freq = 12)
  reg2 = dynlm(d(expinf_cons, 1) ~ d(ipca, 1) + L(res, 1) -1 )
  
  # Procedimento em única etapa
  reg = dynlm(d(expinf_cons, 1) ~ d(ipca, 1) + L(expinf_cons, 1) +
                                  L(ipca, 1) -1)
  
  