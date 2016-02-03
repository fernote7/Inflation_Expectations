nomes = list("ingrid.oliveira", "fernando.teixeira")
for (nome in nomes)
{
  direc = paste0("C:\\Users\\", nome, "\\Dropbox\\10 Expectativas de infla��o - Brasil\\ProgramasTD64\\")
  try(setwd(direc), silent = TRUE)
  
}
#source("Funcoes\\TD64_Modelos.R")
require(dynlm)
require(quantmod)
require(tseries)
require(chron)
require(lubridate)
require(xtable)
require(plyr)

###Importando dados

#MICRODADOS
  microdados_modelos = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\Microdados_Modelos_dez05aset15.csv")
  #microdados_modelos = read.csv2("C:\\Users\\ingrid.oliveira\\Dropbox\\10 Expectativas de inflaÇÃo - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
  microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                         format = "d/m/y", out.format = "d/m/y")
  microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])
  dados = microdados_modelos
  microdados_modelos2 = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\IPCA\\Pasta3.csv")
  microdados_modelos2[,"Mes_Ano"] = chron(as.character(microdados_modelos2[,"Mes_Ano"]),
                                          format = "d/m/y", out.format = "d/m/y")
#TWITTER
  tweet=read.csv2("C:/Users/fernando.teixeira/Dropbox/10 Expectativas de inflação - Brasil/ProgramasTD64/Dados/Twitter/tweetglobo.csv")
  colnames(tweet) <- c("Mes_Ano", "ano", "mes", "contagem")
  tweet = tweet[1:72,]
  tweet[,"Mes_Ano"] = chron(as.character(tweet[,"Mes_Ano"]),
                            format = "d/m/y", out.format = "d/m/y")
  keeps <- c("Mes_Ano","contagem")
  tweet=tweet[keeps]
  microdados_modelos = merge(microdados_modelos, tweet)
  
###dados do IPCA desagregado
  #microdados_modelos2=microdados_modelos2[1:100,]


###CRIANDO VARIÁVEIS DE INFLACAO
  inflacao_def2 = unique(microdados_modelos[,c("Mes_Ano", "Previsao_Focus")])
  inflacao_def2[2:nrow(inflacao_def2),"PF_lag1"] = inflacao_def2[1:(nrow(inflacao_def2) - 1),"Previsao_Focus"]
  inflacao_def2 = inflacao_def2[,c("Mes_Ano", "PF_lag1")]
  
  microdados_modelos = merge(microdados_modelos, inflacao_def2)

  inflacao_def4 = unique(microdados_modelos[,c("Mes_Ano", "IPCA")])
  inflacao_def4[2:nrow(inflacao_def4),"IPCA_lag1"] = inflacao_def4[1:(nrow(inflacao_def4) - 1),"IPCA"]
  inflacao_def4 = inflacao_def4[,c("Mes_Ano", "IPCA_lag1")]
###

###AGREGANDO DADOS POR CIDADE
  aggdata <-aggregate(microdados_modelos$Resposta, by=list(microdados_modelos$Cidade, microdados_modelos$Mes_Ano), 
                      FUN=mean, na.rm=TRUE)
  aggdata$x[aggdata$Group.1 == 1] = aggdata$x[aggdata$Group.1 == 1 ]*0.4278
  aggdata$x[aggdata$Group.1 == 2] = aggdata$x[aggdata$Group.1 == 2]*0.2591
  aggdata$x[aggdata$Group.1 == 3] = aggdata$x[aggdata$Group.1 == 3]*0.0660
  aggdata$x[aggdata$Group.1 == 4] = aggdata$x[aggdata$Group.1 == 4]*0.1120
  aggdata$x[aggdata$Group.1 == 5] = aggdata$x[aggdata$Group.1 == 5]*0.0532
  aggdata$x[aggdata$Group.1 == 6] = aggdata$x[aggdata$Group.1 == 6]*0.0543
  aggdata$x[aggdata$Group.1 == 7] = aggdata$x[aggdata$Group.1 == 7]*0.0276
###

###AGREGANDO DADOS RELEVANTES PARA MODELOS ADL
  dados = microdados_modelos
  classes1 = c("Mes_Ano", "Cidade", "Renda", "IPCA", "Previsao_Focus", "Meta", "contagem")
  expinf1_mes = aggregate(dados[,"Resposta"], dados[,classes1], mean)
  #expinf1_mes = aggregate(x = microdados_modelos$Resposta, by = list(microdados_modelos$Mes_Ano, microdados_modelos$Cidade, microdados_modelos$Renda), FUN = "mean")
  expinf1_mes = expinf1_mes[order(expinf1_mes$Cidade, expinf1_mes$Renda, expinf1_mes$Mes_Ano),]
  colnames(expinf1_mes) = c("Mes_Ano", "Cidade", "Renda", "IPCA", "Previsao_Focus", "Meta","Tweets" , "Resposta")
  aggdata2 <- aggregate(aggdata$x, by=list(aggdata$Group.2), 
                        FUN=sum, na.rm=TRUE)
  aggdata3 = aggregate(expinf1_mes$IPCA, by=list(expinf1_mes$Mes_Ano), 
                       FUN=mean, na.rm=TRUE)
  aggdata4 = aggregate(expinf1_mes$Previsao_Focus, by=list(expinf1_mes$Mes_Ano), 
                       FUN=mean, na.rm=TRUE)
  aggdata5 = aggregate(expinf1_mes$Meta, by=list(expinf1_mes$Mes_Ano), 
                       FUN=mean, na.rm=TRUE)
  aggdata6 = as.data.frame(unique(expinf1_mes$Mes_Ano))
  aggdata7 = aggregate(expinf1_mes$Tweets, by=list(expinf1_mes$Mes_Ano), 
                       FUN=mean, na.rm=TRUE)
  aggdata<-cbind(aggdata2$x,aggdata3$x,aggdata4$x, aggdata5$x, aggdata7$x)
  aggdata <- as.data.frame(aggdata)
  aggdata$V4 = as.numeric(aggdata$V4)
  aggdata$Data <- aggdata6[,1]
  colnames(aggdata) = c("Resposta","IPCA","Previsao_Focus", "Meta", "Tweets", "Mes_Ano")
  rm(aggdata3,aggdata4,aggdata5,aggdata6, aggdata7, aggdata2)
###

###MONTAGEM VARIÁVEIS MODELO ADL COM TWITTER
  aggdata$Meta = -aggdata$Meta
  aggdata2 = ts(aggdata)
  
  fit=dynlm(d(Resposta,1) ~ 0 + d(L(IPCA,1),1)+d(Previsao_Focus,1)+L(Resposta,1)+L(-IPCA,2)+L(-Previsao_Focus,1)+Meta, data = aggdata2)
  summary(fit)  
  fit2=dynlm(d(Resposta,1) ~ 0 + d(L(IPCA,1),1)+d(Previsao_Focus,1)+d(Tweets,1)+L(Resposta,1)+L(-IPCA,2)+L(-Previsao_Focus,1)+L(-Tweets,1)+Meta, data = aggdata2)
  summary(fit2)
  rm(fit, fit2)
###  
  
###IMPORTANDO RENDA AGREGADA  
  renda_agregada = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\IPCA\\RA.csv", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), header = TRUE, dec = ".")
###  

###MODELO RENDA AGREGADA
  #modelo 1
    renda_agregada=renda_agregada[48:100,]
    aggdata$R1 = renda_agregada$R1
    aggdata$R2 = renda_agregada$R2
    aggdata$R3 = renda_agregada$R3
    aggdata$R4 = renda_agregada$R4
    aggdata$Resposta2= renda_agregada$Brasil
    
    microdados_modelos2 = microdados_modelos2[48:100,]
    aggdata$Alimentação.e.bebidas=microdados_modelos2$Alimentação.e.bebidas
    aggdata$Habitação=microdados_modelos2$Habitação
    aggdata$Artigos.de.residência=microdados_modelos2$Artigos.de.residência
    aggdata$Vestuário=microdados_modelos2$Vestuário
    aggdata$Transportes=microdados_modelos2$Transportes
    aggdata$Saúde.e.cuidados.pessoais=microdados_modelos2$Saúde.e.cuidados.pessoais
    aggdata$Despesas.pessoais=microdados_modelos2$Despesas.pessoais
    aggdata$Educação=microdados_modelos2$Educação
    aggdata$Comunicação=microdados_modelos2$Comunicação
      
    aggdata2 = ts(aggdata)
    fit=dynlm(d(Resposta2,1) ~ 0 + d(L(IPCA,1),1)+d(Previsao_Focus,1)+d(Tweets,1)+L(Resposta2,1)+L(-IPCA,2)+L(-Previsao_Focus,1)+L(-Tweets,1)+Meta, data = aggdata2)
    summary(fit)
    
  #modelo 2
    ###teste utilizando resíduos da regressão
    
    ra =  Lag(aggdata[,1],1)
    ia = -Lag(aggdata[,2],2)
    fa = -Lag(aggdata[,3],1)
    ma = -Lag(aggdata[,4],1)
    ta = -Lag(aggdata[,5],1)
    
    mod=lm(ra~ia+fa+ma+ta)
    resid=mod$residuals
    resid=ts(resid)
    fit=dynlm(d(Resposta) ~ 0 + d(L(IPCA,1),1)+d(Previsao_Focus,1)+resid, data = aggdata2)
    summary(fit)
    
    adf.test(resid)
    adf.test(resid, k=10)
    adf.test(fit$residuals)
    rm(fa,ia,ra,ta,resid,keeps,ma)
###  


  
  
  
  
    
  
###MODELO MES-RENDA BASE  
  #expinf1_mes = aggregate(x = microdados_modelos$Resposta, by = list(microdados_modelos$Mes_Ano, microdados_modelos$Cidade, microdados_modelos$Renda), FUN = "mean")
  expinf1_mes1  = expinf1_mes
  expinf1_mes1 = expinf1_mes1[order(expinf1_mes1$Cidade, expinf1_mes1$Renda, expinf1_mes1$Mes_Ano),]
  expinf1_mes1$Resposta[expinf1_mes1$Cidade == 1] = expinf1_mes1$Resposta[expinf1_mes1$Cidade == 1]*0.4278
  expinf1_mes1$Resposta[expinf1_mes1$Cidade == 2] = expinf1_mes1$Resposta[expinf1_mes1$Cidade == 2]*0.2591
  expinf1_mes1$Resposta[expinf1_mes1$Cidade == 3] = expinf1_mes1$Resposta[expinf1_mes1$Cidade == 3]*0.0660
  expinf1_mes1$Resposta[expinf1_mes1$Cidade == 4] = expinf1_mes1$Resposta[expinf1_mes1$Cidade == 4]*0.1120
  expinf1_mes1$Resposta[expinf1_mes1$Cidade == 5] = expinf1_mes1$Resposta[expinf1_mes1$Cidade == 5]*0.0532
  expinf1_mes1$Resposta[expinf1_mes1$Cidade == 6] = expinf1_mes1$Resposta[expinf1_mes1$Cidade == 6]*0.0543
  expinf1_mes1$Resposta[expinf1_mes1$Cidade == 7] = expinf1_mes1$Resposta[expinf1_mes1$Cidade == 7]*0.0276
  
  teste = aggregate(expinf1_mes1$Resposta,
                    by=list(expinf1_mes1$Mes_Ano, expinf1_mes1$Renda), 
                    FUN=sum, na.rm=TRUE)
  teste2 = aggregate(expinf1_mes1$Meta,
                    by=list(expinf1_mes1$Mes_Ano, expinf1_mes1$Renda), 
                    FUN=mean, na.rm=TRUE)
  teste3 = aggregate(expinf1_mes1$Previsao_Focus,
                    by=list(expinf1_mes1$Mes_Ano, expinf1_mes1$Renda), 
                    FUN=mean, na.rm=TRUE)
  teste4 = aggregate(expinf1_mes1$IPCA,
                     by=list(expinf1_mes1$Mes_Ano, expinf1_mes1$Renda), 
                     FUN=mean, na.rm=TRUE)
  colnames(teste)=c("Mes_Ano","Renda", "Resposta")
  colnames(teste2)=c("Mes_Ano","Renda", "Meta")
  colnames(teste3)=c("Mes_Ano","Renda", "Previsao_Focus")
  colnames(teste4)=c("Mes_Ano","Renda", "IPCA")


###variável para modelo mes-renda
  agg_mesano=merge(teste,teste2, by=c("Mes_Ano","Renda"))
  agg_mesano=merge(agg_mesano,teste3, by=c("Mes_Ano","Renda"))
  agg_mesano=merge(agg_mesano,teste4, by=c("Mes_Ano","Renda"))
  rm(teste, teste2, teste3, teste4, classes1)

###

###DUMMIES RENDA
  dummies_renda = as.matrix(table(1:length(agg_mesano[,"Renda"]),
                                  as.factor(agg_mesano[,"Renda"])))
  dummies_renda[which(dummies_renda[,1] == 0 & dummies_renda[,2] == 0 &
                        dummies_renda[,3] == 0 & dummies_renda[,4] == 0),] = rep(NA, 4)
  agg_mesano2 = as.matrix(agg_mesano)
  agg_mesano2 = as.data.frame(cbind(agg_mesano2,dummies_renda))
  agg_mesano2 = agg_mesano2[,-7]
  agg_mesano2$Mes_Ano = as.Date(agg_mesano2$Mes_Ano)
  colnames(agg_mesano2) = c(colnames(agg_mesano),
                            paste0("Renda_",2:4))
  
  agg_mesano2$Mes_Ano = agg_mesano$Mes_Ano
  agg_mesano2 = join(as.data.frame(aggdata[,5:6]),agg_mesano2,type="right")
  agg_mesano3 = ts(agg_mesano2, frequency =4)
###

###MODELO MES-RENDA

  fit=dynlm(d(Resposta,1) ~ 0 + Renda_2 + Renda_3 + Renda_4 + d(L(IPCA,1),1)+d(Previsao_Focus,1)+d(Tweets,1)+L(Resposta,1)+L(-IPCA,2)+L(-Previsao_Focus,1)+L(-Tweets,1)+Meta, data = agg_mesano3)
  summary(fit)

###

###TESTANDO
  #IPCA SEPARADO
    teste=dynlm(d(Resposta,1) ~ 0 + d(L(Alimentação.e.bebidas,1),1)+ d(L(Habitação,1),1)+ d(L(Artigos.de.residência,1),1)+
                                    d(L(Vestuário,1),1)+ d(L(Transportes,1),1)+ d(L(Saúde.e.cuidados.pessoais,1),1)+
                                    d(L(Despesas.pessoais,1),1)+ d(L(Educação,1),1)+ d(L(Comunicação,1),1)+
                  d(Previsao_Focus,1)+ d(Tweets,1)+ L(Resposta,1)+
                  L(-Alimentação.e.bebidas,2)+ L(-Habitação,2)+ L(-Artigos.de.residência,2)+ 
                  L(-Vestuário,2)+ L(-Transportes,2)+ L(-Saúde.e.cuidados.pessoais,2)+ 
                  L(-Despesas.pessoais,2)+ L(-Educação,2)+ L(-Comunicação,2)+ 
                  L(-Previsao_Focus,1)+ L(-Tweets,1) + Meta, data = aggdata2)
    summary(teste)  
  #  

  #Modelo LOG-LOG
    aggdata2[,4] = log(-aggdata[,4]) 
    aggdata2[,4] = - aggdata2[,4]
    teste2=dynlm(d(log(Resposta),1) ~ 0 + d(L(log(IPCA),1),1)+d(log(Previsao_Focus),1)+d(log(Tweets),1)+L(log(Resposta),1)+L(-log(IPCA),2)+L(-log(Previsao_Focus),1)+L(-log(Tweets),1)+Meta, data = aggdata2)
    summary(teste2)
    aggdata2[,4] = exp(aggdata2[,4])
  #




































#IMPORTANDO RENDA AGREGADA

renda_agregada = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\IPCA\\RA.csv", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), header = TRUE, dec = ".")


# #SUB GRUPO IPCA errado pois está com d(...,2) -> trocar!
# fit5=dynlm(d(Resposta) ~ 0 + d(Alimentação.e.bebidas,2)+d(Habitação,2)+d(Artigos.de.residência,2)+
#              d(Vestuário,2)+d(Transportes,2)+d(Saúde.e.cuidados.pessoais,2)+
#              d(Despesas.pessoais,2)+d(Educação,2)+d(Comunicação,2)+
#              d(Focus,1)+L(Resposta,1)+L(Alimentação.e.bebidas1,2)+L(Habitação1,2)+
#              L(Artigos.de.residência1,2)+L(Vestuário1,2)+
#              L(Transportes1,2)+L(Saúde.e.cuidados.pessoais1,2)+L(Despesas.pessoais1,2)+
#              L(Educação1,2)+L(Comunicação1,2)+L(Focus1,1)+Meta2, data = aggdata)
# summary(fit5)
# #SUB GRUPO IPCA RESPOSTA DIEGO
# fit6=dynlm(d(Resposta2) ~ 0 + d(Alimentação.e.bebidas,2)+d(Habitação,2)+d(Artigos.de.residência,2)+
#              d(Vestuário,2)+d(Transportes,2)+d(Saúde.e.cuidados.pessoais,2)+
#              d(Despesas.pessoais,2)+d(Educação,2)+d(Comunicação,2)+
#              d(Focus,1)+L(Resposta2,1)+L(Alimentação.e.bebidas1,2)+L(Habitação1,2)+
#              L(Artigos.de.residência1,2)+L(Vestuário1,2)+
#              L(Transportes1,2)+L(Saúde.e.cuidados.pessoais1,2)+L(Despesas.pessoais1,2)+
#              L(Educação1,2)+L(Comunicação1,2)+L(Focus1,1)+Meta2, data = aggdata)
# summary(fit6)
# 
# 
# ###
# 
# ADL1 = summary(fit)
# ADL2 = summary(fit2)
# ADL3 = summary(fit3)
# ADL4 = summary(fit4)
# ADL5 = summary(fit5)
# ADL6 = summary(fit6)
# 
# 
# 
# 
# 
fit6=dynlm(d(R1) ~ 0 + d(IPCA1,1)+d(Focus,1)+L(R1,1)+L(-IPCA1,1)+L(Focus1,1)+Meta2, data = aggdata)
summary(fit6)
fit7=dynlm(d(R2) ~ 0 + d(IPCA1,1)+d(Focus,1)+L(R2,1)+L(-IPCA1,1)+L(Focus1,1)+Meta2, data = aggdata)
summary(fit7)
fit8=dynlm(d(R3) ~ 0 + d(IPCA1,1)+d(Focus,1)+L(R3,1)+L(-IPCA1,1)+L(Focus1,1)+Meta2, data = aggdata)
summary(fit8)
fit9=dynlm(d(R4) ~ 0 + d(IPCA1,1)+d(Focus,1)+L(R4,1)+L(-IPCA1,1)+L(Focus1,1)+Meta2, data = aggdata)
summary(fit9)






