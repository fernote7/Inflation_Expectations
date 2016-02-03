setwd("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\")
source("Funcoes\\TD64_Modelos.R")
require(dynlm)
require(quantmod)

microdados_modelos2 = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\IPCA\\Pasta2.csv")
microdados_modelos2[,"Mes_Ano"] = chron(as.character(microdados_modelos2[,"Mes_Ano"]),
                                        format = "d/m/y", out.format = "d/m/y")
microdados_modelos2=microdados_modelos2[48:100,]


## Lendo os microdados
microdados_modelos = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
#microdados_modelos = read.csv2("C:\\Users\\ingrid.oliveira\\Dropbox\\10 Expectativas de inflaÃ§Ã£o - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                       format = "d/m/y", out.format = "d/m/y")
microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])
# microdados_modelos = microdados_modelos[, -64]
dados = microdados_modelos


inflacao_def2 = unique(microdados_modelos[,c("Mes_Ano", "Previsao_Focus")])
inflacao_def2[2:nrow(inflacao_def2),"PF_lag1"] = inflacao_def2[1:(nrow(inflacao_def2) - 1),"Previsao_Focus"]
inflacao_def2 = inflacao_def2[,c("Mes_Ano", "PF_lag1")]

microdados_modelos = merge(microdados_modelos, inflacao_def2)




inflacao_def4 = unique(microdados_modelos[,c("Mes_Ano", "IPCA")])
inflacao_def4[2:nrow(inflacao_def4),"IPCA_lag1"] = inflacao_def4[1:(nrow(inflacao_def4) - 1),"IPCA"]
inflacao_def4 = inflacao_def4[,c("Mes_Ano", "IPCA_lag1")]



###AGREGANDO DADOS POR CIDADE
aggdata <-aggregate(microdados_modelos$Resposta, by=list(microdados_modelos$Cidade, microdados_modelos$Mes_Ano), 
                    FUN=mean, na.rm=TRUE)
aggdata$x[aggdata$Group.1 == 1] = aggdata$x[aggdata$Group.1 == 1]*0.4278
aggdata$x[aggdata$Group.1 == 2] = aggdata$x[aggdata$Group.1 == 2]*0.2591
aggdata$x[aggdata$Group.1 == 3] = aggdata$x[aggdata$Group.1 == 3]*0.0660
aggdata$x[aggdata$Group.1 == 4] = aggdata$x[aggdata$Group.1 == 4]*0.1120
aggdata$x[aggdata$Group.1 == 5] = aggdata$x[aggdata$Group.1 == 5]*0.0532
aggdata$x[aggdata$Group.1 == 6] = aggdata$x[aggdata$Group.1 == 6]*0.0543
aggdata$x[aggdata$Group.1 == 7] = aggdata$x[aggdata$Group.1 == 7]*0.0273

aggdata2 <- aggregate(aggdata$x, by=list(aggdata$Group.2), 
                      FUN=sum, na.rm=TRUE)


##Lendo Dados Twitter
tweet=read.csv2("C:/Users/fernando.teixeira/Dropbox/10 Expectativas de inflação - Brasil/ProgramasTD64/Dados/Twitter/tweetglobo.csv")
colnames(tweet) <- c("Mes_Ano", "ano", "mes", "contagem")
tweet = tweet[1:53,]
tweet[,"Mes_Ano"] = chron(as.character(tweet[,"Mes_Ano"]),
                          format = "d/m/y", out.format = "d/m/y")
keeps <- c("Mes_Ano","contagem")
tweet=tweet[keeps]
microdados_modelos = merge(microdados_modelos, tweet)



###AGREGANDO DADOS RELEVANTES
dados = microdados_modelos
classes1 = c("Mes_Ano", "Cidade", "Renda", "IPCA", "Previsao_Focus", "Meta", "contagem")
expinf1_mes = aggregate(dados[,"Resposta"], dados[,classes1], mean)
###


###AGREGANDO DADOS RELEVANTES
dados = microdados_modelos
classes1 = c("Mes_Ano", "Cidade", "Renda", "IPCA", "Previsao_Focus", "Meta")
expinf1_mes = aggregate(dados[,"Resposta"], dados[,classes1], mean)
#expinf1_mes = aggregate(x = microdados_modelos$Resposta, by = list(microdados_modelos$Mes_Ano, microdados_modelos$Cidade, microdados_modelos$Renda), FUN = "mean")
expinf1_mes = expinf1_mes[order(expinf1_mes$Cidade, expinf1_mes$Renda, expinf1_mes$Mes_Ano),]
colnames(expinf1_mes) = c("Mes_Ano", "Cidade", "Renda", "IPCA", "Previsao_Focus", "Meta", "contagem","Resposta")


aggdata2 <- aggregate(aggdata$x, by=list(aggdata$Group.2), 
                      FUN=sum, na.rm=TRUE)
aggdata3 = aggregate(expinf1_mes$IPCA, by=list(expinf1_mes$Mes_Ano), 
                     FUN=mean, na.rm=TRUE)
aggdata4 = aggregate(expinf1_mes$Previsao_Focus, by=list(expinf1_mes$Mes_Ano), 
                     FUN=mean, na.rm=TRUE)
aggdata5 = aggregate(expinf1_mes$Meta, by=list(expinf1_mes$Mes_Ano), 
                     FUN=mean, na.rm=TRUE)
aggdata6 = as.data.frame(unique(expinf1_mes$Mes_Ano))

aggdata7 = aggregate(expinf1_mes$contagem, by=list(expinf1_mes$Mes_Ano), 
                     FUN=mean, na.rm=TRUE)



aggdata<-cbind(aggdata2$x,aggdata3$x,aggdata4$x,aggdata5$x,aggdata7$x)


aggdata <- as.data.frame(aggdata)
aggdata$Data <- aggdata6[,1]
colnames(aggdata) = c("Resposta","IPCA","Previsao_Focus", "Meta", "contagem","Data")

###

###IMPORTANDO RENDA AGREGADA

renda_agregada = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\IPCA\\RA.csv", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), header = TRUE, dec = ".")

###JUNTANDO DADOS
renda_agregada=renda_agregada[48:100,]
aggdata$R1 = renda_agregada$R1
aggdata$R2 = renda_agregada$R2
aggdata$R3 = renda_agregada$R3
aggdata$R4 = renda_agregada$R4
aggdata$Resposta2= renda_agregada$Brasil
aggdata$Alimentação.e.bebidas=microdados_modelos2$Alimentação.e.bebidas
aggdata$Habitação=microdados_modelos2$Habitação
aggdata$Artigos.de.residência=microdados_modelos2$Artigos.de.residência
aggdata$Vestuário=microdados_modelos2$Vestuário
aggdata$Transportes=microdados_modelos2$Transportes
aggdata$Saúde.e.cuidados.pessoais=microdados_modelos2$Saúde.e.cuidados.pessoais
aggdata$Despesas.pessoais=microdados_modelos2$Despesas.pessoais
aggdata$Educação=microdados_modelos2$Educação
aggdata$Comunicação=microdados_modelos2$Comunicação
IPCA1 = Lag(aggdata$IPCA,1)
aggdata=aggdata[2:53,]

###MODELO DE CORRECAO DE ERRO 
Meta2 = - ts(aggdata$Meta)
Focus = ts(aggdata$Previsao_Focus)
Focus1 = - ts(aggdata$Previsao_Focus)
Alimentação.e.bebidas1 = -  ts(aggdata$Alimentação.e.bebidas)
Habitação1 = -  ts(aggdata$Habitação)
Artigos.de.residência1 = -  ts(aggdata$Artigos.de.residência)
Vestuário1 = -  ts(aggdata$Vestuário)
Transportes1 = -  ts(aggdata$Transportes)
Saúde.e.cuidados.pessoais1 = -  ts(aggdata$Saúde.e.cuidados.pessoais)
Despesas.pessoais1 = -  ts(aggdata$Despesas.pessoais)
Educação1 = -  ts(aggdata$Educação)
Comunicação1 = -  ts(aggdata$Comunicação)
IPCA1=ts(IPCA1)
Contagem=ts
aggdata = ts(aggdata)

