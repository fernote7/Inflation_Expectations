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
require(dygraphs)

## Lendo os microdados
microdados_modelos = read.csv2("Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")

microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                       format = "d/m/y", out.format = "d/m/y")
microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])
##

##Lendo Dados Twitter
tweet=read.csv2("Dados\\Twitter\\base\\tweetglobo.csv")
colnames(tweet) <- c("Mes_Ano", "ano", "mes", "contagem")
tweet = tweet[1:53,]
tweet[,"Mes_Ano"] = chron(as.character(tweet[,"Mes_Ano"]),
                          format = "d/m/y", out.format = "d/m/y")
keeps <- c("Mes_Ano","contagem")
tweet=tweet[keeps]
microdados_modelos = merge(microdados_modelos, tweet)
##

##CRIANDO DADOS PARA MODELO ADL
#MICRODADOS
microdados_modelos = read.csv2("Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
#microdados_modelos = read.csv2("Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                       format = "d/m/y", out.format = "d/m/y")
microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])
dados = microdados_modelos
microdados_modelos2 = read.csv2("Dados\\Microdados_Filtrados\\IPCA\\Pasta3.csv")
microdados_modelos2[,"Mes_Ano"] = chron(as.character(microdados_modelos2[,"Mes_Ano"]),
                                        format = "d/m/y", out.format = "d/m/y")
#TWITTER
tweet=read.csv2("Dados\\Twitter\\base\\tweetglobo.csv")
colnames(tweet) <- c("Mes_Ano", "ano", "mes", "contagem")
tweet = tweet[1:53,]
tweet[,"Mes_Ano"] = chron(as.character(tweet[,"Mes_Ano"]),
                          format = "d/m/y", out.format = "d/m/y")
keeps <- c("Mes_Ano","contagem")
tweet=tweet[keeps]
microdados_modelos = merge(microdados_modelos, tweet)

###dados do IPCA desagregado
microdados_modelos2=microdados_modelos2[1:100,]


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
aggdata <-aggregate(microdados_modelos$Resposta, by=list(microdados_modelos$Cidade, microdados_modelos$Mes_Ano, microdados_modelos$Renda), 
                    FUN=mean, na.rm=TRUE)
aggdata$x[aggdata$Group.1 == 1 & aggdata$Group.3 == 1] = aggdata$x[aggdata$Group.1 == 1 & aggdata$Group.3 == 1]*0.1033
aggdata$x[aggdata$Group.1 == 1 & aggdata$Group.3 == 2] = aggdata$x[aggdata$Group.1 == 1 & aggdata$Group.3 == 2]*0.1058
aggdata$x[aggdata$Group.1 == 1 & aggdata$Group.3 == 3] = aggdata$x[aggdata$Group.1 == 1 & aggdata$Group.3 == 3]*0.1008
aggdata$x[aggdata$Group.1 == 1 & aggdata$Group.3 == 4] = aggdata$x[aggdata$Group.1 == 1 & aggdata$Group.3 == 4]*0.1179

aggdata$x[aggdata$Group.1 == 2 & aggdata$Group.3 == 1] = aggdata$x[aggdata$Group.1 == 2 & aggdata$Group.3 == 1]*0.0626
aggdata$x[aggdata$Group.1 == 2 & aggdata$Group.3 == 2] = aggdata$x[aggdata$Group.1 == 2 & aggdata$Group.3 == 2]*0.0688
aggdata$x[aggdata$Group.1 == 2 & aggdata$Group.3 == 3] = aggdata$x[aggdata$Group.1 == 2 & aggdata$Group.3 == 3]*0.0651
aggdata$x[aggdata$Group.1 == 2 & aggdata$Group.3 == 4] = aggdata$x[aggdata$Group.1 == 2 & aggdata$Group.3 == 4]*0.0626

aggdata$x[aggdata$Group.1 == 3 & aggdata$Group.3 == 1] = aggdata$x[aggdata$Group.1 == 3 & aggdata$Group.3 == 1]*0.0160
aggdata$x[aggdata$Group.1 == 3 & aggdata$Group.3 == 2] = aggdata$x[aggdata$Group.1 == 3 & aggdata$Group.3 == 2]*0.0164
aggdata$x[aggdata$Group.1 == 3 & aggdata$Group.3 == 3] = aggdata$x[aggdata$Group.1 == 3 & aggdata$Group.3 == 3]*0.0164
aggdata$x[aggdata$Group.1 == 3 & aggdata$Group.3 == 4] = aggdata$x[aggdata$Group.1 == 3 & aggdata$Group.3 == 4]*0.0172

aggdata$x[aggdata$Group.1 == 4 & aggdata$Group.3 == 1] = aggdata$x[aggdata$Group.1 == 4 & aggdata$Group.3 == 1]*0.0224
aggdata$x[aggdata$Group.1 == 4 & aggdata$Group.3 == 2] = aggdata$x[aggdata$Group.1 == 4 & aggdata$Group.3 == 2]*0.0257
aggdata$x[aggdata$Group.1 == 4 & aggdata$Group.3 == 3] = aggdata$x[aggdata$Group.1 == 4 & aggdata$Group.3 == 3]*0.0311
aggdata$x[aggdata$Group.1 == 4 & aggdata$Group.3 == 4] = aggdata$x[aggdata$Group.1 == 4 & aggdata$Group.3 == 4]*0.0328  

aggdata$x[aggdata$Group.1 == 5 & aggdata$Group.3 == 1] = aggdata$x[aggdata$Group.1 == 5 & aggdata$Group.3 == 1]*0.0179
aggdata$x[aggdata$Group.1 == 5 & aggdata$Group.3 == 2] = aggdata$x[aggdata$Group.1 == 5 & aggdata$Group.3 == 2]*0.0137
aggdata$x[aggdata$Group.1 == 5 & aggdata$Group.3 == 3] = aggdata$x[aggdata$Group.1 == 5 & aggdata$Group.3 == 3]*0.0116
aggdata$x[aggdata$Group.1 == 5 & aggdata$Group.3 == 4] = aggdata$x[aggdata$Group.1 == 5 & aggdata$Group.3 == 4]*0.0100

aggdata$x[aggdata$Group.1 == 6 & aggdata$Group.3 == 1] = aggdata$x[aggdata$Group.1 == 6 & aggdata$Group.3 == 1]*0.0116
aggdata$x[aggdata$Group.1 == 6 & aggdata$Group.3 == 2] = aggdata$x[aggdata$Group.1 == 6 & aggdata$Group.3 == 2]*0.0137
aggdata$x[aggdata$Group.1 == 6 & aggdata$Group.3 == 3] = aggdata$x[aggdata$Group.1 == 6 & aggdata$Group.3 == 3]*0.0145
aggdata$x[aggdata$Group.1 == 6 & aggdata$Group.3 == 4] = aggdata$x[aggdata$Group.1 == 6 & aggdata$Group.3 == 4]*0.0145

aggdata$x[aggdata$Group.1 == 7 & aggdata$Group.3 == 1] = aggdata$x[aggdata$Group.1 == 7 & aggdata$Group.3 == 1]*0.0071
aggdata$x[aggdata$Group.1 == 7 & aggdata$Group.3 == 2] = aggdata$x[aggdata$Group.1 == 7 & aggdata$Group.3 == 2]*0.0071
aggdata$x[aggdata$Group.1 == 7 & aggdata$Group.3 == 3] = aggdata$x[aggdata$Group.1 == 7 & aggdata$Group.3 == 3]*0.0067
aggdata$x[aggdata$Group.1 == 7 & aggdata$Group.3 == 4] = aggdata$x[aggdata$Group.1 == 7 & aggdata$Group.3 == 4]*0.0067

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
##