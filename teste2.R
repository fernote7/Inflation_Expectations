setwd("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflaÃ§Ã£o - Brasil\\ProgramasTD64\\")
source("Funcoes\\TD64_Modelos.R")
require(dynlm)
require(quantmod)
require(tseries)

microdados_modelos2 = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflaÃ§Ã£o - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\IPCA\\Pasta2.csv")
microdados_modelos2[,"Mes_Ano"] = chron(as.character(microdados_modelos2[,"Mes_Ano"]),
                                        format = "d/m/y", out.format = "d/m/y")
microdados_modelos2=microdados_modelos2[1:100,]


## Lendo os microdados
microdados_modelos = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflaÃ§Ã£o - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
#microdados_modelos = read.csv2("C:\\Users\\ingrid.oliveira\\Dropbox\\10 Expectativas de inflaÃÂ§ÃÂ£o - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
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
###


###AGREGANDO DADOS RELEVANTES
dados = microdados_modelos
classes1 = c("Mes_Ano", "Cidade", "Renda", "IPCA", "Previsao_Focus", "Meta")
expinf1_mes = aggregate(dados[,"Resposta"], dados[,classes1], mean)
#expinf1_mes = aggregate(x = microdados_modelos$Resposta, by = list(microdados_modelos$Mes_Ano, microdados_modelos$Cidade, microdados_modelos$Renda), FUN = "mean")
expinf1_mes = expinf1_mes[order(expinf1_mes$Cidade, expinf1_mes$Renda, expinf1_mes$Mes_Ano),]
colnames(expinf1_mes) = c("Mes_Ano", "Cidade", "Renda", "IPCA", "Previsao_Focus", "Meta", "Resposta")

aggdata3 = aggregate(expinf1_mes$IPCA, by=list(expinf1_mes$Mes_Ano), 
                     FUN=mean, na.rm=TRUE)
aggdata4 = aggregate(expinf1_mes$Previsao_Focus, by=list(expinf1_mes$Mes_Ano), 
                     FUN=mean, na.rm=TRUE)
aggdata5 = aggregate(expinf1_mes$Meta, by=list(expinf1_mes$Mes_Ano), 
                     FUN=mean, na.rm=TRUE)
aggdata6 = as.data.frame(unique(expinf1_mes$Mes_Ano))

aggdata<-cbind(aggdata2$x,aggdata3$x,aggdata4$x,aggdata5$x)
aggdata <- as.data.frame(aggdata)
aggdata$Data <- aggdata6[,1]
colnames(aggdata) = c("Resposta","IPCA","Previsao_Focus", "Meta", "Data")

###

###IMPORTANDO RENDA AGREGADA

renda_agregada = read.csv2("C:\\Users\\fernando.teixeira\\Dropbox\\10 Expectativas de inflaÃ§Ã£o - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\IPCA\\RA.csv", colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), header = TRUE, dec = ".")


###JUNTANDO DADOS
renda_agregada=renda_agregada[1:100,]
aggdata$R1 = renda_agregada$R1
aggdata$R2 = renda_agregada$R2
aggdata$R3 = renda_agregada$R3
aggdata$R4 = renda_agregada$R4
aggdata$Resposta2= renda_agregada$Brasil
aggdata$AlimentaÃ§Ã£o.e.bebidas=microdados_modelos2$AlimentaÃ§Ã£o.e.bebidas
aggdata$HabitaÃ§Ã£o=microdados_modelos2$HabitaÃ§Ã£o
aggdata$Artigos.de.residÃªncia=microdados_modelos2$Artigos.de.residÃªncia
aggdata$VestuÃ¡rio=microdados_modelos2$VestuÃ¡rio
aggdata$Transportes=microdados_modelos2$Transportes
aggdata$SaÃºde.e.cuidados.pessoais=microdados_modelos2$SaÃºde.e.cuidados.pessoais
aggdata$Despesas.pessoais=microdados_modelos2$Despesas.pessoais
aggdata$EducaÃ§Ã£o=microdados_modelos2$EducaÃ§Ã£o
aggdata$ComunicaÃ§Ã£o=microdados_modelos2$ComunicaÃ§Ã£o
IPCA1 = Lag(aggdata$IPCA,1)
aggdata=aggdata[2:100,]

###MODELO DE CORRECAO DE ERRO 
Meta2 =  ts(aggdata$Meta)
Focus = ts(aggdata$Previsao_Focus)
Focus1 = - ts(aggdata$Previsao_Focus)
AlimentaÃ§Ã£o.e.bebidas1 = -  ts(aggdata$AlimentaÃ§Ã£o.e.bebidas)
HabitaÃ§Ã£o1 = -  ts(aggdata$HabitaÃ§Ã£o)
Artigos.de.residÃªncia1 = -  ts(aggdata$Artigos.de.residÃªncia)
VestuÃ¡rio1 = -  ts(aggdata$VestuÃ¡rio)
Transportes1 = -  ts(aggdata$Transportes)
SaÃºde.e.cuidados.pessoais1 = -  ts(aggdata$SaÃºde.e.cuidados.pessoais)
Despesas.pessoais1 = -  ts(aggdata$Despesas.pessoais)
EducaÃ§Ã£o1 = -  ts(aggdata$EducaÃ§Ã£o)
ComunicaÃ§Ã£o1 = -  ts(aggdata$ComunicaÃ§Ã£o)
IPCA1=ts(IPCA1)
aggdata = ts(aggdata)

#BÃSICO
fit=dynlm(d(log(Resposta)) ~ 0 + d(log(IPCA1),1)+d(log(Focus),1)+L(log(Resposta),1)+L(log(IPCA1),1)+L(log(Focus),1)+log(Meta2), data = aggdata)
summary(fit)
#RETIRANDO VARIÃVEIS NÃO RELEVANTES
fit2=dynlm(d(Resposta) ~ 0 + d(IPCA1,1)+L(Resposta,1)+L(-IPCA1,1)+Meta2, data = aggdata)
summary(fit2)
#COM RESPOSTAS DO DIEGO
fit3=dynlm(d(Resposta2) ~ 0 + d(IPCA1,1)+d(Focus,1)+L(Resposta2,1)+L(-IPCA1,1)+L(Focus1,1)+Meta2, data = aggdata)
summary(fit3)
#COM RESPOSTAS DO DIEGO RETIRANDO VARIÃVEIS NÃO RELEVANTES
fit4=dynlm(d(Resposta2) ~ 0 + d(IPCA1,1)+L(Resposta2,1)+L(-IPCA1,1)+Meta2, data = aggdata)
summary(fit4)


# #SUB GRUPO IPCA errado pois estÃ¡ com d(...,2) -> trocar!
# fit5=dynlm(d(Resposta) ~ 0 + d(AlimentaÃ§Ã£o.e.bebidas,2)+d(HabitaÃ§Ã£o,2)+d(Artigos.de.residÃªncia,2)+
#              d(VestuÃ¡rio,2)+d(Transportes,2)+d(SaÃºde.e.cuidados.pessoais,2)+
#              d(Despesas.pessoais,2)+d(EducaÃ§Ã£o,2)+d(ComunicaÃ§Ã£o,2)+
#              d(Focus,1)+L(Resposta,1)+L(AlimentaÃ§Ã£o.e.bebidas1,2)+L(HabitaÃ§Ã£o1,2)+
#              L(Artigos.de.residÃªncia1,2)+L(VestuÃ¡rio1,2)+
#              L(Transportes1,2)+L(SaÃºde.e.cuidados.pessoais1,2)+L(Despesas.pessoais1,2)+
#              L(EducaÃ§Ã£o1,2)+L(ComunicaÃ§Ã£o1,2)+L(Focus1,1)+Meta2, data = aggdata)
# summary(fit5)
# #SUB GRUPO IPCA RESPOSTA DIEGO
# fit6=dynlm(d(Resposta2) ~ 0 + d(AlimentaÃ§Ã£o.e.bebidas,2)+d(HabitaÃ§Ã£o,2)+d(Artigos.de.residÃªncia,2)+
#              d(VestuÃ¡rio,2)+d(Transportes,2)+d(SaÃºde.e.cuidados.pessoais,2)+
#              d(Despesas.pessoais,2)+d(EducaÃ§Ã£o,2)+d(ComunicaÃ§Ã£o,2)+
#              d(Focus,1)+L(Resposta2,1)+L(AlimentaÃ§Ã£o.e.bebidas1,2)+L(HabitaÃ§Ã£o1,2)+
#              L(Artigos.de.residÃªncia1,2)+L(VestuÃ¡rio1,2)+
#              L(Transportes1,2)+L(SaÃºde.e.cuidados.pessoais1,2)+L(Despesas.pessoais1,2)+
#              L(EducaÃ§Ã£o1,2)+L(ComunicaÃ§Ã£o1,2)+L(Focus1,1)+Meta2, data = aggdata)
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



###teste utilizando resÃ­duos da regressÃ£o

ra=Lag(aggdata[,1],1)
ia = Lag(aggdata[,2],2)
fa = Lag(aggdata[,3],1)
ma = Lag(aggdata[,4],1)

mod=lm(log(ra)~log(ia)+log(fa)+log(ma))
resid=mod$residuals
resid=ts(resid)
fit=dynlm(d(log(Resposta)) ~ 0 + d(log(IPCA1),1)+d(log(Focus),1)+resid, data = aggdata)
summary(fit)

adf.test(resid)
adf.test(resid, k=10)
adf.test(fit$residuals)




###Teste de cointegracao de resposta e ipca
z=aggdata$Resposta
z=as.data.frame(z)
z$IPCA=aggdata$IPCA
po.test(z) #Phillips-Ouliaris Cointegration Test



# 
# inflacao_def3 = expinf1_mes[,c("Mes_Ano", "Resposta")]
# inflacao_def3[2:nrow(inflacao_def3),"Resultado_lag1"] = inflacao_def3[1:(nrow(inflacao_def3) - 1),"Resposta"]
# expinf1_mes = cbind(expinf1_mes, inflacao_def3$Resultado_lag1)
# colnames(expinf1_mes) = c("Mes_Ano", "Cidade", "Renda", "IPCA", "IPCA_lag1" , "IPCA_lag2", "Previsao_Focus", "PF_lag1", "Meta", "Resposta", "Resposta_lag1")
# expinf1_mes[1,11] = 11.50




#inflacao_def = join(inflacao_def,microdados_modelos2)






#classes1 = c("Mes_Ano", "Cidade", "Renda", "IPCA", "IPCA_lag1", "IPCA_lag2", "Previsao_Focus", "PF_lag1", "Meta", "Resposta", "Resposta_lag1")


####
#teste = aggregate(x = microdados_modelos$Resposta, by = list(microdados_modelos$Mes_Ano, microdados_modelos$Cidade, microdados_modelos$Renda), FUN = "mean")
####



##VariÃ¡veis
# 
# Resposta = expinf1_mes$Resposta - expinf1_mes$Resposta_lag1
# Infla = expinf1_mes$IPCA_lag1 - expinf1_mes$IPCA_lag2
# Focus = expinf1_mes$Previsao_Focus - expinf1_mes$PF_lag1
# Resposta1 = expinf1_mes$Resposta_lag1
# IPCA2 = - expinf1_mes$IPCA_lag2
# Focus1 = - expinf1_mes$PF_lag1
# Meta =  - expinf1_mes$Meta
# 
# 
# formula = Resposta ~ 0 + Infla  + Resposta1 + IPCA2 + Focus1 + Meta
# formula2 = Resposta ~ 0 + Infla  + Focus + Resposta1 + IPCA2 + Focus1 + Meta
# 
# modelo = lm(formula)
# summary(modelo)
# 
# modelo2 = lm(formula2)
# summary(modelo2)

#teste=data.frame(aggdata[4:100,5], aggdata[4:100,1], aggdata[4:100,1])

