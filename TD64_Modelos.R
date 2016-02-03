## Carregando pacotes
require(chron)
require(lubridate)
require(e1071)
require(dummies)


## Lendo os microdados
 microdados_modelos = read.csv("C:/Users/fernando.teixeira/Dropbox/10 Expectativas de inflação - Brasil/ProgramasTD64/Dados/Microdados_Filtrados/Microdados_Modelos_set05aset15.csv", sep = ";")
#microdados_modelos = read.csv2("C:\\Users\\ingrid.oliveira\\Dropbox\\10 Expectativas de infla??o - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                           format = "d/m/y", out.format = "d/m/y")
microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])
# microdados_modelos = microdados_modelos[, -64]

# Obtendo IPCA de dois meses passados
inflacao_def = unique(microdados_modelos[,c("Mes_Ano", "IPCA")])
inflacao_def[3:nrow(inflacao_def),"IPCA_lag2"] = inflacao_def[1:(nrow(inflacao_def) - 2),"IPCA"]
inflacao_def = inflacao_def[,c("Mes_Ano", "IPCA_lag2")]

microdados_modelos = merge(microdados_modelos, inflacao_def)





aux_modelo1 = c("Resposta", 
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
                "Pergunta_1194_2", "Pergunta_1194_3")


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
                                          "IPCA", "IPCA_lag2", "Previsao_Focus", "Learn_4em8")]



#microdados_modelo1[,"Resposta"] = as.numeric(microdados_modelo1[,"Resposta"])
#for(k in 1:length(aux_modelo1)){
#  assign(aux_modelo1[k], microdados_modelo1[,k])
#}
# teste = microdados_modelo1[,2:ncol(microdados_modelo1)]

# modelo1 = lm(microdados_modelo1[,"Resposta"] ~ teste)


##Constru??o das dummies de per?odo
periodos = microdados_modelo1$Mes_Ano
periodos = dummy( as.character(periodos) )
periodos = as.data.frame(periodos)

microdados_modelo1 = cbind(microdados_modelo1, periodos)


# formula e modelo 1
formula = Resposta ~ Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
  Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 +
  Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
  Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 + Pergunta_1177_2 + 
  Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
  Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
  Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
  Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
  Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3

modelo1 = lm(formula, microdados_modelo1)


# formula e modelo 2
formula2 = Resposta ~ 0 + Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
  Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + 
  Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
  Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 + Pergunta_1177_2 + 
  Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
  Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
  Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
  Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
  Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3 + 
  . - Mes_Ano - Previsao_Focus - IPCA

modelo2 = lm(formula2, microdados_modelo1)


#formula e modelo 3
formula3 = Resposta ~ 0+ . - Mes_Ano - (Pergunta_1177_2 + 
     Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
     Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
     Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
     Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
     Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) - IPCA -
     IPCA_lag2 - Previsao_Focus - Learn_4em8

modelo3 = lm(formula3, microdados_modelo1)

bptest(modelo3)

#####






t=ts(cbind(microdados_modelo1[,1:2]))


a=VECM(t, 1, exogen = microdados_modelo1$Renda_2 + microdados_modelo1$Renda_3)


formula4 = Resposta ~ 

b = dynlm(formula4, microdados_modelo1)




compare = function(v) all(sapply( as.list(v[-1]),
                                   FUN=function(z) {identical(z, v[1])}))
