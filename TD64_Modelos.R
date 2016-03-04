## Carregando pacotes
require(chron)
require(lubridate)
require(e1071)
require(dummies)


## Lendo os microdados
microdados_modelos = read.csv2("C:/Users/fernando.teixeira/Dropbox/10 Expectativas de inflação - Brasil/ProgramasTD64/Dados/Microdados_Filtrados/Microdados_Modelos_set05aset15.csv", 
                               sep=";", dec=".",colClasses = c("character", "character", "character",
                                              rep("numeric",65)))
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


star=stargazer(list(modelo3), title = "Expectativas de Inflação por composição de amostra",
               covariate.labels = c("Renda 2","$Renda 3$",
                                    "$Renda 4$","Escolaridade 2",
                                    "Escolaridade 3","Escolaridade 4",
                                    "Cidade 2","Cidade 3","Cidade 4","Cidade 5",
                                    "Cidade 6","Cidade 7", "Idade 2","Idade 3","Idade 4",
                                    "Sexo 2"),
               dep.var.labels = c("$Consumidor_t$"), align = TRUE, 
               label = "ecmmodel", omit.stat = "ser", style="aer")


bptest(modelo3)


#formula e modelo 4
formula4 = Resposta ~  . - Mes_Ano - (Pergunta_1177_2 + 
                                            Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
                                            Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
                                            Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
                                            Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
                                            Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) - IPCA -
    IPCA_lag2 - Previsao_Focus - Learn_4em8

modelo4 = lm(formula4, microdados_modelo1)


star=stargazer(list(modelo4, modelo3), title = "Expectativas de Inflação por composição de amostra",
               covariate.labels = c(">R\\$2100 e $\\leq$ R\\$4800",">R\\$4800 e $\\leq$ R\\$9600",
                                    ">R\\$9600","Ensino Médio",
                                    "Superior","Pós-Graduação",
                                    "Rio de Janeiro","Belo Horizonte","Brasília","Salvador",
                                    "Porto Alegre","Recife", "$\\geq$ 35 e < 45 anos","$\\geq$ 45 e < 60 anos","$\\geq$ 60 anos",
                                    "Mulheres"),
               dep.var.labels = c("$Consumidor_t$"), align = TRUE, 
               label = "ecmmodel", omit.stat = "ser", style="aer")


bptest(modelo4)



#####






t=ts(cbind(microdados_modelo1[,1:2]))


a=VECM(t, 1, exogen = microdados_modelo1$Renda_2 + microdados_modelo1$Renda_3)


formula4 = Resposta ~ 

b = dynlm(formula4, microdados_modelo1)




compare = function(v) all(sapply( as.list(v[-1]),
                                   FUN=function(z) {identical(z, v[1])}))
