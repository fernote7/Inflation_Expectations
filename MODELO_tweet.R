nomes = list("ingrid.oliveira", "fernando.teixeira")
for (nome in nomes)
{
  direc = paste0("C:\\Users\\", nome, "\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\")
  try(setwd(direc), silent = TRUE)
}
rm(nome, nomes)
source(paste0(direc, "\\Funcoes\\Modelos\\bases\\base_modelo_tweet.R"), encoding = c("utf8"))

#formula e modelo 3
  formula3 = Resposta ~ 0 +  
    . - Mes_Ano - IPCA - Índice.geral + (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
                                           Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + 
                                           Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
                                           Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 + contagem) * Previsao_Focus +
    (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
       Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + 
       Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
       Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 + contagem) * (Alimentação.e.bebidas + Habitação  + Artigos.de.residência + Vestuário +
                                                     Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação) +
    (Pergunta_1177_2 + 
       Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
       Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
       Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
       Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
       Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * Previsao_Focus +
    (Pergunta_1177_2 + 
       Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
       Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
       Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
       Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
       Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * (Alimentação.e.bebidas + Habitação  + Artigos.de.residência + Vestuário +
                                                                 Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação)
  
  modelo3 = lm(formula3, teste)
  summary(modelo3)


##Testando modelo só para o Rio
  microdados_modelo_rio = microdados_modelos[complete.cases(microdados_modelos),
                                          c("Mes_Ano","Resposta", 
                                            "Renda_2", "Renda_3", "Renda_4",
                                            "Escolaridade_2", "Escolaridade_3", "Escolaridade_4", 
                                            "Cidade_2", 
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
  
  microdados_modelo_rio <- microdados_modelo_rio[!(microdados_modelo_rio$Cidade_2)<1,]  # Remove it
  
  microdados_modelo_rio<-join(microdados_modelo_rio,microdados_modelos2)
  
  
  formulario = Resposta ~ 0 +  
    . - Mes_Ano - IPCA - Índice.geral + IPCA_lag2 + (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
                                           Escolaridade_3 + Escolaridade_4 + Idade_2 + 
                                           Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 + contagem) * Previsao_Focus +
    (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
       Escolaridade_3 + Escolaridade_4 + Idade_2 + 
       Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 + contagem) * (Alimentação.e.bebidas + Habitação  + Artigos.de.residência + Vestuário +
                                                                Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação) +
    (Pergunta_1177_2 + 
       Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
       Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
       Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
       Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
       Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * Previsao_Focus +
    (Pergunta_1177_2 + 
       Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
       Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
       Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
       Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
       Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * (Alimentação.e.bebidas + Habitação  + Artigos.de.residência + Vestuário +
                                                                 Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação)
  
  modelorio = lm(formulario, microdados_modelo_rio)
  summary(modelorio)

#formula e modelo 3 EM LOG
  teste2 = teste
  teste2$Habitação
  teste2$Artigos.de.residência
  teste2$Vestuário
  teste2$Transportes
  teste2$Habitação
  logformula3 = log(Resposta) ~ 0+Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
                                            Escolaridade_3 + Escolaridade_4 + Idade_2 + 
                                            Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 + log(contagem) + log(Previsao_Focus) +
                                            log(Alimentação.e.bebidas) +log(Habitação ) +log(Artigos.de.residência) +log(Vestuário) +
                                            log(Transportes) +log(Saúde.e.cuidados.pessoais) +log(Despesas.pessoais) +log(Educação) +log(Comunicação) +
                                            Pergunta_1177_2 + 
                                            Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
                                            Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
                                            Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
                                            Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
                                            Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3 +
                                            (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
                                            Escolaridade_3 + Escolaridade_4 + Idade_2 + 
                                            Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 +log(contagem)) * log(Previsao_Focus) +
                                            (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
                                            Escolaridade_3 + Escolaridade_4 + Idade_2 + 
                                            Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 +log(contagem)) * 
                                            (log(Alimentação.e.bebidas) +log(Habitação ) +log(Artigos.de.residência) +log(Vestuário) +
                                             log(Transportes) +log(Saúde.e.cuidados.pessoais) +log(Despesas.pessoais) +log(Educação) +log(Comunicação)) +
    (Pergunta_1177_2 + 
    Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
    Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
    Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
    Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
    Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * log(Previsao_Focus) +
    (Pergunta_1177_2 + 
       Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
       Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
       Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
       Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
       Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * (log(Alimentação.e.bebidas) +log(Habitação) +log(Artigos.de.residência) +log(Vestuário) +
                                                                 log(Transportes) +log(Saúde.e.cuidados.pessoais) +log(Despesas.pessoais) +log(Educação) +log(Comunicação))
  
  modelolog = lm(logformula3, teste2)
  summary(modelolog)













###########################################

formulalh = Resposta ~ 0 + Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
                              Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + 
                              Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
                              Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 + Alimentação.e.bebidas + 
                              Habitação  + Artigos.de.residência + Vestuário +
                              Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação +
                              Pergunta_1177_2 + Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
                              Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + + 
                              Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3 +
  (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
     Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + 
     Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
     Idade_3 + Idade_4 + Sexo_2 + Learn_4em8) * Previsao_Focus +
  (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
     Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + 
     Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
     Idade_3 + Idade_4 + Sexo_2 + Learn_4em8) * (Alimentação.e.bebidas + Habitação  + Artigos.de.residência + Vestuário +
                                                   Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação) +
  (Pergunta_1177_2 + 
     Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
     Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
     Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
     Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
     Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * Previsao_Focus +
  (Pergunta_1177_2 + 
     Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
     Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
     Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
     Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
     Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * (Alimentação.e.bebidas + Habitação  + Artigos.de.residência + Vestuário +
                                                               Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação) -
    `as.character(periodos)01/12/05` - `as.character(periodos)01/01/06` -
    `as.character(periodos)01/02/06` - `as.character(periodos)01/03/06` -
    `as.character(periodos)01/04/06` - `as.character(periodos)01/05/06` -
    `as.character(periodos)01/06/06` - `as.character(periodos)01/07/06` -
    `as.character(periodos)01/08/06` - `as.character(periodos)01/09/06` -
    `as.character(periodos)01/10/06`

modelolh = lm(formulalh, teste)
#linearHypothesis(modelolh,c("Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 + Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + Idade_3 + Idade_4 + Sexo_2 + Learn_4em8 = 0"))
#linearHypothesis(modelolh,c("Escolaridade_2 + Escolaridade_3 + Escolaridade_4=0"))

##DEMOGRAFICO
  lh1=linearHypothesis(modelolh,c("Renda_2 =0", " Renda_3 =0", " Renda_4 =0", " Escolaridade_2 =0", " Escolaridade_3 =0", " Escolaridade_4 =0", " Cidade_2 =0", " Cidade_3 =0", " Cidade_4 =0", " Cidade_5 =0", " Cidade_6 =0", " Cidade_7 =0", " Idade_2 =0", " Idade_3 =0", " Idade_4 =0", " Sexo_2 =0", " Learn_4em8 = 0"))
  lh2=linearHypothesis(modelolh,c("Renda_2=0", "Renda_3 =0", "Renda_4=0"))
  lh3=linearHypothesis(modelolh,c("Escolaridade_2 =0", " Escolaridade_3 =0", " Escolaridade_4=0"))
  lh4=linearHypothesis(modelolh,c("Cidade_2=0", "Cidade_3=0", "Cidade_4=0", "Cidade_5=0", "Cidade_6=0", "Cidade_7=0"))
  lh5=linearHypothesis(modelolh,c("Idade_2 =0", " Idade_3 =0", " Idade_4=0"))

##IPCA
  lh6=linearHypothesis(modelolh,c("Alimentação.e.bebidas=0", "Habitação=0", " Artigos.de.residência =0", " Vestuário =0", " Transportes =0", " Saúde.e.cuidados.pessoais =0", " Despesas.pessoais =0", " Educação =0", " Comunicação=0"))

##OPINIAO
  lh7=linearHypothesis(modelolh,c("Pergunta_1177_2 =0", " Pergunta_1177_3 =0", " Pergunta_1178_2 =0", " Pergunta_1178_3 =0", " Pergunta_1147_2 =0", " Pergunta_1147_3 =0", " Pergunta_1149_2 =0", " Pergunta_1149_3 =0", " Pergunta_1182_2 =0", " Pergunta_1182_3 =0", " Pergunta_1183_2 =0", " Pergunta_1183_3 =0", " Pergunta_1189_2 =0", " Pergunta_1189_3 =0", " Pergunta_1194_2 =0", " Pergunta_1194_3=0"))
  lh8=linearHypothesis(modelolh,c("Pergunta_1177_2 =0", " Pergunta_1177_3 =0")) #current situation of country
  lh9=linearHypothesis(modelolh,c(" Pergunta_1178_2 =0", " Pergunta_1178_3 =0")) #future situation of country
  lh10=linearHypothesis(modelolh,c(" Pergunta_1147_2 =0", " Pergunta_1147_3 =0")) #current sit household
  lh11=linearHypothesis(modelolh,c(" Pergunta_1149_2 =0", " Pergunta_1149_3 =0")) #future sit household
  lh12=linearHypothesis(modelolh,c(" Pergunta_1182_2 =0", " Pergunta_1182_3 =0")) #current employment
  lh13=linearHypothesis(modelolh,c(" Pergunta_1182_2 =0", " Pergunta_1182_3 =0")) #future employment
  lh14=linearHypothesis(modelolh,c(" Pergunta_1189_2 =0", " Pergunta_1189_3 =0")) #interest rate
  lh15=linearHypothesis(modelolh,c(" Pergunta_1194_2 =0", " Pergunta_1194_3 =0")) #purchase of durable
  
  lh16=linearHypothesis(modelolh,c(" Pergunta_1177_2 =0", " Pergunta_1177_3 =0", " Pergunta_1147_2 =0", " Pergunta_1147_3 =0"," Pergunta_1182_2 =0", " Pergunta_1182_3 =0"))
  lh17=linearHypothesis(modelolh,c(" Pergunta_1178_2 =0", " Pergunta_1178_3 =0", " Pergunta_1149_2 =0", " Pergunta_1149_3 =0"," Pergunta_1183_2 =0", " Pergunta_1183_3 =0"))
  
  
  x=rbind(lh1$`Pr(>F)`[2], lh2$`Pr(>F)`[2], lh3$`Pr(>F)`[2], lh4$`Pr(>F)`[2],
           lh5$`Pr(>F)`[2], lh6$`Pr(>F)`[2], lh7$`Pr(>F)`[2], lh8$`Pr(>F)`[2],
           lh9$`Pr(>F)`[2], lh10$`Pr(>F)`[2], lh11$`Pr(>F)`[2], lh12$`Pr(>F)`[2],
           lh13$`Pr(>F)`[2], lh14$`Pr(>F)`[2], lh15$`Pr(>F)`[2], lh16$`Pr(>F)`[2],
          lh17$`Pr(>F)`[2])
  x=as.data.frame(x)
  x$V1= x$V1[x$V1<=0.01]=0.000
  row.names(x) <- c("Demográficas","Renda", "Escolaridade", "Cidade", "Idade", "IPCA",
                    "Opinião", "Current situation of country", "Future situation of country", 
                    "Current sit. household", "Future sit. household", "Current employment",
                    "Future employment", "Interest rate", "Purchase of durable", 
                    "Current economic indicators", "Future economic indicators")
  colnames(x) <- c("P-Values")
  xtable(x)

###########################################



#formula e modelo 4

  formula4 = Resposta ~ 0 + Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
    Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + 
    Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
    Idade_3 + Idade_4 + Sexo_2 + Alimentação.e.bebidas + Habitação  + Artigos.de.residência + Vestuário +
    Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação + Learn_4em8 + Pergunta_1177_2 + 
    Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
    Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
    Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
    Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
    Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3 +
    (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
       Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + 
       Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
       Idade_3 + Idade_4 + Sexo_2 + Learn_4em8) * Previsao_Focus +
    
    (Renda_2 + Renda_3 + Renda_4 + Escolaridade_2 +
       Escolaridade_3 + Escolaridade_4 + Cidade_2 + Cidade_3 + 
       Cidade_4 + Cidade_5 + Cidade_6 + Cidade_7 + Idade_2 + 
       Idade_3 + Idade_4 + Sexo_2 + Learn_4em8) * (Alimentação.e.bebidas + Habitação  + Artigos.de.residência + Vestuário +
                                                     Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação) +
    (Pergunta_1177_2 + 
       Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
       Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
       Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
       Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
       Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * Previsao_Focus +
    (Pergunta_1177_2 + 
       Pergunta_1177_3 + Pergunta_1178_2 + Pergunta_1178_3 +
       Pergunta_1147_2 + Pergunta_1147_3 + Pergunta_1149_2 + 
       Pergunta_1149_3 + Pergunta_1182_2 + Pergunta_1182_3 + 
       Pergunta_1183_2 + Pergunta_1183_3 + Pergunta_1189_2 + 
       Pergunta_1189_3 + Pergunta_1194_2 + Pergunta_1194_3) * (Alimentação.e.bebidas + Habitação  + Artigos.de.residência + Vestuário +
                                                                 Transportes + Saúde.e.cuidados.pessoais + Despesas.pessoais + Educação + Comunicação)
  
  
  
  
  modelo4 = lm(formula4, teste)
  summary(modelo4)
  
  
  linearHypothesis(modelo4,c("Cidade_2+Cidade_3+Cidade_4+Cidade_5+Cidade_6+Cidade_7=0", "Renda_2+Renda_3=0"))


#write.table(teste, "teste.csv", sep = ";", col.names = TRUE)

