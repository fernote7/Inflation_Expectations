#################################################################################
agreg_renda = function(aggdata){
  
  # São Paulo
  aggdata[which(aggdata[,"Cidade"] == 1),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 1),"Resposta"]*0.4278
  
  # Rio de Janeiro
  aggdata[which(aggdata[,"Cidade"] == 2),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 2),"Resposta"]*0.2591
  
  # Belo Horizonte
  aggdata[which(aggdata[,"Cidade"] == 3),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 3),"Resposta"]*0.0660
  
  # Brasília
  aggdata[which(aggdata[,"Cidade"] == 4),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 4),"Resposta"]*0.1120
  
  # Salvador
  aggdata[which(aggdata[,"Cidade"] == 5),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 5),"Resposta"]*0.0532
  
  # Porto Alegre
  aggdata[which(aggdata[,"Cidade"] == 6),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 6),"Resposta"]*0.0543
  
  # Recife
  aggdata[which(aggdata[,"Cidade"] == 7),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 7),"Resposta"]*0.0276
  
  # Agregando por mês
  expinf_mensal = aggregate(aggdata[,"Resposta"], by = list(aggdata[,"Mes_Ano"], aggdata[,"Renda"]),
                            FUN = sum, na.rm = TRUE)
  return(expinf_mensal)
}


#################################################################################
agreg_cidade = function(aggdata){
  
  # Renda 1
  aggdata[which(aggdata[,"Renda"] == 1),"Resposta"] =
    aggdata[which(aggdata[,"Renda"] == 1),"Resposta"]*0.2409
  
  # Renda 2
  aggdata[which(aggdata[,"Renda"] == 2),"Resposta"] =
    aggdata[which(aggdata[,"Renda"] == 2),"Resposta"]*0.2512
  
  # Renda 3
  aggdata[which(aggdata[,"Renda"] == 3),"Resposta"] =
    aggdata[which(aggdata[,"Renda"] == 3),"Resposta"]*0.2462
  
  # Renda 4
  aggdata[which(aggdata[,"Renda"] == 4),"Resposta"] =
    aggdata[which(aggdata[,"Renda"] == 4),"Resposta"]*0.2617
  
  # Agregando por mês
  expinf_mensal = aggregate(aggdata[,"Resposta"], by = list(aggdata[,"Mes_Ano"], aggdata[,"Cidade"]),
                            FUN = sum, na.rm = TRUE)
  return(expinf_mensal)
}


#################################################################################
agreg_fator = function(aggdata, var_agreg){
  
  # São Paulo
  aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 1),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 1),"Resposta"]*0.1033
  aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 2),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 2),"Resposta"]*0.1058
  aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 3),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 3),"Resposta"]*0.1008
  aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 4),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 1 & aggdata[,"Renda"] == 4),"Resposta"]*0.1179
  
  # Rio de Janeiro
  aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 1),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 1),"Resposta"]*0.0626
  aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 2),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 2),"Resposta"]*0.0688
  aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 3),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 3),"Resposta"]*0.0651
  aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 4),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 2 & aggdata[,"Renda"] == 4),"Resposta"]*0.0626
  
  # Belo Horizonte
  aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 1),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 1),"Resposta"]*0.0160
  aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 2),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 2),"Resposta"]*0.0164
  aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 3),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 3),"Resposta"]*0.0164
  aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 4),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 3 & aggdata[,"Renda"] == 4),"Resposta"]*0.0172
  
  # Brasília
  aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 1),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 1),"Resposta"]*0.0224
  aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 2),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 2),"Resposta"]*0.0257
  aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 3),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 3),"Resposta"]*0.0311
  aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 4),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 4 & aggdata[,"Renda"] == 4),"Resposta"]*0.0328
  
  # Salvador
  aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 1),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 1),"Resposta"]*0.0179
  aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 2),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 2),"Resposta"]*0.0137
  aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 3),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 3),"Resposta"]*0.0116
  aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 4),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 5 & aggdata[,"Renda"] == 4),"Resposta"]*0.0100
  
  # Porto Alegre
  aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 1),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 1),"Resposta"]*0.0116
  aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 2),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 2),"Resposta"]*0.0137
  aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 3),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 3),"Resposta"]*0.0145
  aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 4),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 6 & aggdata[,"Renda"] == 4),"Resposta"]*0.0145
  
  # Recife
  aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 1),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 1),"Resposta"]*0.0071
  aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 2),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 2),"Resposta"]*0.0071
  aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 3),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 3),"Resposta"]*0.0067
  aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 4),"Resposta"] =
    aggdata[which(aggdata[,"Cidade"] == 7 & aggdata[,"Renda"] == 4),"Resposta"]*0.0067
  
  # Agregando por mês
  expinf_mensal = aggregate(aggdata[,"Resposta"], by = list(aggdata[,"Mes_Ano"], aggdata[,var_agreg]),
                            FUN = sum, na.rm = TRUE)
  return(expinf_mensal)
}


















## Pacotes necessários
# require(dynlm)
# require(quantmod)
# require(tseries)
require(chron)
require(lubridate)
# require(xtable)
# require(plyr)
require(xts)
# require(urca)
# require(tseries)
# require(MSBVAR)
require(dygraphs)


## Importando microdados
microdados_modelos = read.csv2("C:\\Users\\ingrid.oliveira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\microdados_basefinal_modelos_040815.csv")
microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                       format = "d/m/y", out.format = "d/m/y")
microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])
microdados_modelos = microdados_modelos[complete.cases(microdados_modelos[,c("Mes_Ano", "Codigo", "Cod_Externo", "Resposta", "Renda", "Escolaridade", "Cidade", "Idade", "Sexo")]),]
dados = microdados_modelos
# microdados_modelos2 = read.csv2("C:\\Users\\ingrid.oliveira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\IPCA\\Pasta3.csv")
# microdados_modelos2[,"Mes_Ano"] = chron(as.character(microdados_modelos2[,"Mes_Ano"]),
#                                         format = "d/m/y", out.format = "d/m/y")


## Expectativa de Inflação por Fator Determinante

  # Renda
  expinf_renda = aggregate(microdados_modelos$Resposta, 
                           by = list(microdados_modelos$Mes_Ano, microdados_modelos$Renda, microdados_modelos$Cidade),
                           FUN = mean)
  colnames(expinf_renda) = c("Mes_Ano", "Renda", "Cidade", "Resposta")
  expinf_renda = agreg_renda(expinf_renda)
  expinf_renda = xts(cbind(expinf_renda[which(expinf_renda$Group.2 == 1), "x"],
                           expinf_renda[which(expinf_renda$Group.2 == 2), "x"],
                           expinf_renda[which(expinf_renda$Group.2 == 3), "x"],
                           expinf_renda[which(expinf_renda$Group.2 == 4), "x"]),
                      seq(as.Date("2005-12-01"), as.Date("2013-12-01"), by = "month"),
                      freq = 12)
  colnames(expinf_renda) = c("Renda 1", "Renda 2", "Renda 3", "Renda 4")
  dygraph(expinf_renda)%>%
    dySeries("Renda 1", strokeWidth = 1.5, strokePattern = "dashed", color = "steelblue") %>%
    dySeries("Renda 2", strokeWidth = 1.5, strokePattern = "dotted", color = "seagreen") %>%
    dySeries("Renda 3", strokeWidth = 1.5, strokePattern = "dotdash", color = "indianred") %>%
    dySeries("Renda 4", strokeWidth = 1.5, strokePattern = "solid", color = "orange") %>%
    # dyHighlight(highlightCircleSize = 3, 
    #             highlightSeriesBackgroundAlpha = 0.2,
    #             hideOnMouseOut = FALSE) %>%
    # dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y", label = "% acumulado em 12 meses") %>%
    dyOptions(gridLineColor = "lightgray")
  
  summary(expinf_renda)

  
  # Escolaridade
  expinf_esc = aggregate(microdados_modelos$Resposta, 
                         by = list(microdados_modelos$Mes_Ano, microdados_modelos$Renda, 
                                   microdados_modelos$Cidade, microdados_modelos$Escolaridade),
                         FUN = mean)
  colnames(expinf_esc) = c("Mes_Ano", "Renda", "Cidade", "Escolaridade", "Resposta")
  expinf_esc = agreg_fator(expinf_esc, "Escolaridade")
  expinf_esc = xts(cbind(expinf_esc[which(expinf_esc$Group.2 == 1), "x"],
                         expinf_esc[which(expinf_esc$Group.2 == 4), "x"],
                         expinf_esc[which(expinf_esc$Group.2 == 5), "x"],
                         expinf_esc[which(expinf_esc$Group.2 == 6), "x"]),
                    seq(as.Date("2005-12-01"), as.Date("2013-12-01"), by = "month"),
                    freq = 12)
  colnames(expinf_esc) = c("Esc 1", "Esc 4", "Esc 5", "Esc 6")
  dygraph(expinf_esc)%>%
    dySeries("Esc 1", strokeWidth = 1.5, strokePattern = "dashed", color = "steelblue") %>%
    dySeries("Esc 4", strokeWidth = 1.5, strokePattern = "dotted", color = "seagreen") %>%
    dySeries("Esc 5", strokeWidth = 1.5, strokePattern = "dotdash", color = "indianred") %>%
    dySeries("Esc 6", strokeWidth = 1.5, strokePattern = "solid", color = "orange") %>%
    # dyHighlight(highlightCircleSize = 3, 
    #             highlightSeriesBackgroundAlpha = 0.2,
    #             hideOnMouseOut = FALSE) %>%
    # dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y", label = "% acumulado em 12 meses") %>%
    dyOptions(gridLineColor = "lightgray")

  summary(expinf_esc) # resultado esquisito, dizendo que consumidores com baixa
                      # escolaridade (escolaridade 1 = junção das escolaridades
                      # 1, 2 e 3)
  
  
  # Cidade
  expinf_cidade = aggregate(microdados_modelos$Resposta, 
                            by = list(microdados_modelos$Mes_Ano, microdados_modelos$Renda, 
                                      microdados_modelos$Cidade),
                         FUN = mean)
  colnames(expinf_cidade) = c("Mes_Ano", "Renda", "Cidade", "Resposta")
  expinf_cidade = agreg_cidade(expinf_cidade)
  expinf_cidade = xts(cbind(expinf_cidade[which(expinf_cidade$Group.2 == 1), "x"],
                            expinf_cidade[which(expinf_cidade$Group.2 == 2), "x"],
                            expinf_cidade[which(expinf_cidade$Group.2 == 3), "x"],
                            expinf_cidade[which(expinf_cidade$Group.2 == 4), "x"],
                            expinf_cidade[which(expinf_cidade$Group.2 == 5), "x"],
                            expinf_cidade[which(expinf_cidade$Group.2 == 6), "x"],
                            expinf_cidade[which(expinf_cidade$Group.2 == 7), "x"]),
                      seq(as.Date("2005-12-01"), as.Date("2013-12-01"), by = "month"),
                      freq = 12)
  colnames(expinf_cidade) = c("SP", "RJ", "BH", "BS", "SD", "PA", "RC")
  dygraph(expinf_cidade)%>%
    dySeries("SP", strokeWidth = 1.5, strokePattern = "dashed", color = "steelblue") %>%
    dySeries("RJ", strokeWidth = 1.5, strokePattern = "dotted", color = "seagreen") %>%
    dySeries("BH", strokeWidth = 1.5, strokePattern = "dotdash", color = "indianred") %>%
    dySeries("BS", strokeWidth = 1.5, strokePattern = "solid", color = "orange") %>%
    dySeries("SD", strokeWidth = 1.5, strokePattern = "dashed", color = "gray") %>%
    dySeries("PA", strokeWidth = 1.5, strokePattern = "dotted", color = "black") %>%
    dySeries("RC", strokeWidth = 1.5, strokePattern = "dotdash", color = "palevioletred") %>%
    dyHighlight(highlightCircleSize = 3, 
               highlightSeriesBackgroundAlpha = 0.2,
               hideOnMouseOut = FALSE) %>%
    # dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y", label = "% acumulado em 12 meses") %>%
    dyOptions(gridLineColor = "lightgray")

  summary(expinf_cidade)
  
  
  # Idade
  teste_idade4 = microdados_modelos[which(microdados_modelos$Idade == 4),]
  q1 = quantile(teste_idade4[,"Resposta"], 0.25) - 1.5*(quantile(teste_idade4[,"Resposta"], 0.75) - 
                                                        quantile(teste_idade4[,"Resposta"], 0.25))
  q3 = quantile(teste_idade4[,"Resposta"], 0.75) + 1.5*(quantile(teste_idade4[,"Resposta"], 0.75) - 
                                                        quantile(teste_idade4[,"Resposta"], 0.25))
  teste_idade4 = microdados_modelos[which(microdados_modelos[,"Resposta"] >= q1 &
                                          microdados_modelos[,"Resposta"] <= q3),]
  teste_dataidade4 = aggregate(teste_idade4$Resposta, 
                               by = list(teste_idade4$Mes_Ano, teste_idade4$Renda, 
                                         teste_idade4$Cidade, teste_idade4$Idade),
                               FUN = mean)
  final_dataidade4 = agreg_fator(teste_dataidade4, "Idade")
  
  expinf_idade = aggregate(microdados_modelos$Resposta, 
                           by = list(microdados_modelos$Mes_Ano, microdados_modelos$Renda, 
                                     microdados_modelos$Cidade, microdados_modelos$Idade),
                         FUN = mean)
  colnames(expinf_idade) = c("Mes_Ano", "Renda", "Cidade", "Idade", "Resposta")
  expinf_idade = agreg_fator(expinf_idade, "Idade")
  expinf_idade = xts(cbind(expinf_idade[which(expinf_idade$Group.2 == 1), "x"],
                           expinf_idade[which(expinf_idade$Group.2 == 2), "x"],
                           expinf_idade[which(expinf_idade$Group.2 == 3), "x"],
                           expinf_idade[which(expinf_idade$Group.2 == 4), "x"]),
                      seq(as.Date("2005-12-01"), as.Date("2013-12-01"), by = "month"),
                      freq = 12)
  colnames(expinf_idade) = c("Idade 1", "Idade 2", "Idade 3", "Idade 4")
  dygraph(expinf_idade)%>%
    dySeries("Idade 1", strokeWidth = 1.5, strokePattern = "dashed", color = "steelblue") %>%
    dySeries("Idade 2", strokeWidth = 1.5, strokePattern = "dotted", color = "seagreen") %>%
    dySeries("Idade 3", strokeWidth = 1.5, strokePattern = "dotdash", color = "indianred") %>%
    dySeries("Idade 4", strokeWidth = 1.5, strokePattern = "solid", color = "orange") %>%
    # dyHighlight(highlightCircleSize = 3, 
    #             highlightSeriesBackgroundAlpha = 0.2,
    #             hideOnMouseOut = FALSE) %>%
    # dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y", label = "% acumulado em 12 meses") %>%
    dyOptions(gridLineColor = "lightgray")

  summary(expinf_idade)
  
  # Sexo 
  expinf_sexo = aggregate(microdados_modelos$Resposta, 
                          by = list(microdados_modelos$Mes_Ano, microdados_modelos$Renda, 
                                    microdados_modelos$Cidade, microdados_modelos$Sexo),
                         FUN = mean)
  colnames(expinf_sexo) = c("Mes_Ano", "Renda", "Cidade", "Sexo", "Resposta")
  expinf_sexo = agreg_fator(expinf_sexo, "Sexo")
  expinf_sexo = xts(cbind(expinf_sexo[which(expinf_sexo$Group.2 == 1), "x"],
                          expinf_sexo[which(expinf_sexo$Group.2 == 2), "x"]),
                     seq(as.Date("2005-12-01"), as.Date("2013-12-01"), by = "month"),
                     freq = 12)
  colnames(expinf_sexo) = c("Homens", "Mulheres")
  dygraph(expinf_sexo)%>%
    dySeries("Homens", strokeWidth = 1.5, strokePattern = "dashed", color = "steelblue") %>%
    dySeries("Mulheres", strokeWidth = 1.5, strokePattern = "dotted", color = "palevioletred") %>%
    # dyHighlight(highlightCircleSize = 3, 
    #             highlightSeriesBackgroundAlpha = 0.2,
    #             hideOnMouseOut = FALSE) %>%
    # dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE)
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y", label = "% acumulado em 12 meses") %>%
    dyOptions(gridLineColor = "lightgray")

  summary(expinf_sexo)