## Importando microdados
microdados_modelos = read.csv2("C:\\Users\\ingrid.oliveira\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\Dados\\Microdados_Filtrados\\Microdados_Modelos_set05aset15.csv",
                               stringsAsFactors = FALSE, dec = ".")
microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                       format = "d/m/y", out.format = "d/m/y")
microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])
microdados_modelos = microdados_modelos[complete.cases(microdados_modelos[,c("Mes_Ano", "Codigo", "Cod_Externo", "Resposta", "Renda", "Escolaridade", "Cidade", "Idade", "Sexo")]),]
aggdata = microdados_modelos

aggdata = aggregate(aggdata[,"Resposta"], by = list(aggdata[,"Mes_Ano"], aggdata[,"Cidade"], aggdata[,"Renda"]),
                    FUN = median, na.rm = TRUE)
colnames(aggdata) = c("Mes_Ano", "Cidade", "Renda", "Resposta")

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
expinf_mensal = aggregate(aggdata[,"Resposta"], by = list(aggdata[,"Mes_Ano"]),
                          FUN = sum, na.rm = TRUE)
