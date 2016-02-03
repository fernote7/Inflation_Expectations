microdados_modelos = read.csv2("Dados/Microdados_Filtrados/microdados_basefinal_modelos_040815.csv")
#microdados_modelos = read.csv2("Dados/Microdados_Filtrados/microdados_basefinal_modelos_040815.csv")
microdados_modelos[,"Mes_Ano"] = chron(as.character(microdados_modelos[,"Mes_Ano"]),
                                       format = "d/m/y", out.format = "d/m/y")
microdados_modelos[,"Ano"] = year(microdados_modelos[,"Mes_Ano"])
dados = microdados_modelos
microdados_modelos2 = read.csv2("Dados/Microdados_Filtrados/IPCA/Pasta3.csv", encoding = c('utf-8'))
microdados_modelos2[,"Mes_Ano"] = chron(as.character(microdados_modelos2[,"Mes_Ano"]),
                                        format = "d/m/y", out.format = "d/m/y")


#TWITTER
tweet=read.csv2("Dados/Twitter/base/tweetglobo.csv")
colnames(tweet) <- c("Mes_Ano", "ano", "mes", "contagem")
tweet = tweet[1:53,]
tweet[,"Mes_Ano"] = chron(as.character(tweet[,"Mes_Ano"]),
                          format = "d/m/y", out.format = "d/m/y")
keeps <- c("Mes_Ano","contagem")
tweet=tweet[keeps]
microdados_modelos = merge(microdados_modelos, tweet)
























tweetall = read.csv2("/home/fteixeira/Dropbox/10 Expectativas de inflação - Brasil/ProgramasTD64/Dados/Twitter/tweet_mes.csv", fileEncoding = "ISO-8859-3")
tweetall = tweetall[36:98,]
require(corrgram)
t2all = tweetall[,3:21]
corrgram(t2all)
corr=cor(t2all)
