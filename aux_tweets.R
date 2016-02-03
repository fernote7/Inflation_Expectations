setwd("C:/Users/fernando.teixeira/Dropbox/10 Expectativas de inflação - Brasil/ProgramasTD64/Dados/Twitter")
tweet=read.csv2("C:/Users/fernando.teixeira/Dropbox/10 Expectativas de inflação - Brasil/ProgramasTD64/Dados/Twitter/tweetglobo.csv")
colnames(tweet) <- c("Mes_Ano", "ano", "mes", "contagem")
tweet = tweet[1:53,]

tweet[,"Mes_Ano"] = chron(as.character(tweet[,"Mes_Ano"]),
                                       format = "d/m/y", out.format = "d/m/y")

keeps <- c("Mes_Ano","contagem")
tweet=tweet[keeps]


t=join(microdados_modelos, tweet, by="Mes_Ano")

t2=t[complete.cases(t),]