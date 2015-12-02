## Carregando funções necessárias
  nomes = list("ingrid.oliveira", "fernando.teixeira")
  for (nome in nomes)
  {
    direc = paste0("C:\\Users\\", nome, "\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\")
    try(setwd(direc), silent = TRUE)
    
  }
  source(paste0(direc, "\\Funcoes\\Modelos\\bases\\base_modelo_tweet.R"), encoding = c("utf8"))
##
##Carregando Pacotes
  require(corrgram)

##
##Plotando
  plot(microdados_modelos$Resposta,type='p')
  plot(microdados_modelos$Idade,type='p')
  plot(microdados_modelos$Escolaridade,type='p')
  matplot(microdados_modelos$IPCA,type='l')
  plot(microdados_modelos$Previsao_Focus,type='p')
  plot(microdados_modelos$IPCA~microdados_modelos$Resposta)
  plot(microdados_modelos$IPCA~microdados_modelos$Resposta, xlim=c(0,10))
  plot(microdados_modelos$IPCA~microdados_modelos$Previsao_Focus)
  plot(microdados_modelos$contagem, type='p')
  plot(microdados_modelos$IPCA~microdados_modelos$contagem)
##
  
##CORRELACOES  
  quanti <- microdados_modelos[,c("Resposta","IPCA","Previsao_Focus", "contagem")]
  cor(quanti)
  corrgram(quanti,order=TRUE)
##
  

  
##Plotando2
  plot(aggdata$Resposta,type='p')
  plot(aggdata$IPCA,type='p')
  plot(aggdata$Previsao_Focus,type='p')
  plot(aggdata$IPCA~aggdata$Resposta)
  plot(aggdata$IPCA~aggdata$Resposta, xlim=c(4,10), ylim=c(4,10))
  plot(aggdata$IPCA~aggdata$Previsao_Focus)
  plot(aggdata$IPCA~aggdata$Tweets)
##
##CORRELACOES  
  quanti <- aggdata[,c("Resposta","IPCA","Previsao_Focus", "Tweets")]
  correlacao=cor(quanti)
  corrgram(quanti,order=TRUE, labels = c("Media", "Inf. Exp.", "IPCA", "Focus"))
##

##Dados TWITTER
  tweet_mess = read.csv2("Dados\\Twitter\\tweet_mes.csv")
  tweet_mes = tweet_mess[1:98,3:20]
  tweet_dia = read.csv2("Dados\\Twitter\\tweet_dia.csv")
  
##Gráficos
  tweet_mes2 = ts(tweet_mess[1:98,3:20], start = c(2007,8), end = c(2015,9), frequency = 12)
  matplot(tweet_mes2, type="l", lty = 1)
  boxplot(tweet_mes)
  boxplot(tweet_mes$Soma.de.estadao,tweet_mes$Soma.de.folha,
          tweet_mes$Soma.de.zh,tweet_mes$Soma.de.cb, tweet_mes$Soma.de.atarde,
          tweet_mes$Soma.de.valor, tweet_mes$Soma.de.extra, tweet_mes$Soma.de.oglobo)
  boxplot(tweet_mes$Soma.de.g1, tweet_mes$Soma.de.r7, tweet_mes$Soma.de.uol, tweet_mes$Soma.de.uolnews)
  boxplot(tweet_mes$Soma.de.veja, tweet_mes$Soma.de.ccapital, tweet_mes$Soma.de.istoe, tweet_mes$Soma.de.epoca)
  boxplot(tweet_mes$Soma.de.globonews, tweet_mes$Soma.de.band_news)
  
  
##Séries temporais por área
  jornais <- ts(tweet_mess$Soma.de.total.jornal, start = c(2007,8), end = c(2015,9), frequency = 12)
  portais <- ts(tweet_mess$Soma.de.total.portal, start = c(2007,8), end = c(2015,9), frequency = 12) 
  revistas <- ts(tweet_mess$Soma.de.total.revista, start = c(2007,8), end = c(2015,9), frequency = 12)
  tv <- ts(tweet_mess$Soma.de.total.tv, start = c(2007,8), end = c(2015,9), frequency = 12)
  tudo <- cbind(jornais, portais, revistas, tv)
  dygraph(tudo)
  matplot(tudo, type="l", lty=1)
  
  #média por veículo de mídia
  tudo2 <- cbind(jornais/8, portais/4, revistas/4, tv/2)
  colnames(tudo2)=c("jornais", "portais", "revistas", "tv")
  dygraph(tudo2)
  matplot(tudo2, type="l", lty=1)
  
  quanti <- tweet_mess[1:98,22:25]
  quanti$Soma.de.total.jornal = quanti$Soma.de.total.jornal/8 
  quanti$Soma.de.total.portal = quanti$Soma.de.total.portal/4
  quanti$Soma.de.total.revista = quanti$Soma.de.total.revista/4
  quanti$Soma.de.total.tv = quanti$Soma.de.total.tv/2
  colnames(quanti)=c("jornais", "portais", "revistas", "tv")
  cor(quanti)
  corrgram(quanti,order=FALSE)  
    