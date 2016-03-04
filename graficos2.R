###DADOS DB
###Dados de inflação
query = "SELECT * from Data_Tweets WHERE
tweet_text LIKE '%preÃ§o%' AND (tweet_text LIKE '%sobe%' 
OR tweet_text LIKE '%subir%' 
OR tweet_text LIKE '%alta%'
OR tweet_text LIKE '%aument%'
OR tweet_text LIKE '%subiu%') AND date <= '2015-09-31' AND date >= '2009-03-01' 
AND (account LIKE '@cbonlinedf'
OR account LIKE '@atarde'
OR account LIKE '@em_com'
OR account LIKE '@Estadao'
OR account LIKE '@folha'
OR account LIKE '@JornalOGlobo'
OR account LIKE '@valor_economico'
OR account LIKE '@zerohora'
OR account LIKE '@jc_pe')
"

dados3 = dbGetQuery(db,query)


dados3[,"date"] = chron(as.character(dados3[,"date"]),
                       format = "Y-m-d", out.format = "m/y")
dados3[,"date"] = paste0("01", dados3[,"date"])
dados3[,"date"] = chron(as.character(dados3[,"date"]),
                       format = "dmy", out.format = "d/m/y")

dados30 = aggregate(dados$number_of_tweets, by=list(dados$date), FUN = "sum")
dados31 = aggregate(dados2$number_of_tweets, by=list(dados2$date), FUN = "sum")
dados32 = aggregate(dados3$number_of_tweets, by=list(dados3$date), FUN = "sum")


par(mfrow=c(3,1), xpd=FALSE, mar=c(2,4,2,2), oma=c(2,0,0,0))
plot(dados31, type = 'l', main="", xlab='', ylab='inflação', 
     cex=0.5, yaxt='n')
axis(side = 2, las = 1, cex.axis = 0.6)
grid()
box()
plot(dados30,col='red', lty=2, type = 'l', main="", xlab='', ylab='encarece',
     yaxt='n')
axis(side = 2, las = 1, cex.axis = 0.6)
grid()
box()
par(mar=c(7,4,2,2))
plot(dados32,col='blue', lty=3, type = 'l', main="", xlab='Ano', ylab='preço', 
     yaxt='n')
grid()
box()
axis(side = 2, las = 1, cex.axis = 0.6)



par(xpd=TRUE, mar=c(7,4,2,2))
leg.txt = c("tweets inf","   " ,"tweets tot")
legend(2011,-12, leg.txt,col=c("black","black","red"),lwd=c(1,NA,1),lty=c(1,NA,2),seg.len=2,bty="n", cex = 0.6,ncol=3)
par(xpd=FALSE, cex.main=0.8)
plot(aggdata2[,3], type = 'l', main="", axes=F, xlab='Ano', col='magenta', ylab='tweets', lty=3, cex=0.5,ylim=c(0,14))
lines(aggdata2[,4],col='blue', lty=4)
grid()
box()
axis(side = 1)
axis(side = 2, las = 1, cex.axis = 0.6)
par(xpd=TRUE, mar=c(7,4,2,2))
leg.txt = c("retweets inf","   " , "retweets tot")
legend(2011,-12, leg.txt,col=c("magenta","black", "blue"),lwd=c(1,NA,1),lty=c(3,NA,4),seg.len=2,bty="n", cex = 0.6,ncol=3)


