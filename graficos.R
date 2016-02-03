## Carregando funções necessárias
nomes = list("ingrid.oliveira", "fernando.teixeira")
for (nome in nomes)
{
    direc = paste0("C:\\Users\\", nome, "\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\")
    try(setwd(direc), silent = TRUE)
    
}
rm(nome, nomes)
source(paste0(direc, "\\Funcoes\\Modelos\\bases\\base_modelo_tweet.R"), encoding = c("utf8"))
##



###Criando variáveis base 100 e depois log para aggdata
    for(i in 1:79){ if(i == 1){aggdata$Resposta2[i] = 100} else{aggdata$Resposta2[i] = aggdata$Resposta[i]*aggdata$Resposta2[1]/aggdata$Resposta[1]}}
    for(i in 1:79){ if(i == 1){aggdata$IPCA2[i] = 100} else{aggdata$IPCA2[i] = aggdata$IPCA[i]*aggdata$IPCA2[1]/aggdata$IPCA[1]}}
    for(i in 1:79){ if(i == 1){aggdata$Previsao_Focus2[i] = 100} else{aggdata$Previsao_Focus2[i] = aggdata$Previsao_Focus[i]*aggdata$Previsao_Focus2[1]/aggdata$Previsao_Focus[1]}}
    for(i in 1:79){ if(i == 1){aggdata$News2[i] = 100} else{aggdata$News2[i] = aggdata$News[i]*aggdata$News2[1]/aggdata$News[1]}}
    aggdata$Resposta2 = log(aggdata$Resposta2) 
    aggdata$IPCA2 = log(aggdata$IPCA2)
    aggdata$Previsao_Focus2 = log(aggdata$Previsao_Focus2)
    aggdata$News2 = log(aggdata$News2) 



###Plotando as séries de tweets de infla e tweets totais
    aggdata2 = ts(cbind(aggdata$News, aggdata$newst), start = c(2009,3), frequency = 12)
    colnames(aggdata2) = c('Total Notícias', 'Notícias Inflação')
    
    par(mfrow=c(2,1), xpd=FALSE, oma=c(0,1,0,0), mar=c(7,4,4,2), cex.main=0.8)
    plot(aggdata2[,1], type = 'l', main="", axes=F, xlab='Ano', ylab='tweets', cex=0.6)
    grid()
    box()
    axis(side = 1)
    axis(side = 2, las = 1, cex.axis = 0.6)
    par(xpd=TRUE, mar=c(7,4,2,2))
    leg.txt = c("Inflação", "Total")
    legend(2011,-300, leg.txt,col=c("black", "red"),lwd=c(1,1,1),lty=c(1,1,1),seg.len=2,bty="n", cex = 0.85,ncol=3)
    par(xpd=FALSE, cex.main=0.8)
    plot(aggdata2[,2], type = 'l', main="", axes=F, xlab='Ano', col='red', ylab='tweets')
    grid()
    box()
    axis(side = 1)
    axis(side = 2, las = 1, cex.axis = 0.6)
    
    
    dev.off()


###Plotando as séries comparadas sem tweets
    aggdatats = ts(aggdata, start = c(2009,3), frequency = 12)
    colnames(aggdatats) = colnames(aggdata)
    plot(aggdatats[,11], type = 'l', main="", axes=F, xlab='Ano', ylab='valor', cex=0.6)
    lines(aggdatats[,10], col='red', lty=2)
    lines(aggdatats[,12], col='blue', lty=3)
    grid()
    box()
    axis(side = 1)
    axis(side = 2, las = 1, cex.axis = 0.6)
    leg.txt = c("IPCA", "Resposta", "Focus")
    legend("topleft", leg.txt,col=c("black", "red", "blue"),lwd=c(1,1,1),lty=c(1,2,3),seg.len=2,bty="n", cex = 0.85,ncol=3)


###Plotando as séries comparadas com tweets
    plot(aggdatats[,13], type = 'l', main="", axes=F, xlab='Ano', ylab='valor', col='green', cex=0.6, ylim=c(4,10))
    lines(aggdatats[,11], col='red', lty=2)
    lines(aggdatats[,12], col='blue', lty=3)
    lines(aggdatats[,10], lty=4)
    grid()
    box()
    axis(side = 1)
    axis(side = 2, las = 1, cex.axis = 0.6)
    leg.txt = c("Resposta", "IPCA", "Focus", "Tweets")
    legend(2009,10.2, leg.txt,col=c("black", "red", "blue", "green"),lwd=c(1,1,1),lty=c(1,2,3),seg.len=2,bty="n", cex = 0.5,ncol=1)


## Gráficos a serem rodadas com as séries completas - base ADL tweet
    quanti <- aggdata[,c("Resposta","IPCA","Previsao_Focus")]
    cor(quanti)
    
    
    quanti <- as.data.frame(cbind(aggdata$IPCA[13:121],aggdata$Resposta[1:109],
                                  aggdata$Previsao_Focus[1:109]))
    colnames(quanti) <- c("IPCA", "Resposta", "Focus")
    # cor(quanti)
    cor.test(quanti[,2],quanti[,1])
    cor.test(quanti[,1],quanti[,3])
    # t.test(quanti[,1],quanti[,4])
    
    erro_focus = aggdata$Previsao_Focus[1:109]-aggdata$IPCA[13:121]
    erro_cons = aggdata$Resposta[1:109]-aggdata$IPCA[13:121]
    erro2 = as.data.frame(cbind(erro_focus,erro_cons))
    erro = ts(erro2, start = c(2005,9), frequency = 12)
    
    plot(erro[,1], type = "l", main = "", axes = F,lty=1, pch = 20, xlab="Ano", col=c("red"), ylab="Erro de previsão", ylim=c(-4,6))
    polygon(c(time(erro), rev(time(erro))), c(erro[,2], rev(erro[,1])),
            col=gray(0.85), border = NA, density = 10)
    lines(erro[,2], lty=2, pch = 20,col=c("blue"))
    lines(erro[,1], lty=1, pch = 20,col=c("red"))
    grid()
    box()
    axis(side = 1)
    axis(side = 2, las = 1)
    leg.txt = c("Focus", "Consumidor")
    legend("bottomleft", leg.txt,col=c("red", "blue"),lwd=c(1,1),lty=c(1,2),seg.len=2,bty="n", cex = 0.85)
    
    
    comp = ts(quanti, start = c(2005,9), frequency = 12)
    
    plot(comp[,1], type = "l", main = "", axes = F,lty=1, pch = 20, xlab="Ano", col=c("red"), ylab="Percentual", ylim=c(3,10.5))
    #polygon(c(time(erro), rev(time(erro))), c(erro[,2], rev(erro[,1])),
    #        col=gray(0.85), border = NA, density = 10)
    lines(comp[,2], lty=2, pch = 20,col=c("blue"))
    lines(comp[,3], lty=3, pch = 20,col=c("black"))
    grid()
    box()
    axis(side = 1)
    axis(side = 2, las = 1)
    leg.txt = c("IPCA", "Focus", "Consumidor")
    legend("topleft", leg.txt,col=c("red", "black", "blue"),lwd=c(1,1,1),lty=c(1,3,2),seg.len=2,bty="n", cex = 0.85,ncol=3)

