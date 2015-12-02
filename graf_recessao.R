nomes = list("ingrid.oliveira", "fernando.teixeira")
for (nome in nomes)
{
  direc = paste0("C:\\Users\\", nome, "\\Dropbox\\10 Expectativas de inflação - Brasil\\ProgramasTD64\\")
  try(setwd(direc), silent = TRUE)
}
rm(nome, nomes)
setwd(direc)

rec=read.csv2("Dados\\Pasta1.csv")

rec=ts(rec, start=c(1980), frequency=4)
hilo <- c(64,179)


plot(rec, axes=F, xlab="Years", ylab="GDP Index", xlim=c(1980,2014.1))
#plot.window(c(1980,2014.1), c(62,79))
polygon(x=c(1981.1,1981.1, 1983.1,1983.1),
        y=c(hilo, rev(hilo)),
        density=NA, col="skyblue", border=NA)
polygon(x=c(1987.3,1987.3, 1988.4,1988.4),
        y=c(hilo, rev(hilo)),
        density=NA, col="skyblue", border=NA)
polygon(x=c(1989.3,1989.3, 1992.1,1992.1),
        y=c(hilo, rev(hilo)),
        density=NA, col="skyblue", border=NA)
polygon(x=c(1995.2,1995.2, 1995.3,1995.3),
        y=c(hilo, rev(hilo)),
        density=NA, col="skyblue", border=NA)
polygon(x=c(1998.1,1998.1, 1999.1,1999.1),
        y=c(hilo, rev(hilo)),
        density=NA, col="skyblue", border=NA)
polygon(x=c(2001.2,2001.2, 2001.4,2001.4),
        y=c(hilo, rev(hilo)),
        density=NA, col="skyblue", border=NA)
polygon(x=c(2003.1,2003.1, 2003.2,2003.2),
        y=c(hilo, rev(hilo)),
        density=NA, col="skyblue", border=NA)
polygon(x=c(2008.4,2008.4, 2009.1,2009.1),
        y=c(hilo, rev(hilo)),
        density=NA, col="skyblue", border=NA)
polygon(x=c(2014.2,2014.2, 2015.1,2015.1),
        y=c(hilo, rev(hilo)),
        density=NA, col="skyblue", border=NA)
box()
axis(side=1)
axis(side=2, las=1)
lines(rec, rec, type="l", lwd=1) # paint again so line comes on top

par(xpd=TRUE)
legend(2004, 210, c("Recession Periods"), box.lwd = 0,box.col = "white",bg = "white", fill=c("skyblue"), cex=0.5,horiz=TRUE)

