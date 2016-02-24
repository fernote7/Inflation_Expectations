install.packages("MSBVAR")
#require(MSBVAR)
require(vars)
detach("package:MSBVAR", unload=TRUE)

teste_granger = as.data.frame(cbind(agg13$icc.sas, agg13$GET))
colnames(teste_granger) = c("icc.sas","get")

granger_ts = ts(teste_granger)

###Primeira estimação Causalidade de granger

leg=var.lag.specification(granger_ts, lagmax = 24)
teste2=granger.test(granger_ts, p =2)
teste2
menor_aic =  200000
q_lag= 0

for(i in 1:length(leg$results[,1])){
  if (leg$results[i,2] < menor_aic){
    menor_aic = leg$results[i,2]
    q_lag = i  
  }
}

cat(" The null hypothesis is that the past p values of X do not help in predicting the value of Y.")



###Melhor GLM para função
teste = func_adf1$serie_diff
teste[,10] = agg12$endividamento[2:108]
colnames(teste) = c("commodities_curr","commodities_const","interest", "icc",
                    "desoc", "icc.sas","icc.lag", "GET", "popularidade", "endividamento", "ipca", "rer", "cambio")

teste = as.data.frame(teste[, c("commodities_curr","commodities_const","interest", 
                                "desoc","icc.lag", "GET", "popularidade", "endividamento", "ipca", "rer", "cambio", "icc.sas")])

bestglm(teste, IC = "AIC", method = "exhaustive")
###

###Segunda forma de estimar Causalidade de Granger

# select lag = 2
VARselect(teste_granger, lag.max = 24, type = "const")

a1=VAR(teste_granger, p=2, type = "const")
plot(a1$varresult$icc.sas$residuals, type='l')
plot(a1$varresult$get$residuals, type='l')

#serial auto-correlation of errors? NO!
dwtest(a1$varresult$get)
dwtest(a1$varresult$icc.sas)
bgtest(a1$varresult$get, order = 1)
bgtest(a1$varresult$icc.sas, order=1)

#seeing if there is stability: YES! (red lines)
stab=stability(a1)
plot(stab)

coint1= ca.jo(teste_granger, type=c("trace"), K=2)
# with 99% confidence we have a combination get+Beta*ICC that is stationary: r=0, 26.40>23.52 
#(r1 means that there would be an easy combination like get+icc)
summary(coint1)
coint2= ca.jo(teste_granger, type=c("eigen"), K=2)
# with 99% confidence we have a combination get+Beta*ICC that is stationary: r=0, 20.88>19.19
summary(coint2)


#a1=VAR(teste, p=3, type = "const")
#a2=VAR(teste, p=3, type = "const")


# causality(a1, cause = 'get')$Instant
# causality(a1, cause = 'get')$Granger
# 
# causality(a1, cause = 'icc.sas')$Instant
# causality(a1, cause = 'icc.sas')$Granger


# for (i in 1:4)
# {
#   cat("LAG =", i)
#   print(causality(VAR(teste_granger, p = i, type = "const"), cause = "get")$Granger)
# }


get2=as.data.frame(teste_granger$get)
get2=ts(teste_granger$get)
get2 = lag(get2,k=3)
get2 = get2[4:108]
get2=as.data.frame(get2)
teste_granger2= cbind(teste_granger$icc.sas[1:105], teste_granger$get[1:105])
colnames(teste_granger2) = c('icc.sas', 'get')
a1=VAR(teste_granger2, p=2, type = "const", exogen = t)
causa1 = causality(a1,cause = 'get')
causa2 = causality(a1,cause = 'icc.sas')

####LIMBO

teste = as.data.frame(cbind(agg13$icc.sas, agg13$GET))
colnames(teste) = c("icc.sas","get")

testets = ts(teste)

#ll2=L(testets[,1],2)
#ll2=L(testets[,2],2)
l1=dynlm(testets[,1]~L(testets[,2],1)+L(testets[,1],2), testets)
l2=dynlm(testets[,1]~L(testets[,2],1)+L(testets[,1],1)+L(testets[,2],2)+L(testets[,1],2), testets)
l3=dynlm(testets[,1]~L(testets[,2],1)+L(testets[,1],1)+L(testets[,2],2)+L(testets[,1],2)+L(testets[,2],3)+L(testets[,1],3), testets)
l4=dynlm(testets[,1]~L(testets[,2],1)+L(testets[,1],1)+L(testets[,2],2)+L(testets[,1],2)+L(testets[,2],3)+L(testets[,1],3)+L(testets[,2],4)+L(testets[,1],4), testets)

AIC(l1)
AIC(l2)
AIC(l3)
AIC(l4)

teste2=granger.test(teste, p =2)
