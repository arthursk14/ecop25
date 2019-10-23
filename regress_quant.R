# Pacote para leitura de dados
require("readxl")

# Importa dados das variáveis independentes (Desenvolvimento Financeiro - FMI)
FD <- read_excel("FD_Index_Database.xlsx")

# Importa dados das variaveis dependentes (PIB per capita - Banco Mundial)
GDPv <- read_excel("GDP.xlsx")

# Tratamento para as variáveis independentes. Serão utilizados os indicadores Financial Institutions (FI) 
# e Financial Markets (FM). Ambos são agregados que medem o tamanho, o acesso existente e a eficiência 
# dos mercados de intermediação financeira e de capitais. Além disso, será utilizada a média entre FID e FMD.
# Como uma proxy de "Financial Depth", medindo o tamanho do mercado financeiro de cada país.
# A metodologia está descrita no site IMF Data.

# Cria matriz para os dados tratados
require("dplyr")

FD_t = cbind(distinct(FD[,5]),NA,NA,NA,NA,NA,NA)
colnames(FD_t) = c("pais","FI2017","FM2017","FDepth2017","FI1992","FM1992","FDepth1992")

# Loops para adicionar os dados à matriz
j = 1
n = dim(FD)[1]

for (i in 1:n){
  if (FD[i,9] == 2017){
    FD_t[j,2] = FD[i,11]
    FD_t[j,3] = FD[i,12]
    FD_t[j,4] = (FD[i,13]+FD[i,16])/2
    
    j = j+1
    
  }
}

j = 1

for (i in 1:n){
  if (FD[i,9] == 1992){
    FD_t[j,5] = FD[i,11]
    FD_t[j,6] = FD[i,12]
    FD_t[j,7] = (FD[i,13]+FD[i,16])/2
    
    j = j+1
    
  }
}

# Tratamento para as varíaveis dependentes, utilizaremos a variação do PIB per capita entre 1980-2017
# e a variação entre 1980-1992. A metodologia está descrita no site The World Bank Data.

m = dim(GDPv)[2]
GDPv_t = cbind(GDPv[,1],GDPv$'1980',GDPv$'1992',GDPv$'2017',(GDPv$'2017'/GDPv$'1980')-1,(GDPv$'1992'/GDPv$'1980')-1)
colnames(GDPv_t) = c("pais","1980","1992","2017","2017/1980","1992/1980")

# Organizando a base de dados
dados = merge(FD_t,GDPv_t, by = "pais")

require("tidyr")
dados = drop_na(dados)

# Montando as regressões

require("quantreg")
require("viridis")
require("scales")

# Parâmetros
tau = c(0.10,0.25,0.5,0.75,0.90)
colors = viridis(6, begin = 0.25, end = 0.75)

# Regressão 1 - 1992

y = dados$`1992/1980`
x = dados$FDepth1992

lm1 = lm(y ~ x)
rq1 = rq(y ~ x, tau = tau)

windows()
plot(y ~ x,main = "Variação PIB per capita 1992/1980 ~ Intensidade Financeira",
     ylab = "PIB 1992/1980", xlab = "Intensidade Financeira 1992")

abline(lm1,col="red")

for (j in 1:ncol(rq1$coefficients)) {
  abline(coef(rq1)[, j], col = colors[j])
}

legend("topright", legend=c("MQO", "RQ (0.10)", "RQ (0.25)", "RQ (0.50)", "RQ (0.75)", "RQ (0.90)"),
       col=c("red", colors), lty=1, cex=1)

summary(rq1,se='iid')
summary(lm1)

# Regressão 2 - 1992

y = dados$`1992/1980`
x = dados$FI1992

lm2 = lm(y ~ x)
rq2 = rq(y ~ x, tau = tau)

windows()
plot(y ~ x,main = "Variação PIB per capita 1992/1980 ~ Financial Institutions Index",
     ylab = "PIB 1992/1980", xlab = "FI 1992")

abline(lm2,col="red")

for (j in 1:ncol(rq2$coefficients)) {
  abline(coef(rq2)[, j], col = colors[j])
}

legend("topright", legend=c("MQO", "RQ (0.10)", "RQ (0.25)", "RQ (0.50)", "RQ (0.75)", "RQ (0.90)"),
       col=c("red", colors), lty=1, cex=1)

summary(rq2,se='iid')
summary(lm2)

# Regressão 3 - 1992

y = dados$`1992/1980`
x = dados$FM1992

lm3 = lm(y ~ x)
rq3 = rq(y ~ x, tau = tau)

windows()
plot(y ~ x,main = "Variação PIB per capita 1992/1980 ~ Financial Markets Index",
     ylab = "PIB 1992/1980", xlab = "FM 1992")

abline(lm3,col="red")

for (j in 1:ncol(rq3$coefficients)) {
  abline(coef(rq3)[, j], col = colors[j])
}

legend("topright", legend=c("MQO", "RQ (0.10)", "RQ (0.25)", "RQ (0.50)", "RQ (0.75)", "RQ (0.90)"),
       col=c("red", colors), lty=1, cex=1)

summary(rq3,se='iid')
summary(lm3)

# Regressão 4 - 2017

y = dados$`2017/1980`
x = dados$FDepth2017

lm4 = lm(y ~ x)
rq4 = rq(y ~ x, tau = tau)

windows()
plot(y ~ x,main = "Variação PIB per capita 2017/1980 ~ Intensidade Financeira",
     ylab = "PIB 2017/1980", xlab = "Intensidade Financeira 2017")

abline(lm4,col="red")

for (j in 1:ncol(rq4$coefficients)) {
  abline(coef(rq4)[, j], col = colors[j])
}

legend("topright", legend=c("MQO", "RQ (0.10)", "RQ (0.25)", "RQ (0.50)", "RQ (0.75)", "RQ (0.90)"),
       col=c("red", colors), lty=1, cex=1)

summary(rq4,se='iid')
summary(lm4)

# Regressão 5 - 2017

y = dados$`2017/1980`
x = dados$FI2017

lm5 = lm(y ~ x)
rq5 = rq(y ~ x, tau = tau)

windows()
plot(y ~ x,main = "Variação PIB per capita 2017/1980 ~ Financial Institutions Index",
     ylab = "PIB 2017/1980", xlab = "FI 2017")

abline(lm5,col="red")

for (j in 1:ncol(rq5$coefficients)) {
  abline(coef(rq5)[, j], col = colors[j])
}

legend("topright", legend=c("MQO", "RQ (0.10)", "RQ (0.25)", "RQ (0.50)", "RQ (0.75)", "RQ (0.90)"),
       col=c("red", colors), lty=1, cex=1)

summary(rq5,se='iid')
summary(lm5)

# Regressão 6 - 2017

y = dados$`2017/1980`
x = dados$FM2017

lm6 = lm(y ~ x)
rq6 = rq(y ~ x, tau = tau)

windows()
plot(y ~ x,main = "Variação PIB per capita 2017/1980 ~ Financial Markets Index",
     ylab = "PIB 2017/1980", xlab = "FM 2017")

abline(lm6,col="red")

for (j in 1:ncol(rq6$coefficients)) {
  abline(coef(rq6)[, j], col = colors[j])
}

legend("topright", legend=c("MQO", "RQ (0.10)", "RQ (0.25)", "RQ (0.50)", "RQ (0.75)", "RQ (0.90)"),
       col=c("red", colors), lty=1, cex=1)

summary(rq6,se='iid')
summary(lm6)
