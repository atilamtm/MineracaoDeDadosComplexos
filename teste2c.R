########################################
# Teste 2c - INF-0612          
# Nome(s): Atila de Moura Tavano Moretto 
#          Yakov Nae
########################################


dia <- c(01, 01, 02, 02, 02, 02, 03, 03, 03, 04, 04, 04, 05, 05, 06, 06, 06, 06, 07, 07, 07, 07, 07, 08, 08, 08, 08, 09, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15, 15)

cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Vinhedo', 'Campinas', 'Vinhedo', 'Vinhedo', 'Limeira', 'Limeira', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Vinhedo', 'Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Vinhedo', 'Limeira', 'Vinhedo', 'Campinas', 'Limeira', 'Vinhedo', 'Limeira', 'Campinas', 'Limeira', 'Limeira', 'Campinas', 'Campinas', 'Limeira', 'Limeira')

chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45, 2.63, 0.76, 0.38, 1.26, 2.57, 0.54, 9.87, 3.41, 2.96, 1.37, 6.78, 13.87, 0.11, 1.84, 12.19, 2.86, 11.99, 2.01, 2.32, 11.2, 0.48, 4.33, 13.32, 1.05, 0.56, 1.92, 1.81, 7.66, 2.93, 1.17, 0.7, 2.95, 0.37, 1.08, 1.31, 3.22, 0.49, 1.86, 2.3, 7.65)


## DICA:
## Dado um data frame df[], voce pode remover linhas repetidas considerando duas colunas "c" e "d" 
## usando o comando df[!duplicated(df[,c('c', 'd')]),] (mantendo apenas a primeira ocorrencia) ou o
## comando df[!duplicated(df[,c('c', 'd')], fromLast = TRUE),] (mantendo apenas a ultima ocorrencia)

records<-data.frame(dia,cidade,chuva)
df<-records[order(records$chuva),]
df<-df[!duplicated(df[,c('dia', 'cidade')], fromLast = TRUE),]
records<-records[sort(as.numeric(dimnames(df)[[1]])),]
rm(df)

acumCamp <-  sum(records[records$cidade=="Campinas",]$chuva)
acumLim <-  sum(records[records$cidade=="Limeira",]$chuva)
acumVin <-  sum(records[records$cidade=="Vinhedo",]$chuva)

dmaxCamp <- records[records$chuva==max(records[records$cidade=="Campinas",]$chuva),][1,]$dia
dmaxLim <- records[records$chuva==max(records[records$cidade=="Limeira",]$chuva),][1,]$dia
dmaxVin <- records[records$chuva==max(records[records$cidade=="Vinhedo",]$chuva),][1,]$dia

dminCamp <- records[records$chuva==min(records[records$cidade=="Campinas",]$chuva),][1,]$dia
dminLim <- records[records$chuva==min(records[records$cidade=="Limeira",]$chuva),][1,]$dia
dminVin <- records[records$chuva==min(records[records$cidade=="Vinhedo",]$chuva),][1,]$dia