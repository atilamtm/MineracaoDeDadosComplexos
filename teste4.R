########################################
# Teste 4 - INF-0612          
# Nome(s): Atila de Moura Tavano Moretto
########################################

names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
cepagri <- read.csv("cepagri.csv", header = FALSE, sep = ";", col.names = names)

library(ggplot2)

## 1 - Umidade Relativa do Ar
g <- ggplot(cepagri[as.character(cepagri$Horario) < "11-01-2018-00:00",],
            aes(x = Horario, y = Umidade, group = 1)) + geom_line()
g


## 2 - Sensa????o T??rmica da Segunda Quinzena do M??s
g <- ggplot(cepagri[as.character(cepagri$Horario) >= "15-01-2018-00:00",],
            aes(x = Sensacao)) + geom_histogram(bins=30)
g


## 3 - Temperatura dos ??ltimos Sete Dias do M??s
horarios <- as.POSIXct(as.character(cepagri$Horario), format = "%d/%m/%Y-%H:%M")
cepagri$Dia <- format(horarios, format = "%d")
g <- ggplot(cepagri[as.character(cepagri$Horario) >= "25-01-2018-00:00",],
             aes(x = Dia, y = Temperatura)) + geom_boxplot()
g


## 4 - Ventos do Primeiro Dia do M??s
cepagri$Hora <- format(horarios, format = "%H")
cepagri1D <- cepagri[as.character(cepagri$Horario) < "02-01-2018-00:00",]
maxVento <- aggregate(cepagri1D$Vento,
                      list(cepagri1D$Hora), max)
colnames(maxVento) <- c("Hora", "Vento")
g <- ggplot(maxVento, aes(x = Hora, y = Vento, group = 1)) + geom_point() + geom_smooth()
g


## 5 - Bonus - Os dias 13 e 14 possuem menos amostras que os outros dias
## Um dia com coleta de 10 em 10 minutos deveria ter 10*6*24 amostras = 144
samplesByDay <- aggregate(cepagri$Dia,list(cepagri$Dia),length)
colnames(samplesByDay) <- c("Day", "NumberOfSamples")
g <- ggplot(samplesByDay, aes(x=Day, y = NumberOfSamples, group = 1)) + geom_point()
g
