library(readxl) 
library(ggplot2)
        
data = setwd("C:/Users/mique/Documents/CURSO 2 - SEMESTRE B/PROYECTO II/CSV/estacionesCSV")
        
b = list.files(path = data)




#UNIVERSIDAD POLITÉCNICA

so2 = c()
no2 = c()
o3 = c()
co = c()
pm10 = c()
pm25 = c()

for (fichero in b) {
tabla <- as.data.frame(read.csv(file = fichero, sep = ';'))
so2 <- c(so2, tabla[tabla$nombre == 'Universidad PolitÃ©cnica',5])
no2 <- c(no2, tabla[tabla$nombre == 'Universidad PolitÃ©cnica',6])
o3 <- c(o3, tabla[tabla$nombre == 'Universidad PolitÃ©cnica',7])
co <- c(co, tabla[tabla$nombre == 'Universidad PolitÃ©cnica',8])
pm10 <- c(pm10, tabla[tabla$nombre == 'Universidad PolitÃ©cnica',9])
pm25 <- c(pm25, tabla[tabla$nombre == 'Universidad PolitÃ©cnica',10])
        }
        
so2_u = hist(so2)
text(so2_u$mids,so2_u$counts,labels=so2_u$counts, adj=c(0.5, -0.5))
summary(so2)
no2_u = hist(no2)
text(no2_u$mids,no2_u$counts,labels=no2_u$counts, adj=c(0.5, -0.5))
summary(no2)
o3_u = hist(o3)
text(o3_u$mids,o3_u$counts,labels=o3_u$counts, adj=c(0.5, -0.5))
summary(o3)
#co_u = hist(co)
#text(co_u$mids,co_u$counts,labels=co3_u$counts, adj=c(0.5, -0.5))
#summary(co)
pm10_u = hist(pm10)
text(pm10_u$mids,pm10_u$counts,labels=pm10_u$counts, adj=c(0.5, -0.5))
summary(pm10)
pm25_u = hist(pm25)
text(pm25_u$mids,pm25_u$counts,labels=pm25_u$counts, adj=c(0.5, -0.5))
summary(pm25)


#MOLÍ DEL SOL

so2 = c()
no2 = c()
o3 = c()
co = c()
pm10 = c()
pm25 = c()

for (fichero in b) {
  tabla <- as.data.frame(read.csv(file = fichero, sep = ';'))
  so2_m <- c(so2, tabla[tabla$nombre == 'MolÃ del Sol',5])
  no2_m <- c(no2, tabla[tabla$nombre == 'MolÃ del Sol',6])
  o3_m <- c(o3, tabla[tabla$nombre == 'MolÃ del Sol',7])
  co_m <- c(co, tabla[tabla$nombre == 'MolÃ del Sol',8])
  pm10_m <- c(pm10, tabla[tabla$nombre == 'MolÃ del Sol',9])
  pm25_m <- c(pm25, tabla[tabla$nombre == 'MolÃ del Sol',10])
}

so2_m = hist(so2)
text(so2_m$mids,so2_m$counts,labels=so2_m$counts, adj=c(0.5, -0.5))
summary(so2)
no2_m = hist(no2)
text(no2_m$mids,no2_m$counts,labels=no2_m$counts, adj=c(0.5, -0.5))
summary(no2)
o3_m = hist(o3)
text(o3_m$mids,o3_m$counts,labels=o3_m$counts, adj=c(0.5, -0.5))
summary(o3)
co_m = hist(co)
text(co_m$mids,co_m$counts,labels=co_m$counts, adj=c(0.5, -0.5))
summary(co)
pm10_m = hist(pm10)
text(pm10_m$mids,pm10_m$counts,labels=pm10_m$counts, adj=c(0.5, -0.5))
summary(pm10)
pm25_m = hist(pm25)
text(pm25_m$mids,pm25_m$counts,labels=pm25_m$counts, adj=c(0.5, -0.5))
summary(pm25)

#PISTA DE SILLA

so2 = c()
no2 = c()
o3 = c()
co = c()
pm10 = c()
pm25 = c()

for (fichero in b) {
  tabla <- as.data.frame(read.csv(file = fichero, sep = ';'))
  so2 <- c(so2, tabla[tabla$nombre == 'Pista de Silla',5])
  no2 <- c(no2, tabla[tabla$nombre == 'Pista de Silla',6])
  o3 <- c(o3, tabla[tabla$nombre == 'Pista de Silla',7])
  co <- c(co, tabla[tabla$nombre == 'Pista de Silla',8])
  pm10 <- c(pm10, tabla[tabla$nombre == 'Pista de Silla' ,9])
  pm25 <- c(pm25, tabla[tabla$nombre == 'Pista de Silla',10])
}

so2_p = hist(so2)
text(so2_p$mids,so2_p$counts,labels=so2_p$counts, adj=c(0.5, -0.5))
summary(so2)
no2_p = hist(no2)
text(no2_p$mids,no2_p$counts,labels=no2_p$counts, adj=c(0.5, -0.5))
summary(no2)
o3_p = hist(o3)
text(o3_p$mids,o3_p$counts,labels=o3_p$counts, adj=c(0.5, -0.5))
summary(o3)
co_p = hist(co)
text(co_p$mids,co_p$counts,labels=co_p$counts, adj=c(0.5, -0.5))
summary(co)
pm10_p = hist(pm10)
text(pm10_p$mids,pm10_p$counts,labels=pm10_p$counts, adj=c(0.5, -0.5))
summary(pm10)
pm25_p = hist(pm25)
text(pm25_p$mids,pm25_p$counts,labels=pm25_p$counts, adj=c(0.5, -0.5))
summary(pm25)


#VIVEROS

so2 = c()
no2 = c()
o3 = c()
co = c()
pm10 = c()
pm25 = c()

for (fichero in b) {
  tabla <- as.data.frame(read.csv(file = fichero, sep = ';'))
  so2 <- c(so2, tabla[tabla$nombre == 'Viveros',5])
  no2 <- c(no2, tabla[tabla$nombre == 'Viveros',6])
  o3 <- c(o3, tabla[tabla$nombre == 'Viveros',7])
  co <- c(co, tabla[tabla$nombre == 'Viveros',8])
  pm10 <- c(pm10, tabla[tabla$nombre == 'Viveros',9])
  pm25 <- c(pm25, tabla[tabla$nombre == 'Viveros',10])
}

so2_v = hist(so2)
text(so2_v$mids,so2_v$counts,labels=so2_v$counts, adj=c(0.5, -0.5))
summary(so2)
no2_v = hist(no2)
text(no2_v$mids,no2_v$counts,labels=no2_v$counts, adj=c(0.5, -0.5))
summary(no2)
o3_v = hist(o3)
text(o3_v$mids,o3_v$counts,labels=o3_v$counts, adj=c(0.5, -0.5))
summary(o3)
co_v = hist(co)
text(co_v$mids,co_v$counts,labels=co_v$counts, adj=c(0.5, -0.5))
summary(co)
pm10_v = hist(pm10)
text(pm10_v$mids,pm10_v$counts,labels=pm10_v$counts, adj=c(0.5, -0.5))
summary(pm10)
pm25_v = hist(pm25)
text(pm25_v$mids,pm25_v$counts,labels=pm25_v$counts, adj=c(0.5, -0.5))
summary(pm25)


#FRANCIA 

so2 = c()
no2 = c()
o3 = c()
co = c()
pm10 = c()
pm25 = c()

for (fichero in b) {
  tabla <- as.data.frame(read.csv(file = fichero, sep = ';'))
  so2 <- c(so2, tabla[tabla$nombre == 'Francia',5])
  no2 <- c(no2, tabla[tabla$nombre == 'Francia',6])
  o3 <- c(o3, tabla[tabla$nombre == 'Francia',7])
  co <- c(co, tabla[tabla$nombre == 'Francia',8])
  pm10 <- c(pm10, tabla[tabla$nombre == 'Francia',9])
  pm25 <- c(pm25, tabla[tabla$nombre == 'Francia',10])
}

so2_f = hist(so2)
text(so2_f$mids,so2_f$counts,labels=so2_f$counts, adj=c(0.5, -0.5))
summary(so2)
no2_f = hist(no2)
text(no2_f$mids,no2_f$counts,labels=no2_f$counts, adj=c(0.5, -0.5))
summary(no2)
o3_f = hist(o3)
text(o3_f$mids,o3_f$counts,labels=o3_f$counts, adj=c(0.5, -0.5))
summary(o3)
co_f = hist(co)
text(co_f$mids,co_f$counts,labels=co_f$counts, adj=c(0.5, -0.5))
summary(co)
pm10_f = hist(pm10)
text(pm10_f$mids,pm10_f$counts,labels=pm10_f$counts, adj=c(0.5, -0.5))
summary(pm10)
pm25_f = hist(pm25)
text(pm25_f$mids,pm25_f$counts,labels=pm25_f$counts, adj=c(0.5, -0.5))
summary(pm25)

#Boulevar Sur

so2 = c()
no2 = c()
o3 = c()
co = c()
pm10 = c()
pm25 = c()

for (fichero in b) {
  tabla <- as.data.frame(read.csv(file = fichero, sep = ';'))
  so2 <- c(so2, tabla[tabla$nombre == 'Boulevar Sur',5])
  no2 <- c(no2, tabla[tabla$nombre == 'Boulevar Sur',6])
  o3 <- c(o3, tabla[tabla$nombre == 'Boulevar Sur',7])
  co <- c(co, tabla[tabla$nombre == 'Boulevar Sur',8])
  pm10 <- c(pm10, tabla[tabla$nombre == 'Boulevar Sur',9])
  pm25 <- c(pm25, tabla[tabla$nombre == 'Boulevar Sur',10])
}

so2_b = hist(so2)
text(so2_b$mids,so2_b$counts,labels=so2_b$counts, adj=c(0.5, -0.5))
summary(so2)
no2_b = hist(no2)
text(no2_b$mids,no2_b$counts,labels=no2_b$counts, adj=c(0.5, -0.5))
summary(no2)
o3_b = hist(o3)
text(o3_b$mids,o3_b$counts,labels=o3_b$counts, adj=c(0.5, -0.5))
summary(o3)
co_b = hist(co)
text(co_b$mids,co_b$counts,labels=co_b$counts, adj=c(0.5, -0.5))
summary(co)
pm10_b = hist(pm10)
text(pm10_b$mids,pm10_b$counts,labels=pm10_b$counts, adj=c(0.5, -0.5))
summary(pm10)
pm25_b = hist(pm25)
text(pm25_b$mids,pm25_b$counts,labels=pm25_b$counts, adj=c(0.5, -0.5))
summary(pm25)

#Centro

so2 = c()
no2 = c()
o3 = c()
co = c()
pm10 = c()
pm25 = c()

for (fichero in b) {
  tabla <- as.data.frame(read.csv(file = fichero, sep = ';'))
  so2 <- c(so2, tabla[tabla$nombre == 'Centro',5])
  no2 <- c(no2, tabla[tabla$nombre == 'Centro',6])
  o3 <- c(o3, tabla[tabla$nombre == 'Centro',7])
  co <- c(co, tabla[tabla$nombre == 'Centro',8])
  pm10 <- c(pm10, tabla[tabla$nombre == 'Centro',9])
  pm25 <- c(pm25, tabla[tabla$nombre == 'Centro',10])
}

#so2_c = hist(so2)
#text(so2_c$mids,so2_c$counts,labels=so2_c$counts, adj=c(0.5, -0.5))
summary(so2)
no2_c = hist(no2)
text(no2_c$mids,no2_c$counts,labels=no2_c$counts, adj=c(0.5, -0.5))
summary(no2)
#o3_c = hist(o3)
#text(o3_c$mids,o3_c$counts,labels=o3_c$counts, adj=c(0.5, -0.5))
summary(o3)
#co_c = hist(co)
#text(co_c$mids,co_c$counts,labels=co_c$counts, adj=c(0.5, -0.5))
summary(co)
pm10_c = hist(pm10)
text(pm10_c$mids,pm10_c$counts,labels=pm10_c$counts, adj=c(0.5, -0.5))
summary(pm10)
pm25_c = hist(pm25)
text(pm25_c$mids,pm25_c$counts,labels=pm25_c$counts, adj=c(0.5, -0.5))
summary(pm25)

summary(so2)
#hist(so2)
summary(no2)
hist(no2)
summary(o3)
#hist(o3)
summary(pm10)
hist(pm10)
summary(pm25)
hist(pm25)
summary(co)
#hist(co)

dataframe = data.frame(id, so2, no2, o3, pm10, pm25, co)
id = 1:nrow(dataframe)

grafico = ggplot(data=dataframe, aes(x=id, y=pm25, group=1)) +
  geom_line(colour="red", size=0.3) +
  geom_point(colour="red", size=0.5, shape=21, fill="white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
grafico = grafico + ggtitle('so2')
plot(grafico)
        