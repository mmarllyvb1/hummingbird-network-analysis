install.packages("vegan")
install.packages("sna")
install.packages("statnet")
install.packages("statnet.common")
install.packages("tergm")
install.packages("ergm")
install.packages("bipartite")
install.packages("network")
install.packages("dplyr")
install.packages("extrafont")


#Cargar paquetes
library(vegan)
library(sna)
library(tergm)
library(statnet)
library(statnet.common)
library(bipartite)
library(network)
library(dplyr)
library(extrafont)
font_import(prompt = FALSE)
loadfonts(device = "win")

#Seleccionar directorio de trabajo
#Cargar el archivo en formato "csv"
setwd("C:/Users/XXGAMINGXX/Documents/Proyecto de grado")
redinteraccion <- read.csv("Data/TotalInteracciones.csv")

par(font="3")
par(family="Times New Roman")
gen <- c('F. petiolaris',
         'H. rosa-sinensis',
         'Cavendishia sp.',
         'C. megapotamica',
         'C. indica',
         'Crocosmia sp.',
         'Pelargonium',
         'B. hirsuta',
         'Handroanthus sp.',
         'R. glaucus',
         'Racinaea sp.')
row.names(redinteraccion) <- gen


colb <- c('A. melanogenys', 
'A. kingii',
'C. heliodor',
'C. poortmani',
'C. coeligena',
'C. coruscans',
'C. cyanotus',
'D. ludovicae',
'H. clarisse',
'H. jacula',
'M. tyrianthina',
'O. underwoodii',
'P. syrmatophorus',
'U. franciae')

colnames(redinteraccion) <- colb

#Realizar el gráfico quitando la columna del nombre las plantas
visweb(redinteraccion)
windowsFonts(A=windowsFont("Times New Roman"))
plotweb(redinteraccion, arrow = "both.center", y.width.low = 0.10,
        text.rot = 90, col.high = "turquoise", col.low = "darkolivegreen1", labsize = 1.5)


par(font="3")

##Para grafico con grosor
plotweb(redinteraccion, 
        text.rot = 90, col.high = "purple", col.low = "darkolivegreen1", labsize = 6, low.y = 0.6, high.y = 1.3)


#Mirar los diversos indices
networklevel(redinteraccion)

#Forma de observar de distinta manera que la red
visweb(redinteraccion)

####Ver especializacion a nivel de especies
##plantas
dfun(redinteraccion)
##todas, incluyendo plantas y colibries
specieslevel(redinteraccion)


#################EPOCA LLUVIOSA###########################################
#Cargar el archivo en formato "csv"
setwd("C:/Users/XXGAMINGXX/Documents/Proyecto de grado")
redinteraccion_lluviosa <- read.csv("Data/Lluviosa - copia (2).csv")

#Asignar los nombres de las plantas a la columna para que salga en el gráfico
row.names(redinteraccion_lluviosa) <- gen
colnames(redinteraccion_lluviosa) <- colb

#Realizar el gráfico quitando la columna del nombre las plantas
visweb(redinteraccion_lluviosa)
plotweb(redinteraccion_lluviosa, arrow = "both.center", y.width.low = 0.10,
        text.rot = 90, col.high = "turquoise", col.low = "darkolivegreen1", labsize = 1.05)
##Para grafico con grosor
plotweb(redinteraccion_lluviosa, 
        text.rot = 90, col.high = "purple", col.low = "darkolivegreen1", labsize = 6, low.y = 0.6, high.y = 1.3)


#Mirar los diversos indices
networklevel(redinteraccion_lluviosa)

#Forma de observar de distinta manera que la red
visweb(redinteraccion_lluviosa)

####Ver especializacion a nivel de especies
##plantas
dfun(redinteraccion_lluviosa)
##colibries
redinteraccion_tranlluviosa <- data.frame(t(redinteraccion_lluviosa[-1]))
dfun(redinteraccion_tranlluviosa)

specieslevel(redinteraccion_lluviosa)

#################EPOCA SECA###########################################
#Cargar el archivo en formato "csv"
setwd("C:/Users/XXGAMINGXX/Documents/Proyecto de grado")
redinteraccion_seca <- read.csv("Data/Seca - copia (2).csv")

#Asignar los nombres de las plantas a la columna para que salga en el gráfico
row.names(redinteraccion_seca) <- gen
colnames(redinteraccion_seca) <- colb

#Realizar el gráfico quitando la columna del nombre las plantas
visweb(redinteraccion_seca)
plotweb(redinteraccion_seca, arrow = "both.center", y.width.low = 0.10,
        text.rot = 90, col.high = "turquoise", col.low = "darkolivegreen1", labsize = 1.05)
##Para grafico con grosor
plotweb(redinteraccion_seca, 
        text.rot = 90, col.high = "purple", col.low = "darkolivegreen1", labsize = 6, low.y = 0.6, high.y = 1.3)

#Mirar los diversos indices
networklevel(redinteraccion_seca)

#Forma de observar de distinta manera que la red
visweb(redinteraccion_seca)

####Ver especializacion a nivel de especies
##plantas
dfun(redinteraccion_seca)
##colibries
redinteraccion_transeca <- data.frame(t(redinteraccion_seca[-1]))
dfun(redinteraccion_transeca)

specieslevel(redinteraccion_seca)

###################################################
setwd("C:/Users/XXGAMINGXX/Documents/Proyecto de grado")
corpicorola <- read.csv("Rasgos colibries.csv")
cor_test<-cor.test(corpicorola$Longitud.pico, corpicorola$ï..Longitud.corola,
                   method="spearman", exact=FALSE)
cor_pear <- cor(corpicorola$Longitud.pico, corpicorola$ï..Longitud.corola,
                method="spearman")
print(cor_pear)
print(cor_test)

# Extraer los resultados de cor.test
r <- round(cor_test$estimate, 2) # Coeficiente de correlación
p <- signif(cor_test$p.value, 3) # Valor p


par(family = "Times New Roman")
par(font="3")
plot(corpicorola$Longitud.pico, corpicorola$ï..Longitud.corola, 
     main = "Relación entre longitud del pico y longitud de la corola",
     xlab = "Longitud del pico (mm)",
     ylab = "Longitud de la corola (mm)",
     pch = 16, col = "blue", cex=1.1, cex.lab=1.5, cex.main=2)
lines(lowess(corpicorola$Longitud.pico ~ corpicorola$ï..Longitud.corola), col = "red", lwd = 3)



# Añadir texto al gráfico
legend("topleft", 
       legend = paste("Spearman rho =", r, "\n", "p =", p,"\n" ),
       bty = "n", cex = 1.3) # Sin borde

modelo <- lm(corpicorola$ï..Longitud.corola ~ corpicorola$Longitud.pico)
coeficientes <- coef(modelo)
print(coeficientes)
ecuacion <- paste("y = ", round(coeficientes[2], 2), "*x +", round(coeficientes[1], 2))
text(20, 20, ecuacion, col = "blue")



plot(corpicorola$ï..Longitud.corola, corpicorola$Longitud.pico, 
     main = "Relación entre longitud del pico y longitud de la corola",
     xlab = "Longitud del pico (mm)",
     ylab = "Longitud de la corola (mm)",
     pch = 16, col = "blue", cex=1.1, cex.lab=1.2, cex.main=1.4)
abline(lm(corpicorola$Longitud.pico ~ corpicorola$ï..Longitud.corola), col = "red", lwd = 2)

lines(lowess(corpicorola$ï..Longitud.corola ~ corpicorola$Longitud.pico), col = "red", lwd = 3)

###########################################################

corpicorola <- read.csv("Rasgos colibries.csv")
cor_test<-cor.test(corpicorola$ï..Longitud.corola, corpicorola$Longitud.pico,
                   method="spearman", exact=FALSE)
cor_pear <- cor(corpicorola$ï..Longitud.corola, corpicorola$Longitud.pico,
                method="spearman")
print(cor_pear)
print(cor_test)

# Extraer los resultados de cor.test
r <- round(cor_test$estimate, 2) # Coeficiente de correlación
p <- signif(cor_test$p.value, 3) # Valor p

par(family = "Times New Roman")
par(font="3")

plot(corpicorola$ï..Longitud.corola, corpicorola$Longitud.pico, 
     main = "Relación entre longitud del pico y longitud de la corola",
     ylab = "Longitud del pico (mm)",
     xlab = "Longitud de la corola (mm)",
     pch = 16, col = "blue", cex=1.1, cex.lab=1.5, cex.main=2)
abline(lm(corpicorola$Longitud.pico ~ corpicorola$ï..Longitud.corola), col = "red", lwd = 2)

lines(lowess(corpicorola$Longitud.pico ~ corpicorola$ï..Longitud.corola), col = "red", lwd = 3)
legend("topleft", 
       legend = paste("Spearman rho =", r, "\n", "p =", p,"\n" ),
       bty = "n", cex = 2) # Sin borde
