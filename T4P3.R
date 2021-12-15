library(readr)
T4P3tabla <- read_csv("~/Documents/MLG/T4P3tabla.csv")
View(T4P3tabla)
T4P3tabla = T4P3tabla[-which(is.na(T4P3tabla$helicobacter)),]
T4P3tabla$pda[which(T4P3tabla$pda == 100)] = 99.999
T4P3tabla$pda[which(T4P3tabla$pda == 0)] = 0.001
T4P3tabla$pda = T4P3tabla$pda/100 
T4P3tabla$genero = as.factor(T4P3tabla$genero)
T4P3tabla$grupodeprueba = as.factor(T4P3tabla$grupodeprueba)
T4P3tabla$nosujeto = as.factor(T4P3tabla$nosujeto)

library("betareg")
modelo <- betareg(pda ~ genero  + tiempo +grupodeprueba + heli | nosujeto,
              data = T4P3tabla)

library("betareg")
modelo = betareg(pda ~ genero + tiempo + grupodeprueba + heli,
                  data = T4P3tabla)

usar = which(rownames(T4P3tabla) %in% names(modelo$fitted.values))

plot(predict(modelo,data = T4P3tabla) ~ T4P3tabla$pda[usar],
     xlim = c(0,1), ylim = c(0,1), xlab = "PDA Observado",
     ylab = "PDA Ajustado",col = "red") 
points(x = c(-0.1,1.1), y = c(-0.1,1.1), type = "l")


usar = which(rownames(T4P3tabla) %in% names(modelo2$fitted.values))

points(predict(modelo2,data = T4P3tabla) ~ T4P3tabla$pda[usar], col = "blue") 


text(x = 0.2, y = 0.9, "Modelo sin efecto aleatorio", 
     cex = 0.8, col = "red")
text(x = 0.2, y = 0.8, "Modelo con efecto aleatorio", 
     cex = 0.8, col = "blue")
