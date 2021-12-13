#Porcentajes de alivio
ABC = c(0.1875,0.8125,0.8125)
ACB = c(0.3125,0.8125,0.8125)
BAC = c(0.166666666666667,0.833333333333333,0.833333333333333)
BCA = c(0.166666666666667,0.833333333333333,0.833333333333333)
CAB = c(0.333333333333333,0.666666666666667,0.666666666666667)
CBA = c(0.166666666666667,0.416666666666667,0.833333333333333)



par(mfrow=c(2,2)) 

plot(ABC,
     type = "l",
     ylim = c(0,1),
     main = "% Alivio del dolor",
     xlab = "Secuencia",
     ylab = "Porcentaje")
text(x = 1.1, y = 0.1, "ABC", 
     cex = 1.6, col = "black")

points(ACB,type = "l")
text(x = 1.1, y = 0.5, "ACB", 
     cex = 1.6, col = "black")


plot(BAC,
     type = "l",
     ylim = c(0,1),
     main = "% Alivio del dolor",
     xlab = "Secuencia",
     ylab = "Porcentaje")
text(x = 1.1, y = 0.1, "BAC", 
     cex = 1.6, col = "black")

points(BCA,type = "l")
text(x = 1.1, y = 0.4, "BCA", 
     cex = 1.6, col = "black")

plot(CAB,
     type = "l",
     ylim = c(0,1),
     main = "% Alivio del dolor",
     xlab = "Secuencia",
     ylab = "Porcentaje")
text(x = 1.1, y = 0.5, "CAB", 
     cex = 1.6, col = "black")

points(CBA,type = "l")
text(x = 2, y = 0.2, "CBA", 
     cex = 1.6, col = "black")


library(readr)
T4P2_tabla <- read_csv("~/Documents/MLG/T4P2_tabla.csv")
T4P2_tabla$Tratamiento =  as.factor(T4P2_tabla$Tratamiento)
T4P2_tabla$Tipo = as.factor(T4P2_tabla$Tipo)

library(lme4)
modelo = glmer(cbind(Nalivio,Total) ~ Tipo + (Tipo|Tratamiento),
      family = binomial(link = "logit"),
      data = T4P2_tabla)

# (Tipo|Tratamiento)
# rmarkdown::render_site(encoding = "UTF-8")


#install.packages("geepack")
library("geepack")

modelo = geepack::geeglm(cbind(Nalivio,Total) ~ Tipo + Dosis2 +Dosis3,
               family = binomial(link = "logit"),
               data = T4P2_tabla,
               id = Tratamiento,
               corstr = "ar1")

