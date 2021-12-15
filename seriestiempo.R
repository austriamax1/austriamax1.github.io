#positivos vs negativos
#A vs B
#A vs A
#B vs B

plot(c(100*Series$Positivos/Series$Total) ~ Series$Date,
     type = "l", 
     col = "red",
     xlab = "Fecha",
     ylab = "% de observaciones",
     ylim = c(0,100),
     main = "Negativos vs Positivos"
     )
points(c(100*Series$Negativos/Series$Total)~ Series$Date, type = "l",
       col = "green")
text(x = 18900, y = 30, "Positivos", 
     cex = 0.8, col = "red")
text(x = 18900, y = 80, "Negativos", 
     cex = 0.8, col = "green")

plot(Series$AH1N1 ~ Series$Date,
     type = "l", 
     col = "darkorange4",
     xlab = "Fecha",
     ylab = "Núm de observaciones",
     ylim = c(0,450),
     main = "Contagios de influenza por tipo",
     lwd = 2
)
points(Series$AH3~ Series$Date, type = "l", col = "darkorange3",lwd = 2)
points(Series$Aotro~ Series$Date, type = "l", col = "orange",lwd = 1)
points(Series$Byamagata~ Series$Date, type = "l", col = "dodgerblue4",lwd = 2)
points(Series$Bvictoria~ Series$Date, type = "l", col = "dodgerblue3",lwd = 2)
points(Series$Botro~ Series$Date, type = "l", col = "blue",lwd = 1)

text(x = 18850, y = 400, "A-H1N1", 
     cex = 2, col = "darkorange4")
text(x = 18850, y = 360, "A-H3", 
     cex = 2, col = "darkorange3")
text(x = 18850, y = 320, "A-Otro", 
     cex = 2, col = "orange")
text(x = 18850, y = 280, "B-Yamagata", 
     cex = 2, col = "dodgerblue4")
text(x = 18850, y = 240, "B-Victoria", 
     cex = 2, col = "dodgerblue3")
text(x = 18850, y = 200, "B-Otro", 
     cex = 2, col = "blue")

####################################################

plot(diff(Series$AH1N1) ~ Series$Date[-c(1)],
     type = "l", 
     col = "darkorange4",
     xlab = "Fecha",
     ylab = "Núm de observaciones",
     ylim = c(-160,300),
     main = "Diferencia - Contagios de influenza por tipo",
     lwd = 2
)
points(diff(Series$AH3) ~ Series$Date[-c(1)], type = "l", col = "darkorange3",lwd = 2)
points(diff(Series$Aotro) ~ Series$Date[-c(1)], type = "l", col = "orange",lwd = 1)
points(diff(Series$Byamagata) ~ Series$Date[-c(1)], type = "l", col = "dodgerblue4",lwd = 2)
points(diff(Series$Bvictoria) ~ Series$Date[-c(1)], type = "l", col = "dodgerblue3",lwd = 2)
points(diff(Series$Botro) ~ Series$Date[-c(1)], type = "l", col = "blue",lwd = 1)

text(x = 18850, y = 280, "A-H1N1", 
     cex = 2, col = "darkorange4")
text(x = 18850, y = 250, "A-H3", 
     cex = 2, col = "darkorange3")
text(x = 18850, y = 220, "A-Otro", 
     cex = 2, col = "orange")
text(x = 18850, y = 190, "B-Yamagata", 
     cex = 2, col = "dodgerblue4")
text(x = 18850, y = 160, "B-Victoria", 
     cex = 2, col = "dodgerblue3")
text(x = 18850, y = 130, "B-Otro", 
     cex = 2, col = "blue")


DiffAH1N1 = diff(Series$AH1N1)
DiffAH3 = diff(Series$AH3)
DiffAotro = diff(Series$Aotro)
DiffByamagata = diff(Series$Byamagata)
DiffBvictoria = diff(Series$Bvictoria)
DiffBotro = diff(Series$Botro)

diferencias = cbind(DiffAH1N1,DiffAH3,DiffAotro,DiffByamagata,DiffBvictoria,DiffBotro)

tiempo = Series$Date[-c(1)]
tiempo = as.numeric(tiempo)
tiempo = tiempo/max(tiempo)

Proyecto = cbind(rep(tiempo,6),
                 c(DiffAH1N1,DiffAH3,DiffAotro,
                   DiffByamagata,DiffBvictoria,DiffBotro))
Proyecto = as.data.frame(Proyecto)
colnames(Proyecto) = c("T1","SerieD")
Proyecto$T2 = Proyecto$T1^2
Proyecto$T3 = Proyecto$T1^3
Proyecto$T4 = Proyecto$T1^4
Proyecto$T5 = Proyecto$T1^5
Proyecto$T6 = Proyecto$T1^6
Proyecto$T7 = Proyecto$T1^7
Proyecto$Tipo = as.factor(c(rep("A-H1N1",253),rep("A-H3",253),rep("A-otro",253),
                            rep("B-Yamagata",253),rep("B-Victoria",253),rep("B-otro",253)))
Proyecto$Observacion = 1:dim(Proyecto)[1]

##############################################################

library(hglm)

modelo <- hglm2(SerieD ~  (1|Observacion),
             family = gaussian(link = identity),
             disp = ~ T1 + T2,
             data = Proyecto)

EffAlea = matrix(as.numeric(modelo$ranef), ncol = 6)
colnames(EffAlea) = c("A-H1N1","A-H3","A-otro",
                      "B-Yamagata","B-Victoria","B-otro")

##############################################################

Proyecto = cbind(rep(tiempo,6), 
                 c(Series$AH1N1,Series$AH3,Series$Aotro,
                   Series$Byamagata,Series$Bvictoria,
                   Series$Botro))
Proyecto = as.data.frame(Proyecto)
colnames(Proyecto) = c("T1","Infecciones")

Proyecto$Tipo = as.factor(c(rep("A-H1N1",254),rep("A-H3",254),rep("A-otro",254),
                  rep("B-Yamagata",254),rep("B-Victoria",254),rep("B-otro",254)))

Proyecto$T2 = Proyecto$T1^2
Proyecto$T3 = Proyecto$T1^3
Proyecto$T4 = Proyecto$T1^4
Proyecto$T5 = Proyecto$T1^5
Proyecto$T6 = Proyecto$T1^6
Proyecto$T7 = Proyecto$T1^7
Proyecto$Observacion = 1:dim(Proyecto)[1]
Proyecto$Observacion = as.factor(Proyecto$Observacion)

Proyecto$Tipo = as.factor(c(rep("A-H1N1",253),rep("A-H3",253),
                            rep("A-otro",253),
                            rep("B-Yamagata",253),
                            rep("B-Victoria",253),
                            rep("B-otro",253)))


library(hglm)
# Ajustamos el modelo jerarquico:
modelo = hglm(fixed = SerieD ~ Tiempo,
              random = 1|Tiempo,
              family = gaussian(),
              data = Proyecto)



Series = 
  
  disp = ~ Tiempo,
disp = ~ Tiempo,
  library(hglm)
# Ajustamos el modelo jerarquico:
modelo = hglm(fixed = Infecciones ~ Tiempo + I(Tiempo^2) + I(Tiempo^3)
              + I(Tiempo^4) + I(Tiempo^5) + I(Tiempo^6) + Tipo,
              Z = Tipo,
              family = poisson(link = "log"),
              data = Proyecto)

m12 <- hglm2(y ~ x1 + x3 + x5 + x6 + (1|Device),
             family = Gamma(link = log),
             disp = ~ x2 + x3, data = semiconductor)
summary(m12)


library(hglm)
# Ajustamos el modelo jerarquico:
modelo = hglm(fixed = SerieD ~ 1,
              random = ~ 1|tiempo,
              family = gaussian(link = "identity"),
              disp = ~ Tipo,
              data = Proyecto)
summary(m11)


