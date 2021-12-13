T3P2 = T3P2[-which(T3P2$Owner == 0),]
T3P2$Owner  = as.factor(T3P2$Owner)
T3P2$Race = as.factor(T3P2$Race)
T3P2$Hospital =  as.factor(T3P2$Hospital)

###################################################################
#Modelo Poisson Cero truncado
##################################################################
#install.packages("VGAM")
library("VGAM")
modelo = vglm(LOS ~  Age + Gender + Insurer + Owner,
                         family =  pospoisson(),
                         data = T3P2)


output <- data.frame(resid = resid(modelo), fitted = fitted(modelo))
ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_smooth(method="loess")

output <- within(output, {
  broken <- cut(fitted, hist(fitted, plot=FALSE)$breaks)
})

ggplot(output, aes(broken, resid)) +
  geom_boxplot() +
  geom_jitter(alpha=.25)

################################################################
#Modelo Poisson
################################################################
modelo2 = glm(LOS ~  Age + Gender + Insurer + Owner,
              family =  poisson(),
              data = T3P2)
plot(modelo2)

################################################################
#Comparación
###############################################################
plot(predict(modelo) ~ T3P2$LOS,
     xlim = c(0,10), 
     ylim = c(0,10),
     xlab = "LOSS",
     ylab = "Ajustado",
     main = "Ajustado vs Observado",
     col = "red")
points(x = T3P2$LOS, y = predict(modelo2), col = "blue")
points(x = c(0,10),y=c(0,10), type = "l")
points(x = c(0,10),y=c(0,0), type = "l", col = "grey")
text(x = 1.6, y = 8, "Regresión Poisson cero truncada", 
     cex = 1.6, col = "blue")
text(x = 1, y = 7, "Regresión Poisson", cex = 1.6, col = "red")

