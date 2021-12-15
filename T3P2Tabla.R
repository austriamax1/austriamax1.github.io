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

library(lme4)

modelo2 = glm(LOS ~  Age + Gender + Insurer + Owner,
              family =  poisson(),
              data = T3P2)
modelo3 = glmer(LOS ~  Age + Gender + Insurer + Owner + (1|Hospital),
              family =  poisson(),
              data = T3P2)
anova(modelo3,modelo2)

library(ggplot2, lib.loc = "/usr/lib/R/site-library")
output <- data.frame(resid = resid(modelo3), fitted = fitted(modelo3))
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
plot(predict(modelo3) ~ T3P2$LOS,
     xlim = c(0,10), 
     ylim = c(0,10),
     xlab = "LOSS",
     ylab = "Ajustado",
     main = "Ajustado vs Observado",
     col = "blue")
points(x = T3P2$LOS, y = predict(modelo2), col = "red")
points(x = c(0,10),y=c(0,10), type = "l")
points(x = c(0,10),y=c(0,0), type = "l", col = "grey")
text(x = 2, y = 8, "Regresión Poisson con efecto aleatorio", 
     cex = 1.6, col = "blue")
text(x = 2, y = 7, "Regresión Poisson sin efecto aleatorio",
     cex = 1.6, col = "red")

