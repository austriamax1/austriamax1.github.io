library("catspec")

Freq <- c(93, 17, 44, 7, 10, 9, 46, 11, 0, 9, 17, 11, 
          155, 9, 12, 6, 4, 9, 15, 2,10, 4, 12, 2, 27)
primera<-gl(5,5,labels=c("High Point","Testers Choice","Sanka","Nescafe","Brim"))
segunda<-gl(5,1,labels=c("High Point","Testers Choice","Sanka","Nescafe","Brim"))
Compras <- data.frame(primera,segunda,Freq)

indep<-glm(Freq~primera+segunda,family=poisson(),data=Compras)
summary(indep)

fitmacro(indep)

wt <- as.numeric(primera != segunda)


#modelo de cuasi independencia
qi0<-glm(Freq~primera+segunda,
         weights=wt,
         family=poisson(),
         data=Compras)
summary(qi0)

##Modelo de cuasi-simetria

glm.qsym<-glm(Freq~primera+segunda+mob.symint(primera,segunda),
              family=poisson(),
              data=Compras)

summary(glm.qsym)


#Modelo Homegeneo-Marginal

glm.hrc1<-glm(Freq~primera+segunda+mob.rc1(primera,segunda,equal=TRUE),
              family=poisson(),
              data=Compras)
summary(glm.hrc1)


