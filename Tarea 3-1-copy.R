# Tarea 3

# Ejercicio 1:



# Creamos la base de datos:
sexo = c("Mujer","Mujer","Mujer","Mujer","Hombre","Hombre","Hombre","Hombre")
comunidad = c("Urbana","Urbana","Rural","Rural","Urbana","Urbana","Rural","Rural")
cinturon = c(0,1,0,1,0,1,0,1)
r1 = c(7287,11587,3246,6134,10381,10969,6123,6693)
r2 = c(175,126,73,94,136,83,141,74)
r3 = c(720,577,710,564,566,259,710,353)
r4 = c(91,48,159,82,96,37,188,74)
r5 = c(10,8,31,17,14,1,45,12)

# Leyenda:
# class_1: No se lastim?, 
# class_2: lastimado, pero sin necesidad de ser transportado en ambulancia, 
# class_3: lastimado, transportado por ambulancia, pero no hospitalizado, 
# class_4: lastimado, hospitalizado, pero sobrevivi?, 
# class_5: muerto.
# N: No.
# Y = Si.
# U = Urbana.
# R = Rural.
# M = Masculino.
# F = Femenino.

# Guardamos las listas en datos_1:
T3P1 = data.frame(sexo,comunidad,cinturon,r1,r2,r3,r4,r5)
T3P1$sexo = as.factor(T3P1$sexo)
T3P1$comunidad = as.factor(T3P1$comunidad)

# Ajustamos el modelo con el vector de todos los resultados como respuesta:
# Importamos los paquetes:
library("VGAM")
modelo = vglm(cbind(r1,r2,r3,r4,r5)~sexo+comunidad+ cinturon,
                data = T3P1,
                family = multinomial(refLevel = 1)) 

summary(modelo)

# Para este modelo se puede ver que al cambiar de un estado a otro (estamos tomando "no se lastimo" como nivel de referencia)
# el numero de personas por clase va a disminuir si se usa el cinturon o si se anda en zona rural, asi como que los hombres es mas
# probable que salgan lastimados respecto a las mujeres. 

# Resultado 1 como respuesta, suponiendo que esta ordenado:
modelo_2 = vglm(cbind(class_5)~
                  as.factor(genre)+as.factor(location)
                +as.factor(sit_belt),family = cumulative(loglink, parallel = T, reverse = F), data = datos_1)
summary(modelo_2)

# Aqui el Hauck Donner effect nos advierte de losinterceptos 1,2,5,6,7 y el genero masculino por tener standar errors
# mayores que el resto (aunque notemos que son bien bajos).
# Por otra parte una vez mas el uso de cinturon ayuda a que las personas no salgan lastimadas y en zonas urbanas se ven mayor 
# numero de accidentes, en cuanto al genero las conclusiones se mantienen, los hombres se accidentan m?s.

# Resultado 1 como respuesta, suponiendo que no esta ordenado (para ello asumimos
# la familia multinomial):
modelo_3 = vglm(class_1~as.factor(genre)+as.factor(location)
                +as.factor(sit_belt),data = datos_1,family = multinomial(refLevel = 1))

summary(modelo_3)

#  Bajo el smpuesto de de que el resultado 1 es ordenado una vez que ajustamos el
# modelo vemos que tenemos 14 warnings y asu vez la interpretacion de los coeficientes
# difiere de las 2 anteriores, ademas de que los errores standar son bien altos, lo 
# que nos habla de la posble interpretacion erroneo del modelo ajustado.


# Ejercicio 3:

# Creamos el dataset:

# Guardamos las varaibles primera compra y segunda compra junto con la cantidad:
firts_buy = rep(c("High point","Taster's choice","Sanka","Nescafe", "Brim"),c(5,5,5,5,5))
second_buy = rep(c("High point","Taster's choice","Sanka","Nescafe", "Brim"),5)
count = c(93,17,44,7,10,9,46,11,0,9,17,11,155,9,12,6,4,9,15,2,10,4,12,2,27)

# Las declaramos de tipo factor a las variables primera y segunda compra:
firts_buy = factor(firts_buy)
second_buy = factor(second_buy)


# Creamos las variables indicadoras para los elementos de la diagonal principal:
ih_point = (firts_buy == "High point")*(second_buy == "High point")
it_choice = (firts_buy == "Taster's choice")*(second_buy == "Taster's choice")
isanka = (firts_buy == "Sanka")*(second_buy == "Sanka")
inescafe =  (firts_buy == "Nescafe")*(second_buy == "Nescafe")
ibrim = (firts_buy == "Brim")*(second_buy == "Brim")


first_buy_h_point = (firts_buy == "High point")
first_buy_t_choice = (firts_buy == "Taster's choice")
first_buy_sanka = (firts_buy == "Sanka")
first_buy_nescafe = (firts_buy == "Nescafe")
first_buy_brim = (firts_buy == "Brim")
second_buy_h_point = (second_buy == "High point")
second_buy_t_choice = (second_buy == "Taster's choice")
second_buy_sanka = (second_buy == "Sanka")
second_buy_nescafe = (firts_buy == "Nescafe")
second_buy_brim = (second_buy == "Brim")

# Obtenemos el resto de variables indicadoras basado en la tabla del enunciado:
symm1 = 1*(firts_buy == "High point")*(second_buy == "High point")
symm2 = 2*(firts_buy == "Taster's choice")*(second_buy == "Taster's choice")
symm3 = 3*(firts_buy == "Sanka")*(second_buy == "Sanka")
symm4 = 4*(firts_buy == "Nescafe")*(second_buy == "Nescafe")
symm5 = 5*(firts_buy == "Brim")*(second_buy == "Brim")


symm6 = 6*(firts_buy == "Taster's choice")*(second_buy == "High point")+
        6*(firts_buy == "High point")*(second_buy == "Taster's choice")
symm7 = 7*(firts_buy == "Sanka")*(second_buy == "High point")+
        7*(firts_buy == "High point")*(second_buy == "Sanka")
symm8 = 8*(firts_buy == "Nescafe")*(second_buy == "High point")+
        8*(firts_buy == "High point")*(second_buy == "Nescafe")
symm9 = 9*(firts_buy == "Brim")*(second_buy == "High point")+
        9*(firts_buy == "High point")*(second_buy == "Brim")

symm10 = 10*(firts_buy == "Taster's choice")*(second_buy == "Sanka")+
         10*(firts_buy == "Sanka")*(second_buy == "Taster's choice")
symm11 = 11*(firts_buy == "Taster's choice")*(second_buy == "Nescafe")+
         11*(firts_buy == "Nescafe")*(second_buy == "Taster's choice")
symm12 = 12*(firts_buy == "Taster's choice")*(second_buy == "Brim")+
         12*(firts_buy == "Brim")*(second_buy == "Taster's choice")


symm13 = 13*(firts_buy == "Sanka")*(second_buy == "Nescafe")+
         13*(firts_buy == "Nescafe")*(second_buy == "Sanka")
symm14 = 14*(firts_buy == "Sanka")*(second_buy == "Brim")+
         14*(firts_buy == "Brim")*(second_buy == "Sanka")


symm15 = 15*(firts_buy == "Nescafe")*(second_buy == "Brim")+
         15*(firts_buy == "Brim")*(second_buy == "Nescafe")

symm = symm3+symm1+symm4+symm6+symm2+symm5+symm7+symm8+symm9+symm10+symm11+symm12+
  symm13+symm14+symm15

# Creamos el dataset de las marcas de caf?:
cofee=data.frame(firts_buy,second_buy,count,ih_point,it_choice,isanka,inescafe,
                 ibrim,symm)

# Tablas de contingencia:
table=xtabs(count~firts_buy+second_buy)
Contingency_Table=list(Frequency=table,Percent=prop.table(table),RowPct=prop.table(table,1),ColPct=prop.table(table,2))
Contingency_Table
result=chisq.test(table,correct=FALSE)

# Simple Kappa Coefficient:
library(vcd)
kappa=Kappa(table)
kappa

options(contrast=c("contr.treatment","contr.poly"))

### Quasi-Independence Model
model1=glm(count~firts_buy+second_buy+ih_point+it_choice+isanka+inescafe+
             ibrim,
           family=poisson(link=log))
summary(model1)

### Symmetry Model
model2=glm(count~symm1+symm4+symm6+symm2+symm5+symm3+symm7+symm8+symm9+symm10+symm11+symm12+
             symm13+symm14+symm15,family=poisson(link=log))
summary(model2)

### Marginal Homogeneity Model
LoglinModel=loglm(count~firts_buy+second_buy,param=T,fit=T) 
loglin(fitted(LoglinModel),margin=list(1,2),param=T,fit=T) 
summary(LoglinModel) 

anova(LoglinModel) 
fitted(LoglinModel)

model3=glm(count~first_buy_h_point+first_buy_t_choice+first_buy_sanka+first_buy_nescafe+first_buy_brim+
             second_buy_h_point+second_buy_t_choice+second_buy_sanka+second_buy_nescafe+second_buy_brim+
              +second_buy+symm1+symm4+symm6+symm2+symm5+symm3
             +symm7+symm8+symm9+symm10+symm11+symm12+symm13+symm14+symm15,
              family=poisson(link=log))
summary(model3)

# Los modelos anteriores nos hablan de las relacionesentre marcas, para los cuales
# los modelos toman a la marca brim como nivel de referencia y se pueden observar 
# como las marcas sanka, high point y taster choice  son las mas consumidas. Ademas
# podemos notar las preferencias dada una primera compra como es el caso de que los que
# compran high point tienden a comprar sanka, asi como los que compran sanka en su primera
# compra luego prefieren comprar high point o Brim.





