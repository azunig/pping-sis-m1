library(dplyr)
p_new <- read.csv("pruebappn.csv", encoding="UTF-8")

str(p_new)

p_new2 <- read.csv("pruebappn2.csv", encoding="UTF-8")


p_new$estado_n <- ifelse(p_new$Daño %in% c("sin daño","pandeo"), 0,1)
str(p_new$estado)
#glimpse(p_new)

continuous <- select_if(p_new, is.numeric)
summary(continuous)

#acá se utiliza el modelo de una regresion lineal generalizada, binomial porque la 
#variable respuesta es si o no, colapsa o no. 
mod_fit <-glm(estado_n ~  Mag. +  D.m. , data = p_new, family = "binomial")
#se ejecuta la revision de los datos que arroja el modelo
summary(mod_fit) # los resutados 
#estos muestran que Mag. es NA  -> No existe variabilidad.

#acá se ve que mag. solo tiene un valor
summary(p_new)


# la primera conclusion antes de hacer el análisis es determinar cuales son 
# las primeras variables a utilizar para probar el modelo. 
# Ojo: más que "mucho datos" necesitamos variabilidad.
# para que exista este fenómeno, una vez que sepamos que variables tomaremos 
# en función de determinar la probabilidad de colapso, haré una simulación de 
# valores para llegar a la variabilidad necesaria. 




confint(mod_fit) # 95% Intervalo de confianza -> coefficients
exp(coef(mod_fit)) # exponenciales ->  coefficients
exp(confint(mod_fit)) # 95% Intervalos de con./ expon --> coefficients
predict(mod_fit, type="response") # predicción de valores 
residuals(mod_fit, type="deviance") # residuales.

#
plot(residuals(mod_fit, type="deviance"), xlab="...",
     ylab="_", yscale=100,
     main="") 

#magnitud
#glm(estado_n ~  Mag. +  D.m. , data = p_new, family = "binomial")

