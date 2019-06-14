library("readxl")
library("psych")
baro <- as.data.frame(read_excel("datos para analizar_v04_26-10.xlsx"))
horasanuales <- 1800
baro[ , 22] <- ((baro[ , 22]/baro[ , 14])/horasanuales)


#FUERZA TRABAJO
#Recodificamos FT10 en un porcentaje:
baro[ , 22] <- baro[ , 22]/baro[ , 13]
#coste personal/ numero empleados
baro[ , 15] <- baro[ , 15]/baro[ , 13]

baro <- as.data.frame(baro, col.names = NULL,)

baro1 <- header(baro) <- NULL

baro <- as.data.frame(unclass(baro))

library(mice)
baro <- baro[ , 7:22]
baro1 <- mice(baro,m=3,maxit=10,meth='pmm',seed=0)
baro <- complete(baro1, 1)

write.table(baro, "~/datosimputados.txt", sep="\t")