#import from excel
library("readxl")
library(psych)
baro <- as.data.frame(read_excel("datos para analizar_v04_26-10.xlsx"))
horasanuales <- 1800
baro[ , 22] <- ((baro[ , 22]/baro[ , 14])/horasanuales)

#Get rid of two problematic cases where responses make no sense 
#(filled with letter instead of numbers and data)
#baro <- baro[-18, ]
#baro <- baro[-20, ]
View(baro)

#1- DESCRIPTIVES
#Year
anno <- table(baro[,3]); anno

##Procedencia:	1- reciclado; 2- 1 año después; 3- 2018	
phases <- table(baro[ ,4]); phases
completed <- phases[2]; completed
ncases <- nrow(baro); ncases

#IDENTIDAD
scaleID <- baro[ , (6:12)] 
describe(scaleID)
nitemsID <- sum(length(6:12)); nitemsID
dimID <- nitemsID*ncases; dimID
NAid <- sum(is.na(scaleID)); NAid
percentageNAid <- NAid/dimID; percentageNAid
cat("percentage of missing data in this scale is", percentageNAid*100)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
v <- baro[ , 9]
result <- getmode(v)
print(result)
##IDEN01	Puesto (1 director de selección; 2 HRBP; 3 Responsable desarrollo y formación; 4 HR Manager; 5 Director HR; 6 Account Manager; 7 Socio director; 8 Director desarrollo negocio; 9 director general; 10 responsable técnico; 11 modelo información rh; 12 people analyticis expert							
##IDEN02	Departamento (1 selección formación y desrrololo; 2 gestión económica y previsión social; 3 Desarrollo y formación; 4 recursos humanos; 5 ventas; 6 dirección general; 7 desarrollo de negocio; 8 modelo de información rh; 9 consultoría; 10 otros muy diferentes; 11 experiencia empleado; 12 HR Strategy							
##IDEN03	Años en la Organización							
##IDEN04	Sector (1 petroleo y energia) (2 materiales básicos industria y construcción) (3 bienes de consumo) (4 servicios de consumo) (5 servicios financieros e inmobilidarios) (6 tecnología y telecomunicaciones) (7 salud y servicios sociales)							
##IDEN05	Ambito de Actuación (1 local) (2 nacional) ((3 internacional) (4 multinacional global)							
##IDEN06	Facturación en Millones de Euros							
##IDEN07	EBITDA En Millones de euros			

#FUERZA TRABAJO
#Recodificamos FT10 en un porcentaje:
baro[ , 22] <- baro[ , 22]/baro[ , 13]
#coste personal/ numero empleados
baro[ , 15] <- baro[ , 15]/baro[ , 13]
scaleFT <- baro[ , 13:27] 
a <- describe(scaleFT)
as.matrix(round(a$mean))
nitemsFT <- sum(length(13:27)); nitemsFT
dimFT <- nitemsFT*ncases; dimFT
NAft <- sum(is.na(scaleFT)); NAft
percentageNAft <- NAft/dimFT; percentageNAft
cat("percentage of missing data in this scale is", percentageNAft*100)


##FT01	Numero empleados a comienzo de año							
##FT02	Numero de empleados a final de año							
##FT03	Coste de personal (coste total de los empleados)							
##FT04	% jubilaciones							
##FT05	% despidos							
##FT06	% finalizaciones de contrato							
##FT07	% rotación							
##FT08	% contratos fijos							
##FT09	% mujeres en la organización							
##FT10	% horas de absentismo en el año							
##FT11	categoría del % de menores de 25 años (1 20% o menos) (2 entre 20 y 30%) (3 30-40%) (4 40-60%) (5 60-70%) (6 70-80%) (7 mas del 80%)							
##FT12	categoría del % de entre 25 y 35 años (porcentajes iguales a los anteriores)							
##FT13	categoría del % de entre 36 y 45 años (porcentajes iguales a los anteriores)							
##FT14	categoría del % de entre 46 y 55 años (porcentajes iguales a los anteriores)							
##FT15	categoría del % de más de 55 años (porcentajes iguales a los anteriores)							

#EFECTIVIDAD
scaleef <- baro[ , (28:34)]
describe(scaleef)
nitemsEF <- sum(length(28:34)); nitemsEF
dimEF <- nitemsEF*ncases; dimEF
NAef <- sum(is.na(scaleef)); NAef
percentageNAef <- NAef/dimEF; percentageNAef
cat("percentage of missing data in this scale is", percentageNAef*100)

##EFE01	Efectividad de la organización item01							
##EFE02	Efectividad de la organización item02							
##EFE03	Efectividad de la organización item03							
##EFE04	Efectividad de la organización item04							
##EFE05	Efectividad de la organización item05							
##EFE06	Efectividad de la organización item06							
##EFE07	Efectividad de la organización item07							

#GENERAL
scalegen <- baro[ , 35:40]
describe(scalegen)
nitemsgen <- sum(length(35:40)); nitemsgen
dimGEN <- nitemsgen*ncases; dimGEN
NAgen <- sum(is.na(scalegen)); NAgen
percentageNAgen <- NAgen/dimGEN; percentageNAgen
cat("percentage of missing data in this scale is", percentageNAgen*100)

##GEN01	Puntuación Global SALUD							
##GEN02	Puntuación Global ADAPTACIÓN E INFLUENCIA							
##GEN03	Puntuación Global EXPERIENCIA EMPLEADO							
##GEN04	Puntuación Global EQUIPOS DE TRABAJO							
##GEN05	Puntuación Global LÍDERES							
##GEN06	Puntuación Global VALOR EMPLEADO							

#ESPECIFICO
scaleesp <- baro[ , 41:65]
describe(scaleesp)
nitemsESP <- sum(length(41:65))
dimESP <- nitemsESP*ncases; dimESP
NAESP <- sum(is.na(scaleesp)); NAESP
percentageNAESP <- NAESP/dimESP; percentageNAESP
cat("percentage of missing data in this scale is", percentageNAESP*100)

##ESPE01	Puntuación en sub-dimension 01 (Capacidad de Aprendizaje)							
##ESPE02	Puntuación en sub-dimension 02 (Cultura conexión)							
##ESPE03	Puntuación en sub-dimension 03 (Digitalización)							
##ESPE04	Puntuación en sub-dimension 04 (Flex. Equipo humano)							
##ESPE05	Puntuación en sub-dimension 05 (Flex. Toma decisiones)							
##ESPE06	Puntuación en sub-dimension 06 (Flex. estructural)							
##ESPE07	Puntuación en sub-dimension 07 (Innovación)							
##ESPE08	Puntuación en sub-dimension 08 (Inversión des. equipos)							
##ESPE09	Puntuación en sub-dimension 09 (Facilitación trabajo equipos)							
##ESPE10	Puntuación en sub-dimension 10 (Autogestion equipos)							
##ESPE11	Puntuación en sub-dimension 11 (Resultados Equipos)							
##ESPE12	Puntuación en sub-dimension 12 (Condiciones de trabajo)							
##ESPE13	Puntuación en sub-dimension 13 (Bienestar Empleados/daño)							
##ESPE14	Puntuación en sub-dimension 14 (Interacción organización)							
##ESPE15	Puntuación en sub-dimension 15 (Interacción Tarea)							
##ESPE16	Puntuación en sub-dimension 16 (Interacción compañeros)							
##ESPE17	Puntuación en sub-dimension 17 (Calidad Organización)							
##ESPE18	Puntuación en sub-dimension 18 (Inversión Salud)							
##ESPE19	Puntuación en sub-dimension 19 (Inversión Experiencia Empleado)							
##ESPE20	Puntuación en sub-dimension 19 (Actitudes Empleados)							
##ESPE21	Puntuación en sub-dimension 20 (Características Empleados)							
##ESPE22	Puntuación en sub-dimension 21 (Consecuciones de los empleados)							
##ESPE23	Puntuación en sub-dimension 22 (Inversión en liderazgo)							
##ESPE24	Puntuación en sub-dimension 23 (Excelencia liderazgo desarrollado)							
##ESPE25	Puntuación en sub-dimension 24 (Impacto Liderazgo en organ.)

# 2 MISSING DATA
NAindata <- sum(is.na(baro[ , -(1:4)])); NAindata
percentageNAbaro <- NAindata/prod(dim(baro[ , -(1:4)])); percentageNAbaro
cat("percentage of missing data is", percentageNAbaro*100)

##Indicadores efectividad

#facturacion/empleados final año
facturacion <- baro[ , 11]
empleados <- baro[ , 14]
efectividad1 <- as.matrix(facturacion/empleados); efectividad1


#EBITDA/empleados final año
ebitda <- baro[ , 12]
efectividad2 <- as.matrix(ebitda/empleados); efectividad2

#facturacion/coste personal
costepersonal <- baro[ , 15]
inefectividad1 <- facturacion/costepersonal

#EBITDA/ coste personal
inefectividad2 <- ebitda/costepersonal

baro [ , 66]<- efectividad1
baro [ , 67]<- efectividad2
baro [ , 68]<- inefectividad1
baro [ , 69]<- inefectividad2



#Correlaciones economic
cor.test(efectividad1, efectividad2)
cor.test(inefectividad1, inefectividad2)

#3- METHODS TO SUBSTITUTE NAS
#if possible, check if it is MAR
library(mice)
md.pattern(scaleef, plot = TRUE)
md.pattern(scalegen, plot = TRUE)
md.pattern(scaleesp, plot = TRUE)

scaleef1 <- mice(scaleef,m=5,maxit=50,meth='pmm',seed=500)
scaleef <- complete(scaleef1, 1)

scalegen1 <- mice(scalegen,m=5,maxit=50,meth='pmm',seed=500)
scalegen <- complete(scalegen1, 1)

scaleesp1 <- mice(scaleesp,m=5,maxit=50,meth='pmm',seed=500)
scaleesp <- complete(scaleesp1, 1)

baro <- as.data.frame(as.matrix(baro[ ,3:72]))
baro1 <- mice(baro,m=5,maxit=50,meth='pmm',seed=500)
baro <- complete(baro1, 1)
#2- CHECK RELIABILITY WITHIN SUBSCALES
library("GPArotation")

#Check unidimensionality
fa.parallel(scaleef)
fa.parallel(scalegen)
fa.parallel(scaleesp)


fa(scaleesp, nfactors=2)
fa(scaleesp, nfactors=1)

#Check omega
omega(scaleef,nfactors=1)

omega(scalegen,nfactors=1)

omega(scaleesp,nfactors=1)


#4- CORRELATIONS
scaleef <- as.matrix(apply(scaleef, 1, sum))
scalegen <- as.matrix(apply(scalegen, 1, sum))
scaleesp <- as.matrix(apply(scaleesp, 1, sum))
  
  
# CORRELATIONS between scales
cor.test(scaleef, scalegen)
cor.test(scaleef, scaleesp)
cor.test(scalegen, scaleesp)

#CORRELATIONS BETWEEN ECONOMIC ITEMS AND SCALES

#ID4 sector
cor.test(baro[ , 9], baro[ , 17])
cor.test(baro[ , 9], baro[ , 19])
cor.test(baro[ , 9], baro[ , 22])

cor.test(baro[ , 9], scaleef)
cor.test(baro[ , 9], scalegen)
cor.test(baro[ , 9], scaleesp)

#ID6 facturacion
cor.test(facturacion, baro[ , 17])
cor.test(facturacion, baro[ , 19])
cor.test(facturacion, baro[ , 22])
cor.test(facturacion, scaleef)
cor.test(facturacion, scalegen)
cor.test(facturacion, scaleesp)

#ID7 ebitda
cor.test(ebitda, baro[ , 17])
cor.test(ebitda, baro[ , 19])
cor.test(ebitda, baro[ , 22])
cor.test(ebitda, scaleef)
cor.test(ebitda, scalegen)
cor.test(ebitda, scaleesp)

#CORRELATIONS BETWEEN KPI'S AND SCALES

#FT4 jubilaciones
cor.test(baro[ , 16], scaleef)
cor.test(baro[ , 16], scalegen)
cor.test(baro[ , 16], scaleesp)

#FT5 despidos
cor.test(baro[ , 17], scaleef)
cor.test(baro[ , 17], scalegen)
cor.test(baro[ , 17], scaleesp)

#FT6 finalizacion
cor.test(baro[ , 18], scaleef)
cor.test(baro[ , 18], scalegen)
cor.test(baro[ , 18], scaleesp)


#FT7 rotacion
cor.test(baro[ , 19], scaleef)
cor.test(baro[ , 19], scalegen)
cor.test(baro[ , 19], scaleesp)

#FT8 contratos fijos
cor.test(baro[ , 20], scaleef)
cor.test(baro[ , 20], scalegen)
cor.test(baro[ , 20], scaleesp)

#FT9 mujeres
cor.test(baro[ , 21], scaleef)
cor.test(baro[ , 21], scalegen)
cor.test(baro[ , 21], scaleesp)

#FT10 absentismo
cor.test(baro[ , 22], scaleef)
cor.test(baro[ , 22], scalegen)
cor.test(baro[ , 22], scaleesp)

#FT11 25 años
cor.test(baro[ , 23], scaleef)
cor.test(baro[ , 23], scalegen)
cor.test(baro[ , 23], scaleesp)

#FT12 25-35 años
cor.test(baro[ , 24], scaleef)
cor.test(baro[ , 24], scalegen)
cor.test(baro[ , 24], scaleesp)

#FT13 36-45 años
cor.test(baro[ , 25], scaleef)
cor.test(baro[ , 25], scalegen)
cor.test(baro[ , 25], scaleesp)

#FT14 46-55 años
cor.test(baro[ , 26], scaleef)
cor.test(baro[ , 26], scalegen)
cor.test(baro[ , 26], scaleesp)

#FT15 mayores 55 años
cor.test(baro[ , 27], scaleef)
cor.test(baro[ , 27], scalegen)
cor.test(baro[ , 27], scaleesp)

#Correlaciones KPIs añadidos y escalas

#efectividad1
cor.test(efectividad1, scaleef)
cor.test(efectividad1, scalegen)
cor.test(efectividad1, scaleesp)

#efectividad2
cor.test(efectividad2, scaleef)
cor.test(efectividad2, scalegen)
cor.test(efectividad2, scaleesp)

#inefectividad1
cor.test(inefectividad1, scaleef)
cor.test(inefectividad1, scalegen)
cor.test(inefectividad1, scaleesp)

#inefectividad2
cor.test(inefectividad2, scaleef)
cor.test(inefectividad2, scalegen)
cor.test(inefectividad2, scaleesp)



#5- VISUALS
#PLOT

contratosfijos <- baro[ , 20]
despidos <- baro[ , 17]
absentismo <- baro[ , 22]

plot(contratosfijos,scaleef,type="p",col="red")

plot(contratosfijos,scaleesp,type="p",col="red")

plot(facturacion,despidos,type="p",col="red")

plot(facturacion,absentismo,type="p",col="red")


#graficos correlaciones escalas
plot(scaleef,scalegen,type="p",col="red")

plot(scaleef,scaleesp,type="p",col="red")

plot(scalegen,scaleesp,type="p",col="red")

#predictions
prediction1 <- lm(scaleef ~ contratosfijos); prediction1
prediction2 <- lm(scaleesp ~ contratosfijos); prediction2
prediction3 <- lm(despidos ~ facturacion); prediction3
prediction4 <- lm(absentismo ~ facturacion); prediction4




#correlaciones item: otras escalas, FTs, KPI's y compuestos



baro [ , 70] <- scaleef
baro [ , 71] <- scalegen
baro [ , 72] <- scaleesp
View(baro)
pvalue <- 0.05
ncorrections <- 66
correction <- pvalue/ncorrections

n <- 71
print(n)
for (i in 72:72) {
  print(i)
  correlacion <- cor.test(as.numeric(baro[ ,n]), as.numeric(baro[ ,i]), method = "kendall")
  
   if (correlacion$p.value <= correction) {print(correlacion)}
   }


# Ward Hierarchical Clustering
d <- dist(baro[ , 35:40], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

write.table(baro, "~/datosimputados.txt", sep="\t")


lmgenef <- lm(weight ~ group)
plot(bodymass, height, pch = 16, cex = 1.3, col = "blue", main = "HEIGHT PLOTTED AGAINST BODY MASS", xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")
abline(lm(height ~ bodymass))

lmespef <- lm(weight ~ group)
plot(bodymass, height, pch = 16, cex = 1.3, col = "blue", main = "HEIGHT PLOTTED AGAINST BODY MASS", xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")
abline(lm(height ~ bodymass))