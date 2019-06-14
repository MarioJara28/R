#import from excel
library("readxl")
library(psych)
baro <- as.data.frame(read_excel("datos para analizar_v04_26-10imputados.xlsx"))
scaleef <- baro[ , (28:34)]
scaleeft <- rowSums(scaleef)

scalegen <- baro[ , 35:40]
scalegent <- rowSums(scalegen)

scaleesp <- baro[ , 41:65]
scaleespt <- rowSums(scaleesp)


lmgenef <- lm(weight ~ group)
plot(bodymass, height, pch = 16, cex = 1.3, col = "blue", main = "HEIGHT PLOTTED AGAINST BODY MASS", xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")
abline(lm(height ~ bodymass))

lmespef <- lm(weight ~ group)
plot(bodymass, height, pch = 16, cex = 1.3, col = "blue", main = "HEIGHT PLOTTED AGAINST BODY MASS", xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")
abline(lm(height ~ bodymass))



library("ggplot2")

ggplot(data = baro, aes(x = scaleeft, y = scaleespt)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)
