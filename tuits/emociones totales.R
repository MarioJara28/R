

#trabajo con datos

sujeto <- matrix(NA, 449)

for (i in 1:dim(validos1)) {
a <- as.matrix(unlist(lapply(validos1[[i]], sd)))
b <- as.matrix(a[4:23, ])
sujeto[i] <- apply(b, 2, sd)
}


emocion <- matrix(NA)

  a <- as.matrix(unlist(lapply(validos1[[i]], sd)))
  emocion[i] <- as.matrix(a[4:23, ])


for (i in 1:dim(validos1)) {
emocion[i] <- (as.matrix(unlist(lapply(validos1[[i]][, 25:44], sd))))
}

emocionsd <- emocion

for (i in 1:dim(validos1)) {
  emocion[i] <- (as.matrix(unlist(lapply(validos1[[i]][, 25:44], sum))))
}
emocionsum <- emocion
probabilidademocion <- emocionsum/1000
mediaprobabilidad <- mean(probabilidademocion)
sdprobabilidad <- sd(probabilidademocion)
wholedatasetemotions <- 377*1000
rm(list = c("emocion", "a", "b"))

