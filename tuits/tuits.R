
#datos
tuits <- read.csv("tweets_usuarios_analizados.csv", sep = ";", row.names=NULL)

#limpieza
tuits <- na.omit(tuits)
tuits <- split(tuits, tuits[,"sujeto"])
tuits <- as.array(tuits)

###sujetos sin nombre claro tipo "---"
validos <- tuits[100:571]

###inspeccion visual
validos1 <- validos[-c(9, 10, 20, 21, 116, 229, 230, 296, 297, 298, 330, 341, 381, 393, 409, 410, 411, 412, 436, 454, 455, 465, 466)]

remove(list=c("validos", "tuits")) 


