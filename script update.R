library("xlsx")
library("dplyr")
library("purrr")
uno1 <- as.data.frame(read.xlsx2("double-check.xlsx", sheetIndex = 1))
dos1 <- as.data.frame(read.xlsx2("double-check.xlsx", sheetIndex = 2))

#we delete columns that changed but do not interest us
uno <- uno1[ ,c(-2,-16)]
dos <- dos1[ ,c(-2,-16)]


#values in uno which are not in dos
a1 <- anti_join(uno, dos)
#values in dos that are not in uno
a2 <- anti_join(dos,uno)


#find differences within rows a1 to uno; a2 to dos

ida1 <- as.numeric(as.character((unlist(uno[,1]))))
ida2 <- as.numeric(as.character((unlist(dos[ ,1]))))


incorporaciones <- which((ida2 %in% ida1) == FALSE)
bajas <- which((ida1 %in% ida2) == FALSE)
estables <- ida1[which((ida1 %in% ida2) == TRUE)]


a <- list(0) 

#Now, from the estable employees
for (i in 1:length(estables)) {
  a[[i]] <- ((uno[which(estables[i] == uno[ ,1]), ]== dos[which(estables[i] == dos[ ,1]), ]))
  
}

#select estable employees (all == TRUE); from changed employees (any==FALSE)
b <- list(0)
for (i in 1:length(a)){
  if(any(a[[i]]==FALSE)){
    b[[i]]<- (a[[i]])}
 
}


#Select ids from changed employees
c <- 0
for (i in 1:length(b)) {
  if(!is.null(b[[i]])) {
  c[i] <- i
} 
}
idcambios <- as.matrix(ida1[which(is.na(c)==FALSE)])
e <- which(is.na(c)==FALSE)

f <- list(0)
for (i in 1:length(e)) {
 f[[i]]<-(b[[e[i]]]) 
}

as.numeric(unlist(as.character(f[[1]]))),nrow = 46, ncol = 35)

as.integer(is.logical(f[[1]]))
