#comparar con simulaciones

mediaemocionessujeto <- mean(emocionsum)
sdemocionessujeto <- sd(emocionsum)


d <- seq(0,80,by = 1)
hist(emocionsum, d)

f <- seq(920,1000,by = 1)
noinfo <- 1000-emocionsum
hist(noinfo, f)


sujetosconemociones <- emocionsum[which(emocionsum != 0)]

##PLOTTING distribution of dataset
N <- 1000
n <- 1
p <- mediaprobabilidad
x <- rbinom(N,n,p)
hist(x, 
     xlim = c(min(x), max(x)), 
     probability = TRUE, 
     nclass = max(x) - min(x) + 1, 
     col = 'lightblue',
     main = 'Binomial distribution, n=total tweets, p=media probabilidad')
lines(density(x, bw=1), col = 'red', lwd = 3)
##
sum(rbinom(N,n,p))

##trimmed dataset distribution
N <- 50
n <- 1
p <- mediaprobabilidad
y <- rbinom(N,n,p)
hist(y, 
     xlim = c(min(y), max(y)), 
     probability = TRUE, 
     nclass = max(y) - min(y) + 1, 
     col = 'lightblue',
     main = 'Binomial distribution, n=trimmed tweets, p=media probabilidad')
lines(density(y, bw=1), col = 'red', lwd = 3)
##





y <- repeat(y,2)
lm(x~)
length(x)
length(y)








#Compare two distributions
library(lattice)

histogram(~ x|y, data = dat, col = "gray60", layout = c(1, 2),
          xlab = list("Months of flowering"),
          ylab = list("Percentage of total"),
          scales = list(y = list(alternating = F)),
          strip = strip.custom(factor.levels = c("alien", "indigenous")))

qqplot(dat$Flowering[dat$Status == "indigen"],
       dat$Flowering[dat$Status == "Neophyt"])
abline(a = 0, b = 1, lty = 3)

ks.test(dat$Flowering[dat$Status == "indigen"],
        dat$Flowering[dat$Status == "Neophyt"])

wilcox.test(Flowering ~ Status, data = dat)

t.test(x ~ y)





