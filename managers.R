###################################MANAGERS' ASSESSMENT

#libraries
library("xlsx")
library("dplyr")
library("tidyverse")
library("plyr")
library("readr")

###Datasets
assess <- as.data.frame(read.xlsx2("assess.xlsx", sheetIndex = 1))
selfasassess <- as.data.frame(read.xlsx2("assess.xlsx", sheetIndex = 2))
assessment <- assess[,-(75:78)]
selfassessment <- selfasassess[,-(75:78)]


#Defining managers
manageras <- c(as.character(unique(assessment[,1])))
managersas <- c(as.character(unique(selfassessment[,1])))
managerwithoutselfassessment <- manageras[!manageras%in%managersas]


#Creating dataframes to work with
df1 <- data.frame(matrix(unlist(assessment), nrow=length(assessment), byrow=T))
df2 <- data.frame(matrix(unlist(selfassessment), nrow=length(selfassessment), byrow=T))

#Getting scores each manager
manager <- list(0)
for (i in 1:length(manageras)) {
   manager[[i]] <- (as.data.frame(df1[ ,c(which(df1[1, ]==manageras[i]))])) 
}


#Getting means
averages <- list(0)
for (i in 1:length(manager)) 
 {
        averages[[i]] <- apply(matrix(as.numeric(as.character(unlist(manager[[i]][-1,]))), nrow = 73),1,mean,na.rm=TRUE)
}


#Getting sds
deviations <- list(0)
for (i in 1:length(manager)) 
{
  deviations[[i]] <- apply(matrix(as.numeric(as.character(unlist(manager[[i]][-1,]))), nrow = 73),1,sd,na.rm=TRUE)
}


#Relating assessment and selfassessment, difference
differences <- list(0)
man <- c(c(1:2),c(4:12),c(14:length(manager)))
for (i in man) 
{
  differences[[i]] <- unlist(averages[[which(df2[1, ] ==unlist(as.character((manager[[i]][1,1]))))]])-unlist(as.numeric(as.character(df2[-1,which(df2[1, ] ==unlist(as.character((manager[[i]][1,1]))))])))
}


#Merge data for each manager

var <- c("differences", "deviations", "averages")
finished <- list(0)
for (i in 1:length(manager)) {
  finished[[i]] <- data.frame(matrix(cbind(differences[[i]],deviations[[i]], averages[[i]]), nrow = 73))
}

for (i in 1:length(manager)) {
  finished[[i]] <- data.frame(matrix(cbind(differences[[i]],deviations[[i]], averages[[i]]), nrow = 73))
}

write.xlsx(finished, "finished.xlsx")
write.xlsx(manager, "manager.xlsx")


###########################################3#Graphs
for (i in man) {
 plot(differences[[i]],averages[[i]])
plot(deviations[[i]],averages[[i]]) 
}



##Create benchmark: managers' overall vs manager average
##and graphs 
## graphs average manager vs self manager




#########################################Qualitative
qasses <- assess[,(75:78)]
qself <- selfasassess[,(75:78)]

#Creating dataframes to work with
df3 <- data.frame(matrix(unlist(qasses), nrow=length(qasses), byrow=T))
df4 <- data.frame(matrix(unlist(qself), nrow=length(qself), byrow=T))

#Getting scores each manager
quali <- list(0)
for (i in 1:length(manageras)) {
  quali[[i]] <- (as.data.frame(df3[ ,c(which(df1[1, ]==manageras[i]))])) 
}


write.xlsx(manageras, "orden_managers.xlsx")
######################################################################################3
#Merge data for each manager


qq <- list(0)
for (i in 1:length(manager)) {
  qq[[i]] <- data.frame(matrix(cbind(as.character(quali[[i]])), nrow = 73))
}





