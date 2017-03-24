trainDF$Survived<-factor(trainDF$Survived,levels=c(0,1),labels=c("Passed","Survived"))
trainDF$Pclass<-factor(trainDF$Pclass,levels=c(1,2,3),labels=c("First","Second","Third"))
trainDF$Sex<-factor(trainDF$Sex)
trainDF<-na.omit(trainDF)

#levels(trainDF$Survived)[1]<-"Passed"
#levels(trainDF$Survived)[2]<-"Lived"
#levels(trainDF$Pclass)[1]<-"First"
#levels(trainDF$Pclass)[2]<-"Second"
#levels(trainDF$Pclass)[3]<-"Third"