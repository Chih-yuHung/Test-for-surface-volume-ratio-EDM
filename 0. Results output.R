#To output the plots in the manuscript Edmonton results
ri=27.5
#Read results for 50%
temp = list.files(path="result/Wind speed/50%",pattern="csv")
n<-length(temp)
temp50<-list()
for (i in 1:n) {
  temp50[[i]]<-read.csv(paste("result/Wind speed/50%/",temp[i],sep=""))
}

#Create a dataframe to store  80% results. 
result50<-data.frame(ID=temp,maxtemp=c(1:n),mintemp=c(1:n),
                     surface=ri^2*pi,
                     maxvolume=c(1:n),minvolume=c(1:n),maxTa=c(1:n),
                     minTa=c(1:n))

#Read results for 100% wind
temp = list.files(path="result/Wind speed/100%",pattern="csv")
n<-length(temp)
temp100<-list()
for (i in 1:n) {
  temp100[[i]]<-read.csv(paste("result/Wind speed/100%/",temp[i],sep=""))
}

#Create a dataframe to store 100% results. 
result100<-data.frame(ID=temp,maxtemp=c(1:n),mintemp=c(1:n),
                     surface=ri^2*pi,
                     maxvolume=c(1:n),minvolume=c(1:n),maxTa=c(1:n),
                     minTa=c(1:n))

#Read results for 150%
temp = list.files(path="result/Wind speed/150%",pattern="csv")
n<-length(temp)
temp150<-list()
for (i in 1:n) {
  temp150[[i]]<-read.csv(paste("result/Wind speed/150%/",temp[i],sep=""))
}

#Create a dataframe to store  150% results. 
result150<-data.frame(ID=temp,maxtemp=c(1:n),mintemp=c(1:n),
                     surface=ri^2*pi,
                     maxvolume=c(1:n),minvolume=c(1:n),maxTa=c(1:n),
                     minTa=c(1:n))

#Obtain the warmest month: temperature and volume,50%
for (i in 1:n) {
  a<-tapply(temp50[[i]]$Temperature.C[731:1095]
            ,temp50[[i]]$Month[731:1095],mean)
  result50$maxtemp[i]<-max(a)
  result50$mintemp[i]<-min(a)
  
  b<-tapply(temp50[[i]]$Volume.m3[731:1095]
            ,temp50[[i]]$Month[731:1095],mean)
  result50$maxvolume[i]<-b[a==max(a)]
  result50$minvolume[i]<-b[a==min(a)]
}

result50$SVratio.max<-result50$surface/result50$maxvolume
result50$SVratio.min<-result50$surface/result50$minvolume

#Obtain the warmest month: temperature and volume,100%
for (i in 1:n) {
  a<-tapply(temp100[[i]]$Temperature.C[731:1095]
            ,temp100[[i]]$Month[731:1095],mean)
  result100$maxtemp[i]<-max(a)
  result100$mintemp[i]<-min(a)
  
  b<-tapply(temp100[[i]]$Volume.m3[731:1095]
            ,temp100[[i]]$Month[731:1095],mean)
  result100$maxvolume[i]<-b[a==max(a)]
  result100$minvolume[i]<-b[a==min(a)]
}

result100$SVratio.max<-result100$surface/result100$maxvolume
result100$SVratio.min<-result100$surface/result100$minvolume

#Obtain the warmest month: temperature and volume,150%
for (i in 1:n) {
  a<-tapply(temp150[[i]]$Temperature.C[731:1095]
            ,temp150[[i]]$Month[731:1095],mean)
  result150$maxtemp[i]<-max(a)
  result150$mintemp[i]<-min(a)
  
  b<-tapply(temp150[[i]]$Volume.m3[731:1095]
            ,temp150[[i]]$Month[731:1095],mean)
  result150$maxvolume[i]<-b[a==max(a)]
  result150$minvolume[i]<-b[a==min(a)]
}

result150$SVratio.max<-result150$surface/result150$maxvolume
result150$SVratio.min<-result150$surface/result150$minvolume

#To obtain air temperature
Envir.daily<-read.csv("daily env input_RH.csv",header=T)
Envir.daily$Tmean<-(Envir.daily$AirTmax1+Envir.daily$AirTmin1)/2
Tmean.air<-tapply(Envir.daily$Tmean[731:1095]
                  ,Envir.daily$Month[731:1095],mean)
result50$maxTa<-max(Tmean.air)
result50$minTa<-min(Tmean.air)
result100$maxTa<-max(Tmean.air)
result100$minTa<-min(Tmean.air)
result150$maxTa<-max(Tmean.air)
result150$minTa<-min(Tmean.air)


#the difference between max Ta and max Tm, and min Ta and min Tm
result50$diff.max<-result50$maxtemp-result50$maxTa
cor.test(result50$SVratio.max,result50$diff.max) #R = 0.99, P < 0.001
result100$diff.max<-result100$maxtemp-result100$maxTa
cor.test(result100$SVratio.max,result100$diff.max) # R = 0.99, P < 0.001
result150$diff.max<-result150$maxtemp-result150$maxTa
cor.test(result150$SVratio.max,result150$diff.max) # R = 0.99, P < 0.001

result50.EDM<-result50
result100.EDM<-result100
result150.EDM<-result150

save(result50.EDM,file="result50%_EDM")
save(result100.EDM,file="result100%_EDM")
save(result150.EDM,file="result150%_EDM")



