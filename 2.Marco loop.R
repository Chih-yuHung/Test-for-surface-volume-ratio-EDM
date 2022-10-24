library(REdaS)
library(xlsx)
#global environment
#Set location, initial date and end time
Location<-"Edmonton"
start.date<-"1973-1-1"
end.date<-"1975-12-31"
removal.dates<-as.numeric(as.Date(c("1973-4-25","1973-10-7"
                                    ,"1973-4-25","1973-10-7"
                                    ,"1974-4-25","1974-10-7"
                                    ,"1975-4-25","1975-10-7"),by="days"))
#Read environment data input
Envir.daily<-read.csv("daily env input_RH.csv",header=T)
#To produce an extra year for balance soil temperature
Envir.daily<-Envir.daily[c(1:365,1:1095),]
#to know how many days we have for the loop
d.length<-nrow(Envir.daily)
#Calculate Tmin2, Tmax, cloud cover
Envir.daily$AirTmin2<-c(Envir.daily$AirTmin1[2:length(Envir.daily$AirTmin1)],Envir.daily$AirTmin1[1])
Envir.daily$AirTmax0<-c(Envir.daily$AirTmax1[length(Envir.daily$AirTmax1)],Envir.daily$AirTmax1[1:(length(Envir.daily$AirTmax1)-1)])
#initial manure temp
ini.M.Temp<-read.csv("Initial M temp.csv",header=T)
ini.M.Temp<-ini.M.Temp[,"Initial.Temp"]
#Create Soil and M temp matrix 
S.Temp<-matrix(nrow=300,ncol=288)  #Soil temp
M.Temp<-matrix(ncol=288,nrow=30)   #manure temp calculation, F133:KG162
# Set Output Headers and Write parameters to Output
Output<-data.frame(matrix(ncol = 14,nrow=(d.length-365)))
colnames(Output)<-c("Date ID","Year","Month","Day","DOY","Temperature.C","Depth.cm","Volume.m3"
                    ,"Evaporation.cm","Precipitation.cm","total radiation","M.temp0.5","M.temptop","M.tempbot")
Output$`Date ID`<-as.numeric(seq(as.Date(start.date), as.Date(end.date), by = "days"))
Output$Year<-format(seq(as.Date(start.date),as.Date(end.date),by="days"),"%Y")
Output$Month<-format(seq(as.Date(start.date),as.Date(end.date),by="days"),"%m")
Output$Day<-format(seq(as.Date(start.date),as.Date(end.date),by="days"),"%d")
Output$DOY<-as.numeric(strftime(seq(as.Date(start.date), as.Date(end.date), by = "days"),format = "%j"))
Output<-rbind(Output[1:365,],Output)

#read the constant
source("Constants.R",echo = F)

starttime<-Sys.time()
for (i in 1:d.length){
#To read daily data from environment input.
print(paste("Date ID =",Output$`Date ID`[i],"DOY=",Output$DOY[i])) 
T.day<-Output$DOY[i]
AirTmax0<-Envir.daily$AirTmax0[i]
AirTmax1<-Envir.daily$AirTmax1[i]
AirTmin1<-Envir.daily$AirTmin1[i]
AirTmin2<-Envir.daily$AirTmin2[i]
SR<-Envir.daily$SR[i]
wind<-Envir.daily$wind[i]/3.6*1.0       #daily wind speed
wind.v<-wind*4.87/(log(67.8*10-5.42)) #daily wind speed at 2m
wind.f<-(2.36+1.67*wind.v)*Au^(-0.05) #wind function (for evaporation), mm/d/kPa, G16
cc<-Envir.daily$cloud[i] #cloud clover
precip.d<-Envir.daily$precip[i]/1000

#Reset M.depth and ini.M.temp after soil temperature stabilization
if (i==366){
M.depth<-1
Zmmax<-M.depth
ini.M.Temp<-read.csv("Initial M temp.csv",header=T)
ini.M.Temp<-ini.M.Temp[,"Initial.Temp"]
}

# If current date = removal dates then update depth and average temperature
if (is.element(Output$`Date ID`[i],removal.dates)) {
    cat(paste("manure removal date =",i))
    M.depth<-removal.depth           #Reset manure depth
    Zmmax<-M.depth
    VT<-Final.M.Temp*M.volume
    Avg.M.temp<-sum(VT)/sum(M.volume)-273.15  #avg. manure temp, F86 , degree C
    ini.M.Temp<-c(rep(Avg.M.temp+273.15,30)) #Update all the temperatures to average temperature
   }

#To calculate manure volume, encoding to be change if use mac
source("Manure volume.R",echo=F)
#To calculate solar radiation, soil temp, and manure temp at 300 sec steps.
source("Solar radiation and soil temp.R",echo=F)
#To calculate final hourly temp
source("hourly temp.R",echo=F)


#Write the results, only write after first year
Output[i,6]<-Avg.M.temp.d #Daily manure temperature (C), before depth adjustment
print(paste("ID",i,"And Manure temp",Avg.M.temp.d))
Output[i,7]<-M.depth*100  #Daily manure depth (cm)
Output[i,8]<-M.volume.current #Daily manure volume(m3)
Output[i,9]<-Evap.depth.d*100 #Daily Evaporation (cm)
Output[i,10]<-precip.d*100 #Daily Precipitation (cm)
Output[i,11]<-sum(q.net.rad)  #Net solar radiation, F106:KG106

#write the result for manure temperature at 0.5m
source("8.Manure temperature at 0.5.R",echo=F)

#daily changing depth of manure for next day, L32<-L37
M.depth<-M.depth+depthchange.d
Zmmax<-M.depth    
#Save the new temperatures
#In the sheet, it save a final temp to R60:R89, this was not used, so I skipped it.
ini.M.Temp<-Final.M.Temp
#This the soil temperature(to depth 2.995m), it was a two step to reset ini.S.Temp in marco
ini.S.Temp<-S.Temp[,288]

# Update the Depth of manure
}
endtime<-Sys.time()
Output<-Output[366:d.length,] # first year is for soil temp stabilization
totaltime<-endtime-starttime
totaltime

# Summary for the results, part 2. Only output after all simulation is done. 
#The data of manure tank, i.e output L1:M18
Output.tank<-data.frame(matrix(ncol=2,nrow = 18))
Output.tank[1:18,1]<-c("Location","SurfaceArea(m2)","Starting.Depth(m)"
                       ,"Starting.Volume.m3","Total.Solids(%)",""
                       ,"Tank.Storage","Total.Tank.Volume(m3)"
                       ,"Yearly Maximum Storage Volume.m3","Yearly.Rain.Volume.m3"
                       ,"Yearly.Manure.Storage.Volume.m3","Tank.Diameter.m",""
                       ,"Average.Tm.C","Max.Tm.C","Min.Tm.C","","Max.d.cm")
Output.tank[1,2]<-Location
Output.tank[2:3,2]<-c(Au,M.depth)        #area and initial depth, m2 and m
Output.tank[4,2]<-as.numeric(Output.tank[2,2])*as.numeric(Output.tank[3,2]) #starting volume.
Output.tank[5,2]<-Total.solid                 #%
Output.tank[c(6,7,13,17),2]<-""                      #blank
Output.tank[8,2]<-Tank.v                      #Total Tank volume, m3
Output.tank[9,2]<-Max.storage                 #yearly manure storage,m3
Output.tank[10,2]<-Rain.v                     #yearly rain volume in storage,m3
Output.tank[11,2]<-M.storage                  #yearly Manure storage,m3
Output.tank[12,2]<-ri*2                       #Diameter of tank,m
Output.tank[14,2]<-mean(Output$Temperature.C) #Avg. manure Temperature for the estimate years
Output.tank[15,2]<-max(Output$Temperature.C)  #Max manure Temperature
Output.tank[16,2]<-min(Output$Temperature.C)  #Min manure Temperature
Output.tank[18,2]<-max(Output$Depth.cm)       #Maximum Manure Depth

#output to an excel file
#write.csv(Output, file=paste("result/r=",ri," volume=",M.storage,".csv",sep=""), row.names=FALSE)
write.csv(Output, file=paste("result/r=",ri," volume=",M.storage," Latitude =",L,".csv",sep=""), row.names=FALSE)
#write.xlsx(Output.tank, file=paste(Location,"1.xlsx"), sheetName="Output2", append=TRUE, row.names=FALSE)
