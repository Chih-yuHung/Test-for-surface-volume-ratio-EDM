#Line1-38 constant related to manure volume and temperature
#Input from manure storage
n<-c(1:30)  #cell numbers, P60:P89
delta.zu<-c(0:29)
delta.zd<-c(1:30)
zu<-c(0:29)
zd<-c(1:30)
delta.z<-c(1:30)
zp<-c(1:30)

Htank<-5    # height of tank, m, B29
#ri<-30      #Inner raidus of tank, m, B32
Au<-ri^2*pi #tank area, m2, F55
#Tank storage, M26:30
Tank.v<-Au*Htank # Total tank volume, m3, M26
#M.storage<-24300  #yearly manure storage volume, m3, M29 =P32
Rain.v<-(sum(Envir.daily$precip[1:365])/10^3)*Au #yearly precipitation volume, m3, M28
Max.storage<-M.storage+Rain.v
Freeboard<-0.3         #freeboard, m, P34
Rain.max<-Rain.v/Au    #height of max precipitation fall in a day,m, P35
sludge<-0.5            #m, P36
M.depth<-1             #This is the initial manure depth, m, L32
Zmmax<-M.depth         #Depth of manure, m, B31
Tank.design<-M.storage/Au+Freeboard+sludge+Rain.max #m, P37
removal.depth<-0.5   #the depth after removal, m, S52

#Manure properties, R26:29
Total.solid<-8
rho.m<-996.4+4.439*Total.solid         #Manure density, kg/m3, B48,R28
cell.n<-30                             #number of cell,F59
cell.size<-2                           #cell size ratio, F60
grid.c<-cell.size^(1/((cell.n/2)-1))-1 #grid constant, F58

#Manure constant for enthalpy
k.m<-0.6173-0.0069*Total.solid          #Manure thermal conductivity, W/(mK), B47
C.pm<-4187.5-28.9*Total.solid           #Manure specific heat, J/kg K, B49,F49
C.pm.fusion<-C.pm+334000                #Frozen Manure specific heat-fusion, J/kg K, F48
E.272<-272.15*rho.m*C.pm/10^6           #Enthalpy at 272.15 (MJ/m3),S202
E.273<-E.272+(1*rho.m*C.pm.fusion)/10^6 #Enthalpy at 273.15 (MJ/m3),S203
fusion<-rho.m*C.pm.fusion/10^6          #Fusion of manure

#Average annual temperature
ann.T<-mean(((Envir.daily$AirTmax1+Envir.daily$AirTmin1)/2)[1:365])

#Input manure temperature
Avg.Barn.temp<-ann.T         #degree C, avg. annual barn temp, L46
Barn.temp.amp<-10             #degree C, amplitude of annual temp, L47
Temp.cost<-4.32              #Temp phase constant, L48

#Solar data
L<-53.66667                #Latitude
alpha.s<-0.8               #solar absorptivity, B18
Eb<-1395                   #extraterrestrial solar flux density, W m-2
tau<-0.75                  #Atmospheric transimttance, 0.75 clear, 0.4 overcast
A<-670                    #Altitude, m
Pa<-101325*exp(-A/8200)    #Local air pressure, Pa
e.sigma<-5.67*10^-8        #Stefan-Boltzmann constant, B25
epsilon<-0.95              #emissivity,B26

#Air input
ka<-0.025                  #air thermal conductivity, W/(mK), B11
Vair<-14.2*10^-6           #air kinematic viscosity, m2 s-1, B13
Pr<-0.714                  #Air Prandtl Number, unitless, B12

#Evaporation rate calculation, Campbell&Norman, 1998.
Teten.H2Oa<-0.611          #kPa, G9
Teten.H2Ob<-17.502         #unitless, G10
Teten.H2Oc<-240.97         #degree C, G11
Teten.Iceb<-21.87          #unitless, G12
Teten.Icec<-265.5          #degree C, G13
rho.w<-1000                #water density. kg/m3, G14
Lambda<-2.45*10^6          #latent heat of vaporization, J kg-1, G15  

#2.6 Radiative heat transfer
T.delta<-300                                          #F62
T.step<-c(1:288)                                      #F93:KG93   
T.second<-seq(300,by=300,length.out=length(T.step))   #F94:KG94
T.hour<-seq(300/3600,24,length.out=length(T.step))    #F95:KG95
H<-15*(T.hour-12)                                     #hour angle, F100:KG100

# constants for soil temperature
dep.s<-0.01    #depth of soil slice, F9101
den.s<-1800    #Soil density, kg/m3, B41,
ks<-0.915      #soil thermal conductivity,W/mk, B42, 
annualT<-mean(c(Envir.daily$AirTmax1,Envir.daily$AirTmin1)) #for ini. soil temp, assume equal to mean annual air temp, B43
annualT.K<-annualT+273.15 #soil temp at K, B44
ini.S.Temp<-rep(annualT.K,300) #initial soil temp was assumed to annual air
Cp.s<-1220     #specific heat of soil, J/(kgK), B45
ks.cp<-ks/Cp.s #thermal conductivity/specific heat, B46,F9102
