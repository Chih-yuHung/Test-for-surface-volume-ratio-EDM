#To calculate manure temperature under various scenarios
temp.radius<-c(10,20,30,40,50)
temp.r<-seq(from=10,to=50,length=20)
temp.Mstorage<-c(2400,9600,21200,38400,60000)
temp.latitude<-c(20,30,40,50,60)
ri<-27.5
#To simulate various diameter/volume only
for (l in 1:10) {
    M.storage<-ri^2*pi*5*1.5*0.97^l
    source("2.Marco loop.R",echo = F)
}

#To simulate various diameter and various annual manure input
# for (l in 1:5){
#   for (p in c(1,0.9,0.8,0.7)){
#     ri<-temp.radius[l]
#     M.storage<-temp.Mstorage[l]*p
#     source("2.Marco loop.R",echo = F)
#   }
# }

# #to simulate in differe latitude
# for (l in 1:5){
#   for (p in 1:5){
#     ri<-temp.radius[l]
#     M.storage<-temp.Mstorage[l]
#     L<-temp.latitude[p]
#     source("2.Marco loop.R",echo = F)
#   }
# }

