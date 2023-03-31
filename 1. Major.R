#To calculate manure temperature under various scenarios
ri<-27.5
#To simulate various diameter/volume only
for (l in 1:10) {
    M.storage<-ri^2*pi*5*1.5*0.92^l
    source("2.Marco loop.R",echo = F)
}
