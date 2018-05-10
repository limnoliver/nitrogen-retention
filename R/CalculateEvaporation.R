

EvapRate <- function(U, Ew, Ea){
  
  E = 0.165*(0.8+U*86.4*0.447/100)*(Ew-Ea)
  return(E)
  #E is in mm/day
}

Weather<-read.csv('Data/WeatherUnderground.csv', header=T)

Weather$E_mmPerDay<-EvapRate(Weather$Wind, Weather$Ew, Weather$Ea)

# 0.447 multiplier for mph to m/s

# LaCrosse_E<- EvapRate(U=8, Ew=36.13, Ea=17.68)
# print(LaCrosse_E)
# 
# StLouis_E<- EvapRate(U=4, Ew=42.44, Ea=20.36)
# print(StLouis_E)
# 
# LaCrosse_E*101.5/86.400
# 
# StLouis_E*57.9/86.400

