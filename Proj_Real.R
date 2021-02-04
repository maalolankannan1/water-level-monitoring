#Datasets of Water levels and rainfall of 4 reservoirs 
water_level = read.csv("F:\\R Studio Proj\\chennai-water-management\\chennai_reservoir_levels.csv")
rainfall = read.csv("F:\\R Studio Proj\\chennai-water-management\\chennai_reservoir_rainfall.csv")

#Creating dataframes splitting by years
i=0
l=1
poondi=c(1:15)
cholavaram = c(1:15)
redhills = c(1:15)
chembarambakkam = c(1:15)
x=c(366,365,365,365,366,365,365,365,366,365,365,365,366,365,365)
for (k in x) {
  poondi[l]<- sum(water_level$POONDI[(i+1):(i+k)])/k
  cholavaram[l]=sum(water_level$CHOLAVARAM[(i+1):(i+k)])/k
  redhills[l]=sum(water_level$REDHILLS[(i+1):(i+k)])/k
  chembarambakkam[l]=sum(water_level$CHEMBARAMBAKKAM[(i+1):(i+k)])/k
  i<-i+x[l]
  l<-l+1
}
poondi
cholavaram
redhills
chembarambakkam
yr = c(2004:2018)
waterlevel = data.frame(yr,poondi,cholavaram,redhills,chembarambakkam)
waterlevel
i=0
l=1
for (k in x) {
  poondi[l]<- sum(rainfall$POONDI[(i+1):(i+k)])/k
  cholavaram[l]=sum(rainfall$CHOLAVARAM[(i+1):(i+k)])/k
  redhills[l]=sum(rainfall$REDHILLS[(i+1):(i+k)])/k
  chembarambakkam[l]=sum(rainfall$CHEMBARAMBAKKAM[(i+1):(i+k)])/k
  i<-i+x[l]
  l<-l+1
}
rainlevel = data.frame(yr,poondi,cholavaram,redhills,chembarambakkam)
rainlevel

#Plotting bargraphs for each river
barplot(waterlevel$poondi,names.arg=yr,xlab = "Year",ylab = "Poondi",col="blue",main = "Poondi water level",border = "red")
barplot(waterlevel$cholavaram,names.arg=yr,xlab = "Year",ylab = "Cholavaram",col="red",main = "Cholavaram water level",border = "blue")
barplot(waterlevel$redhills,names.arg=yr,xlab = "Year",ylab = "Redhills",col="green",main = "Redhills water level",border = "yellow")
barplot(waterlevel$chembarambakkam,names.arg=yr,xlab = "Year",ylab = "Chembarambakkam",col="yellow",main = "Chembarambakkam water level",border = "black")
barplot(rainlevel$poondi,names.arg=yr,xlab = "Year",ylab = "Poondi",col="blue",main = "Poondi rain level",border = "red")
barplot(rainlevel$cholavaram,names.arg=yr,xlab = "Year",ylab = "Cholavaram",col="red",main = "Cholavaram rain level",border = "blue")
barplot(rainlevel$redhills,names.arg=yr,xlab = "Year",ylab = "Redhills",col="green",main = "Redhills rain level",border = "yellow")
barplot(rainlevel$chembarambakkam,names.arg=yr,xlab = "Year",ylab = "Chembarambakkam",col="yellow",main = "Chembarambakkam rain level",border = "black")

#comparison piechart for water level distrivution in 2004 and 2018
water2004 = c(waterlevel$poondi[1],waterlevel$cholavaram[1],waterlevel$redhills[1],waterlevel$chembarambakkam[1])
water2018 = subset(waterlevel,waterlevel$yr==2018)
water2018 = c(water2018$poondi[1],water2018$cholavaram[1],water2018$redhills[1],water2018$chembarambakkam[1])
par(mfrow = c(1,2))
pie(water2004,labels = c("Poondi","cholavaram","redhills","chembarambakkam"),main="Water level 2004")
pie(water2018,labels = c("Poondi","cholavaram","redhills","chembarambakkam"),main = "Water level 2018")
par(mfrow=c(1,1))

#Plotting Comparison of waterlevel and rainfall in various lakes
barplot(matrix(c(waterlevel$poondi,rainlevel$poondi),nr=2),names.arg = yr,beside = T,col = c("red","blue"),main = "Comparison of waterlevel and rainfall in Poondi")
legend("topright",c("waterlevel","rainfall"),pch = 15, cex = 1.3, col = c("red","blue"))
barplot(matrix(c(waterlevel$cholavaram,rainlevel$cholavaram),nr=2),names.arg = yr,beside = T,col = c("yellow","blue"),main = "Comparison of waterlevel and rainfall in Cholavaram")
legend("topright",c("waterlevel","rainfall"),pch = 15, cex = 1.3, col = c("yellow","blue"))
barplot(matrix(c(waterlevel$redhills,rainlevel$redhills),nr=2),names.arg = yr,beside = T,col = c("hotpink","lightcyan"),main = "Comparison of waterlevel and rainfall in Redhills")
legend("topright",c("waterlevel","rainfall"),pch = 15, cex = 1.3, col = c("hotpink","lightcyan"))
barplot(matrix(c(waterlevel$chembarambakkam,rainlevel$chembarambakkam),nr=2),names.arg = yr,beside = T,col = c("maroon","greenyellow"),main = "Comparison of waterlevel and rainfall in Chembarambakkam")
legend("topright",c("waterlevel","rainfall"),pch = 15, cex = 1.3,col = c("maroon","greenyellow"))

#Finding Correlation Coefficient and Applying Linear Regression
cor(water_level$REDHILLS,rainfall$REDHILLS)
scatter.smooth(water_level$REDHILLS,rainfall$REDHILLS)
lr = lm(water_level$REDHILLS~poly(rainfall$REDHILLS,3))
summary(lr)

cor(rainlevel$cholavaram,rainlevel$redhills)
scatter.smooth(rainlevel$cholavaram,rainlevel$redhills)
rel = lm(rainlevel$cholavaram~rainlevel$redhills,rainlevel)
summary(rel)

#Hypothesis testing using t table analysis
print("H0 : The mean of rainfall level in Cholavaram is 3.5")
mu = 3.5
x = rainlevel$cholavaram
xbar = mean(x)
sd=sqrt(var(x))
n=length(x)
t = abs((xbar-mu)/(sd/sqrt(n)))
t
alpha = 0.05
tv=qt(1-(alpha/2),df=n-1)
tv
if(t<tv)
{
  print("H0 accepted. The mean is 3.5.")
} else
{
  print("H0 rejected. The mean is not 3.5.")
}
