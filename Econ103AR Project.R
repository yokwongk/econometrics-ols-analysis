rm(list=ls())
setwd("C:/Users/aroon/OneDrive - sjsu.edu/FA25/Econ103A")
library(readxl)
library(car)

Econ103A_data_project <- read_excel("Econ103A data project.xlsx", sheet = "Master")
data <- Econ103A_data_project



# using GDP per capita to reflect the quadratic shape of the data.
data$capitasq <- data$capitarate **2 



# 4.1 Statistics of Each Variables
summary(data$ugenroll)
sd(data$ugenroll)
summary(data$gradenroll)
sd(data$gradenroll)
summary(data$gradcost)
sd(data$gradcost, na.rm=TRUE)
summary(data$undergradcost)
sd(data$undergradcost, na.rm=TRUE)
summary(data$capitarate)
sd(data$capitarate)
summary(data$capitasq)
sd(data$capitasq)
summary(data$unemployment)
sd(data$unemployment)
summary(data$RIR)
sd(data$RIR)

#4.2.1 Unrestricted model (grad Enrollment)
grad.mols = lm(data$gradenroll ~ data$capitasq+ data$capitarate +data$gradcost +data$unemployment+ data$RIR)
summary(grad.mols)
plot(x= data$unemployment, y=data$gradenroll, xlab = "Unemployment Rate", ylab ="Graduate Enrollment")
plot(x= data$capitasq , y=data$gradenroll, xlab = "GDP per Capita^2", ylab ="Graduate Enrollment")
plot(x= data$RIR, y=data$gradenroll, xlab = "RIR", ylab ="Graduate Enrollment")
plot(x= data$capitarate , y=data$gradenroll, xlab = "GDP per Capita Rate", ylab ="Graduate Enrollment")


#4.2.2Unrestriced model (Undergraduate Enrollment)
undergrad.mols = lm(data$ugenroll ~ data$capitasq+data$capitarate+data$undergradcost +data$unemployment+ data$RIR)
summary(undergrad.mols)
plot(x= data$unemployment, y=data$ugenroll, xlab = "Unemployment Rate", ylab ="Undergraduate Enrollment")
plot(x= data$capitasq , y=data$ugenroll, xlab = "GDP per Capita Rate^2", ylab ="Undergraduate Enrollment")
plot(x= data$RIR, y=data$ugenroll, xlab = "Real Interest Rate", ylab ="Undergraduate Enrollment")
plot(x= data$capitarate , y=data$ugenroll, xlab = "GDP per Capita Rate", ylab ="Undergraduate Enrollment")

#Figure 1 and 2
par(mfrow = c(1,2))
plot(x= data$capitarate , y=data$gradenroll, xlab = "GDP per Capita Rate", ylab ="Graduate Enrollment")
plot(x= data$capitarate , y=data$ugenroll, xlab = "GDP per Capita Rate", ylab ="Undergraduate Enrollment")

#4.4Auxillary Regressions 

#4.4.1 GDP per capita auxillary
GDPcap.aux = lm(data$gdpcapitarate ~ data$unemployment+ data$gradcost+ data$undergradcost +data$RIR)
summary(GDPcap.aux)
plot(data$gradcost, data$gdpcapitarate, main = "GDP per Capita vs Grad Cost",
     xlab ="Grad Cost", ylab = "GDP capita")

#4.4.2 Grad Cost auxillary
gradcost.aux = lm(data$gradcost ~ data$gdpcapitarate+data$unemployment+ data$RIR)
summary(gradcost.aux) ##most correlated with RIR; 
par(mfrow = c(1,2))
plot(data$RIR, data$gradcost, main = "Grad Cost vs RIR",
     xlab ="RIR", ylab = "Grad Cost")
plot(data$unemployment, data$gradcost, main = "Grad Cost vs Unemployment",
     xlab ="Unemployment Rate", ylab = "Grad Cost")

#4.4.3Undergrad Cost auxillary
undgradcost.aux = lm(data$undergradcost ~ data$gdpcapitarate+data$unemployment+ data$RIR)
summary(undgradcost.aux) ##most correlated with RIR; 
par(mfrow = c(1,2)) ## two plots next to each other
plot(data$RIR, data$undergradcost, main = "Undergrad Cost vs RIR",
     xlab ="RIR", ylab = "Undergrad Cost")
plot(data$unemployment, data$undergradcost, main = "Undergrad Cost vs Unemployment",
     xlab ="Unemployment", ylab = "Undergrad Cost")


#4.5 Joint significant test 

#4.5.1unemployment and RIR
linearHypothesis(undergrad.mols, "data$unemployment-data$RIR=0")
linearHypothesis(grad.mols , "data$unemployment-data$RIR=0")

#4.5.2 capita and unemployment
linearHypothesis(undergrad.mols, "data$unemployment-data$capitarate=0")
linearHypothesis(grad.mols , "data$unemployment-data$capitarate=0")


