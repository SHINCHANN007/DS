


atoz <- read.csv(file.choose(),sep=",",header = T)
var.test(atoz$A,atoz$B,alternative="two.sided")

time <-read.csv(file.choose(),header = T)
var.test(time$g1,time$g2,alternative="two.sided")

count <-read.csv(file.choose(),header = T)
var.test(count$UK,count$GER,alternative="two.sided")




sat<-read.csv(file.choose(),sep = ",",header = T)
names(sat)
head(sat)
summary(sat)
summary(aov(formula=Satindex~Dept,data=sat))


satin<-read.csv(file.choose(), sep=",", header=T) 
names(satin) 
head(satin) 
summary(satin) 
summary(aov(formula=si~dept*exp, data=satin)) 

poliedu<-read.csv(file.choose(), sep=",", header=T) 
names(poliedu) 
head(poliedu) 
summary(poliedu) 
summary(aov(formula=pf~Education*Gender, data=poliedu)) 

