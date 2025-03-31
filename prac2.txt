
heights <- c(63,63, 66, 67, 68, 69, 70, 70, 71 ,71)

t.test(heights,mu = 66)


marks <- c(50, 49, 52, 44, 45, 48, 46, 45, 49, 45)
t.test(marks,mu = 50)

iq <- c(70, 120, 110, 101, 88, 83, 95, 88, 107, 100)
t.test(iq,mu=100)

data(mtcars)
t.test(mtcars$mpg,mu=20)

drugA = c(10,12,13,11,14)
drugB = c(8,9,12,14,15,10,9)

t.test(drugA,drugB)

before = c(53,28,31,48,50,42)
after = c(58,29,30,55,56,45)

t.test(before, after, paired = TRUE)

library(MASS)
tables <- table(survey$Smoke,survey$Exer)
chisq.test(tables)

data(mtcars)
tablec <- table(mtcars$cyl,mtcars$carb)
chisq.test(tablec)

Data<-matrix(c(30,15,50,10,30,60),nrow = 2,byrow = TRUE)
rownames(Data)<-c('female','male')
colnames(Data)<-c('Archery','Boxing','Cycling')
chisq.test(Data)

handed <-matrix(c(8,10,12,178,21,21),nrow = 2,byrow = TRUE)
rownames(handed)<-c('left','right')
colnames(handed)<-c('parent 0','parent 1','parent 2')
chisq.test(handed)

housetask <-read.csv(file.choose(),header = T)
chisq.test(housetask)