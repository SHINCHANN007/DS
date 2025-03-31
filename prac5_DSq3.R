yearsofservice<-c(11,7,9,5,8,6,10)
income<-c(17,15,13,12,16,14,18)

data<-data.frame(yearsofservice,income)

model<-lm(income~yearsofservice,data=data)
summary(model)

newdata<-data.frame(yearsofservice=10)

predict_income<-predict(model,newdata)
print(paste("income of new data is :",round(predict_income,2)))

plot(yearsofservice,income,main = "YEARS of Service vs Income",
     xlab = "Years of Service",ylab = "Income",pch=16,col="blue")
abline(model,col="green")

boxplot(income,main="BoxPlot of Income",
        ylab="Income(Rs)",col = "lightblue",horizontal = TRUE)

qqnorm(model$residuals,main="Q-Q Plot od Residuals")
qqline(model$residuals,col="red")

plot(fitted(model),residuals(model),main="Residual vs Fitted",xlab = "Fitted",ylab = "Residual",col="purple",pch=16)
abline(h=0,col="blue",lwd=2)