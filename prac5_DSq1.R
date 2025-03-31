height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152)
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62)

data<-data.frame(height,weight)

model <- lm(weight~height,data)
summary(model)

new_height<- data.frame(height=140)
predicted_weight<-predict(model,new_height)
print(paste("predicted weight for height 140cm : ",round(predicted_weight,2)))

plot(data$height,data$weight,main = "Height vs Weight",
xlab = "Height(cm)",ylab = "Weight(kgs)",
pch = 16,col = "blue")
abline(model,col = "red",lwd=2 )

qqnorm(model$residuals,main = "Q-Q Plot of Residuals")
qqline(model$residuals,col="red")

plot(fitted(model),residuals(model),main="Residual vs Fitted",xlab = "Fitted Values",ylab = "Residual",pch=16,col="purple")
abline(h=0,col="grey",lwd=2)


