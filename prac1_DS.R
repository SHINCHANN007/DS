Student <- read.csv(file.choose(),header = TRUE,sep = ',')

head(Student)

str(Student)

summary(Student)

dim(Student)

High_marks <- subset(Student,Tmarks>600)
print(High_marks)

Filter_data = subset(Student,Tmarks > 600 & class == "SYCS")
result <- Filter_data[,1:2]
print(result)


sorted_data <- Student[order(Student$Tmarks),]
print(sorted_data)

na <- Student[is.na(Student$Tmarks),]
print(na)