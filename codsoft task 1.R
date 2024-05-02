######### TASK 1 ##############################

data<-read.csv(file.choose())
data
head(data)
tail(data)
data_new<-data.frame(data$Pclass,data$Name,data$Sex,data$Age,data$Survived)
data_new
#d<-na.omit(data_new)
#hist(d$data.Survived)
as.factor(d$data.Survived)
#as.factor(d$data.Sex)

######Create Bar plot#####

gender<-data$Sex

gender<-ifelse(data$Sex=="male",1,0)

barplot(table(gender,data$Survived),xlab="Survival Rate",ylab="Frequency",
        col=c("brown","darkgreen"))

legend("topright",c("Male","Female"),fill=c("brown","darkgreen"))

Pc_class<-data$Pclass

barplot(table(Pc_class,data$Survived),beside = TRUE,xlab = "Survival rate",
        ylab="frequency",col=c("yellow","darkgreen","darkred"))

legend("topright",c("PC_class 1","PC_class 2","PC_class 3"),
       fill=c("yellow","darkgreen","darkred"))

gender_new<-na.omit(gender)
       

####### Replace missing value with mean Age
Age<-data$Age
missing_val<-30.27259036
Age[is.na(Age)]<-missing_val
Age 

Survival<-data$Survived
Gender<-gender
logistic_model<-glm(Survival~Gender+Pc_class+Age,family=binomial(link="logit"))
summary(logistic_model)
fitted_values <- fitted.values(logistic_model, type = "response")
head(fitted_values)
head(Survival)
e=Survival-fitted_values
head(e)
shapiro.test(e)
hist(e)
table(Survival)
table(fitted_values)
tail(Survival)
tail(fitted_values)


