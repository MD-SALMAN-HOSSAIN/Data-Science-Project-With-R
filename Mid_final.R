df <- read.csv("data.csv")
head(df)
tail(df)
summary(df)
names(df)
str(df)
df$gender <- ifelse(df$gender == "male",1,0)
df$class <- ifelse(df$class == "positive",1,0)
df
summary(df$age)
mean(df$age, na.rm =TRUE)
df$age <- as.integer(replace(df$age, is.na(df$age), mean(df$age, na.rm = TRUE)))
df$pressurehight[is.na(df$pressurehight)] <- table(df$pressurehight)[which.max(table(df$pressurehight))]
boxplot(df$age)
which(df$age>100)
which(scale(df$age)<100)
which(colnames(df)=="age")
df <- df[-138, ] 
df <- df[-147, ] 

boxplot(df$impluse,df$pressurehight,df$pressurelow,df$glucose)
which(df$impluse>120)
df <- df[-22, ] 
which(colnames(df)=="impluse")
df <- df[-c(31, 126), ]
summary(df$impluse)
df$impluse[df$impluse == 1111] <- 111


summary(df$pressurehight)
any(df$pressurehight < 0)
which(df$pressurehight< 0)
df$pressurehight[df$pressurehight == -160] <- 160
which(df$pressurehight>160)
df$pressurehight[df$pressurehight >= 160] <- 160

summary(df$pressurelow)
which(df$pressurelow< 40)
df$pressurelow[df$pressurelow < 40] <- 40

summary(df$glucose)
df$glucose[df$glucose > 200] <- mean(df$glucose)
which(df$glucose > 300)
df$glucose <- as.integer(df$glucose)

boxplot(df$age,df$impluse,df$pressurehight,df$pressurelow,df$glucose,main = "Boxplot of Patient Data",xlab = "Variable",
ylab = "Value" )

library(ggplot2)


ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Age Histogram", x = "Age", y = "Frequency")

ggplot(df, aes(x =impluse )) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "impluse Histogram", x = "impluse", y = "Frequency")


ggplot(df, aes(x = pressurehight)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black") +
  labs(title = "Pressure High Histogram", x = "Pressure High", y = "Frequency")

ggplot(df, aes(x = pressurelow)) +
  geom_histogram(binwidth = 10, fill = "purple", color = "black") +
  labs(title = "Pressure Low Histogram", x = "Pressure Low", y = "Frequency")

ggplot(df, aes(x = glucose)) +
  geom_histogram(binwidth = 20, fill = "orange", color = "black") +
  labs(title = "Glucose Histogram", x = "Glucose", y = "Frequency")

ggplot(df, aes(x = class, fill = class)) +
  geom_bar() +
  labs(title = "Class Distribution", x = "Class", y = "Count") +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red"))

