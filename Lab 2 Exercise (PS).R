
#read the data
data <- read.table(file.choose(), header = TRUE)

##explore the dataset
data
head(data)
dim(data)
names(data)
str(data)

##Gender (categorical)
with(data, gender)
with(data, sum(is.na(gender)))
with(data, summary(gender))
data_t <- data[!is.na(data$gender), ]
with(data_t, sum(is.na(gender)))
#frequency table
gender_table <- with(data, table(gender, exclude = NULL)) #with exlude = NULL also NA's visible
gender_table
barplot(gender_table)
#proportion
gender_proportion <- prop.table(gender_table)
gender_proportion
barplot(gender_table/length(data$gender), ylab = "relative frequency")
barplot(gender_proportion, ylab = "relative frequency") 

##Age (continuous)
with(data, age)
with(data, sum(is.na(age)))
plot(data$age)
with(data, mean(age, na.rm = TRUE))
with(data, var(age, na.rm = TRUE))
with(data, sd(age, na.rm = TRUE))
with(data, summary(age))
boxplot(data$age) #outliners!
hist(data$age) #Right skewed distribution!
qqnorm(data$age)
qqline(data$age)

##Phd (Binary)
with(data, phd)
data$phd <- as.factor(data$phd)
with(data, levels(phd))
levels(data$phd) <- c("No","Yes") #this makes it easier to read
table(data$phd)

## Gender x phd plot -> maybe not good because there are more male
table(data$gender, data$phd)
table(data$phd, data$gender)
barplot(table(data$phd, data$gender), beside = TRUE, col = c("red", "blue"), legend = TRUE,
        names.arg = c("Man", "Women"))
title("Having a Phd", line = 1.5)

#gender and full time equivalent years? 
ggplot(data, aes(x = phd, y = totdfte, fill= gender)) +
  geom_boxplot()

#older people less full time equivalent years?
plot(data$totdfte, data$age)
ggplot(data, aes(x = totdfte, y = age)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Full time equivalent years", y = "Age", title = "Line Plot")
#but older people also get there phd in less year??
ggplot(data, aes(x = years, y = age)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Years", y = "Age", title = "Line Plot")

#! after removing outliners different results!
data_t$zscores <- scale(data_t$age)
threshold <- 2.5
data_clean <- data_t[data_t$zscores <=threshold,]
ggplot(data_clean, aes(x = totdfte, y = age)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Full time equivalent years", y = "Age", title = "Plot without age outliners")
ggplot(data_clean, aes(x = years, y = age)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Years", y = "Age", title = "Plot without age outliners")

#how many PhD for each faculty?
data$faculty
table(data$faculty)
table(data$faculty, data$phd)
ggplot(data, aes(x = faculty, fill = phd)) +
  geom_bar(position = "dodge") +
  labs(x = "Faculty", y = "Frequency", title = "Bar Plot of PhD by Faculty") +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

#which gender in which faculty?
ggplot(data, aes(x = faculty, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(x = "Faculty", y = "Frequency", title = "Bar Plot of Gender by Faculty") +
  scale_fill_manual(values = c("M" = "blue", "W" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

#Phd holders by gender and faculty
phd_data <- data[data$phd== "Yes", ] # Filter the data for PhD holders
ggplot(phd_data, aes(x = faculty, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(x = "Faculty", y = "Number of PhD Holders", title = "Bar Plot of PhD Holders by Faculty and Gender") +
  scale_fill_manual(values = c("M" = "blue", "W" = "red")) +
  theme_minimal()

