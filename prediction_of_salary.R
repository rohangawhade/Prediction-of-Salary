# Importing the csv file
dataset = read.csv('Predicting_Salaries.csv')

# Splitting of dataset into training and test set
library(caTools)
set.seed(90909)
split = sample.split(dataset$AnnualSalary, SplitRatio = 3/4)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

# Fitting simple linear regression to training_set
linearR = lm(AnnualSalary~YearsOfExperience, training_set)

sal_pred = predict(linearR, testing_set)
sal_pred
df = data.frame(testing_set$YearsOfExperience, testing_set$AnnualSalary, sal_pred)
df

# Visualize training set results
library(ggplot.multistats)
library(ggplot2)
png("D:\\R adv_programs\\Prediction of Salary\\train.jpg")
ggplot()+
  geom_point(aes(x=training_set$YearsOfExperience, y=training_set$AnnualSalary), colour = 'red')+
  geom_line(aes(x=training_set$YearsOfExperience, y=predict(linearR, newdata=training_set)), colour = 'navy')+
  ggtitle('Annual Salaries of Data Scientists vs Experience in Years(Training set)')+
  xlab('Years of Experience')+
  ylab('Annual Salary')+
  scale_x_continuous(limits = c(0,12))+
  scale_y_continuous(limits = c(0, 150000))
dev.off()


# Visualising the Test set results
png("D:\\R adv_programs\\Prediction of Salary\\test.jpg")
ggplot() +
  geom_point(aes(x=testing_set$YearsOfExperience, y=testing_set$AnnualSalary), colour = 'red') +
  geom_line (aes( x= training_set$YearsOfExperience, y=predict(linearR, newdata= training_set)),
             colour = 'navy') +
  ggtitle ('Annual Salaries of Data Scientists vs Experience in Years (Test Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary')
dev.off()