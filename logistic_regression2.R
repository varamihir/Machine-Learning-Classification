# Logistic Regression
# Lets find out the coorelation between age and the salary to find out who is going to buy SUV?

# Importing data set
dataset <- read.csv("Social_Network_Ads.csv")
head(dataset)
dataset <- dataset[,3:5]
# Spilitting the dataset into training and testing datasets
library(caTools)
set.seed(1234)
spilit <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, spilit== TRUE)
test_set <- subset(dataset, spilit == FALSE)
View(training_set)

                   
# Since this data is classification better to feature scaling
training_set[,1:2]<- scale(training_set[,1:2])
test_set[,1:2]<- scale(test_set[,1:2])
View(training_set)
View(test_set)
# lets build a Logistic Regression Model to training set
library(ElemStatLearn)
classifier <- glm(formula = Purchased ~.,family = binomial, data= training_set)

# predict test set results using logistic classifier
prob_pred <- predict(classifier, type = 'response', newdata = test_set[-3])
prob_pred
# Creating a vector of predicted results in 0 and 1 and it is easy to interpret the results
y_pred <- ifelse(prob_pred > 0.5 ,1,0 )
y_pred
# Lets build a confusion Matrix
cm <- table(test_set[,3],y_pred )
cm
# Visualizing the training set results
set <- training_set
X1 <- seq(min(set[,1])-1 , max(set[,1]) +1, by = 0.01)
X2 <- seq(min(set[,2])-1, max(set[,2]) +1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set)<- c('Age', 'EstimatedSalary')
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set >0.5, 1,0)

plot(set[,-3],
main = 'Logistic Regression (Training set)',
xlab = 'Age', ylab = 'Estimates Salary',
xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1),length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid ==1, 'springgreen3', "tomato"))
points(set, pch = 21, bg = ifelse(set[,3]==1, 'green','red3'))
# Visualizing the test set results
set <- test_set
X1 <- seq(min(set[,1])-1 , max(set[,1]) +1, by = 0.01)
X2 <- seq(min(set[,2])-1, max(set[,2]) +1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set)<- c('Age', 'EstimatedSalary')
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set >0.5, 1,0)

plot(set[,-3],
     main = 'Logistic Regression (test set)',
     xlab = 'Age', ylab = 'Estimates Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1),length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid ==1, 'springgreen3', "tomato"))
points(set, pch = 21, bg = ifelse(set[,3]==1, 'green','red3'))





