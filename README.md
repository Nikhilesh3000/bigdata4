#4Implement Decision tree classification techniques 

Code-
library("party")
print(head(readingSkills))
str(iris)
iris_ctree <- ctree(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data=iris)
print (iris_ctree)
plot(iris_ctree)


#5 Implement SVM classification techniques
Code-

dataset = read.csv(' E:/NIKHILESH/social.csv')

dataset = dataset[3:5]
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
install.packages('caTools')  # Run only once
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])


install.packages('e1071')  # Run only once
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')


y_pred = predict(classifier, newdata = test_set[-3])


cm = table(test_set[, 3], y_pred)
print("Confusion Matrix:")
print(cm)


install.packages("ElemStatLearn")  # Run only once
library(ElemStatLearn)


plot_svm <- function(set, title) {
  X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
  X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
  grid_set = expand.grid(X1, X2)
  colnames(grid_set) = c('Age', 'EstimatedSalary')
  y_grid = predict(classifier, newdata = grid_set)
  plot(set[, -3],
       main = title,
       xlab = 'Age', ylab = 'Estimated Salary',
       xlim = range(X1), ylim = range(X2))
  contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
  points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
  points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
}
# Plot training and test results
plot_svm(training_set, 'SVM Classification (Training set)')



#6 Linear regression practical 
code-

college <- read.csv("https://raw.githubusercontent.com/ropensci/datapack/main/inst/extdata/pkg-example/binary.csv")
head(college)
nrow(college)


install.packages("caTools")  # Run only once
library(caTools)


set.seed(123)
split <- sample.split(college$admit, SplitRatio = 0.75)
training_reg <- subset(college, split == TRUE)
test_reg <- subset(college, split == FALSE)


fit_logistic_model <- glm(admit ~ ., data = training_reg, family = "binomial")


coef(fit_logistic_model)["gre"]
coef(fit_logistic_model)["gpa"]
coef(fit_logistic_model)["rank"]


predict_reg <- predict(fit_logistic_model, newdata = test_reg, type = "response")


cdplot(as.factor(admit) ~ gpa, data = college)
cdplot(as.factor(admit) ~ gre, data = college)
cdplot(as.factor(admit) ~ rank, data = college)

predict_binary <- ifelse(predict_reg > 0.5, 1, 0)
table(Actual = test_reg$admit, Predicted = predict_binary)
output


#7 Code-
Explain Multiple regression in detail.
college <- read.csv("https://raw.githubusercontent.com/csquared/udacity-dlnd/master/nn/binary.csv")
head(college)
nrow(college)

install.packages("caTools")  # Only the first time
library(caTools)

set.seed(123)
split <- sample.split(college$admit, SplitRatio = 0.75)
training_reg <- subset(college, split == TRUE)
test_reg <- subset(college, split == FALSE)

fit_MRegressor_model <- glm(formula = admit ~ gre + gpa + rank, data = training_reg, family = binomial)

predict_reg <- predict(fit_MRegressor_model, newdata = test_reg, type = "response")
head(predict_reg)
predict_class <- ifelse(predict_reg > 0.5, 1, 0)

cdplot(as.factor(admit) ~ gpa, data = college)
cdplot(as.factor(admit) ~ gre, data = college)
cdplot(as.factor(admit) ~ rank, data = college)

table(Actual = test_reg$admit, Predicted = predict_class)


#8 CLASSIFICATION MODEL a. Install relevant package for classification. b. Choose classifier for classification problem. c. Evaluate the performance of classifier. 
Navebyse
code:
data(iris)
str(iris)
install packages("e1071")
install packages("caTools")
install packages("caret")
library(e1071)
library(caTools)
library(caret)
split <- sample.split(iris,SplitRatio=0.7)
train_c1 <-subset(iris,split=="TRUE")
test_c1 <- subset(iris,split == "FALSE")
train_scale <- scale(train_c1[, 1:4])
test_scale <- scale(test_c1[,1:4])

set.seed(120)
classifier_c1 <- naiveBayes(Species ~ ., data = train_c1)
classifier_c1

y_pred <- predict(classifier_c1, newdata= test_c1)
cm <- table(test_c1$Species, y_pred)
cm
confusionMatrix(cm) data(iris)
str(iris)
install packages("e1071")
install packages("caTools")
install packages("caret")
library(e1071)
library(caTools)
library(caret)
split <- sample.split(iris,SplitRatio=0.7)
train_c1 <-subset(iris,split=="TRUE")
test_c1 <- subset(iris,split == "FALSE")
train_scale <- scale(train_c1[, 1:4])
test_scale <- scale(test_c1[,1:4])

set.seed(120)
classifier_c1 <- naiveBayes(Species ~ ., data = train_c1)
classifier_c1


#9 CLUSTERING MODEL a. Clustering algorithms for unsupervised classification. 
b. Plot the cluster data using R visualizations. 

install.packages("plyr")
install.packages("ggplot2")
install.packages("cluster")
install.packages("lattice")
install.packages("grid")
install.packages("gridExtra")
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(grid)
library(gridExtra)
grade_input=as.data.frame(read.csv("E:/Rajdeep/bigdata pract/dataset/grades_km_input.csv"))
kmdata_orig=as.matrix(grade_input[, c ("Student","English","Math","Science")])
kmdata=kmdata_orig[,2:4]
kmdata[1:10,]
wss=numeric(15)
for(k in 1:15)wss[k]=sum(kmeans(kmdata,centers=k,nstart=25)$withinss)
plot(1:15,wss,type="b",xlab="Number of Clusters",ylab="Within sum of square")
km = kmeans(kmdata,3,nstart=25)
km
c( wss[3] , sum(km$withinss))
df=as.data.frame(kmdata_orig[,2:4])
df$cluster=factor(km$cluster)
centers=as.data.frame(km$centers)
g1=ggplot(data=df, aes(x=English, y=Math, color=cluster )) +
geom_point() + theme(legend.position="right") +
geom_point(data=centers,aes(x=English,y=Math, color=as.factor(c(1,2,3))),size=10, alpha=.3, show.legend =FALSE)
g2=ggplot(data=df, aes(x=English, y=Science, color=cluster )) +
geom_point () +geom_point(data=centers,aes(x=English,y=Science, color=as.factor(c(1,2,3))),size=10, alpha=.3, show.legend=FALSE)
g3 = ggplot(data=df, aes(x=Math, y=Science, color=cluster )) +
geom_point () + geom_point(data=centers,aes(x=Math,y=Science, color=as.factor(c(1,2,3))),size=10, alpha=.3, show.legend=FALSE)
tmp=ggplot_gtable(ggplot_build(g1))
grid.arrange(arrangeGrob(g1 + theme(legend.position="none"),g2 + theme(legend.position="none"),g3 + theme(legend.position="none"),top ="High School Student Cluster Analysis" ,ncol=1))



9a. Aprori Practical
code-
library(arules)
library(arulesViz)
library(RColorBrewer)

data(Groceries)
Groceries

summary(Groceries)
class(Groceries)

rules = apriori(Groceries, parameter = list(supp = 0.02, conf = 0.2))
summary (rules)

inspect(rules[1:10])

arules::itemFrequencyPlot(Groceries, topN = 20,
col = brewer.pal(8, 'Pastel2'),
main = 'Relative Item Frequency Plot',
type = "relative",
ylab = "Item Frequency (Relative)")

itemsets = apriori(Groceries, parameter = list(minlen=2, maxlen=2,support=0.02, target="frequent itemsets"))
summary(itemsets)
inspect(itemsets)
itemsets_3 = apriori(Groceries, parameter = list(minlen=3, maxlen=3,support=0.02, target="frequent itemsets"))
summary(itemsets_3)
inspect(itemsets_3)
