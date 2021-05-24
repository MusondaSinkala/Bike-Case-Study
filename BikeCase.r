library(caret)
library(cowplot)
library(ggplot2)
library(plyr)
library(rpart)
library(rpart.plot)
library(sqldf)
library(tidyverse)
library(tree)

rm(list = ls())

set.seed(2021)

######### Read in the data #########

setwd("C:/Users/muson/OneDrive/Documents/Applications/Interviews/Business Science Corporation/Case Study")

data <- read.table(file = "Bike_Buyer_Data_edited.txt", 
                          sep = ",", header = TRUE)
head(data)
str(data)
summary(data)

######### Clean the data #########

sqldf("SELECT * FROM data WHERE X != ''") # searching for non-empty "X" values
data[which(data$ID == 23963), ]           # deriving the index of entry with non-empty "X" value; index = 9
# The lines below shift all of the data one line to the left for the 9th entry in the data set, thus making the "X" variable unnecesary.
data[9, ]
data[9, 2]  <- data[9, 3]; data[9, 3] <- data[9, 4]; data[9, 4] <- data[9, 5]; data[9, 5] <- data[9, 6]
data[9, 6]  <- data[9, 7]; data[9, 7] <- data[9, 8]; data[9, 8] <- data[9, 9]; data[9, 9] <- data[9, 10]
data[9, 10] <- data[9, 11]; data[9, 11] <- data[9, 12]; data[9, 12] <- data[9, 13]; data[9, 13] <- data[9, 14]
data[9, 14] <- ""; data[9, ]

# The lines below check which records have null/improbable values and replaces them with the most appearing values for the 
# respective variables
sqldf("SELECT * FROM data WHERE Gender == 'NULL'")
data[which(data$Gender == 'NULL'), 3]     <- 'Male'
sqldf("SELECT * FROM data WHERE Education == 'NULL'")
data[which(data$Education == 'NULL'), 6]  <- 'Bachelors'
sqldf("SELECT * FROM data WHERE Cars == -1")
data[which(data$Cars == -1), 9]           <- '1'
sqldf("SELECT * FROM data WHERE Region == 'NULL'")
data[which(data$Region == 'NULL'), 11]    <- 'North America'


# Variables are henceforth either treated as integers or factors.
data$Income           <- as.integer(data$Income)
data$Children         <- as.integer(data$Children)
data$Cars             <- as.integer(data$Cars)
data$Age              <- as.integer(data$Age)

data$Marital.Status   <- as.factor(data$Marital.Status)
data$Gender           <- as.factor(data$Gender)
data$Education        <- as.factor(data$Education)
data$Occupation       <- as.factor(data$Occupation)
data$Home.Owner       <- as.factor(data$Home.Owner)
data$Commute.Distance <- as.factor(data$Commute.Distance)
data$Region           <- as.factor(data$Region)
data$Purchased.Bike   <- as.factor(data$Purchased.Bike)

str(data)
summary(data)

data        <- subset(data, select = -14) # The "X" value is excluded from the data set.

head(data)
tail(data)
attach(data)

######### Exploring the data #########

### Continuous variables

# Histograms of Income and age

Income.hist <- ggplot(data, aes(x = Income)) + 
  geom_histogram(aes(y = ..count..), colour = "red", fill = "grey", binwidth = 10000) +
  theme_classic() + 
  labs(title = "Histogram of Income",
       subtitle = "Diagram constructed by Musonda Sinkala",
       caption = "Histogram of income with mean of income given by blue dotted line",
       x = "Income",
       y = "Frequency") + 
  geom_vline(aes(xintercept = mean(Income)),
             color = "blue", linetype = "dashed", size = 1)
Income.hist

Age.hist <- ggplot(data, aes(x = Age)) + 
  geom_histogram(aes(y = ..count..), colour = "red", fill = "grey", binwidth = 1) +
  theme_classic() + 
  labs(title = "Histogram of Age",
       subtitle = "Diagram constructed by Musonda Sinkala",
       caption = "Histogram of age with mean of age given by blue dotted line",
       x = "Age",
       y = "Frequency") + 
  geom_vline(aes(xintercept = mean(Age)),
             color = "blue", linetype = "dashed", size = 1)
Age.hist

### Discrete variables

summary(data)

### Joint exploratory data analysis

# The tables below give the proportions of the various variables that purchased motorcycles

round(prop.table(table(data$Marital.Status, data$Purchased.Bike), 1) * 100, 2)
round(prop.table(table(data$Gender, data$Purchased.Bike), 1) * 100, 2)
round(prop.table(table(data$Children, data$Purchased.Bike), 1) * 100, 2)
round(prop.table(table(data$Education, data$Purchased.Bike), 1) * 100, 2)
round(prop.table(table(data$Occupation, data$Purchased.Bike), 1) * 100, 2)
round(prop.table(table(data$Home.Owner, data$Purchased.Bike), 1) * 100, 2)
round(prop.table(table(data$Cars, data$Purchased.Bike), 1) * 100, 2)
round(prop.table(table(data$Commute.Distance, data$Purchased.Bike), 1) * 100, 2)
round(prop.table(table(data$Region, data$Purchased.Bike), 1) * 100, 2)

# Histograms of age and income coded for the groups of subjects who bought motorcycles

mu.age <- ddply(data, "Purchased.Bike", summarise, grp.mean = mean(Age))
Age_bike.hist <- ggplot(data, aes(x = Age, fill = Purchased.Bike, colour = Purchased.Bike)) + 
  geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
  theme_classic() + 
  labs(title = "Histogram of Age grouped by Purchased bike",
       subtitle = "Diagram constructed by Musonda Sinkala",
       caption = "Histogram of age grouped by whether subjects purchased a bike or not.
       The mean age of the two groups is given by dotted lines.",
       x = "Age",
       y = "Frequency") + 
  geom_vline(data = mu.age, aes(xintercept = grp.mean, color = Purchased.Bike),
             linetype = "dashed", linetype = "dashed", size = 1)
Age_bike.hist

mu.inc <- ddply(data, "Purchased.Bike", summarise, grp.mean = mean(Income))
Income_bike.hist <- ggplot(data, aes(x = Income, fill = Purchased.Bike, colour = Purchased.Bike)) + 
  geom_histogram(binwidth = 10000, alpha = 0.5, position = "identity") +
  theme_classic() + 
  labs(title = "Histogram of Income grouped by Purchased bike",
       subtitle = "Diagram constructed by Musonda Sinkala",
       caption = "Histogram of income grouped by whether subjects purchased a bike or not.
       The mean income of the two groups is given by dotted lines.",
       x = "Income",
       y = "Frequency") + 
  geom_vline(data = mu.inc, aes(xintercept = grp.mean, color = Purchased.Bike),
             linetype = "dashed", linetype = "dashed", size = 1)
Income_bike.hist

### Creating a numeric-coded data set

numeric.data <- as.data.frame(matrix(nrow = nrow(data),
                                     ncol = ncol(data)))

for (i in 1:ncol(data))
{
  numeric.data[, i] <- as.numeric(data[, i])
}

str(numeric.data)
cor(numeric.data[, -1], numeric.data[, 13]) # Correlation between Purchased.Bike and the other variables.

######### Splitting the data #########

# Splitting the data into a 75%:25% proportion with 75% used for training the models and the remaining 25% used for testing the models.
ind    <- sample(1:nrow(data), 0.75 * nrow(data))
train  <- data[ind, ]
test   <- data[-ind, ]

######### Model building #########

### Classification tree using rpart package

# building the classification tree
tree_bike <- rpart(formula   = Purchased.Bike ~ .,
                   data      = train[, -1],
                   method    = 'class',
                   minsplit  = 24, # this number was selected based on the problem. Other minsplit values were giving really large trees
                   cp        = -1) # in an attempt to build as large a tree as possible given the other constraints
plot(tree_bike)

tree_bike$cptable
plotcp(tree_bike)
cp <- tree_bike$cptable[6, "CP"] # the training cross-validation error doesn't differ much for the tree with the lowest cp and the selected
                                 # thus the selected cp was chosen as it gives low x-validation error whilst retaining model simplicity

pruned.tree <- prune(tree_bike, cp)
plot(pruned.tree)
text(tree_bike, pretty = 0, cex = 0.9)

prp(x = pruned.tree,
    space = 2, 
    split.cex = 1, 
    nn.border.col = 0,
    yesno = 2, 
    xflip = FALSE,
    varlen = 5,
    faclen = 15,
    roundint = TRUE,
    shadow.col = 'grey')

# Making predictions
yhat.tree  <- predict(pruned.tree, test[, -c(1, 13)], type = 'class') # predictions with decision tree
(cmat      <- table(yhat.tree, test$Purchased.Bike))                  # confusion matrix
sum(diag(cmat))/nrow(test) * 100                                      # classification accuracy
1 - sum(diag(cmat))/nrow(test)                                        # misclassification rate

# variable importance plot

# the lines below collect and process the variable importance data
tree.varImportance <- data.frame(imp = pruned.tree$variable.importance)
df2 <- tree.varImportance %>% 
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot(df2) +                                 # plot
  geom_col(aes(x = variable, y = imp),
           col = "black", show.legend = F, fill = c(1, 2, 3, 4, 5, 6, 7, 8, 'red')) +
  coord_flip() +
  scale_fill_grey() +
  theme_classic() + 
  labs(title = "Feauture importance plot for Decision Tree",
       subtitle = "Diagram constructed by Musonda Sinkala",
       caption = "Plot showing the relative importance of each of the variables used
       in building the Decision Tree model.",
       x = "Variable",
       y = "Importance")


### logistic regression

glm_bike <- glm(formula = Purchased.Bike ~., 
                family  = binomial(link = 'logit'),
                data    = train[, -1])
summary(glm_bike)
stepAIC(object = glm_bike, scope = list(upper = ~., lower = ~1), direction = "backward", trace = TRUE)

glm_bike2 <- glm(formula = Purchased.Bike ~ Marital.Status + Income + Children + Education + 
                   Home.Owner + Cars + Commute.Distance + Region, 
                 family  = binomial(link = 'logit'),
                 data    = train[, -1])
summary(glm_bike2)
anova(glm_bike, glm_bike2)

glm_bike3 <- glm(formula = Purchased.Bike ~ Marital.Status + Income + Children + Education + 
                   Home.Owner + Cars + Commute.Distance + Region + Marital.Status:Children, 
                 family  = binomial(link = 'logit'),
                 data    = train[, -1])
summary(glm_bike3)

glm_bike4 <- glm(formula = Purchased.Bike ~ Marital.Status + Income + Children + Education + 
                   Home.Owner + Cars + Commute.Distance + Region + Marital.Status:Children +
                   Cars:Commute.Distance + Cars:Region + Region:Commute.Distance,
                 family  = binomial(link = 'logit'),
                 data    = train[, -1])
summary(glm_bike4)
anova(glm_bike3, glm_bike4)

varImp(glm_bike4)

# Plot

predicted.data      <- data.frame(prob.of.buying.bike = glm_bike4$fitted.values, 
                                  Purchased.Bike = train$Purchased.Bike)
predicted.data      <- predicted.data[order(predicted.data$prob.of.buying.bike, decreasing = FALSE), ]
predicted.data$rank <- 1:nrow(predicted.data)
ggplot(data = predicted.data, 
       aes(x = rank, y = prob.of.buying.bike)) + 
  geom_point(aes(color = Purchased.Bike), alpha = 1, shape = 4, stroke = 2) + 
  theme_classic() +
  labs(title = "Graph of predicted probabilities of purchasing motor cycle 
       coded for purchase status",
       subtitle = "Diagram constructed by Musonda Sinkala",
       caption = "Plot showing the predicted probability of select subjects purchasing a motor cycle 
       coded for whether the motor cycle was purchased or not.",
       x = "Index",
       y = "Predicted probability of purchasing a bike")
# most patients that purchased a bike are predicted to have a high probability of purchasing one. The unclear sections show that the model
# is not as strong as one would hope it would be.

# making prediction with logistic regression

glm.predict        <- predict(object  = glm_bike4, 
                              newdata = test[, -c(1, 13)], 
                              type = 'response')
glm.classification <- as.factor(ifelse(glm.predict > 0.5, "Yes", "No"))
confusionMatrix(glm.classification,  test$Purchased.Bike)

