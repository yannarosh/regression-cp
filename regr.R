## Regression Models Course Project


## Analysis Setup

require(ggplot2)

my_data <- mtcars

head(my_data)

str(my_data)

# Transform variables to factors where appropriate

my_data$cyl <- factor(my_data$cyl)
my_data$vs <- factor(my_data$vs)
my_data$am <- factor(my_data$am, labels = c("auto", "man"))
my_data$gear <- factor(my_data$gear, labels = c("3", "4", "5"))
my_data$carb <- factor(my_data$carb)

str(my_data)

## 1. Is an automatic or manual transmission better for MPG?

g <- ggplot(my_data, aes(x = am, y = mpg))
g + geom_boxplot(aes(group = am), fill = "olivedrab1") +
        xlab("transmission type") + 
        ylab("miles/(US) gallon") + 
        ggtitle("Miles/(US) gallon by type of transmission")

# From the boxplot there seems to be a clear difference in mpg between automatic
# and manual transmission. Let's test that more formally with a hypothesis test.

# H_0: There is no significant difference in mpg between auto and man trans.
# H_1: Automatic transmission is associated with lower values of mpg

t.test(mpg ~ am, data = my_data, paired = FALSE, alt = "less")

# The p-value is 0.0007 which means that we reject the null at any reasonable
# significance level, i.e. Manual transmission is better for MPG. 

## 2. Quantify the MPG difference between automatic and manual transmissions.

# Get a first idea about the difference between automatic and manual transmissions

aggregate(my_data[, 1], list(my_data$am), mean)

# Let's try and fit some linear models with MPG as the dependent variable to quantify
# the difference between type of transmission while accounting for the effect of 
# other variables available in the dataset.