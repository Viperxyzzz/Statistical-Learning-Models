library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)


data <- read_csv('insurance.csv')

data <- data %>%
  mutate(sex = ifelse(sex == 'male', 0, 1),
         smoker = ifelse(smoker == 'yes', 1, 0),
         region = case_when(
           region == 'southwest' ~ 0,
           region == 'southeast' ~ 1,
           region == 'northwest' ~ 2,
           region == 'northeast' ~ 3
         ))

head(data)



# Calculate the mean of the "charges" variable
threshold <- mean(data$charges)

# Create a new column "insurance_claim" based on the mean as the threshold
data <- data %>%
  mutate(insurance_class = ifelse(charges > threshold, 1, 0))


data <- select(data, -charges)

# Display the modified data
head(data)

# Display the count of each class
table(data$insurance_class)


# Assuming 'data' is your dataframe
correlation_matrix <- cor(data)
correlation_matrix


########################################
#LINEAR REGRESSION IN ONE VARIABLE TIME#
########################################

logistic <- glm(insurance_class ~ smoker, data=data, family="binomial")
summary(logistic)

#charges = -1.83067 + 7.44015 * the person smokes
#log(odds) that a person who doesn't smokes pays an above median charge is -1.83067
#log(odds ratio) of the odds that a smoker will have to pay above median charge over the odds that a non smoker has to

#low p-values so our log(odds) and log(odds ratio) are statistically significant

#AIC can be used to compare one model to another

# estimate of the parameter in front of Xi is 7.44015 s positive, it suggests that an increase in X_most_correlated is associated with an increase in the log-odds of the event.

#calculating P(Xi)

smoker_prob <- 1 / (1 + exp(-(logistic$coefficients[1] + logistic$coefficients[2] * 1)))
smoker_prob
non_smoker_prob <- 1 / (1 + exp(-(logistic$coefficients[1] + logistic$coefficients[2] * 0)))
non_smoker_prob


########################################
#MULTIPLE LINEAR REGRESSION TIME       #
########################################

logistic <- glm(insurance_class ~ . - bmi, data=data, family="binomial")
summary(logistic)

#age - 0.072099
#sex - 0.265272
#children - 0.123953
#smoker - 8.394893
#region - 0.180770

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

(ll.null - ll.proposed) / ll.null #calculating R-Squared

1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients) - 1)) # tiny p-value, which means our R-Square is effective

predicted.data <- data.frame(
  probability.of.insurance_class=logistic$fitted.values,
  insurance_class=data$insurance_class
)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.insurance_class, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)


ggplot(data=predicted.data, aes(x=rank, y=probability.of.insurance_class)) +
  geom_point(aes(color=insurance_class), alpha=1,shape=4,stroke=2) +
  xlab("Index") +ylab("Probability of getting an above average charge")
ggsave("above_average_charge.pdf")

# Coefficients from the logistic regression model
intercept <- coef(logistic)[1]
coef_age <- coef(logistic)[2]
coef_sex <- coef(logistic)[3]
coef_children <- coef(logistic)[4]
coef_smoker <- coef(logistic)[5]
coef_region <- coef(logistic)[6]

# Example values for the independent variables
example_values <- data.frame(
  age = 30,
  sex = 1,  # Assuming female (you can use 0 for male)
  children = 2,
  smoker = 1,  # Assuming smoker (you can use 0 for non-smoker)
  region = 2  # Assuming northwest
)

# Calculate the logit
logit_value <- intercept +
  coef_age * example_values$age +
  coef_sex * example_values$sex +
  coef_children * example_values$children +
  coef_smoker * example_values$smoker +
  coef_region * example_values$region

# Calculate p(X) using the logistic function
probability_value <- 1 / (1 + exp(-logit_value))

cat("Logit value:", logit_value, "\n")
cat("Probability of insurance_class = 1:", probability_value, "\n")

    