
mtcars %>% 
  arrange(cyl) %>% 
  bind_rows(
    data.frame(cyl = rep(9, 10), mpg = rnorm(10, 26.7, 4.51))
  ) %>% 
  group_by(cyl) %>% 
  summarise(mean = mean(mpg)
            ,sd = sd(mpg)
            ,var = var(mpg)
            ,sderr = sd(mpg)/sqrt(n()))

lm(mpg~cyl, mtcars %>% 
     bind_rows(
       data.frame(cyl = rep(9, 10), mpg = rnorm(10, 0, 6))
     ) %>%  
  mutate(cyl = as.factor(cyl))) %>% 
  summary()



##linear model w/ poor perfromance===========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set a random seed for reproducibility
set.seed(42)

# Number of data points
num_samples <- 100

# Generating random X values between 0 and 10
X <- runif(num_samples, min = 0, max = 10)

# Generating the corresponding Y values with a weak linear relationship (Y = 2*X + 3 + noise)
Y <- rnorm(num_samples, mean = 1, sd = 0)*(2 * X + 3) + rnorm(num_samples, mean = 0, sd = 15)

plot(X, Y)

# Create a data frame to store the data
data <- data.frame(X = X, Y = Y)

# Display the first few rows of the data
head(data)

lm_model <- lm(Y ~ X, data = data)

# Print the model summary
summary(lm_model)




##logisitic model w/ poor perfromance===========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load necessary library
library(pROC)
library(tidyverse)

# Set a random seed for reproducibility
set.seed(42)

# Number of data points
num_samples <- 100

# Generating random X values between -3 and 3
X <- runif(num_samples, min = -3, max = 3)

# Generating the corresponding Y values (factor variable with severe class imbalance)
Y <- rep(0, num_samples)
Y[1:5] <- 1  # Introduce 5 positive instances (minority class)
Y <- as.factor(Y)
Y = ifelse(runif(num_samples) < .1, 0, 1) %>% as.factor()


# Generating the corresponding Z values (factor variable with severe class imbalance)
Z <- rep(0, num_samples)
Z[1:5] <- 1  # Introduce 5 positive instances (minority class)
Z <- as.factor(Z)
Z = ifelse(runif(num_samples) < .6, 0, 1) %>% as.factor()

# Create a data frame to store the data
data <- data.frame(X = X, Y = Y, Z = Z)

# Fit the logistic regression model
logit_model <- glm(Y ~ X + Z, data = data, family = binomial)
logit_model <- glm(Y ~  Z, data = data, family = binomial)
summary(logit_model)

# Predict probabilities using the model
probs <- predict(logit_model, type = "response")

# Calculate the AUC-ROC
roc_auc <- roc(data$Y, probs)
roc_auc <- auc(roc_auc)

# Calculate accuracy
predicted_classes <- ifelse(probs >= 0.5, 1, 0)
accuracy <- sum(predicted_classes == data$Y) / num_samples

# Print the AUC-ROC and accuracy values
print(paste("AUC-ROC:", roc_auc))
print(paste("Accuracy:", accuracy))





