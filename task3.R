# Load and fit model from task 2
# The mccase.txt is in Fixed Width Format
columns <- c(agarald = 2L, kon = 1L, zon = 1L, mcklass = 1L, fordald = 2L, bonuskl = 1L, duration = 8L, antskad = 4L, skadkost = 8L)
column.classes <- c("integer", rep("factor", 3), "integer", "factor", "numeric", rep("integer", 2))
con <- url("http://www2.math.su.se/~esbj/GLMbook/mccase.txt")
# Change "con" below to file path if you do not want to grab file from web
mccase <- read.fwf(con,
    widths = columns, header = FALSE,
    col.names = names(columns),
    colClasses = column.classes
)
# Ignore warning message

# Import packages
library(dplyr)
library(ggplot2)
library(tidyr)


# Fit a frequency-severity model
fit_sev_freq_model <- function(data, freq_formula, sev_formula) {
    # Fit a Frequency GLM
    freq_model <- glm(freq_formula,
        data = data, family = quasipoisson(), offset = log(duration)
    )

    # Fit a Severity GLM
    sev_model <- glm(sev_formula,
        data = data %>% filter(skadkost > 0), family = gaussian(link = "identity"), offset = log(antskad)
    )

    # Create Summary table
    # Join summary estimate and standard error into a single table
    freq_summary <- data.frame(summary(freq_model)$coefficients)[c("Estimate", "Std..Error")] %>%
        rename(freq_estimate = Estimate, freq_std_error = Std..Error)

    sev_summary <- data.frame(summary(sev_model)$coefficients)[c("Estimate", "Std..Error")] %>%
        rename(sev_estimate = Estimate, sev_std_error = Std..Error)

    # Merge the two tables
    summary_table <- merge(freq_summary, sev_summary, by = "row.names", all = TRUE)

    # Fill nans
    summary_table[is.na(summary_table)] <- 0

    # Calculate the relative factors and 95% CI
    phi <- qnorm(0.975)
    # Relative Frequency Factor
    summary_table$rel_freq <- exp(summary_table$freq_estimate)
    summary_table$rel_freq_lower <- exp(summary_table$freq_estimate - phi * summary_table$freq_std_error)
    summary_table$rel_freq_upper <- exp(summary_table$freq_estimate + phi * summary_table$freq_std_error)

    # Relative Severity Factor
    summary_table$rel_sev <- exp(summary_table$sev_estimate)
    summary_table$rel_sev_lower <- exp(summary_table$sev_estimate - phi * summary_table$sev_std_error)
    summary_table$rel_sev_upper <- exp(summary_table$sev_estimate + phi * summary_table$sev_std_error)


    # Total Relative Factor
    summary_table$rel_pure_premium <- summary_table$rel_freq * summary_table$rel_sev
    joint_sd <- sqrt(summary_table$freq_std_error^2 + summary_table$sev_std_error^2)
    summary_table$rel_pure_premium_lower <- summary_table$rel_pure_premium * exp(-phi * joint_sd)
    summary_table$rel_pure_premium_upper <- summary_table$rel_pure_premium * exp(phi * joint_sd)

    return(list(freq_model = freq_model, sev_model = sev_model, summary_table = summary_table))
}

########### Task 1 fit adjusted model
# New zone defition
# Create grouping variable
mccase$zon_cat <- sapply(as.numeric(mccase$zon), function(x) ifelse(x %in% c(3,5,4,6,7), 3, x))
mccase %>% distinct(mccase$zon_cat)

# Vehicle age
age_lims <- c(0,1,5,9,13)
mccase$vehage_cat <- sapply(as.numeric(mccase$fordald), function(x) sum(x >= age_lims))
mccase %>% distinct(mccase$vehage_cat)

# mcclass
mccase$mcklass_cat <- sapply(mccase$mcklass, function(x) {
    ifelse(x %in% c(1, 2, 7, 3, 4, 5), 1, x)
})
mccase %>% distinct(mccase$mcklass_cat)

# Bonus class
bonus_lims <- c(0, 6)
mccase$bonus_cat <- sapply(mccase$bonuskl, function(x) 
    ifelse(x=="1",1,
    ifelse(x=="2",1,
    ifelse(x=="3",1,
    ifelse(x=="4",1,
    ifelse(x=="5",1,
    ifelse(x=="6",1,
    ifelse(x=="7",2,))))))))
mccase %>% distinct(mccase$bonus_cat)

# Add gender
mccase$kon_cat <- as.factor(mccase$kon)

# Add owner age
age_lims <- c(25, 30, 40, 60)
mccase$age_cat <- sapply(mccase$agarald, function(x) sum(x >= age_lims))

# Add interaction term
mccase$kon_age <- interaction(mccase$kon_cat, mccase$age_cat, sep = ".")
mccase$kon_age_cat <- sapply(mccase$kon_age, function(x) {
    ifelse(x %in% c("M.0"), "1",
        ifelse(x %in% c("M.1"), "2", "0")
    )
})
mccase %>% distinct(mccase$kon_age_cat)

# Add owner age
age_lims <- c(40)
mccase$age_cat <- sapply(mccase$agarald, function(x) sum(x >= age_lims))

# Change data to factors
mccase$zon_cat <- factor(mccase$zon_cat, levels = c("3", "2", "1"))
mccase$mcklass_cat <- factor(mccase$mcklass_cat, levels = c("1", "6"))
mccase$vehage_cat <- factor(mccase$vehage_cat, levels = c("3", "4", "2", "1", "5"))
mccase$bonus_cat <- factor(mccase$bonus_cat, levels = c("1", "2"))
mccase$age_cat <- factor(mccase$age_cat, levels = c("1", "0"))
mccase$kon_age_cat <- factor(mccase$kon_age_cat, c("0", "1", "2"))

mccase <- mccase %>% filter(duration > 0)
mccase %>% head(10)

# Fit the model
freq_formula <- antskad ~ zon_cat + vehage_cat + kon_age_cat + age_cat + mcklass_cat
sev_formula <- log(skadkost) ~ zon_cat + vehage_cat + kon_age_cat + age_cat + mcklass_cat
res <- res <- fit_sev_freq_model(mccase, freq_formula, sev_formula)
df_sub <- res$summary_table[c("Row.names", "rel_pure_premium", "rel_pure_premium_lower", "rel_pure_premium_upper")]
print.data.frame(df_sub, digits = 2)
summary(res$freq_model)


########## Task 2 Relax duration assumption and estimate coefficent
freq_model <- glm(formula <- antskad ~ zon_cat + vehage_cat + bonus_cat + log(duration) + kon_age_cat,
    data = mccase, family = quasipoisson()
)
summary(freq_model)

### Task 3 Evaluate predictive power
# translate mccase to dummy data
x <- mccase %>% select(zon_cat, vehage_cat, mcklass_cat, kon_age_cat,age_cat)
# Create dummy variables
x <- data.frame(model.matrix(~., data = x)[, -1])
predictors <- colnames(x)
x$antskad <- mccase$antskad
x$duration <- mccase$duration
x$skadkost <- mccase$skadkost

# Split data into training and test set
set.seed(12345)
train_index <- sample(1:nrow(x), 0.5 * nrow(x), replace = FALSE)
train_data <- x[train_index, ]
test_data <- x[-train_index, ]

# Fit the model
freq_formula <- as.formula(paste("antskad", "~", paste(predictors, collapse = " + ")))
sev_formula <- as.formula(paste("log(skadkost)", "~", paste(predictors, collapse = " + ")))
model <- fit_sev_freq_model(train_data, freq_formula, sev_formula)
df_sub <- model$summary_table[c("Row.names", "rel_pure_premium", "rel_pure_premium_lower", "rel_pure_premium_upper")]
print.data.frame(df_sub, digits = 3)

# Plot histogram of price
ggplot(data.frame(mu = mu), aes(x = mu)) +
    geom_histogram(binwidth = 100, fill = "blue", color = "black") +
    labs(title = "Histogram of predicted price",
         x = "Price",
         y = "Frequency")

##################### Create Simpler model
cross_val_deviance <- function(formula, data, folds, family, offset_col = NULL, devience_func = NULL) {
    total_deviance <- 0

    # Split data into folds
    fold_idxs <- split(1:nrow(data), sample(1:folds, nrow(data), replace = TRUE))

    for (fold_idx in seq_along(fold_idxs)) {
        # Split data into training and validation sets
        valid_idxs <- fold_idxs[[fold_idx]]
        train_idxs <- setdiff(1:nrow(data), valid_idxs)
        
        train_data <- data[train_idxs, ]
        valid_data <- data[valid_idxs, ]

        # Fit the model on training data
        model <- glm(formula, family = family(), data = train_data, offset = log(offset))

        # Predict on validation data
        predictions <- predict(model, newdata = valid_data, type = "response", offset = log(offset))

        # Compute deviance on validation data
        y <- valid_data[[all.vars(formula)[1]]]
        if (is.null(devience_func)) dev <- sum(family()$dev.resids(y, predictions, rep(1, length(y))))
        else dev <- devience_func(y, predictions)

        # Accumulate deviance
        total_deviance <- total_deviance + dev
    }

    return(total_deviance)
}

backward_selection_cv <- function(x, response, predictors, folds, family,devience_func = NULL) {

    best_formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
    best_deviance <- cross_val_deviance(best_formula, x, folds, family,devience_func = devience_func)
    print(best_deviance)

    repeat {
        improved <- FALSE
        current_predictors <- all.vars(best_formula)[-1] # Exclude the response

        for (term in current_predictors) {
            # Create a new formula without the current term
            new_predictors <- setdiff(current_predictors, term)
            new_formula <- as.formula(paste(response, "~", paste(new_predictors, collapse = " + ")))

            # Evaluate the new formula with CV
            new_deviance <- cross_val_deviance(new_formula, x, folds, family,devience_func = devience_func)
            print(new_deviance)
            # If improvement is found, update the best formula and deviance
            if (new_deviance <= best_deviance) {
                print(new_deviance)
                best_formula <- new_formula
                best_deviance <- new_deviance
                improved <- TRUE
            }
        }

        # Stop if no improvement is found
        if (!improved) break
    }

    # Return the final model
    return(best_formula)
}

# Create formula based on dummies
train_data$offset <- train_data$duration
freq_formula_simple <- backward_selection_cv(train_data, "antskad", predictors, 10, quasipoisson)
freq_formula_simple
freq_formula

# Do the same for severity
train_data$offset <- train_data$antskad
sev_formula_simple <- backward_selection_cv(train_data %>% filter(antskad > 0), "log(skadkost)", predictors, 10, gaussian)
sev_formula_simple
sev_formula
simple_model
model
simple_model <- fit_sev_freq_model(train_data, freq_formula_simple, sev_formula_simple)
df_sub <- simple_model$summary_table[c("Row.names", "rel_pure_premium", "rel_pure_premium_lower", "rel_pure_premium_upper")]
print.data.frame(df_sub, digits = 3)

# Plot histogram of price
ggplot(data.frame(mu = mu_simple), aes(x = mu_simple)) +
    geom_histogram(binwidth = 100, fill = "blue", color = "black") +
    labs(title = "Histogram of predicted price",
         x = "Price",
         y = "Frequency")

############## Evaluate Performance on test data

# Poisson unit devience
poisson_deviance <- function(y, mu) {
    # Handle cases where y is zero
    term <- ifelse(y == 0, 0, y * log(y / mu))
    return(2 * mean(term - (y - mu)))
}

# Gaussian unit devience
gaussian_deviance <- function(y, mu) {
    return(mean((y - mu)^2))
}

### Predict on test set

# Frequency
mu_simple_f <- predict(simple_model$freq_model, newdata = test_data, type = "response", offset = log(duration))
mu_f <- predict(model$freq_model, newdata = test_data, type = "response", offset = log(duration))

# Fix for severity, for prediction across all data we have to do the following
antskad_tmp <- test_data$antskad
test_data$antskad <- 1
mu_simple_s <- exp(predict(simple_model$sev_model, newdata = test_data, type = "response"))
mu_s <- exp(predict(model$sev_model, newdata = test_data, type = "response"))
test_data$antskad <- antskad_tmp

### Predict on train data

# Frequancy
mu_simple_f_train <- predict(simple_model$freq_model, newdata = train_data, type = "response", offset = log(duration))
mu_f_train <- predict(model$freq_model, newdata = train_data, type = "response", offset = log(duration))

# Fix for severity, for prediction across all data we have to do the following
antskad_tmp <- train_data$antskad
train_data$antskad <- 1
mu_simple_s_train <- exp(predict(simple_model$sev_model, newdata = train_data, type = "response"))
mu_s_train <- exp(predict(model$sev_model, newdata = train_data, type = "response"))
train_data$antskad <- antskad_tmp

# Subset severity for when we have claims
train_claims <- train_data %>% filter(antskad > 0)
test_claims <- test_data %>% filter(antskad > 0)
mu_simple_s_claims <- exp(predict(simple_model$sev_model, newdata = train_claims, type = "response"))
mu_s_claims <- exp(predict(model$sev_model, newdata = train_claims, type = "response"))
mu_simple_s_claims_test <- exp(predict(simple_model$sev_model, test_claims, type = "response"))
mu_s_claims_test <- exp(predict(model$sev_model, test_claims, type = "response"))


###### Evaluation ##########

### Frequency
# Test data
dev_freq_simple <- poisson_deviance(test_data$antskad, mu_simple_f)
dev_freq <- poisson_deviance(test_data$antskad, mu_f)
dev_freq_simple
dev_freq 

# Train data
dev_freq_train <- poisson_deviance(train_data$antskad, mu_f_train)
dev_freq_simple_train <- poisson_deviance(train_data$antskad, mu_simple_f_train)
dev_freq_simple_train
dev_freq_train

### Severity models
# Test Data
dev_sev_simple <- gaussian_deviance(test_claims$skadkost, mu_simple_s_claims_test)
dev_sev <- gaussian_deviance(test_claims$skadkost, mu_s_claims_test)
dev_sev_simple
dev_sev

# Train Data
dev_sev_simple_train <- gaussian_deviance(train_claims$skadkost, mu_simple_s_claims)
dev_sev_train <- gaussian_deviance(train_claims$skadkost, mu_s_claims)
dev_sev_simple_train
dev_sev_train

######## Task 4

# Evaluate using Poisson deviance
train_simple_dev <- poisson_deviance(train_data$skadkost, mu_simple_f_train*mu_simple_s_train)
test_simple_dev <- poisson_deviance(test_data$skadkost, mu_simple_f*mu_simple_s)
train_simple_dev
test_simple_dev

# Other model
train_dev <- poisson_deviance(train_data$skadkost, mu_f_train*mu_s_train)
test_dev <- poisson_deviance(test_data$skadkost, mu_f*mu_s)
train_dev
test_dev

# Evaluate using Gaussian deviance
train_simple_dev <- gaussian_deviance(train_data$skadkost, mu_simple_f_train*mu_simple_s_train)
test_simple_dev <- gaussian_deviance(test_data$skadkost, mu_simple_f*mu_simple_s)
train_simple_dev
test_simple_dev

# Other model
train_dev <- gaussian_deviance(train_data$skadkost, mu_f_train*mu_s_train)
test_dev <- gaussian_deviance(test_data$skadkost, mu_f*mu_s)
train_dev
test_dev
