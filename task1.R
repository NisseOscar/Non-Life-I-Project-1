# The mccase.txt is in Fixed Width Format
columns <- c(agarald = 2L, kon = 1L, zon = 1L, mcklass = 1L, fordald = 2L, bonuskl = 1L, duration = 8L, antskad = 4L, skadkost = 8L)
column.classes <- c("integer", rep("factor", 3),"integer","factor", "numeric", rep("integer", 2))
con <- url("http://www2.math.su.se/~esbj/GLMbook/mccase.txt")
# Change "con" below to file path if you do not want to grab file from web
mccase <- read.fwf(con, widths = columns, header = FALSE,
                   col.names = names(columns),
                   colClasses = column.classes)
# Ignore warning message

# Import packages
library(dplyr)
library(ggplot2)
library(tidyr)


data.frame(mccase) %>% head(10)

### Create cell variables ########### 
# Note zon and MCclass are already defined

# Vehicle age
age_lims = c(0,2,5)
mccase$vehage_cat <- sapply(mccase$fordald, function(x) sum(x >= age_lims))
distinct(mccase, bonus_cat)

# Bonus class
bonus_lims = c(0,3,5)
mccase$bonus_cat <- sapply(as.numeric(as.character(mccase$bonuskl)), function(x) sum(integer(x) >= bonus_lims))


# Aggregate into tariff cells
mccase_agg <- mccase %>%
    group_by(zon,mcklass,vehage_cat,bonus_cat) %>%
    summarise(duration = sum(duration),
                antskad = sum(antskad),
                skadkost = sum(skadkost)) %>%
    filter(duration > 0)

# Change to factors
mccase_agg$zon <- factor(mccase_agg$zon, levels = c("4","3","2","1","5","6","7"))
mccase_agg$mcklass <- factor(mccase_agg$mcklass, levels = c("3","2","1","4","5","6","7"))
mccase_agg$vehage_cat <- factor(mccase_agg$vehage_cat, levels = c("3","2","1"))
mccase_agg$bonus_cat <- factor(mccase_agg$bonus_cat, levels = c("3","2","1"))

# Calculate baseline factor
base_price = 183

########### Frequency Model #####################

# Create histogram over number of claims
ggplot(mccase_agg, aes(x = antskad)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = "Histogram of number of claims",
         x = "Number of claims",
         y = "Frequency")

# Fit a Frequency GLM
freq_model <- glm(antskad ~ zon + mcklass + vehage_cat + bonus_cat, 
    data = mccase_agg, family = poisson(),offset = log(duration))


# Test goodness of fit
chisq <- sum(residuals(freq_model, type = "pearson")^2)
df <- nrow(mccase_agg) - length(coefficients(freq_model))
pchisq(chisq, df, lower.tail = FALSE)

# Check for overdispersion
dispersion_test <- sum(residuals(freq_model, type = "pearson")^2)/df
dispersion_test


########### Severity Model #####################

# Create histogram over claim costs
ggplot(mccase_agg %>% filter(skadkost>0), aes(x = skadkost/antskad)) +
    geom_histogram(bins=20, fill = "blue", color = "black") +
    labs(title = "Histogram of average claim costs",
         x = "Claim costs",
         y = "Frequency")

# Test Gamma, Lognormal and Inverse Gaussian
sev_model_gamma <- glm(skadkost ~ zon + mcklass + vehage_cat + bonus_cat,
    data = mccase_agg %>% filter(skadkost>0), family = Gamma(link = "log"),offset = log(antskad))
sev_model_lognormal <- glm(log(skadkost) ~ zon + mcklass + vehage_cat + bonus_cat,
    data = mccase_agg %>% filter(skadkost>0), family = gaussian(link = "identity"),offset = log(antskad))

# Compare AIC
AIC(sev_model_gamma, sev_model_lognormal)

# Likelihood ratio test
anova(sev_model_lognormal, sev_model_gamma, test = "Chisq")

# Fit a Severity GLM
sev_model <- glm(log(skadkost) ~ zon + mcklass + vehage_cat + bonus_cat,
    data = mccase_agg %>% filter(skadkost>0), family = gaussian(link = "identity"),offset = log(antskad))

summary(sev_model)

# Join summary estimate and standard error into a single table
freq_summary <- data.frame(summary(freq_model)$coefficients)[c("Estimate", "Std..Error")] %>%
    rename(freq_estimate = Estimate, freq_std_error=Std..Error)

sev_summary <- data.frame(summary(sev_model)$coefficients)[c("Estimate", "Std..Error")] %>%
    rename(sev_estimate = Estimate, sev_std_error = Std..Error)

# Merge the two tables
summary_table <- merge(freq_summary, sev_summary, by = "row.names")
summary_table

# Calculate the relative factors and 95% CI
phi <- qnorm(0.975)
# Relative Frequency Factor
summary_table$rel_freq <- exp(summary_table$freq_estimate)
summary_table$rel_freq_lower <- exp(summary_table$freq_estimate-phi*summary_table$freq_std_error)
summary_table$rel_freq_upper <- exp(summary_table$freq_estimate+phi*summary_table$freq_std_error)

# Relative Severity Factor
summary_table$rel_sev <- exp(summary_table$sev_estimate)
summary_table$rel_sev_lower <- exp(summary_table$sev_estimate-phi*summary_table$sev_std_error)
summary_table$rel_sev_upper <- exp(summary_table$sev_estimate+phi*summary_table$sev_std_error)


# Total Relative Factor
summary_table$rel_pure_premium <- summary_table$rel_freq * summary_table$rel_sev
joint_sd = sqrt(summary_table$freq_std_error^2 +summary_table$sev_std_error^2)
summary_table$rel_pure_premium_lower <- summary_table$rel_pure_premium * exp(-phi * joint_sd)
summary_table$rel_pure_premium_upper <- summary_table$rel_pure_premium * exp(phi * joint_sd)

# Add duration n_claims and total_cost per category
baseline_cell <- c("4","3","3","3")
baseline <- mccase_agg %>% filter(zon == baseline_cell[1], mcklass == baseline_cell[2],
    vehage_cat == baseline_cell[3], bonus_cat == baseline_cell[4])
summary_table$duration <- baseline$duration
summary_table$n_claims <- baseline$antskad
summary_table$tot_cost <- baseline$skadkost
for (i in 2:nrow(summary_table)) {
    var_name <- substr(summary_table[i, 1], 1, nchar(summary_table[i, 1]) - 1)
    var_value <- substr(summary_table[i, 1], nchar(summary_table[i, 1]), nchar(summary_table[i, 1]))
    summary_table$duration[i] <- sum(mccase_agg$duration[mccase_agg[,var_name] == var_value])
    summary_table$n_claims[i] <- sum(mccase_agg$antskad[mccase_agg[,var_name] == var_value])
    summary_table$tot_cost[i] <- sum(mccase_agg$skadkost[mccase_agg[, var_name] == var_value])
}

# Save to csv
write.csv(summary_table, "results/summary_table.csv", row.names = TRUE)
summary_table
