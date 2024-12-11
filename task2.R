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


data.frame(mccase) %>% head(10)


# Key ratio plots for frequency
for (feature in c("zon", "mcklass", "fordald", "bonuskl","kon","agarald")) {
    agg_df <- mccase %>%
        group_by_(feature) %>%
        summarise(
            claims = sum(antskad), 
            duration = sum(duration), 
            avg_freq = sum(antskad) / sum(duration), 
            avg_freq_std = sd(antskad) / mean(duration)
        )
    agg_df$rel_freq <- agg_df$avg_freq * agg_df$duration / mean(agg_df$avg_freq * agg_df$duration)
    agg_df$rel_freq_std <- agg_df$avg_freq_std / mean(agg_df$avg_freq * agg_df$duration)
    data <- agg_df %>%
        mutate(
            lower_ci = rel_freq - 1.96 * rel_freq_std,
            upper_ci = rel_freq + 1.96 * rel_freq_std
        )
    # Create the plot
    p <- ggplot(data, aes_string(x = feature)) +
        geom_bar(aes(y = duration), stat = "identity", fill = "grey", alpha = 0.6) +
        geom_line(aes(y = rel_freq * max(duration), group = 1), color = "blue", size = 1) +
        geom_ribbon(
            aes(
                ymin = lower_ci * max(duration),
                ymax = upper_ci * max(duration),
                group = 1
            ),
            fill = "blue", alpha = 0.5
        ) +
        # Secondary axis for avg_freq
        scale_y_continuous(
            name = "Duration",
            sec.axis = sec_axis(~ . / max(data$duration), name = "Average Frequency")
        ) + # Set secondary axis lims
        labs(title = "Key Ratio Plot", x = feature) +
        theme_minimal()
    ggsave(paste0("results_task2/freq_key_ratio_plot_", feature, ".png"), p)
}
mccase %>% head(10)

# Key ratio plots for Severity
for (feature in c("zon", "mcklass", "fordald", "bonuskl", "kon","agarald")) {
    agg_df <- mccase %>%
        filter(antskad>0) %>%
        group_by_(feature) %>%
        summarise(
            duration = sum(duration),
            avg_freq = sum(skadkost) / sum(antskad),
            avg_freq_std = sd(skadkost) / mean(antskad)
        ) %>% filter(avg_freq > 0)
    agg_df$rel_freq <- agg_df$avg_freq * agg_df$duration / mean(agg_df$avg_freq * agg_df$duration)
    agg_df$rel_freq_std <- agg_df$avg_freq_std / mean(agg_df$avg_freq * agg_df$duration)
    data <- agg_df %>%
        mutate(
            lower_ci = rel_freq - 1.96 * rel_freq_std,
            upper_ci = rel_freq + 1.96 * rel_freq_std
        )
    # Create the plot
    p <- ggplot(data, aes_string(x = feature)) +
        geom_bar(aes(y = duration), stat = "identity", fill = "grey", alpha = 0.6) +
        geom_line(aes(y = rel_freq * max(duration), group = 1), color = "blue", size = 1) +
        geom_ribbon(
            aes(
                ymin = lower_ci * max(duration),
                ymax = upper_ci * max(duration),
                group = 1
            ),
            fill = "blue", alpha = 0.5
        ) +
        # Secondary axis for avg_freq
        scale_y_continuous(
            name = "Duration",
            sec.axis = sec_axis(~ . / max(data$duration), name = "Average Severity")
        ) + # Set secondary axis lims
        labs(title = "Key Ratio Plot", x = feature) +
        theme_minimal()
    ggsave(paste0("results_task2/severity_key_ratio_plot_", feature, ".png"), p)
}

# Key ratio plots for Claims
for (feature in c("zon", "mcklass", "fordald", "bonuskl", "kon","agarald")) {
    agg_df <- mccase %>%
        group_by_(feature) %>%
        summarise(
            duration = sum(duration),
            avg_freq = sum(skadkost) / duration,
            avg_freq_std = sd(skadkost) / duration
        ) %>% filter(avg_freq > 0)
    agg_df$rel_freq <- agg_df$avg_freq * agg_df$duration / mean(agg_df$avg_freq * agg_df$duration)
    agg_df$rel_freq_std <- agg_df$avg_freq_std / mean(agg_df$avg_freq * agg_df$duration)
    data <- agg_df %>%
        mutate(
            lower_ci = rel_freq - 1.96 * rel_freq_std,
            upper_ci = rel_freq + 1.96 * rel_freq_std
        )
    # Create the plot
    p <- ggplot(data, aes_string(x = feature)) +
        geom_bar(aes(y = duration), stat = "identity", fill = "grey", alpha = 0.6) +
        geom_line(aes(y = rel_freq * max(duration), group = 1), color = "blue", size = 1) +
        geom_ribbon(
            aes(
                ymin = lower_ci * max(duration),
                ymax = upper_ci * max(duration),
                group = 1
            ),
            fill = "blue", alpha = 0.5
        ) +
        # Secondary axis for avg_freq
        scale_y_continuous(
            name = "Duration",
            sec.axis = sec_axis(~ . / max(data$duration), name = "Average Relative Claim")
        ) + # Set secondary axis lims
        labs(title = "Key Ratio Plot", x = feature) +
        theme_minimal()
    ggsave(paste0("results_task2/key_ratio_plot_", feature, ".png"), p)
}

# Fit a frequency-severity model
fit_sev_freq_model <- function(data, freq_formula, sev_formula) {
    # Fit a Frequency GLM
    freq_model <- glm(freq_formula,
        data = data, family = quasipoisson(), offset = log(duration)
    )

    # Fit a Severity GLM
    sev_model <- glm(sev_formula,
        data = data %>% filter(skadkost > 0), family = Gamma(link = "log"), offset = log(antskad)
    )

    # Create Summary table
    # Join summary estimate and standard error into a single table
    freq_summary <- data.frame(summary(freq_model)$coefficients)[c("Estimate", "Std..Error")] %>%
        rename(freq_estimate = Estimate, freq_std_error = Std..Error)

    sev_summary <- data.frame(summary(sev_model)$coefficients)[c("Estimate", "Std..Error")] %>%
        rename(sev_estimate = Estimate, sev_std_error = Std..Error)

    # Merge the two tables
    summary_table <- merge(freq_summary, sev_summary, by = "row.names")

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
mccase$zon_cat <- sapply(as.numeric(mccase$zon), function(x) 
    ifelse(x %in% c(3,5,4,6,7), 3,x))
mccase %>% distinct(mccase$zon_cat)

# Vehicle age
age_lims <- c(0,1,5,9,13)
mccase$vehage_cat <- sapply(as.numeric(mccase$fordald), function(x) sum(x >= age_lims))
mccase %>% distinct(mccase$vehage_cat)

#mcclass
mccase$mcklass_cat <- sapply(mccase$mcklass, function(x) 
    ifelse(x %in% c(1,2,7,4,5), 1,x))
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
    ifelse(x=="7",2))))))))
mccase %>% distinct(mccase$bonus_cat)

# Aggregate into tariff cells
mccase_agg <- mccase %>%
    group_by(zon_cat, mcklass_cat, vehage_cat, bonus_cat) %>%
    summarise(
        duration = sum(duration),
        antskad = sum(antskad),
        skadkost = sum(skadkost)
    ) %>%
    filter(duration > 0)

# Change to factors
mccase_agg$zon_cat <- factor(mccase_agg$zon_cat, levels = c("3", "2", "1"))
mccase_agg$mcklass_cat <- factor(mccase_agg$mcklass_cat, levels = c("1","3","6"))
mccase_agg$vehage_cat <- factor(mccase_agg$vehage_cat, levels = c("3","4", "2", "1","5"))
mccase_agg$bonus_cat <- factor(mccase_agg$bonus_cat, levels = c("1", "2"))
mccase_agg %>% head(10)

# Fit the model
freq_formula <- antskad ~ zon_cat + mcklass_cat + vehage_cat + bonus_cat
sev_formula <- skadkost ~ zon_cat + mcklass_cat + vehage_cat + bonus_cat
res <- fit_sev_freq_model(mccase_agg, freq_formula, sev_formula)
df_sub <- res$summary_table[c("Row.names","rel_pure_premium","rel_pure_premium_lower","rel_pure_premium_upper")]
print.data.frame(df_sub, digits = 2)
summary(res$freq_model)
summary(res$sev_model)


### Task 2 Add variables

# Add gender
mccase$kon_cat <- as.factor(mccase$kon)

# Add owner age
age_lims <- c(25, 30, 40, 60)
mccase$age_cat <- sapply(mccase$agarald, function(x) sum(x >= age_lims))

# Aggregate into tariff cells
mccase_agg <- mccase %>%
    group_by(zon_cat, mcklass_cat, vehage_cat, bonus_cat,kon_cat,age_cat) %>%
    summarise(
        duration = sum(duration),
        antskad = sum(antskad),
        skadkost = sum(skadkost)
    ) %>%
    filter(duration > 0)

# Change to factors
mccase_agg$zon_cat <- factor(mccase_agg$zon_cat, levels = c("3", "2", "1"))
mccase_agg$mcklass_cat <- factor(mccase_agg$mcklass_cat, levels = c("1", "3", "6"))
mccase_agg$vehage_cat <- factor(mccase_agg$vehage_cat, levels = c("3", "4", "2", "1", "5"))
mccase_agg$bonus_cat <- factor(mccase_agg$bonus_cat, levels = c("1", "2"))
mccase_agg$kon_cat <- factor(mccase_agg$kon_cat, levels = c("K", "M"))
mccase_agg$age_cat <- factor(mccase_agg$age_cat, levels = c("2","1","3","4","0"))
mccase_agg %>% head(10)

# Fit the model
freq_formula <- antskad ~ zon_cat + mcklass_cat + vehage_cat + kon_cat + age_cat
sev_formula <- skadkost ~ zon_cat + mcklass_cat + vehage_cat + bonus_cat + kon_cat + age_cat
res <- fit_sev_freq_model(mccase_agg, freq_formula, sev_formula)
df_sub <- res$summary_table[c("Row.names","rel_pure_premium","rel_pure_premium_lower","rel_pure_premium_upper")]
print.data.frame(df_sub, digits = 2)
summary(res$freq_model)

########################### Add interaction terms

# Add owner age
age_lims <- c(25,30, 40, 60)
mccase$age_cat <- sapply(mccase$agarald, function(x) sum(x >= age_lims))

# Add interaction term
mccase$kon_age <- interaction(mccase$kon_cat, mccase$age_cat, sep = ".")
mccase$kon_age_cat <- sapply(mccase$kon_age, function(x) 
    ifelse(x %in% c("M.0"), "M_age0_25",
    ifelse(x %in% c("M.1"),"M_age25_30","0")))
mccase %>% distinct(mccase$kon_age_cat)

# Add owner age
age_lims <- c(40)
mccase$age_cat <- sapply(mccase$agarald, function(x) sum(x >= age_lims))

# mcclass
mccase$mcklass_cat <- sapply(mccase$mcklass, function(x) {
    ifelse(x %in% c(1, 2, 7,3, 4, 5), 1, x)
})
mccase %>% distinct(mccase$mcklass_cat)

# Aggregate into tariff cells
mccase_agg <- mccase %>%
    group_by(zon_cat, mcklass_cat, vehage_cat, bonus_cat,age_cat,kon_age_cat) %>%
    summarise(
        duration = sum(duration),
        antskad = sum(antskad),
        skadkost = sum(skadkost)
    ) %>%
    filter(duration > 0)

# Change to factors
mccase_agg$zon_cat <- factor(mccase_agg$zon_cat, levels = c("3", "2", "1"))
mccase_agg$mcklass_cat <- factor(mccase_agg$mcklass_cat, levels = c("1", "6"))
mccase_agg$vehage_cat <- factor(mccase_agg$vehage_cat, levels = c("3", "4", "2", "1", "5"))
mccase_agg$bonus_cat <- factor(mccase_agg$bonus_cat, levels = c("1", "2"))
mccase_agg$age_cat <- factor(mccase_agg$age_cat, levels = c("1", "0"))
mccase_agg$kon_age_cat <- factor(mccase_agg$kon_age_cat, c("0", "M_age0_25", "M_age25_30"))
mccase_agg %>% head(10)

# Fit the model
freq_formula <- antskad ~ zon_cat + vehage_cat + kon_age_cat + age_cat + mcklass_cat
sev_formula <- skadkost ~ zon_cat + vehage_cat + kon_age_cat + age_cat + mcklass_cat
res <- fit_sev_freq_model(mccase_agg, freq_formula, sev_formula)
df_sub <- res$summary_table[c("Row.names", "rel_pure_premium", "rel_pure_premium_lower", "rel_pure_premium_upper")]
print.data.frame(df_sub, digits = 2)
summary(res$freq_model)

