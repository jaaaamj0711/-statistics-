# View the late_shipments dataset
View(late_shipments)

# Calculate the proportion of late shipments
late_prop_samp <-  late_shipments %>% summarize(mean(late == 'Yes'))

# See the results
late_prop_samp

# Hypothesize that the proportion is 6%
late_prop_hyp <- 0.06

# Calculate the standard error
std_error <- late_shipments_boot_distn %>% 
  summarize(sd_late_prop = sd(late_prop)) %>% 
  pull(sd_late_prop)

# Find z-score of late_prop_samp
z_score <- (late_prop_samp - late_prop_hyp) / std_error

# See the results
z_score


# Calculate the z-score of late_prop_samp
z_score <- (late_prop_samp - late_prop_hyp) / std_error

# Calculate the p-value
p_value <- pnorm(z_score, lower.tail = FALSE)

# See the result
p_value


# Calculate 95% confidence interval using quantile method
conf_int_quantile <- late_shipments_boot_distn %>%
  summarize(
    lower = quantile(prop_late_shipments, 0.025),
    higher = quantile(prop_late_shipments, 0.975)
  )

# See the result
conf_int_quantile
