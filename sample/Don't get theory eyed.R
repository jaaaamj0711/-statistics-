# View the attrition_pop dataset
View(attrition_pop)

# Set the seed
set.seed(100)

attrition_samp <- attrition_pop %>% 
  # Add a row ID column
  rowid_to_column() %>% 
  # Get 200 rows using simple random sampling
  slice_sample(n = 200)

# View the attrition_samp dataset
View(attrition_samp)

# Set the sample size to 200
sample_size <- 200

# Get the population size from attrition_pop
pop_size <- nrow(attrition_pop)

# Calculate the interval
interval <- pop_size %/% sample_size

# Get row indexes for the sample
row_indexes <- seq_len(sample_size) * interval

attrition_sys_samp <- attrition_pop %>% 
  # Add a row ID column
  rowid_to_column() %>% 
  # Get 200 rows using systematic sampling
  slice(row_indexes)

# See the result
View(attrition_sys_samp)

# Add a row ID column to attrition_pop
attrition_pop_id <- attrition_pop %>% 
  rowid_to_column()

# Using attrition_pop_id, plot YearsAtCompany vs. rowid
ggplot(attrition_pop_id, aes(x = rowid, y = YearsAtCompany)) +
  # Make it a scatter plot
  geom_point() +
  # Add a smooth trend line
  geom_smooth()

# Shuffle the rows of attrition_pop then add row IDs
attrition_shuffled <- attrition_pop %>% 
  slice_sample(prop = 1) %>% 
  rowid_to_column()

# Using attrition_shuffled, plot YearsAtCompany vs. rowid
# Add points and a smooth trend line
ggplot(attrition_shuffled, aes(rowid, YearsAtCompany)) +
  geom_point() +
  geom_smooth()


education_counts_pop <- attrition_pop %>% 
  # Count the employees by Education level, sorting by n
  count(Education, sort = TRUE) %>% 
  # Add a percent column
  mutate(percent = 100 * n / sum(n))

# See the results
education_counts_pop

# From previous step
attrition_pop %>% 
  count(Education, sort = TRUE) %>% 
  mutate(percent = n / sum(n))

# Use proportional stratified sampling to get 
# 40% of each Education group
attrition_strat <- attrition_pop%>%
  group_by(Education) %>%
  slice_sample(prop = 0.4) %>%
  ungroup()

# See the result
attrition_strat

# Get the counts and percents from attrition_strat
education_counts_strat <- attrition_strat %>%
  count(Education, sort = TRUE) %>%
  mutate(percent = 100 * n / sum(n))

# See the results
education_counts_strat

# Use equal counts stratified sampling to get 
# 30 employees from each Education group
attrition_eq <- attrition_pop %>%
  group_by(Education) %>%
  slice_sample(n = 30)

# See the results
attrition_eq

# Get the counts and percents from attrition_strat
education_counts_eq <- attrition_eq %>% 
  count(Education, sort = TRUE) %>% 
  mutate(percent = 100 * n / sum(n))

# See the results
education_counts_eq

# Using attrition_pop, plot YearsAtCompany 
# as a histogram with binwidth 1
ggplot(attrition_pop, aes(x = YearsAtCompany)) +
  geom_histogram(binwidth = 1)

# Sample 400 employees weighted by YearsAtCompany
attrition_weight <- attrition_pop %>% 
  slice_sample(n = 400, weight_by = YearsAtCompany)

# See the results
attrition_weight

# Using attrition_weighted, plot YearsAtCompany 
# as a histogram with binwidth 1
ggplot(attrition_weight, aes(x = YearsAtCompany)) +
  geom_histogram(binwidth = 1)

# Get unique JobRole values
job_roles_pop <- unique(attrition_pop$JobRole)

# Randomly sample four JobRole values
job_roles_samp <- sample(job_roles_pop, size = 4)

# See the result
job_roles_samp

# From previous step
job_roles_pop <- unique(attrition_pop$JobRole)
job_roles_samp <- sample(job_roles_pop, size = 4)

# Filter for rows where JobRole is in job_roles_samp
attrition_filtered <- attrition_pop %>%
  filter(JobRole %in% job_roles_samp)

# Randomly sample 10 employees from each sampled job role
attrition_clus <- attrition_filtered %>%
  group_by(JobRole) %>%
  slice_sample(n = 10)

# See the result
attrition_clus


# Perform simple random sampling to get 0.25 of the population
attrition_srs <- attrition_pop %>%
  slice_sample(prop = 0.25)

# Perform stratified sampling to get 0.25 of each relationship group
attrition_strat <- attrition_pop %>%
  group_by(RelationshipSatisfaction) %>%
  slice_sample (prop = 0.25) %>%
  ungroup()

# Get unique values of RelationshipSatisfaction
satisfaction_unique <- unique(attrition_pop$RelationshipSatisfaction)

# Randomly sample for 2 of the unique satisfaction values
satisfaction_samp <- sample(satisfaction_unique, size = 2)

# Perform cluster sampling on the selected group 
# getting 0.25 of the population
attrition_strat <- attrition_pop %>%
  filter(RelationshipSatisfaction %in% satisfaction_samp) %>% 
  group_by(RelationshipSatisfaction) %>% 
  slice_sample(n = nrow(attrition_pop) / 4) %>% 
  ungroup()

# Use the whole population dataset 
mean_attrition_pop <- attrition_pop %>% 
  # Group by relationship satisfaction level
  group_by(RelationshipSatisfaction) %>% 
  # Calculate the proportion of employee attrition
  summarize(mean_attrition <- mean(Attrition == "Yes"))

# See the result
mean_attrition_pop

# Calculate the same thing for the simple random sample 
mean_attrition_srs <- attrition_srs %>%
  group_by(RelationshipSatisfaction) %>% 
  summarize(mean_attrition_srs <- mean(Attrition == "Yes"))

# See the result
mean_attrition_srs

# Calculate the same thing for the stratified sample 
mean_attrition_strat <- attrition_strat %>%
  group_by(RelationshipSatisfaction) %>% 
  summarize(mean_attrition_strat <- mean(Attrition == "Yes"))

# See the result
mean_attrition_strat

# Calculate the same thing for the cluster sample 
mean_attrition_clust <- attrition_clust %>%
  group_by(RelationshipSatisfaction) %>%
  summarize(mean_attrition_clust <- mean(Attrition == "Yes"))


# See the result
mean_attrition_clust
