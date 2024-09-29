set.seed(3225)
k <- 1820
lambda <- 27

# Step 1: Generate the exponential sample
sample <- rexp(k, rate = 1/lambda)

# Step 2: Calculate the cumulative sums and the total time
cumulative_sums <- cumsum(sample)
T <- ceiling(cumulative_sums[k])

# Step 3: Divide the interval into subintervals and count the number of events per subinterval
interval_width <- 1
num_subintervals <- ceiling(T / interval_width)
event_counts <- tabulate(floor(cumulative_sums / interval_width), nbins = num_subintervals)

# Step 4: Calculate the mean and absolute deviation
mean_events <- mean(event_counts)
expected_events <- lambda * interval_width
absolute_deviation <- abs(mean_events - expected_events)
rounded_deviation <- round(absolute_deviation, 4)

rounded_deviation
