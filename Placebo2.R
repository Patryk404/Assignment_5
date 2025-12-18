#Using the main code, but changing the treatment unit from Samoa (ID=1) to Fiji (ID=4)

# Data preparation, as for some reason my excel data is not in right format
df <- df %>%
  mutate(
    country = as.character(country),
    year = as.integer(year),
    fatalities = as.numeric(fatalities),
    gdp_pp = as.numeric(gdp_pp),
    alcohol = as.numeric(alcohol),
    pop_density = as.numeric(pop_density)
  )

# Preparation for Synth package
# https://cran.r-project.org/web/packages/Synth/Synth.pdf this manual served me as an example
dataprep_out <- dataprep(
  foo = as.data.frame(df),
  predictors = c("alcohol", "pop_density", "gdp_pp","urbanization"),
  predictors.op = "mean",
  time.predictors.prior = 2000:2008,
  dependent = "fatalities",
  unit.variable = "country_id",
  unit.names.variable = "country",
  time.variable = "year",
  treatment.identifier = 4,
  controls.identifier = setdiff(unique(df$country_id), 4),
  time.optimize.ssr = 2000:2008,
  time.plot = 2000:2016
)

# Run synthetic control and getting the results
synth_out <- synth(dataprep_out)

synth_tables <- synth.tab(
  dataprep.res = dataprep_out,
  synth.res = synth_out
)

# Printing results
print("Unit Weights:")
print(synth_tables$tab.w)

print("\nPredictor Balance:")
print(synth_tables$tab.pred)

print("\nMean Squared Prediction Error:")
print(synth_tables$tab.loss)

# Plot results
# Path plot
path.plot(
  synth.res = synth_out,
  dataprep.res = dataprep_out,
  Ylab = "Fatalities",
  Xlab = "Year",
  Legend = c("Fiji", "Synthetic Fiji"),
  Legend.position = "bottomleft",
  Main = "Fiji vs Synthetic Control"
)
abline(v = 2009, lty = 2, col = "red")

# Gap plot (treatment effect)
gaps.plot(
  synth.res = synth_out,
  dataprep.res = dataprep_out,
  Ylab = "Gap in Fatalities",
  Xlab = "Year",
  Main = "Gap between Fiji and Synthetic Control"
)
abline(v = 2009, lty = 2, col = "red")
abline(h = 0, lty = 2, col = "gray")

# Treatment effects
# Extracting actual and synthetic values
actual <- dataprep_out$Y1plot
synthetic <- dataprep_out$Y0plot %*% synth_out$solution.w

treatment_effect <- actual - synthetic

# Post-treatment average effect
post_treatment_effect <- mean(treatment_effect[10:17]) # 2009-2016
print(paste("\nAverage Post-Treatment Effect:", round(post_treatment_effect, 2)))

# Results dataframe
results_df <- data.frame(
  year = 2000:2016,
  actual = as.vector(actual),
  synthetic = as.vector(synthetic),
  gap = as.vector(treatment_effect)
)

print("\nYear-by-Year Results:")
print(results_df)

# Split into pre and post periods
pre_treatment_gap <- treatment_effect[1:9]   # 2000-2008
post_treatment_gap <- treatment_effect[10:17] # 2009-2016

# Calculate Cohen's d
mean_post_effect <- mean(post_treatment_gap)
sd_pre_treatment <- sd(pre_treatment_gap)
cohens_d <- mean_post_effect / sd_pre_treatment

print(paste("Cohen's d:", round(cohens_d, 3)))

# Interpretation
cat("\nInterpretation (Cohen's guidelines):\n")
cat("Small effect: |d| = 0.2\n")
cat("Medium effect: |d| = 0.5\n")
cat("Large effect: |d| = 0.8\n")
