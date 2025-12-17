# code block for your analysis
# if code doesn't run on forum, paste your code AND your results here

# Data preparation, as for some reason my excel data is not in right format
df <- df %>%
  mutate(
    country = as.character(country),
    year = as.integer(year),
    fatalities = as.numeric(fatalities),
    gdp_pc = as.numeric(gdp_pc),
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
  treatment.identifier = 1,
  controls.identifier = setdiff(unique(df$country_id), 1),
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
  Legend = c("Samoa", "Synthetic Samoa"),
  Legend.position = "bottomleft",
  Main = "Samoa vs Synthetic Control"
)
abline(v = 2009, lty = 2, col = "red")

# Gap plot (treatment effect)
gaps.plot(
  synth.res = synth_out,
  dataprep.res = dataprep_out,
  Ylab = "Gap in Fatalities",
  Xlab = "Year",
  Main = "Gap between Samoa and Synthetic Control"
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

