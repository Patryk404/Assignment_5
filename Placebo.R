# IN-TIME PLACEBO: Pretending treatment happened in 2006 instead of 2009

# Run synthetic control pretending treatment was in 2006
dataprep_2006 <- dataprep(
  foo = as.data.frame(df),
  predictors = c("alcohol", "pop_density", "gdp_pp", "urbanization"),
  predictors.op = "mean",
  time.predictors.prior = 2000:2005,  # Pre-treatment: 2000-2005
  dependent = "fatalities",
  unit.variable = "country_id",
  unit.names.variable = "country",
  time.variable = "year",
  treatment.identifier = 1,
  controls.identifier = setdiff(unique(df$country_id), 1),
  time.optimize.ssr = 2000:2005,
  time.plot = 2000:2008  # Stop at 2008, before real treatment
)

synth_2006 <- synth(dataprep_2006)

# Calculate gaps for 2006 placebo
actual_2006 <- dataprep_2006$Y1plot
synthetic_2006 <- dataprep_2006$Y0plot %*% synth_2006$solution.w
gap_2006 <- actual_2006 - synthetic_2006

# PLOT: 2006 placebo test

plot(2000:2008, gap_2006,
     type = "l",
     col = "black",
     lwd = 2,
     xlab = "Year",
     ylab = "Gap in Fatalities",
     main = "In-Time Placebo Test: Pretend Treatment in 2006",
     ylim = c(-5, 5))

abline(v = 2006, lty = 2, col = "black", lwd = 1.5)
abline(h = 0, lty = 2, col = "gray")

# COMPARING EFFECTS

# Effect at 2006 (the fake treatment year)
effect_at_2006 <- gap_2006[7]  # 2006 is 7th year (2000-2006)

# Post-2006 average (2006-2008)
post_2006_effect <- mean(gap_2006[7:9])

cat("\nPlacebo test results (fake treatment in 2006):\n")
cat("Effect at 2006:", round(effect_at_2006, 2), "\n")
cat("Average effect 2006-2008:", round(post_2006_effect, 2), "\n")
cat("\nCompare to actual 2009 effect:", round(results_df$gap[10], 2), "\n")

