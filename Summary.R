# Install if needed
install.packages("stargazer")
library(stargazer)

# Basic summary statistics table
stargazer(df,
          type = "latex",
          summary = TRUE,
          title = "Summary Statistics",
          digits = 2)

# Save to file
stargazer(df,
          type = "latex",
          summary = TRUE,
          title = "Summary Statistics",
          out = "summary_stats.tex")
