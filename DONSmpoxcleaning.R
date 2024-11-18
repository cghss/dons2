library(readr)
library(ggplot2)

#get the data in here
df<- read.csv("https://raw.githubusercontent.com/owid/monkeypox/main/owid-monkeypox-data.csv")

#get rid of the extra stuff
new_df <- df[df$location == "World", ]
mpox_curve <- new_df[, c("location", "date", "total_cases")]

mpox_curve <- mpox_curve %>%
  mutate(NewCases = c(total_cases[1], diff(total_cases)))

mpox_curve$date <- as.Date(mpox_curve$date)

# Plot using ggplot2
ggplot(mpox_curve, aes(x = date, y = NewCases)) +
  geom_line(color = "black") +
  labs(title = "Epidemiological Curve", x = "Date", y = "New Cases")

write.csv(mpox_curve, "epidemiological_curve.csv", row.names = FALSE)
