# ==========================================================
# Project: Watershed Water Quality Analysis
# Purpose: Examine nitrate concentrations across watersheds
#          and explore their relationship with temperature.
# Author: [Kailer Meredith]
# Dataset: Simulated environmental monitoring observations
# Software: R
# ==========================================================

monitoring <- data.frame(
  Site = c("A01", "A02", "A03",
           "B01", "B02", "B03",
           "C01", "C02", "C03"),
  
  Watershed = c("North", "North", "North",
                "South", "South", "South",
                "Central", "Central", "Central"),
  
  Nitrate = c(1.2, 2.4, 1.8,
              3.1, 2.7, 3.5,
              1.4, 1.9, 2.2),
  
  Temperature = c(12.4, 14.1, 13.2,
                  15.2, 14.8, 15.7,
                  12.8, 13.5, 14.0)
)

print(monitoring)
str(monitoring)
summary(monitoring)

colSums(is.na(monitoring))

sum(duplicated(monitoring))

range(monitoring$Nitrate)
range(monitoring$Temperature)

unique(monitoring$Watershed)

sum(monitoring$Nitrate < 0)

sum(monitoring$Temperature < -5 |
      monitoring$Temperature > 40)

setdiff(
  unique(monitoring$Watershed),
  c("North", "South", "Central")
)

write.csv(
  monitoring,
  "data/monitoring_data.csv",
  row.names = FALSE
)

watershed_summary <- monitoring %>%
  group_by(Watershed) %>%
  summarise(
    Average_Nitrate = mean(Nitrate),
    Maximum_Nitrate = max(Nitrate),
    Minimum_Nitrate = min(Nitrate),
    Average_Temperature = mean(Temperature),
    Observation_Count = n()
  )

print(watershed_summary)

write.csv(
  watershed_summary,
  "outputs/watershed_summary.csv",
  row.names = FALSE
)

nitrate_plot <- ggplot(
  watershed_summary,
  aes(x = Watershed, y = Average_Nitrate)
) +
  geom_col() +
  labs(
    title = "Average Nitrate Concentration by Watershed",
    x = "Watershed",
    y = "Average Nitrate (mg/L)"
  ) +
  theme_minimal()

print(nitrate_plot)

nitrate_temp_plot <- ggplot(
  monitoring,
  aes(x = Temperature, y = Nitrate, color = Watershed)
) +
  geom_point(size = 3) +
  labs(
    title = "Nitrate Concentration and Water Temperature",
    x = "Temperature (°C)",
    y = "Nitrate (mg/L)",
    color = "Watershed"
  ) +
  theme_minimal()

print(nitrate_temp_plot)

ggsave(
  "outputs/average_nitrate_by_watershed.png",
  plot = nitrate_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/nitrate_temperature_scatterplot.png",
  plot = nitrate_temp_plot,
  width = 8,
  height = 5,
  dpi = 300
)

