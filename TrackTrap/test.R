library(TrackTrap)

crs_trap_data <- data.frame(
  date = seq(as.Date("2026-03-01"), as.Date("2026-05-20"), by = "weeks"),
  trap_counts = sample(5:30, 12, replace = TRUE)
)
crs_trap_data$trap_counts[1:2]<-0


pheno_data <- TrackTrap::calc_pest_phenology(
  trap_data      = crs_trap_data,
  pest           = "CM",
  lat            = 36.321,
  lon            = -119.543,
  weather_source = "open_meteo"
)

TrackTrap::plot_trap_phenology(
  pheno_data,
  pest = "CM"
)
