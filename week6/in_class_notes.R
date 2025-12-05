library(tidyverse)
energy = read_csv("week6/data/energydata.csv")
weather = read_csv("week6/data/weather.csv")
energy_weather = full_join(energy, weather, by = c("Date", "State"))




factor_levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
energy_weather |> 
  
  ## Filter to the State of Victoria (VIC)
  filter(State == "VIC" &
         !is.na(Day)) |>
  
  mutate(Day = factor(Day, levels = factor_levels)) |>
  
  # Plot the MaxTemp against Demand
  ggplot(aes(x = MaxTemp, y = Demand)) +
  
  geom_point(alpha = 0.4) + 
  
  # Don't want this it's misleading
  # geom_smooth() +
  
  ## But wrap by the Day of the week
  facet_wrap(vars(Day), nrow = 4) 

### Next one 




energy_weather |> 
  filter(!is.na(Day)) |>
  ## create the new variable for weekday and weekend
  mutate(
    Day_type = if_else(Day %in% c("Sat", "Sun"),
                       "Weekend", "Weekday")
  ) |> 
  filter(!is.na(Day)) |>
  # Plot the MaxTemp against Demand
  ggplot(aes(x = MaxTemp, y = Demand, group = Day_type, col = Day_type)) +
  geom_point(alpha = 0.25) + 
  geom_smooth() +
  # Create a grid that shows Day_type and State
  facet_wrap(vars(State),
             scales = "free")
  
