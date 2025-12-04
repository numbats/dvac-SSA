library(tidyverse)
?diamonds

## exercise 1
ex1 = diamonds |> filter(price > 2000 & 
                     carat < 3)
dim(ex1)

## exercise 2 

ex2 = diamonds |> filter(price > 2000 & 
        cut %in% c("Premium", "Very Good", "Ideal"))
dim(ex2)

## exercise 3

ex3 = diamonds |> filter(carat < 2 | 
                           price > 500)
dim(ex3)

## exercise 4

ex4 = diamonds |> filter(carat < 2 &
                           cut != "Premium")

dim(ex4)

## from slides
vec = c(1,12,4)
mean_vec = mean(vec)
sqrt_mean_vec = sqrt(mean_vec)

out <- c(1, 12, 4) |>
  mean() |>
  sqrt()

## Creating new variables

mpg_processed = mpg |> 
  mutate(cty_metric = 0.425144 * cty,
         hwy_metric = 0.425144 * hwy) |> 
  # Add another new variable called efficient - 
  # cars are efficient if the hwy_metric < 6
  mutate(efficient = hwy_metric < 6) |>
  # created efficient - but what kind of variable is this? 
  # character, discrete, continuous, logical 
  #next step: data set of manufacturer and model 
  # that contains only the efficient cars
  filter(efficient == TRUE) |>
  select(cty, hwy, manufacturer, model) |> 
  distinct() 

nrow(mpg_processed)

## Exercises 

# In the diamonds data set, 
# find the average price for each cut of diamond
diamonds |> 
  group_by(cut) |>
  summarise(average_price = mean(price))

# wouldn't name a variable like this if 
# i didn't have to
test = diamonds |> 
  group_by(cut) |>
  summarise(`average price` = mean(price))

# In the diamonds data set, 
#find the average price for each cut of diamond 
# given that price is above 2000.

diamonds |> 
  filter(price > 2000) |> 
  # need to filter before we summarise
  # otherwise the price variable will disappear
  group_by(cut) |>
  summarise(average_price = mean(price)) 
  # after we summarise, we'll have cut and average_price

# In the mpg data set find the 
# average fuel efficiency in the city for each year
mpg |> 
  group_by(year) |> 
  summarise(average_efficiency = mean(cty))

# In the mpg data set find the 
# average fuel efficiency in the city 
# for each manufacturer and model
average_efficiency_df = mpg |> 
  group_by(manufacturer, model) |> 
  summarise(average_efficiency = mean(cty)) |>
  ungroup()

View(average_efficiency_df)

# In the mpg data set, consider 
# Toyota, Nissan and Honda. 
# Find the value of hwy for each of these 
# manufacturer’s most fuel efficient car on the 
# highway.

output = mpg |> 
  filter(manufacturer %in% c("toyota", "nissan", "honda")) |>
  group_by(manufacturer) |>
  summarise(most_efficient = max(hwy)) |>
  ungroup()

## Last exercise 

# incorrect - need " " 
mpg |>
  pivot_longer(
    cols = c(cty, hwy), 
    names_to = road_type,
    values_to = mpg
  )

# correct version 
output = mpg |>
  pivot_longer(
    cols = c(cty, hwy), 
    names_to = "road_type",
    values_to = "mpg"
  )

