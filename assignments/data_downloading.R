# Clean data provided by https://github.com/williamorim/pokemon. No cleaning was necessary.

library(tidyverse)

# Pokemon 

# install.packages("pokemon")
pokemon_df <- pokemon::pokemon
write_csv(pokemon_df, "pokemon.csv")

# Netflix

tuesdata <- tidytuesdayR::tt_load('2025-07-29')

movies <- tuesdata$movies |> mutate(type = "movies")
shows <- tuesdata$shows |> mutate(type = "shows")

netflix_data = rbind(movies, shows)
write_csv(netflix_data, "netflix.csv")

# # 
# read_excel(xlsx_example, sheet = "chickwts")