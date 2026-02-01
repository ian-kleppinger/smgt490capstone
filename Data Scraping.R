library(tidyverse)
library(jsonlite)

url <- "https://baseballsavant.mlb.com/gf?game_pk=776481"


raw <- jsonlite::fromJSON(url)
View(raw$exit_velocity)
