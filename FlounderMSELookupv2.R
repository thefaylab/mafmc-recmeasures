fluke <- readRDS("/Users/kam-macpro/Downloads/state_lookup.rds")
View(fluke)

library(dplyr)
library(data.table)

flukecatch <- tibble(fluke)

# states = NC, DE, MA, RI, CT, NY, MD, VA, NJ

# for target catch value not in lookup table; closest value 

catch = 500000

functioncatch <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State) %>% 
    filter(abs(catch - land) == min(abs(catch - land))
    )
}
functioncatch(flukecatch, State, land)

# for target catch value not in lookup table; closest value less than or equal to

functioncatch2 <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State) %>% 
    filter(land == max(land[land <= catch]))
}
functioncatch2(flukecatch, State, land)

