fluke <- readRDS("/Users/kam-macpro/Downloads/state_lookup.rds")
View(fluke)

library(dplyr)
library(data.table)

flukecatch <- tibble(fluke)

# states = NC, DE, MA, RI, CT, NY, MD, VA, NJ

# state name and regs 
state = "CT"
bag = 6
minlen = 15
seasonlen = 225

# example lookup catch based on regs to input into downstream function
lookupcatch <- function(flukecatch, State, Bag, MinLen, SeasonLen){
  flukecatch %>%
  filter(
    State == state,
    Bag == bag,
    MinLen == minlen,
    SeasonLen == seasonlen
  )
}
d <- lookupcatch(flukecatch, State, Bag, MinLen, SeasonLen)
d
# target catch and state name
x1 = #low end of target catch range
x2 = #high end of target catch range
state = "NC"
catch = d$land

#find regulations based on target catch 
functionsinglecatch <- function(flukecatch, State, land){
  flukecatch %>%
    filter(
      State == state, 
      land == catch
    )
}
functionsinglecatch(flukecatch, State, land)

#find regulations based on range of catches 
functionrange <- function(flukecatch, State, targetcatch){
 flukecatch %>%
    filter(
    State == state) %>%
    mutate(targetcatch = 
             between(land, x1, x2)) %>% 
    filter (
      targetcatch == TRUE
    )
}

functionrange(flukecatch, State, land)







