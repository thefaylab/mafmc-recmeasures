
fluke <- readRDS("/Users/kam-macpro/Downloads/state_lookup.rds")
#View(fluke)

library(dplyr)
library(data.table)
flukecatch <- tibble(fluke)

# States: NC, DE, MA, RI, CT, NY, MD, VA, NJ

#2023 Regulations: taken from F&G websites
#- NJ (coastal waters) = 18", 3 fish, 148 days
 #   3 fish bag limit, lookup table starts at 4 
#- NY = 18.5", 4 fish, 153 days 

#Regs don't line up exactly with lookup table values 

#Single State:

#Expected Catch Given Current Regulations:

#Inputs
#- Look up catch in table that's to the closest season length (range of bag and minimum lengths seem to be covered, with some exceptions), or find a way to calculate exactly what the catch would be?

state = "NY"
bag = 4
minlen = 18.5
seasonlen = 153

# Example: B/BMSY = 0.4
x = 0.4

lookupcatch <- function(flukecatch, State, Bag, MinLen, SeasonLen){
  flukecatch %>%
    filter(
      State == state,
      Bag == bag,
      MinLen == minlen,
      SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])
    )
}
d <- lookupcatch(flukecatch, State, Bag, MinLen, SeasonLen)
d
expectedcatch <- d$land

#Determine Target Catch via Control Rule Settings: 

if(x <= 0.5){y=0.6}
if(between(x, 0.5, 1.2)){y=1}
if(x >= 1.2){y=1.4}

targetcatch = expectedcatch*y
targetcatch


#Lookup regulation:

functioncatch2 <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State==state) %>% 
    filter(
      State==state,
      land == max(land[land <= targetcatch]))
}
functioncatch2(flukecatch, State, land)

#Multi State:

#Common Set of Regulations: Starting with this first

#Expected Catch:
functioncatch_commonreg <- function(flukecatch, State, Bag, MinLen, SeasonLen){
  flukecatch %>%
    group_by(State) %>%
    filter(
      Bag == bag,
      MinLen == minlen,
      SeasonLen == max(SeasonLen[SeasonLen <= seasonlen])
    )
}
catchallstates_commonreg <- functioncatch_commonreg(flukecatch, State, land)
catchallstates_commonreg

#Determine Target Catch via Control Rule Settings:

if(x <= 0.5){y=0.6}
if(between(x, 0.5, 1.2)){y=1}
if(x >= 1.2){y=1.4}

catchallstates_commonreg$land = catchallstates_commonreg$land*y
catchallstates_commonreg

#Lookup Regulation: 

NCcatch_common <- catchallstates_commonreg$land[catchallstates_commonreg$State=="NC"]
DEcatch_common <- catchallstates_commonreg$land[catchallstates_commonreg$State=="DE"]
MAcatch_common <- catchallstates_commonreg$land[catchallstates_commonreg$State=="MA"]
RIcatch_common <- catchallstates_commonreg$land[catchallstates_commonreg$State=="RI"]
NYcatch_common <- catchallstates_commonreg$land[catchallstates_commonreg$State=="NY"]
MDcatch_common <- catchallstates_commonreg$land[catchallstates_commonreg$State=="MD"]
CTcatch_common <- catchallstates_commonreg$land[catchallstates_commonreg$State=="CT"]
VAcatch_common <- catchallstates_commonreg$land[catchallstates_commonreg$State=="VA"]
NJcatch_common <- catchallstates_commonreg$land[catchallstates_commonreg$State=="NJ"]

functioncatchNC_common <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="NC") %>% 
    filter(
      State=="NC",
      land == max(land[land <= NCcatch_common]))
}
NC_common <- functioncatchNC_common(flukecatch, State, land)

functioncatchDE_common <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="DE") %>% 
    filter(
      State =="DE",
      land == max(land[land <= DEcatch_common]))
}
DE_common <- functioncatchDE_common(flukecatch, State, land)

functioncatchMA_common <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="MA") %>% 
    filter(
      State=="MA",
      land == max(land[land <= MAcatch_common]))
}
MA_common <- functioncatchMA_common(flukecatch, State, land)

functioncatchRI_common <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="RI") %>% 
    filter(
      State=="RI",
      land == max(land[land <= RIcatch_common]))
}
RI_common <- functioncatchRI_common(flukecatch, State, land)

functioncatchNY_common <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="NY") %>% 
    filter(
      State=="NY",
      land == max(land[land <= NYcatch_common]))
}
NY_common <- functioncatchNY_common(flukecatch, State, land)

functioncatchMD_common <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="MD") %>% 
    filter(
      State=="MD",
      land == max(land[land <= MDcatch_common]))
}
MD_common <- functioncatchMD_common(flukecatch, State, land)

functioncatchCT_common <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="CT") %>% 
    filter(
      State=="CT",
      land == max(land[land <= CTcatch_common]))
}
CT_common <- functioncatchCT_common(flukecatch, State, land)

functioncatchVA_common <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="VA") %>% 
    filter(
      State=="VA",
      land == max(land[land <= VAcatch_common]))
}
VA_common <- functioncatchVA_common(flukecatch, State, land)

functioncatchNJ_common <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="NJ") %>% 
    filter(
      State=="NJ",
      land == max(land[land <= NJcatch_common]))
}
NJ_common <- functioncatchNJ_common(flukecatch, State, land)

#Return Regs:

targetcatchregs_common <- list(NJ_common, VA_common, CT_common, MD_common, NY_common, RI_common, MA_common, NC_common, DE_common)
targetcatchregs_common_ <- rbindlist(targetcatchregs_common, fill = TRUE, )
targetcatchregs_common_ = subset(targetcatchregs_common_, select = c(State, Bag, MinLen, SeasonLen, land, disc))
targetcatchregs_common_

#State-Specific Regulations: 
  
  #Expected Catch:

State = c("NC", "DE", "MA", "RI", "NY", "MD", "CT", "VA", "NJ")
Bag = c(4, 5, 4, 5, 5, 4, 4, 5, 6)
MinLen = c(18.5, 20, 17.5, 18, 18, 18, 18, 18, 18)
SeasonLen2 = c(153,183,163,173,161,150,150, 150,150)

df <- data.frame(State, Bag, MinLen, SeasonLen2)

lookupcatchmulti <- function(flukecatch, State, Bag, MinLen, SeasonLen){left_join(df, flukecatch, by=c("State", "Bag", "MinLen")) %>%
    group_by(State) %>%
    filter(
      SeasonLen == max(SeasonLen[SeasonLen <= SeasonLen2])
    )   
  # slice(which.min(abs(SeasonLen - SeasonLen2))) 
}
catchallstates <- lookupcatchmulti(flukecatch, State, land)
catchallstates

#Determine Target Catch via Control Rule Settings: 
  
# Example: B/BMSY = 0.4
x = 0.4

if(x <= 0.5){y=0.6}
if(between(x, 0.5, 1.2)){y=1}
if(x >= 1.2){y=1.4}

catchallstates$land <- catchallstates$land*y
catchallstates
# catchallstates2 = subset(catchallstates, select = -c(Bag, MinLen, SeasonLen2, SeasonLen))

#Lookup Regulation: 

NCcatch <- catchallstates$land[catchallstates$State=="NC"]
DEcatch <- catchallstates$land[catchallstates$State=="DE"]
MAcatch <- catchallstates$land[catchallstates$State=="MA"]
RIcatch <- catchallstates$land[catchallstates$State=="RI"]
NYcatch <- catchallstates$land[catchallstates$State=="NY"]
MDcatch <- catchallstates$land[catchallstates$State=="MD"]
CTcatch <- catchallstates$land[catchallstates$State=="CT"]
VAcatch <- catchallstates$land[catchallstates$State=="VA"]
NJcatch <- catchallstates$land[catchallstates$State=="NJ"]

functioncatchNC <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="NC") %>% 
    filter(
      State=="NC",
      land == max(land[land <= NCcatch]))
}
NC <- functioncatchNC(flukecatch, State, land)

functioncatchDE <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="DE") %>% 
    filter(
      State =="DE",
      land == max(land[land <= DEcatch]))
}
DE <- functioncatchDE(flukecatch, State, land)
# what do we do in a scenario in which there is no value lower than what 0.6 * expected catch would be?

functioncatchMA <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="MA") %>% 
    filter(
      State=="MA",
      land == max(land[land <= MAcatch]))
}
MA <- functioncatchMA(flukecatch, State, land)

functioncatchRI <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="RI") %>% 
    filter(
      State=="RI",
      land == max(land[land <= RIcatch]))
}
RI <- functioncatchRI(flukecatch, State, land)

functioncatchNY <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="NY") %>% 
    filter(
      State=="NY",
      land == max(land[land <= NYcatch]))
}
NY <- functioncatchNY(flukecatch, State, land)

functioncatchMD <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="MD") %>% 
    filter(
      State=="MD",
      land == max(land[land <= MDcatch]))
}
MD <- functioncatchMD(flukecatch, State, land)

functioncatchCT <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="CT") %>% 
    filter(
      State=="CT",
      land == max(land[land <= CTcatch]))
}
CT <- functioncatchCT(flukecatch, State, land)

functioncatchVA <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="VA") %>% 
    filter(
      State=="VA",
      land == max(land[land <= VAcatch]))
}
VA <- functioncatchVA(flukecatch, State, land)

functioncatchNJ <- function(flukecatch, State, land){
  flukecatch %>% 
    group_by(State=="NJ") %>% 
    filter(
      State=="NJ",
      land == max(land[land <= NJcatch]))
}
NJ <- functioncatchNJ(flukecatch, State, land)


targetcatchregsmulti <- list(NJ, VA, CT, MD, NY, RI, MA, NC)
targetcatchregsmulti_ <- rbindlist(targetcatchregsmulti, fill = TRUE, )
targetcatchregsmulti_ = subset(targetcatchregsmulti_, select = c(State, Bag, MinLen, SeasonLen, land, disc))
targetcatchregsmulti_

