library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)
library(gratia)

# Rearranges rows in regulations table so length distributions don't increase with regs, adds SSB as covariate

setwd("~/Desktop/FlounderMSE")

blankinput <- readRDS("blankinputtables.rds")

#FORM PLAUSIBLE DISTRIBUTION OF REGULATIONS 

sample_seas <- c(60, 
                 90,
                 120,
                 150, 
                 180, 
                 210,
                 240,
                 270, 
                 300)
sample_bag <- c(4,5,6,7,8)
sample_minlen <- c(14,15,16,17,18,19,20)
state <- c("NC", "VA", "MA", "RI", "NY", "NJ", "CT", "DE", "MD")

combinations_sample <- expand.grid(SeasonLen = unique(sample_seas), Bag = unique(sample_bag),
                                   MinLen = unique(sample_minlen))

rbc4 <- combinations_sample
#View(rbc4)
#random
rbc4 <- rbc4[sample(nrow(rbc4)), ]
row.names(rbc4) <- NULL  # reset row names
#View(filltable)

rbcNC <- rbc4 %>% mutate(State = "NC")
rbcNJ <- rbc4 %>% mutate(State = "NJ")
rbcNY <- rbc4 %>% mutate(State = "NY")
rbcMD <- rbc4 %>% mutate(State = "MD")
rbcMA <- rbc4 %>% mutate(State = "MA")
rbcRI <- rbc4 %>% mutate(State = "RI")
rbcVA <- rbc4 %>% mutate(State = "VA")
rbcDE <- rbc4 %>% mutate(State = "DE")
rbcCT <- rbc4 %>% mutate(State = "CT")

rbc4 <- rbind(rbcCT, rbcDE, rbcVA, rbcRI, rbcMA, rbcMD, rbcNY,
              rbcNJ, rbcNC)

#for filling in RDM output table below 

# sequential                      
filltable <- dplyr::select(rbc4[1:315,], SeasonLen, Bag, MinLen) 

Alternative = "scen1"
#extract regulations for each time step 
if(Alternative == "scen1"){setwd("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength")}
if(Alternative == "scen1"){scendir = "~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/"}

# CONVERT STARTING REGULATIONS INTO RDM INPUT FORMAT    
n = 315

## -----------------------------------------------------------------------------

NJtableHCR <- rbc4 %>% filter(State == "NJ")
#View(NYtableHCR)
NJdatalist = list()
NJdatalist = vector("list", length = n)

for (x in 1:length(NJtableHCR$State)){
  NJinput2 <- blankinput$NJinput2
  if(NJtableHCR$SeasonLen[x]==60){NJinput2$fluke_bag[13:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[13:16] = NJtableHCR$MinLen[x]} else 
    if(NJtableHCR$SeasonLen[x]==75){NJinput2$fluke_bag[12:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[12:16] = NJtableHCR$MinLen[x]} else
      if (NJtableHCR$SeasonLen[x]==90){NJinput2$fluke_bag[11:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[11:16] = NJtableHCR$MinLen[x]} else 
        if (NJtableHCR$SeasonLen[x]==105){NJinput2$fluke_bag[10:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[10:16] = NJtableHCR$MinLen[x]} else
          if (NJtableHCR$SeasonLen[x]==120){NJinput2$fluke_bag[9:16] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:16] = NJtableHCR$MinLen[x]} else 
            if (NJtableHCR$SeasonLen[x]==135){NJinput2$fluke_bag[9:17] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:17] = NJtableHCR$MinLen[x]} else 
              if (NJtableHCR$SeasonLen[x]==150){NJinput2$fluke_bag[9:18] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:18] = NJtableHCR$MinLen[x]} else 
                if (NJtableHCR$SeasonLen[x]==165){NJinput2$fluke_bag[9:19] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:19] = NJtableHCR$MinLen[x]} else 
                  if (NJtableHCR$SeasonLen[x]==180){NJinput2$fluke_bag[9:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[9:20] = NJtableHCR$MinLen[x]} else 
                    if (NJtableHCR$SeasonLen[x]==195){NJinput2$fluke_bag[8:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[8:20] = NJtableHCR$MinLen[x]} else 
                      if (NJtableHCR$SeasonLen[x]==210){NJinput2$fluke_bag[7:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[7:20] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 2} else #for when season length change results in increase of bounds from regulations_option 2019 spreadsheet
                        if (NJtableHCR$SeasonLen[x]==225){NJinput2$fluke_bag[6:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[6:20] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 3} else 
                          if (NJtableHCR$SeasonLen[x]==240){NJinput2$fluke_bag[5:20] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:20] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4} else 
                            if (NJtableHCR$SeasonLen[x]==255){NJinput2$fluke_bag[5:21] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:21] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4} else 
                              if (NJtableHCR$SeasonLen[x]==270){NJinput2$fluke_bag[5:22] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:22] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4} else 
                                if (NJtableHCR$SeasonLen[x]==285){NJinput2$fluke_bag[5:23] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:23] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4; NJinput2$pd_multiplier[22] <- 2} else 
                                  if (NJtableHCR$SeasonLen[x]==300){NJinput2$fluke_bag[5:24] = NJtableHCR$Bag[x]; NJinput2$fluke_min[5:24] = NJtableHCR$MinLen[x]; NJinput2$pd_multiplier[8] <- 4; NJinput2$pd_multiplier[22] <- 3} 
  NJinput2$fluke_bag <- ifelse(is.na(NJinput2$fluke_bag), 0, NJtableHCR$Bag[x])
  NJinput2$fluke_min <- ifelse(is.na(NJinput2$fluke_min), 100, NJtableHCR$MinLen[x])
  if (NJinput2$fluke_bag[10]!=0){NJinput2$pd_multiplier[10] <- 2} # I've been keeping pd_multiplier as 1 if that period isn't open for a scenario
  if (NJinput2$fluke_bag[18]!=0){NJinput2$pd_multiplier[18] <- 2.5}
  NJinputmiddleman <- na.omit(NJinput2)
  NJdatalist[[x]] <- NJinputmiddleman %>% mutate(Nsim = x)
}
NJbig_data = do.call(rbind, NJdatalist)

## -----------------------------------------------------------------------------

NYtableHCR <- rbc4 %>% filter(State == "NY")

NYdatalist = list()
NYdatalist = vector("list", length = n)

for (x in 1:length(NYtableHCR$State)){
  NYinput2 <- blankinput$NYinput2
  if(NYtableHCR$SeasonLen[x]==60){NYinput2$fluke_bag[13:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[13:16] = NYtableHCR$MinLen[x]} else 
    if(NYtableHCR$SeasonLen[x]==75){NYinput2$fluke_bag[12:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[12:16] = NYtableHCR$MinLen[x]} else
      if (NYtableHCR$SeasonLen[x]==90){NYinput2$fluke_bag[11:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[11:16] = NYtableHCR$MinLen[x]} else 
        if (NYtableHCR$SeasonLen[x]==105){NYinput2$fluke_bag[10:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[10:16] = NYtableHCR$MinLen[x]} else
          if (NYtableHCR$SeasonLen[x]==120){NYinput2$fluke_bag[9:16] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:16] = NYtableHCR$MinLen[x]} else 
            if (NYtableHCR$SeasonLen[x]==135){NYinput2$fluke_bag[9:17] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:17] = NYtableHCR$MinLen[x]} else 
              if (NYtableHCR$SeasonLen[x]==150){NYinput2$fluke_bag[9:18] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:18] = NYtableHCR$MinLen[x]} else 
                if (NYtableHCR$SeasonLen[x]==165){NYinput2$fluke_bag[9:19] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:19] = NYtableHCR$MinLen[x]} else 
                  if (NYtableHCR$SeasonLen[x]==180){NYinput2$fluke_bag[9:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[9:20] = NYtableHCR$MinLen[x]} else 
                    if (NYtableHCR$SeasonLen[x]==195){NYinput2$fluke_bag[8:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[8:20] = NYtableHCR$MinLen[x]} else 
                      if (NYtableHCR$SeasonLen[x]==210){NYinput2$fluke_bag[7:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[7:20] = NYtableHCR$MinLen[x]} else 
                        if (NYtableHCR$SeasonLen[x]==225){NYinput2$fluke_bag[6:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[6:20] = NYtableHCR$MinLen[x]} else 
                          if (NYtableHCR$SeasonLen[x]==240){NYinput2$fluke_bag[5:20] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:20] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2} else 
                            if (NYtableHCR$SeasonLen[x]==255){NYinput2$fluke_bag[5:21] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:21] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2; NYinput2$pd_multiplier[20] <- 2} else 
                              if (NYtableHCR$SeasonLen[x]==270){NYinput2$fluke_bag[5:22] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:22] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2; NYinput2$pd_multiplier[20] <- 3} else 
                                if (NYtableHCR$SeasonLen[x]==285){NYinput2$fluke_bag[5:23] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:23] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2; NYinput2$pd_multiplier[20] <- 4} else 
                                  if (NYtableHCR$SeasonLen[x]==300){NYinput2$fluke_bag[5:24] = NYtableHCR$Bag[x]; NYinput2$fluke_min[5:24] = NYtableHCR$MinLen[x]; NYinput2$pd_multiplier[6] <- 2; NYinput2$pd_multiplier[20] <- 5} 
  NYinput2$fluke_bag <- ifelse(is.na(NYinput2$fluke_bag), 0, NYtableHCR$Bag[x])
  NYinput2$fluke_min <- ifelse(is.na(NYinput2$fluke_min), 100, NYtableHCR$MinLen[x])
  if (NYinput2$fluke_bag[9]!=0){NYinput2$pd_multiplier[9] <- 1.25}
  NYinputmiddleman <- na.omit(NYinput2)
  NYdatalist[[x]] <- NYinputmiddleman %>% mutate(Nsim = x)
}
NYbig_data = do.call(rbind, NYdatalist)


## -----------------------------------------------------------------------------

NCtableHCR <- rbc4 %>% filter(State == "NC")

NCdatalist = list()
NCdatalist = vector("list", length = n)

for (x in 1:length(NCtableHCR$State)){
  NCinput2 <- blankinput$NCinput2
  if(NCtableHCR$SeasonLen[x]==60){NCinput2$fluke_bag[13:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[13:16] = NCtableHCR$MinLen[x]} else 
    if(NCtableHCR$SeasonLen[x]==75){NCinput2$fluke_bag[12:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[12:16] = NCtableHCR$MinLen[x]} else
      if (NCtableHCR$SeasonLen[x]==90){NCinput2$fluke_bag[11:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[11:16] = NCtableHCR$MinLen[x]} else 
        if (NCtableHCR$SeasonLen[x]==105){NCinput2$fluke_bag[10:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[10:16] = NCtableHCR$MinLen[x]} else
          if (NCtableHCR$SeasonLen[x]==120){NCinput2$fluke_bag[9:16] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:16] = NCtableHCR$MinLen[x]} else 
            if (NCtableHCR$SeasonLen[x]==135){NCinput2$fluke_bag[9:17] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:17] = NCtableHCR$MinLen[x]} else 
              if (NCtableHCR$SeasonLen[x]==150){NCinput2$fluke_bag[9:18] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:18] = NCtableHCR$MinLen[x]} else 
                if (NCtableHCR$SeasonLen[x]==165){NCinput2$fluke_bag[9:19] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:19] = NCtableHCR$MinLen[x]} else 
                  if (NCtableHCR$SeasonLen[x]==180){NCinput2$fluke_bag[9:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[9:20] = NCtableHCR$MinLen[x]} else 
                    if (NCtableHCR$SeasonLen[x]==195){NCinput2$fluke_bag[8:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[8:20] = NCtableHCR$MinLen[x]} else 
                      if (NCtableHCR$SeasonLen[x]==210){NCinput2$fluke_bag[7:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[7:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 2} else 
                        if (NCtableHCR$SeasonLen[x]==225){NCinput2$fluke_bag[6:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[6:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 3} else 
                          if (NCtableHCR$SeasonLen[x]==240){NCinput2$fluke_bag[5:20] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:20] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4} else 
                            if (NCtableHCR$SeasonLen[x]==255){NCinput2$fluke_bag[5:21] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:21] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 2} else 
                              if (NCtableHCR$SeasonLen[x]==270){NCinput2$fluke_bag[5:22] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:22] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 3} else 
                                if (NCtableHCR$SeasonLen[x]==285){NCinput2$fluke_bag[5:23] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:23] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 4} else 
                                  if (NCtableHCR$SeasonLen[x]==300){NCinput2$fluke_bag[5:24] = NCtableHCR$Bag[x]; NCinput2$fluke_min[5:24] = NCtableHCR$MinLen[x]; NCinput2$pd_multiplier[8] <- 4; NCinput2$pd_multiplier[20] <- 5} 
  NCinput2$fluke_bag <- ifelse(is.na(NCinput2$fluke_bag), 0, NCtableHCR$Bag[x])
  NCinput2$fluke_min <- ifelse(is.na(NCinput2$fluke_min), 100, NCtableHCR$MinLen[x])
  if (NCinput2$fluke_bag[16]!=0){NCinput2$pd_multiplier[16] <- 2}
  NCinputmiddleman <- na.omit(NCinput2)
  NCdatalist[[x]] <- NCinputmiddleman %>% mutate(Nsim = x)
}
NCbig_data = do.call(rbind, NCdatalist)


## -----------------------------------------------------------------------------

RItableHCR <- rbc4 %>% filter(State == "RI")

RIdatalist = list()
RIdatalist = vector("list", length = n)

for (x in 1:length(RItableHCR$State)){
  RIinput2 <- blankinput$RIinput2
  if(RItableHCR$SeasonLen[x]==60){RIinput2$fluke_bag[13:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[13:16] = RItableHCR$MinLen[x]} else 
    if(RItableHCR$SeasonLen[x]==75){RIinput2$fluke_bag[12:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[12:16] = RItableHCR$MinLen[x]} else
      if (RItableHCR$SeasonLen[x]==90){RIinput2$fluke_bag[11:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[11:16] = RItableHCR$MinLen[x]} else 
        if (RItableHCR$SeasonLen[x]==105){RIinput2$fluke_bag[10:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[10:16] = RItableHCR$MinLen[x]} else
          if (RItableHCR$SeasonLen[x]==120){RIinput2$fluke_bag[9:16] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:16] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
            if (RItableHCR$SeasonLen[x]==135){RIinput2$fluke_bag[9:17] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:17] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
              if (RItableHCR$SeasonLen[x]==150){RIinput2$fluke_bag[9:18] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:18] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
                if (RItableHCR$SeasonLen[x]==165){RIinput2$fluke_bag[9:19] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:19] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
                  if (RItableHCR$SeasonLen[x]==180){RIinput2$fluke_bag[9:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[9:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1/0.867)} else 
                    if (RItableHCR$SeasonLen[x]==195){RIinput2$fluke_bag[8:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[8:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (1+(1/0.867))} else 
                      if (RItableHCR$SeasonLen[x]==210){RIinput2$fluke_bag[7:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[7:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (2+(1/0.867))} else 
                        if (RItableHCR$SeasonLen[x]==225){RIinput2$fluke_bag[6:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[6:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (3+(1/0.867))} else 
                          if (RItableHCR$SeasonLen[x]==240){RIinput2$fluke_bag[5:20] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:20] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867))} else 
                            if (RItableHCR$SeasonLen[x]==255){RIinput2$fluke_bag[5:21] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:21] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867))} else 
                              if (RItableHCR$SeasonLen[x]==270){RIinput2$fluke_bag[5:22] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:22] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[21] <- 2} else 
                                if (RItableHCR$SeasonLen[x]==285){RIinput2$fluke_bag[5:23] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:23] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[21] <- 3} else 
                                  if (RItableHCR$SeasonLen[x]==300){RIinput2$fluke_bag[5:24] = RItableHCR$Bag[x]; RIinput2$fluke_min[5:24] = RItableHCR$MinLen[x]; RIinput2$pd_multiplier[9] <- (4+(1/0.867)); RIinput2$pd_multiplier[21] <- 4} 
  RIinput2$fluke_bag <- ifelse(is.na(RIinput2$fluke_bag), 0, RItableHCR$Bag[x])
  RIinput2$fluke_min <- ifelse(is.na(RIinput2$fluke_min), 100, RItableHCR$MinLen[x])
  RIinputmiddleman <- na.omit(RIinput2)
  RIdatalist[[x]] <- RIinputmiddleman %>% mutate(Nsim = x)
}
RIbig_data = do.call(rbind, RIdatalist)


## -----------------------------------------------------------------------------

MDtableHCR <- rbc4 %>% filter(State == "MD")

MDdatalist = list()
MDdatalist = vector("list", length = n)

for (x in 1:length(MDtableHCR$State)){
  MDinput2 <- blankinput$MDinput2
  if(MDtableHCR$SeasonLen[x]==60){MDinput2$fluke_bag[13:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[13:16] = MDtableHCR$MinLen[x]} else 
    if(MDtableHCR$SeasonLen[x]==75){MDinput2$fluke_bag[12:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[12:16] = MDtableHCR$MinLen[x]} else
      if (MDtableHCR$SeasonLen[x]==90){MDinput2$fluke_bag[11:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[11:16] = MDtableHCR$MinLen[x]} else 
        if (MDtableHCR$SeasonLen[x]==105){MDinput2$fluke_bag[10:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[10:16] = MDtableHCR$MinLen[x]} else
          if (MDtableHCR$SeasonLen[x]==120){MDinput2$fluke_bag[9:16] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:16] = MDtableHCR$MinLen[x]} else 
            if (MDtableHCR$SeasonLen[x]==135){MDinput2$fluke_bag[9:17] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:17] = MDtableHCR$MinLen[x]} else 
              if (MDtableHCR$SeasonLen[x]==150){MDinput2$fluke_bag[9:18] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:18] = MDtableHCR$MinLen[x]} else 
                if (MDtableHCR$SeasonLen[x]==165){MDinput2$fluke_bag[9:19] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:19] = MDtableHCR$MinLen[x]} else 
                  if (MDtableHCR$SeasonLen[x]==180){MDinput2$fluke_bag[9:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[9:20] = MDtableHCR$MinLen[x]} else 
                    if (MDtableHCR$SeasonLen[x]==195){MDinput2$fluke_bag[8:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[8:20] = MDtableHCR$MinLen[x]} else 
                      if (MDtableHCR$SeasonLen[x]==210){MDinput2$fluke_bag[7:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[7:20] = MDtableHCR$MinLen[x]} else 
                        if (MDtableHCR$SeasonLen[x]==225){MDinput2$fluke_bag[6:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[6:20] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 2} else 
                          if (MDtableHCR$SeasonLen[x]==240){MDinput2$fluke_bag[5:20] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:20] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3} else 
                            if (MDtableHCR$SeasonLen[x]==255){MDinput2$fluke_bag[5:21] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:21] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3} else 
                              if (MDtableHCR$SeasonLen[x]==270){MDinput2$fluke_bag[5:22] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:22] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3; MDinput2$pd_multiplier[21] <- 2} else 
                                if (MDtableHCR$SeasonLen[x]==285){MDinput2$fluke_bag[5:23] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:23] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3; MDinput2$pd_multiplier[21] <- 3} else 
                                  if (MDtableHCR$SeasonLen[x]==300){MDinput2$fluke_bag[5:24] = MDtableHCR$Bag[x]; MDinput2$fluke_min[5:24] = MDtableHCR$MinLen[x]; MDinput2$pd_multiplier[7] <- 3; MDinput2$pd_multiplier[21] <- 4} 
  MDinput2$fluke_bag <- ifelse(is.na(MDinput2$fluke_bag), 0, MDtableHCR$Bag[x])
  MDinput2$fluke_min <- ifelse(is.na(MDinput2$fluke_min), 100, MDtableHCR$MinLen[x])
  MDinputmiddleman <- na.omit(MDinput2)
  MDdatalist[[x]] <- MDinputmiddleman %>% mutate(Nsim = x)
}
MDbig_data = do.call(rbind, MDdatalist)


## -----------------------------------------------------------------------------

VAtableHCR <- rbc4 %>% filter(State == "VA")

VAdatalist = list()
VAdatalist = vector("list", length = n)

for (x in 1:length(VAtableHCR$State)){
  VAinput2 <- blankinput$VAinput2
  if(VAtableHCR$SeasonLen[x]==60){VAinput2$fluke_bag[13:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[13:16] = VAtableHCR$MinLen[x]} else 
    if(VAtableHCR$SeasonLen[x]==75){VAinput2$fluke_bag[12:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[12:16] = VAtableHCR$MinLen[x]} else
      if (VAtableHCR$SeasonLen[x]==90){VAinput2$fluke_bag[11:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[11:16] = VAtableHCR$MinLen[x]} else 
        if (VAtableHCR$SeasonLen[x]==105){VAinput2$fluke_bag[10:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[10:16] = VAtableHCR$MinLen[x]} else
          if (VAtableHCR$SeasonLen[x]==120){VAinput2$fluke_bag[9:16] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:16] = VAtableHCR$MinLen[x]} else 
            if (VAtableHCR$SeasonLen[x]==135){VAinput2$fluke_bag[9:17] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:17] = VAtableHCR$MinLen[x]} else 
              if (VAtableHCR$SeasonLen[x]==150){VAinput2$fluke_bag[9:18] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:18] = VAtableHCR$MinLen[x]} else 
                if (VAtableHCR$SeasonLen[x]==165){VAinput2$fluke_bag[9:19] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:19] = VAtableHCR$MinLen[x]} else 
                  if (VAtableHCR$SeasonLen[x]==180){VAinput2$fluke_bag[9:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[9:20] = VAtableHCR$MinLen[x]} else 
                    if (VAtableHCR$SeasonLen[x]==195){VAinput2$fluke_bag[8:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[8:20] = VAtableHCR$MinLen[x]} else 
                      if (VAtableHCR$SeasonLen[x]==210){VAinput2$fluke_bag[7:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[7:20] = VAtableHCR$MinLen[x]} else 
                        if (VAtableHCR$SeasonLen[x]==225){VAinput2$fluke_bag[6:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[6:20] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 2} else 
                          if (VAtableHCR$SeasonLen[x]==240){VAinput2$fluke_bag[5:20] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:20] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3} else 
                            if (VAtableHCR$SeasonLen[x]==255){VAinput2$fluke_bag[5:21] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:21] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3} else 
                              if (VAtableHCR$SeasonLen[x]==270){VAinput2$fluke_bag[5:22] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:22] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3} else 
                                if (VAtableHCR$SeasonLen[x]==285){VAinput2$fluke_bag[5:23] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:23] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3; VAinput2$pd_multiplier[22] <- 2} else 
                                  if (VAtableHCR$SeasonLen[x]==300){VAinput2$fluke_bag[5:24] = VAtableHCR$Bag[x]; VAinput2$fluke_min[5:24] = VAtableHCR$MinLen[x]; VAinput2$pd_multiplier[7] <- 3; VAinput2$pd_multiplier[22] <- 3} 
  VAinput2$fluke_bag <- ifelse(is.na(VAinput2$fluke_bag), 0, VAtableHCR$Bag[x])
  VAinput2$fluke_min <- ifelse(is.na(VAinput2$fluke_min), 100, VAtableHCR$MinLen[x])
  VAinputmiddleman <- na.omit(VAinput2)
  VAdatalist[[x]] <- VAinputmiddleman %>% mutate(Nsim = x)
}
VAbig_data = do.call(rbind, VAdatalist)


## -----------------------------------------------------------------------------

CTtableHCR <- rbc4 %>% filter(State == "CT")

CTdatalist = list()
CTdatalist = vector("list", length = n)

for (x in 1:length(CTtableHCR$State)){
  CTinput2 <- blankinput$CTinput2
  if(CTtableHCR$SeasonLen[x]==60){CTinput2$fluke_bag[13:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[13:16] = CTtableHCR$MinLen[x]} else 
    if(CTtableHCR$SeasonLen[x]==75){CTinput2$fluke_bag[12:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[12:16] = CTtableHCR$MinLen[x]} else
      if (CTtableHCR$SeasonLen[x]==90){CTinput2$fluke_bag[11:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[11:16] = CTtableHCR$MinLen[x]} else 
        if (CTtableHCR$SeasonLen[x]==105){CTinput2$fluke_bag[10:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[10:16] = CTtableHCR$MinLen[x]} else
          if (CTtableHCR$SeasonLen[x]==120){CTinput2$fluke_bag[9:16] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:16] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
            if (CTtableHCR$SeasonLen[x]==135){CTinput2$fluke_bag[9:17] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:17] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
              if (CTtableHCR$SeasonLen[x]==150){CTinput2$fluke_bag[9:18] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:18] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
                if (CTtableHCR$SeasonLen[x]==165){CTinput2$fluke_bag[9:19] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:19] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
                  if (CTtableHCR$SeasonLen[x]==180){CTinput2$fluke_bag[9:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[9:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 1.25} else 
                    if (CTtableHCR$SeasonLen[x]==195){CTinput2$fluke_bag[8:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[8:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 2.25} else 
                      if (CTtableHCR$SeasonLen[x]==210){CTinput2$fluke_bag[7:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[7:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 3.25} else 
                        if (CTtableHCR$SeasonLen[x]==225){CTinput2$fluke_bag[6:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[6:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 4.25} else 
                          if (CTtableHCR$SeasonLen[x]==240){CTinput2$fluke_bag[5:20] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:20] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25} else 
                            if (CTtableHCR$SeasonLen[x]==255){CTinput2$fluke_bag[5:21] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:21] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25; CTinput2$pd_multiplier[20] <- 2} else 
                              if (CTtableHCR$SeasonLen[x]==270){CTinput2$fluke_bag[5:22] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:22] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25; CTinput2$pd_multiplier[20] <- 3} else 
                                if (CTtableHCR$SeasonLen[x]==285){CTinput2$fluke_bag[5:23] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:23] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25; CTinput2$pd_multiplier[20] <- 4} else 
                                  if (CTtableHCR$SeasonLen[x]==300){CTinput2$fluke_bag[5:24] = CTtableHCR$Bag[x]; CTinput2$fluke_min[5:24] = CTtableHCR$MinLen[x]; CTinput2$pd_multiplier[9] <- 5.25; CTinput2$pd_multiplier[20] <- 5} 
  CTinput2$fluke_bag <- ifelse(is.na(CTinput2$fluke_bag), 0, CTtableHCR$Bag[x])
  CTinput2$fluke_min <- ifelse(is.na(CTinput2$fluke_min), 100, CTtableHCR$MinLen[x])
  CTinputmiddleman <- na.omit(CTinput2)
  CTdatalist[[x]] <- CTinputmiddleman %>% mutate(Nsim = x)
}
CTbig_data = do.call(rbind, CTdatalist)


## -----------------------------------------------------------------------------

DEtableHCR <- rbc4 %>% filter(State == "DE")

DEdatalist = list()
DEdatalist = vector("list", length = n)

for (x in 1:length(DEtableHCR$State)){
  DEinput2 <- blankinput$DEinput2
  if(DEtableHCR$SeasonLen[x]==60){DEinput2$fluke_bag[13:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[13:16] = DEtableHCR$MinLen[x]} else 
    if(DEtableHCR$SeasonLen[x]==75){DEinput2$fluke_bag[12:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[12:16] = DEtableHCR$MinLen[x]} else
      if (DEtableHCR$SeasonLen[x]==90){DEinput2$fluke_bag[11:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[11:16] = DEtableHCR$MinLen[x]} else 
        if (DEtableHCR$SeasonLen[x]==105){DEinput2$fluke_bag[10:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[10:16] = DEtableHCR$MinLen[x]} else
          if (DEtableHCR$SeasonLen[x]==120){DEinput2$fluke_bag[9:16] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:16] = DEtableHCR$MinLen[x]} else 
            if (DEtableHCR$SeasonLen[x]==135){DEinput2$fluke_bag[9:17] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:17] = DEtableHCR$MinLen[x]} else 
              if (DEtableHCR$SeasonLen[x]==150){DEinput2$fluke_bag[9:18] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:18] = DEtableHCR$MinLen[x]} else 
                if (DEtableHCR$SeasonLen[x]==165){DEinput2$fluke_bag[9:19] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:19] = DEtableHCR$MinLen[x]} else 
                  if (DEtableHCR$SeasonLen[x]==180){DEinput2$fluke_bag[9:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[9:20] = DEtableHCR$MinLen[x]} else 
                    if (DEtableHCR$SeasonLen[x]==195){DEinput2$fluke_bag[8:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[8:20] = DEtableHCR$MinLen[x]} else 
                      if (DEtableHCR$SeasonLen[x]==210){DEinput2$fluke_bag[7:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[7:20] = DEtableHCR$MinLen[x]} else 
                        if (DEtableHCR$SeasonLen[x]==225){DEinput2$fluke_bag[6:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[6:20] = DEtableHCR$MinLen[x]} else 
                          if (DEtableHCR$SeasonLen[x]==240){DEinput2$fluke_bag[5:20] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:20] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2} else 
                            if (DEtableHCR$SeasonLen[x]==255){DEinput2$fluke_bag[5:21] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:21] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2} else 
                              if (DEtableHCR$SeasonLen[x]==270){DEinput2$fluke_bag[5:22] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:22] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2; DEinput2$pd_multiplier[21] <- 2} else 
                                if (DEtableHCR$SeasonLen[x]==285){DEinput2$fluke_bag[5:23] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:23] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2; DEinput2$pd_multiplier[21] <- 3} else 
                                  if (DEtableHCR$SeasonLen[x]==300){DEinput2$fluke_bag[5:24] = DEtableHCR$Bag[x]; DEinput2$fluke_min[5:24] = DEtableHCR$MinLen[x]; DEinput2$pd_multiplier[6] <- 2; DEinput2$pd_multiplier[21] <- 4} 
  DEinput2$fluke_bag <- ifelse(is.na(DEinput2$fluke_bag), 0, DEtableHCR$Bag[x])
  DEinput2$fluke_min <- ifelse(is.na(DEinput2$fluke_min), 100, DEtableHCR$MinLen[x])
  DEinputmiddleman <- na.omit(DEinput2)
  DEdatalist[[x]] <- DEinputmiddleman %>% mutate(Nsim = x)
}
DEbig_data = do.call(rbind, DEdatalist)


## -----------------------------------------------------------------------------

MAtableHCR <- rbc4 %>% filter(State == "MA")

MAdatalist = list()
MAdatalist = vector("list", length = n)

for (x in 1:length(MAtableHCR$State)){
  MAinput2 <- blankinput$MAinput2
  if(MAtableHCR$SeasonLen[x]==60){MAinput2$fluke_bag[13:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[13:16] = MAtableHCR$MinLen[x]} else 
    if(MAtableHCR$SeasonLen[x]==75){MAinput2$fluke_bag[12:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[12:16] = MAtableHCR$MinLen[x]} else
      if (MAtableHCR$SeasonLen[x]==90){MAinput2$fluke_bag[11:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[11:16] = MAtableHCR$MinLen[x]} else 
        if (MAtableHCR$SeasonLen[x]==105){MAinput2$fluke_bag[10:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[10:16] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- (1/0.563)} else
          if (MAtableHCR$SeasonLen[x]==120){MAinput2$fluke_bag[9:16] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:16] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
            if (MAtableHCR$SeasonLen[x]==135){MAinput2$fluke_bag[9:17] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:17] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
              if (MAtableHCR$SeasonLen[x]==150){MAinput2$fluke_bag[9:18] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:18] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
                if (MAtableHCR$SeasonLen[x]==165){MAinput2$fluke_bag[9:19] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:19] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
                  if (MAtableHCR$SeasonLen[x]==180){MAinput2$fluke_bag[9:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[9:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 1+(1/0.563)} else 
                    if (MAtableHCR$SeasonLen[x]==195){MAinput2$fluke_bag[8:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[8:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 2+(1/0.563)} else 
                      if (MAtableHCR$SeasonLen[x]==210){MAinput2$fluke_bag[7:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[7:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 3+(1/0.563)} else 
                        if (MAtableHCR$SeasonLen[x]==225){MAinput2$fluke_bag[6:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[6:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 4+(1/0.563)} else 
                          if (MAtableHCR$SeasonLen[x]==240){MAinput2$fluke_bag[5:20] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:20] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563)} else 
                            if (MAtableHCR$SeasonLen[x]==255){MAinput2$fluke_bag[5:21] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:21] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563); MAinput2$pd_multiplier[20] <- 2} else 
                              if (MAtableHCR$SeasonLen[x]==270){MAinput2$fluke_bag[5:22] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:22] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563); MAinput2$pd_multiplier[20] <- 3} else 
                                if (MAtableHCR$SeasonLen[x]==285){MAinput2$fluke_bag[5:23] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:23] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563); MAinput2$pd_multiplier[20] <- 4} else 
                                  if (MAtableHCR$SeasonLen[x]==300){MAinput2$fluke_bag[5:24] = MAtableHCR$Bag[x]; MAinput2$fluke_min[5:24] = MAtableHCR$MinLen[x]; MAinput2$pd_multiplier[10] <- 5+(1/0.563);  MAinput2$pd_multiplier[20] <- 5} 
  MAinput2$fluke_bag <- ifelse(is.na(MAinput2$fluke_bag), 0, MAtableHCR$Bag[x])
  MAinput2$fluke_min <- ifelse(is.na(MAinput2$fluke_min), 100, MAtableHCR$MinLen[x])
  if (MAinput2$fluke_bag[19]!=0){MAinput2$pd_multiplier[19] <- (1/0.6)}
  MAinputmiddleman <- na.omit(MAinput2)
  MAdatalist[[x]] <- MAinputmiddleman %>% mutate(Nsim = x)
}
MAbig_data = do.call(rbind, MAdatalist)
#View(MAbig_data)
Big_Data <- rbind(CTbig_data, DEbig_data, MAbig_data, MDbig_data, NCbig_data, NJbig_data, NYbig_data, 
                  RIbig_data, VAbig_data)

Big_Data <- Big_Data %>% arrange(Big_Data$Nsim)
#View(Big_Data)

n = 315 

bigdatalist = list()
bigdatalist = vector("list", length = n)
for (x in 1:315){
  Big_Data2 <- Big_Data %>% 
    filter(Nsim == x )
  # Big_Data2$Nsim <- NULL
  bigdatalist[[x]] <- Big_Data2
}  

#View(bigdatalist[[315]])
#View(directed_trips_table)

#RDM FOR EACH SET OF REGULATIONS AND EACH LENGTH DISTRIBUTION 

pkgs_to_use <- c("tidyr",
                 "magrittr",
                 #"reshape2",
                 #"splitstackshape",
                 #"doBy",
                 #"WriteXLS",
                 #'Rcpp',
                 #"ggplot2",
                 "dplyr",
                 "rlist",
                 #"fitdistrplus",
                 #"MASS",
                 #"psych",
                 #"rgl",
                 "copula",
                 #"VineCopula",
                 #"scales",
                 #"univariateML",
                 #"logspline",
                 #"readr",
                 "data.table",
                 "conflicted")
#install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)
# library(readxl)
# library(writexl)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflicts_prefer(copula::`%in%`) #why do we need this all of a sudden?
### 

#year of the prediction
#iyr <- 2040
#iyr <- as.integer(args[1])
#hcr_output <- args[2]

n = 315

RDMoutput_edit = list()
RDMoutput_edit = vector("list", length = n)

for (x in 1:315){
  
  mgmt_scen <- 1
  directed_trips_table <- bigdatalist[[x]] %>% 
    tibble() %>% 
    rename(dtrip_2019 = dtrip2019) %>% 
    mutate(state = factor(state, levels = c("MA","RI","CT","NY","NJ","DE","MD","VA","NC"))) 
  directed_trips_table <- split(directed_trips_table, directed_trips_table$state)
  
  #directed_trips_table <- data.frame(read_excel("directed_trips_region - alternative regs test.xlsx"))
  #directed_trips_table <- readRDS("directed_trips_regions_bimonthly.rds")
  #directed_trips_table <- readxl::read_xlsx("directed_trips_regions_bimonthly_test.xlsx")
  #  for (i in c(3:8)) {
  # #    xx <- readxl::read_xlsx(paste0("regulations_option",i,".xlsx"))
  #    directed_trips_table <- readRDS(paste0("regulations_option",i,".rds"))
  #     directed_trips_table$state <- factor(directed_trips_table$state, levels =c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #     directed_trips_table <- split(directed_trips_table, directed_trips_table$state)
  #    saveRDS(directed_trips_table,file=paste0("regulations_option",i,".rds"))
  #   }
  #directed_trips_table <- readRDS("coastwide_regulations_scenario.rds")
  # Input the calibration output which contains the number of choice occasions needed to simulate
  #calibration_data = data.frame(readxl::read_excel("calibration_output_by_period.xlsx"))
  calibration_data_table <- readRDS("calibration_output_by_period.rds")
  #utility parameter draws
  #param_draws_all <- readRDS("param_draws_all.rds")
  source("gen_params.R")
  #costs
  #costs_new <- readRDS( "costs_all.rds")
  #costs_new <- readRDS( "costs_all_1000.rds")
  costs_new <- readRDS( "costs_alt.rds")
  # for (i in 1:9) costs_new[[i]] <- costs_new[[i]] %>% filter(tripid<=1000)
  # saveRDS(costs_new, file = "costs_all_1000.rds")
  
  #selectivity (read in moved from the otehr file)
  selectivity <- readRDS("rec_selectivity_20210422.rds")

  ## OM 2 ##
  # MRIP bias
  #calibration_data_table <- readRDS("calibration_output_by_period_lb.rds")
  #calibration_data_table$state <- factor(calibration_data_table$state, levels =c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #calibration_data_table <- split(calibration_data_table, calibration_data_table$state)
  #saveRDS(calibration_data_table, file = "calibration_output_by_period_lb.rds")
  #selectivity <- readRDS("rec_selectivity_by_state_cdf_star_raw_18_19_lb.rds")
  #xx <- readxl::read_excel("rec_selectivity_by_state_cdf_star_raw_18_19_lb.xlsx")
  #saveRDS(xx,file="rec_selectivity_by_state_cdf_star_raw_18_19_lb.rds")
  
  
  ## OM 3 ##
  ## read in biomass shift params
  # the adjustment gets made in predicted catch per trip.
  #prop_rel <- readRDS('region_availability.rds') %>% 
  #  #ungroup() %>% 
  #  #mutate(region = fct_relevel(region,c("NO","NJ","SO"))) %>% 
  #  #arrange(region) %>% 
  #  filter(year == iyr) %>% 
  #  select(prop_rel) %>% 
  #  t() %>% as.numeric()
  #print("proportions!")
  #print(prop_rel)  
  #comment out below if OM 3
  prop_rel <- c(1,1,1)
  
  # READ IN LENGTH DISTRIBUTION FOR EACH SET OF REGULATIONS 
  Nlen <- 42 
  Nlengthbin <- bigdatalist[[x]]$Nsim[1]
  om_length_cm <-  
    if(Nlengthbin %in% seq(1,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2020.dat",n=Nlen+1)} else #29781.11
      if(Nlengthbin %in% seq(2,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2021.dat",n=Nlen+1)} else #42332.81
        if(Nlengthbin %in% seq(3,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2022.dat",n=Nlen+1)} else #56688.65
          if(Nlengthbin %in% seq(4,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2039.dat",n=Nlen+1)} else #64506.83 
            if(Nlengthbin %in% seq(5,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2024.dat",n=Nlen+1)} else  #76233.42        
              if(Nlengthbin %in% seq(6,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2027.dat",n=Nlen+1)} else #86364.19
                if(Nlengthbin %in% seq(7,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2045.dat",n=Nlen+1)} else #92265.67 
                  if(Nlengthbin %in% seq(8,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2025.dat",n=Nlen+1)} else #101915.23 2025
                    if(Nlengthbin %in% seq(9,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2045.dat",n=Nlen+1)} else #110281.24 2045
                      if(Nlengthbin %in% seq(10,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2037.dat",n=Nlen+1)} else #152473.71 2037 
                        if(Nlengthbin %in% seq(11,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2032.dat",n=Nlen+1)} else #167042.76 2032
                          if(Nlengthbin %in% seq(12,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2026.dat",n=Nlen+1)} else #181257.26 2026  
                            if(Nlengthbin %in% seq(13,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2027.dat",n=Nlen+1)} else #210394.15 2027
                              if(Nlengthbin %in% seq(14,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2028.dat",n=Nlen+1)} else #225460.28 2028 
                                if(Nlengthbin %in% seq(15,315, by = 15)){scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2034.dat",n=Nlen+1)} #254612.89 2034
  SSB[x] <- if(Nlengthbin %in% seq(1,315, by = 15)){SSB <- 29781.11} else #29781.11
    if(Nlengthbin %in% seq(2,315, by = 15)){SSB <- 42332.81} else #42332.81
      if(Nlengthbin %in% seq(3,315, by = 15)){SSB <- 56688.65} else #56688.65
        if(Nlengthbin %in% seq(4,315, by = 15)){SSB <- 64506.83} else #64506.83
          if(Nlengthbin %in% seq(5,315, by = 15)){SSB <- 76233.42 } else  #76233.42        
            if(Nlengthbin %in% seq(6,315, by = 15)){SSB <- 86364.19} else #86364.19
              if(Nlengthbin %in% seq(7,315, by = 15)){SSB <- 92265.67 } else #92265.67 
                if(Nlengthbin %in% seq(8,315, by = 15)){SSB <- 101915.23} else #101915.23 2025
                  if(Nlengthbin %in% seq(9,315, by = 15)){SSB <- 110281.24} else #110281.24 2045
                    if(Nlengthbin %in% seq(10,315, by = 15)){SSB <- 152473.71} else #152473.71 2037 
                      if(Nlengthbin %in% seq(11,315, by = 15)){SSB <- 167042.76} else #167042.76 2032
                        if(Nlengthbin %in% seq(12,315, by = 15)){SSB <- 181257.26} else #181257.26 2026  
                          if(Nlengthbin %in% seq(13,315, by = 15)){SSB <- 210394.15} else #210394.15 2027
                            if(Nlengthbin %in% seq(14,315, by = 15)){SSB <- 225460.28} else #225460.28 2028 
                              if(Nlengthbin %in% seq(15,315, by = 15)){SSB <- 254612.89} #254612.89 2034
  
  #cm2in <- read_csv("cm2in.csv", col_names = FALSE, show_col_types = FALSE)
  cm2in <- readRDS("cm2in.rds")
  lenbinuse <- as.integer(unlist(cm2in[,1]))
  Nlen_in <- length(lenbinuse)
  cm2in <- cm2in %>%
    dplyr::select(-1) %>%
    as.matrix() %>%
    I()
  om_length_in <- om_length_cm[-1] %*% t(cm2in)
  # size_data <- data.frame(fitted_prob = rep(om_length_in,3),
  #                         fitted_length = rep(lenbinuse,3),
  #                         region = rep(c("SO","NJ","NO"), each = Nlen_in),
  #                         year = rep("y2",3*Nlen_in))
  
  
  # catch-at-length and catch-per-trip for summer flounder
  source("catch at length given stock structure - prediction.R")
  
  # #catch data
  # sf_catch_data_ma <- readRDS("predicted_catch_MA.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_ri <- readRDS("predicted_catch_RI.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_ct <- readRDS("predicted_catch_CT.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # 
  # sf_catch_data_ny <- readRDS("predicted_catch_NY.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_nj <- readRDS("predicted_catch_NJ.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_de <- readRDS("predicted_catch_DE.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # 
  # sf_catch_data_md <- readRDS("predicted_catch_MD.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # sf_catch_data_va <- readRDS("predicted_catch_VA.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  # 
  # sf_catch_data_nc <- readRDS("predicted_catch_NC.rds") %>% 
  #   tibble() %>% 
  #   rename(tot_sf_catch = sf_t_nb,
  #          tot_bsb_catch = bsb_t_nb) %>%
  #   I()
  
  # Read-in the current population length composition  #don't need this in final as it's already an object.
  #size_data_read <- data.frame(read_excel("sf_fitted_sizes_y2plus.xlsx"))
  #size_data_read <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble()
  
  #calibration_data_table$state <- factor(calibration_data_table$state,levels=c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #calibration_data_table <- split(calibration_data_table %>% tibble(),calibration_data_table$state)
  #directed_trips_table$state <- factor(directed_trips_table$state, levels =c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #directed_trips_table <- split(directed_trips_table, directed_trips_table$state)
  #size_data_read$region <- factor(size_data_read$region, levels =c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"))
  #size_data_read <- split(size_data_read, size_data_read$region)
  
  # loop over states
  params <- list(state1 = c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"),
                 region1 = c(rep("NO",4),"NJ",rep("SO",4)),
                 calibration_data_table = calibration_data_table, #rep(list(calibration_data_table),9),
                 directed_trips_table = directed_trips_table, #rep(list(directed_trips_table),9),   #split(directed_trips_table,directed_trips_table$state), #
                 size_data_read = size_data_read, #rep(list(size_data_read),9),
                 param_draws_MA = param_draws_all,
                 costs_new_all_MA = costs_new,
                 
                 #sf_catch_data_all = c(rep(list(sf_catch_data_no),4),list(sf_catch_data_nj),rep(list(sf_catch_data_so),4)),
                 sf_catch_data_all = c(list(sf_catch_data_ma),list(sf_catch_data_ri),
                                       list(sf_catch_data_ct),list(sf_catch_data_ny),
                                       list(sf_catch_data_nj),list(sf_catch_data_de),
                                       list(sf_catch_data_md),list(sf_catch_data_va), list(sf_catch_data_nc)),
                 # prop_bsb_keep = #rep(0.33,9))  # add Lou's p* values here!
                 #   c(1-0.67,
                 #     1-0.66,
                 #     1-0.77,
                 #     1-0.87,
                 #     1-0.93,
                 #     1-0.945,
                 #     1-0.96,
                 #     1-0.92,
                 #     0.001),
                 prop_bsb_keep = c(
                   1-.53,
                   1-.38,
                   1-.7,
                   1-.83,
                   1-.92,
                   1-.942,
                   1-.96,
                   1-.92,
                   0.001), #1),
                 dchoose = rep(1,9),
                 mgmt_scen = rep(mgmt_scen,9)) #1-1.1))
  
  # params <- list(state1 = "MA",
  #                region1 = "NO",
  #                calibration_data_table = list(calibration_data_table),
  #                directed_trips_table = list(directed_trips_table),
  #                size_data_read = list(size_data_read),
  #                param_draws_MA = list(param_draws_all[[1]]),
  #                costs_new_all_MA = list(costs_new[[1]]),
  #                sf_catch_data_all = list(sf_catch_data_ma),
  #                prop_bsb_keep = 1-0.53,
  #                dchoose = 1)
  # 
  # params <- list(state1 = "NJ",
  #                region1 = "NJ",
  #                calibration_data_table = list(calibration_data_table),
  #                directed_trips_table = list(directed_trips_table),
  #                size_data_read = list(size_data_read),
  #                param_draws_MA = list(param_draws_all[[5]]),
  #                costs_new_all_MA = list(costs_new[[5]]),
  #                sf_catch_data_all = list(sf_catch_data_nj),
  #                prop_bsb_keep = 1-0.92,
  #                dchoose = 1)
  
  #source("prediction-all.R")
  #source("prediction-vec.R")
  source("prediction-vec-sim.R")
  
  # set.seed(1989)
  # simkeep <- NULL
  # simrel <- NULL
  # simagg <- NULL
  # for (jsim in 1:30) {
  # ##########  need to add link to OM scenario regulations
  # print(jsim)
  #params$dchoose <- rep(sample(1:1000,1),9)
  #profvis::profvis({
  safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
  xx <- purrr::pmap(params, safe_predict_rec_catch)
  #})
  
  # profvis::profvis(testMA <- predict_rec_catch(state1 = "MA",
  #                         region1 = "NO",
  #                         calibration_data_table = calibration_data_table,
  #                         directed_trips_table = directed_trips_table,
  #                         size_data_read = size_data_read,
  #                         param_draws_MA = param_draws_all[[1]],
  #                         costs_new_all_MA = costs_new[[1]],
  #                         sf_catch_data_all = sf_catch_data_no,
  #                         prop_bsb_keep = 0.33))
  # 
  # xx <- predict_rec_catch(state1 = "NC",
  #                         region1 = "SO",
  #                         calibration_data_table = calibration_data_table,
  #                         directed_trips_table = directed_trips_table,
  #                         size_data_read = size_data_read,
  #                         param_draws_MA = param_draws_all[[9]],
  #                         costs_new_all_MA = costs_new[[9]],
  #                         sf_catch_data_all = sf_catch_data_so)
  
  
  prediction_output_by_period <- purrr::map(xx, 1)
  
  #saveRDS(prediction_output_by_period, file = "prediction_output_by_period.rds")
  
  #aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs, period))
  aggregate_prediction_output <- prediction_output_by_period %>% 
    list.stack(fill = TRUE) %>% 
    mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
    dplyr::select(-state, -alt_regs) %>%  #, -period) %>% 
    group_by(sim) %>% 
    summarize_if(is.numeric, .funs = sum,na.rm=TRUE) # %>% 
  #dplyr::select(order(colnames(.))) %>% 
  #I()
  #  = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
  #write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")
  #saveRDS(aggregate_prediction_output, file = "aggregate_prediction_output.rds") 
  ##########  
  
  pred_len <- tibble(aggregate_prediction_output) %>% 
    dplyr::select(contains("length")) %>% 
    pivot_longer(cols = 1:ncol(.), names_to = "bin",values_to = "num") %>% 
    separate(bin, into =c("type","len"),sep = "_length_") %>% 
    mutate(len = as.numeric(len)) #%>% 
  #I()
  mulen <- pred_len %>%
    group_by(type) %>%
    summarize(mulen = sum(len*num)/sum(num),
              .groups = "drop") %>% 
    pivot_wider(names_from = type,
                names_glue = "mulen_{type}",
                values_from = mulen) %>% 
    bind_cols(aggregate_prediction_output) %>%
    select(sim, tot_keep, tot_rel, observed_trips, n_choice_occasions, change_CS, cost, keep_one, mulen_keep, mulen_release)
  #mulen
  
  biglen <- pred_len %>% 
    filter(type == "keep") %>% 
    mutate(numbig = ifelse(len>=28,num,0))  %>% 
    summarize(fracbig = sum(numbig)/sum(num))
  mulen <- bind_cols(mulen, biglen)  
  
  #pred_len
  out_lens <- tibble(type = rep(c("release","keep"),each=Nlen_in),
                     len = rep(lenbinuse,2)) %>% 
    left_join(pred_len, by = c("type", "len")) %>% 
    replace_na(list(num=0)) #%>% 
  #I()
  #out_lens
  #in2cm <- readr::read_csv("in2cm.csv", col_names = FALSE, show_col_types = FALSE)[,-1]
  in2cm <- readRDS("in2cm.rds")
  keep <- out_lens %>% 
    filter(type == "keep") %>% 
    dplyr::select(num) %>% 
    unlist()# %>%
  #I()
  keep <- keep %*% t(in2cm)
  release <- out_lens %>% 
    filter(type == "release") %>% 
    dplyr::select(num) %>% 
    unlist() #%>%
  #I()
  release <- release %*% t(in2cm)
  write.table(round(rbind(keep,release)/1000,3),file = "rec-catch.out", row.names = FALSE, col.names = FALSE)
  #write(with(aggregate_prediction_output,observed_trips),file = "rec-catch.out", append = TRUE)
  write.table(mulen,file = "rec-catch.out", append = TRUE, row.names = FALSE, col.names = FALSE)
  
  extra_output <- prediction_output_by_period %>% 
    list.stack(fill = TRUE) %>% 
    mutate_at(vars(contains("length")), replace_na, replace = 0)
  
  pred_len2 <- tibble(extra_output) %>% 
    dplyr::select(state, contains("length")) %>% 
    pivot_longer(cols = 2:ncol(.), names_to = "bin",values_to = "num") %>% 
    separate(bin, into =c("type","len"),sep = "_length_") %>% 
    mutate(len = as.numeric(len))
  pred_len <- pred_len2 %>% 
    group_by(state, type) %>%
    summarize(mulen = sum(len*num)/sum(num), 
              .groups = "drop") %>% 
    pivot_wider(names_from = type,
                names_glue = "mulen_{type}",
                values_from = mulen)
  
  biglen <- pred_len2 %>% 
    filter(type == "keep") %>% 
    mutate(numbig = ifelse(len>=28,num,0))  %>% 
    group_by(state) %>% 
    summarize(fracbig = sum(numbig)/sum(num))
  pred_len <- left_join(pred_len, biglen, by = c("state"))  
  
  extra_output <- extra_output %>% 
    select(state, tot_keep, tot_rel, observed_trips, n_choice_occasions, change_CS, cost, keep_one) %>% 
    group_by(state) %>% 
    summarize_if(is.numeric, .funs = sum,na.rm=TRUE) %>% 
    left_join(pred_len, by = c("state"))    
  RDMoutput_edit[[x]] <- extra_output %>% mutate(Nsim = x, SeasonLen = filltable$SeasonLen[x], Bag = filltable$Bag[x],
                                                 MinLen = filltable$MinLen[x], SSBcov = SSB[x])
  write.table(extra_output,file = "rec-catch.out", append = TRUE, row.names = FALSE, col.names = FALSE)
}
#View(RDMoutput_edit[[x]])
#AGGREGATE OUTPUT 

RDMoutputbind_edit <- select(bind_rows(RDMoutput_edit), state, tot_keep,  SeasonLen, Bag, MinLen, SSBcov, tot_rel)
sum(RDMoutputbind_edit$tot_rel)
#View(RDMoutputbind_edit)
#saveRDS(RDMoutputbind_edit, "~/Desktop/FlounderMSE/RDMgam/RDMoutputbind_SSBscramble.rds") #trial 1 <- use this

############################# FIT MODEL ########################################
library(mgcv)
library(gratia)
#library(gamreg)
#RDMoutputbind_edit <- readRDS("~/Desktop/FlounderMSE/RDMgam/RDMoutputbind_SSBscramble.rds") #trial 1 <- use this 
#RDMoutputbind_edit <- readRDS("~/Desktop/FlounderMSE/RDMgam/RDMoutputbind_superscramble.rds")

tot_keep <- RDMoutputbind_edit$tot_keep
tot_rel <- RDMoutputbind_edit$tot_rel
state <- RDMoutputbind_edit$state
SeasonLen <- RDMoutputbind_edit$SeasonLen
Bag <- RDMoutputbind_edit$Bag
MinLen <- RDMoutputbind_edit$MinLen
SSB <- RDMoutputbind_edit$SSBcov

inputdata <- data.frame(tot_keep, tot_rel,
                        state, SeasonLen, Bag, MinLen, SSB) 
#View(RDMoutputbind_edit)
#hist(inputdata$tot_rel)

# --------------------------------------- GAM -----------------------------------------------#
g1 <- gam(tot_keep ~ state + s(SSB, k = 3) + s(SeasonLen, k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
            s(Bag, k = 3) + s(MinLen, k = 3), data = inputdata, 
          family = Gamma(link = log), method = "REML") 
summary(g1)
gam.check(g1)
#saveRDS(g1, "~/Desktop/FlounderMSE/RDMgam/gam_RDMland.rds") #disordered length
#g1 <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDMland.rds") #disordered length

# --------------------------------------- GAM without SSB to compare ------------------------------ #
#g1.1 <- gam(tot_keep ~ state + s(SeasonLen, k = 3) + #9,5,7
#            s(Bag, k = 3) + s(MinLen, k = 3), data = inputdata, 
#          family = Gamma(link = log), method = "REML") 
#saveRDS(g1.1, "~/Desktop/FlounderMSE/RDMgam/gam_RDMland_noSSB.rds") #disordered length
#g1.1 <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDMland_noSSB.rds") #disordered length

# -------------------------------- Poisson w/ smooth ---------------------------------------#
#g4 <- gam(as.integer(tot_keep) ~ state + s(SSB, k = 3) + s(SeasonLen, k = 3) + #9,5,7
#            s(Bag, k = 3) + s(MinLen, k=3), data = inputdata, 
#          family = poisson(link = log), method = "REML")  
#summary(g4)
#gam.check(g4)

# -------------------- gaussian log-transformed response w/ smooth -------------------------#
#g6 <- gam(log(tot_keep) ~ state + s(SSB, k = 3) + s(SeasonLen, k = 3) + #9,5,7
#            s(Bag, k = 3) + s(MinLen, k=3), data = inputdata, 
#          family = gaussian(), method = "REML") 
#summary(g6)
#gam.check(g6)

# ------------------------------- DISC GAM 1-------------------------------------------------#
d1 <- gam(tot_rel ~  state + s(SSB, k = 3) + s(SeasonLen, k = 3) + 
            s(Bag, k = 3) + s(MinLen, k = 3), data = inputdata, 
          family = Gamma(link = log), method = "REML") 
summary(d1)
gam.check(d1)
#saveRDS(d1, "~/Desktop/FlounderMSE/RDMgam/gam_RDM_disc.rds") #disordered length
#d1 <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDM_disc.rds") #disordered length

################### DISC GAM TROUBLESHOOTING ######################

#extracting data for each state 
inputdataVA <- inputdatadisc %>% filter(state == "VA")
inputdataNC <- inputdatadisc %>% filter(state == "NC")
inputdataMD <- inputdatadisc %>% filter(state == "MD")
inputdataMA <- inputdatadisc %>% filter(state == "MA")
inputdataCT <- inputdatadisc %>% filter(state == "CT")
inputdataDE <- inputdatadisc %>% filter(state == "DE")
inputdataRI <- inputdatadisc %>% filter(state == "RI")
inputdataNY <- inputdatadisc %>% filter(state == "NY")
inputdataNJ <- inputdatadisc %>% filter(state == "NJ")
target <- c("NC", "CT", "DE", "RI", "NY", "NJ")

#looking at partial effects in residual distributions

#discard predictions
gamfit <- predict(d2)
resid2 <- residuals(d2, type = c("deviance"))

#visualize
pred <- gamfit #change this depending on model 
simdata <- data.frame(inputdata$Bag, pred, resid2) 

#bag
simdata4 <- simdata %>% filter(inputdata.Bag == 4)
simdata5 <- simdata %>% filter(inputdata.Bag == 5)
simdata6 <- simdata %>% filter(inputdata.Bag == 6)
simdata7 <- simdata %>% filter(inputdata.Bag == 7)
simdata8 <- simdata %>% filter(inputdata.Bag == 8)

#length
simdata4 <- simdata %>% filter(inputdata$MinLen == 14)
simdata5 <- simdata %>% filter(inputdata$MinLen == 15)
simdata6 <- simdata %>% filter(inputdata$MinLen == 16)
simdata7 <- simdata %>% filter(inputdata$MinLen == 17)
simdata8 <- simdata %>% filter(inputdata$MinLen == 18)
simdata9 <- simdata %>% filter(inputdata$MinLen == 19)
simdata10 <- simdata %>% filter(inputdata$MinLen == 20)

#seasonlen
simdata4 <- simdata %>% filter(inputdata$SeasonLen == 60)
simdata5 <- simdata %>% filter(inputdata$SeasonLen == 90)
simdata6 <- simdata %>% filter(inputdata$SeasonLen == 120)
simdata7 <- simdata %>% filter(inputdata$SeasonLen == 150)
simdata8 <- simdata %>% filter(inputdata$SeasonLen == 210)
simdata9 <- simdata %>% filter(inputdata$SeasonLen == 240)
simdata10 <- simdata %>% filter(inputdata$SeasonLen == 270)
simdata11 <- simdata %>% filter(inputdata$SeasonLen == 300)

#state
simdataNC <- simdata %>% filter(inputdatadisc$state == "NC")
simdataVA <- simdata %>% filter(inputdatadisc$state == "VA")
simdataNJ <- simdata %>% filter(inputdatadisc$state == "NJ")
simdataNY <- simdata %>% filter(inputdatadisc$state == "NY")
simdataCT <- simdata %>% filter(inputdatadisc$state == "CT")
simdataDE <- simdata %>% filter(inputdatadisc$state == "DE")
simdataMA <- simdata %>% filter(inputdatadisc$state == "MA")
simdataRI <- simdata %>% filter(inputdatadisc$state == "RI")
simdataMD <- simdata %>% filter(inputdatadisc$state == "MD")

#plot, just change data names or composition 
plot(resid2~pred, data = simdataNC, xlim = c(10,21), ylim = c(-2,2), xlab = "Linear Predictor", ylab = "Deviance Residuals")
points(resid2~pred, data = simdataVA, col= "blue")
points(resid2~pred, data = simdataNJ, col= "red")
points(resid2~pred, data = simdataNY, col= "yellow")
points(resid2~pred, data = simdataCT, col= "green")
points(resid2~pred, data = simdataDE, col= "orange")
points(resid2~pred, data = simdataMA, col= "purple")
points(resid2~pred, data = simdataRI, col= "pink")
points(resid2~pred, data = simdataMD, col= "brown")
legend(20, 1, legend=c("NC","VA", "NJ","NY","CT", "DE","MA", "RI", "MD"),  
       fill = c("black","blue", "red", "yellow", "green", "orange", "purple",
                "pink", "brown") )

#incorporating size structure into discards GAM

Nlen <- 42

omlength2978111 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2020.dat",n=Nlen+1)} #29781.11
omlength2978111_nodate <- omlength2978111[2:43]
omlength2978111_sum <- sum(omlength2978111[2:43])
omlength2978111_nodatedata <- as.data.frame(as.list(omlength2978111_nodate))
colnames(omlength2978111_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength2978111_nodatedata <- omlength2978111_nodatedata %>% mutate(SSBcov = 29781.11)

omlength4233281 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2021.dat",n=Nlen+1)}  #42332.81
omlength4233281_nodate <- omlength4233281[2:43]
omlength4233281_sum <- sum(omlength4233281[2:43])
omlength4233281_nodatedata <- as.data.frame(as.list(omlength4233281_nodate))
colnames(omlength4233281_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength4233281_nodatedata <- omlength4233281_nodatedata %>% mutate(SSBcov = 42332.81)

omlength5668865 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2022.dat",n=Nlen+1)}  #56688.65
omlength5668865_nodate <- omlength5668865[2:43]
omlength5668865_sum <- sum(omlength5668865[2:43])
omlength5668865_nodatedata <- as.data.frame(as.list(omlength5668865_nodate))
colnames(omlength5668865_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength5668865_nodatedata <- omlength5668865_nodatedata %>% mutate(SSBcov = 56688.65)

omlength6450683 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2039.dat",n=Nlen+1)}  #64506.83 
omlength6450683_nodate <- omlength6450683[2:43]
omlength6450683_sum <- sum(omlength6450683[2:43])
omlength6450683_nodatedata <- as.data.frame(as.list(omlength6450683_nodate))
colnames(omlength6450683_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength6450683_nodatedata <- omlength6450683_nodatedata %>% mutate(SSBcov = 64506.83)

omlength7623342 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2024.dat",n=Nlen+1)}   #76233.42        
omlength7623342_nodate <- omlength7623342[2:43]
omlength7623342_sum <- sum(omlength7623342[2:43])
omlength7623342_nodatedata <- as.data.frame(as.list(omlength7623342_nodate))
colnames(omlength7623342_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength7623342_nodatedata <- omlength7623342_nodatedata %>% mutate(SSBcov = 76233.42)

omlength8636419 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2027.dat",n=Nlen+1)}  #86364.19
omlength8636419_nodate <- omlength8636419[2:43]
omlength8636419_sum <- sum(omlength8636419[2:43])
omlength8636419_nodatedata <- as.data.frame(as.list(omlength8636419_nodate))
colnames(omlength8636419_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength8636419_nodatedata <- omlength8636419_nodatedata %>% mutate(SSBcov = 86364.19)

omlength9226567 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength/om-length2045.dat",n=Nlen+1)}  #92265.67 
omlength9226567_nodate <- omlength9226567[2:43]
omlength9226567_sum <- sum(omlength9226567[2:43])
omlength9226567_nodatedata <- as.data.frame(as.list(omlength9226567_nodate))
colnames(omlength9226567_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength9226567_nodatedata <- omlength9226567_nodatedata %>% mutate(SSBcov = 92265.67)

omlength10191523 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2025.dat",n=Nlen+1)}  #101915.23 2025
omlength10191523_nodate <- omlength10191523[2:43]
omlength10191523_sum <- sum(omlength10191523[2:43])
omlength10191523_nodatedata <- as.data.frame(as.list(omlength10191523_nodate))
colnames(omlength10191523_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength10191523_nodatedata <- omlength10191523_nodatedata %>% mutate(SSBcov = 101915.23)

omlength11028124 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2045.dat",n=Nlen+1)}  #110281.24 2045
omlength11028124_nodate <- omlength11028124[2:43]
omlength11028124_sum <- sum(omlength11028124[2:43])
omlength11028124_nodatedata <- as.data.frame(as.list(omlength11028124_nodate))
colnames(omlength11028124_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength11028124_nodatedata <- omlength11028124_nodatedata %>% mutate(SSBcov = 110281.24)

omlength15247371 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2037.dat",n=Nlen+1)}  #152473.71 2037 
omlength15247371_nodate <- omlength15247371[2:43]
omlength15247371_sum <- sum(omlength15247371[2:43])
omlength15247371_nodatedata <- as.data.frame(as.list(omlength15247371_nodate))
colnames(omlength15247371_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength15247371_nodatedata <- omlength15247371_nodatedata %>% mutate(SSBcov = 152473.71)

omlength16704276 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2032.dat",n=Nlen+1)}  #167042.76 2032
omlength16704276_nodate <- omlength16704276[2:43]
omlength16704276_sum <- sum(omlength16704276[2:43])
omlength16704276_nodatedata <- as.data.frame(as.list(omlength16704276_nodate))
colnames(omlength16704276_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength16704276_nodatedata <- omlength16704276_nodatedata %>% mutate(SSBcov = 167042.76)

omlength18125726 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2026.dat",n=Nlen+1)}  #181257.26 2026  
omlength18125726_nodate <- omlength18125726[2:43]
omlength18125726_sum <- sum(omlength18125726[2:43])
omlength18125726_nodatedata <- as.data.frame(as.list(omlength18125726_nodate))
colnames(omlength18125726_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength18125726_nodatedata <- omlength18125726_nodatedata %>% mutate(SSBcov = 181257.26)

omlength21039415 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2027.dat",n=Nlen+1)}  #210394.15 2027
omlength21039415_nodate <- omlength21039415[2:43]
omlength21039415_sum <- sum(omlength21039415[2:43])
omlength21039415_nodatedata <- as.data.frame(as.list(omlength21039415_nodate))
colnames(omlength21039415_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength21039415_nodatedata <- omlength21039415_nodatedata %>% mutate(SSBcov = 210394.15)

omlength22546028 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2028.dat",n=Nlen+1)}  #225460.28 2028 
omlength22546028_nodate <- omlength22546028[2:43]
omlength22546028_sum <- sum(omlength22546028[2:43])
omlength22546028_nodatedata <- as.data.frame(as.list(omlength22546028_nodate))
colnames(omlength22546028_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength22546028_nodatedata <- omlength22546028_nodatedata %>% mutate(SSBcov = 225460.28)

omlength25461289 <- {scan("~/Desktop/FlounderMSE/RDMgam/storelengths/sim_storelength_contrast/om-length2034.dat",n=Nlen+1)} #254612.89 2034
omlength25461289_nodate <- omlength25461289[2:43]
omlength25461289_sum <- sum(omlength25461289[2:43])
omlength25461289_nodatedata <- as.data.frame(as.list(omlength25461289_nodate))
colnames(omlength25461289_nodatedata) <- paste0( seq(10, 92, by = 2))
omlength25461289_nodatedata <- omlength25461289_nodatedata %>% mutate(SSBcov = 254612.89)

#FOR NUMBER OF FISH, PROPORTIONS OF FISH LOWER THAN MIN SIZE
omlengthdata <- rbind(omlength2978111_nodatedata, omlength4233281_nodatedata,
                      omlength5668865_nodatedata, omlength6450683_nodatedata,
                      omlength7623342_nodatedata, omlength8636419_nodatedata,
                      omlength9226567_nodatedata, omlength10191523_nodatedata,
                      omlength11028124_nodatedata, omlength15247371_nodatedata,
                      omlength16704276_nodatedata, omlength18125726_nodatedata,
                      omlength21039415_nodatedata, omlength22546028_nodatedata,
                      omlength25461289_nodatedata)

RDMoutputbind_disc1 <- left_join(RDMoutputbind_edit, omlengthdata, by = c("SSBcov"))

RDMoutputbind_disc2 <- RDMoutputbind_disc1 %>% mutate(LengthSize = ifelse(
  SSBcov == 29781.11, omlength2978111_sum, ifelse(
    SSBcov == 42332.81, omlength4233281_sum, ifelse(
      SSBcov == 56688.65, omlength5668865_sum, ifelse(
        SSBcov == 64506.83, omlength6450683_sum, ifelse(
          SSBcov == 76233.42, omlength7623342_sum, ifelse(
            SSBcov == 86364.19, omlength8636419_sum, ifelse(
              SSBcov == 92265.67, omlength9226567_sum, ifelse(
                SSBcov == 101915.23, omlength10191523_sum, ifelse(
                  SSBcov == 110281.24, omlength11028124_sum, ifelse(
                    SSBcov == 152473.71, omlength15247371_sum, ifelse(
                      SSBcov == 167042.76, omlength16704276_sum, ifelse(
                        SSBcov == 181257.26, omlength18125726_sum, ifelse(
                          SSBcov == 210394.15, omlength21039415_sum, ifelse(
                            SSBcov == 225460.28, omlength22546028_sum, ifelse(
                              SSBcov == 254612.89, omlength25461289_sum, 0))))))))))))))))
#View(RDMoutputbind_disc4)

#NUMBER UNDER MINSIZE
RDMoutputbind_disc3 <- RDMoutputbind_disc2 %>% mutate(SumRel = ifelse(
  MinLen == 14, rowSums(RDMoutputbind_disc2[, 8:21], na.rm = TRUE), ifelse(
    MinLen == 15, rowSums(RDMoutputbind_disc2[, 8:22], na.rm = TRUE), ifelse(
      MinLen == 16, rowSums(RDMoutputbind_disc2[, 8:23], na.rm = TRUE), ifelse(
        MinLen == 17, rowSums(RDMoutputbind_disc2[, 8:25], na.rm = TRUE), ifelse(
          MinLen == 18, rowSums(RDMoutputbind_disc2[, 8:26], na.rm = TRUE), ifelse(
            MinLen == 19, rowSums(RDMoutputbind_disc2[, 8:27], na.rm = TRUE), ifelse(
              MinLen == 20, rowSums(RDMoutputbind_disc2[, 8:28], na.rm = TRUE), NA
              ))))))))

#MED LENGTH AT MATURITY FROM 2013 BENCHMARK STOCK ASSESSMENT - 26.8cm, 10.5 inches
RDMoutputbind_disc4 <- RDMoutputbind_disc3 %>% mutate(PropRel = SumRel/LengthSize,
                                                      LenMat = rowSums(RDMoutputbind_disc3[,8:16], na.rm = TRUE),
                                                      PropLenMat = LenMat/LengthSize)

#SUMMED SIZE STRUCTURE ONLY 
RDMoutputbind_disc <- RDMoutputbind_edit %>% mutate(LengthSize = ifelse(
  SSBcov == 29781.11, omlength2978111_sum, ifelse(
    SSBcov == 42332.81, omlength4233281_sum, ifelse(
      SSBcov == 56688.65, omlength5668865_sum, ifelse(
        SSBcov == 64506.83, omlength6450683_sum, ifelse(
          SSBcov == 76233.42, omlength7623342_sum, ifelse(
            SSBcov == 86364.19, omlength8636419_sum, ifelse(
              SSBcov == 92265.67, omlength9226567_sum, ifelse(
                SSBcov == 101915.23, omlength10191523_sum, ifelse(
                  SSBcov == 110281.24, omlength11028124_sum, ifelse(
                    SSBcov == 152473.71, omlength15247371_sum, ifelse(
                      SSBcov == 167042.76, omlength16704276_sum, ifelse(
                        SSBcov == 181257.26, omlength18125726_sum, ifelse(
                          SSBcov == 210394.15, omlength21039415_sum, ifelse(
                            SSBcov == 225460.28, omlength22546028_sum, ifelse(
                              SSBcov == 254612.89, omlength25461289_sum, 0))))))))))))))))

#for summed size structure only 
tot_rel <- RDMoutputbind_disc$tot_rel
state <- RDMoutputbind_disc$state
SeasonLen <- RDMoutputbind_disc$SeasonLen
Bag <- RDMoutputbind_disc$Bag
MinLen <- RDMoutputbind_disc$MinLen
SSB <- RDMoutputbind_disc$SSBcov
LengthSize <- RDMoutputbind_disc$LengthSize

#for sum or proportion below min size, size at maturity  
tot_rel <- RDMoutputbind_disc4$tot_rel
state <- RDMoutputbind_disc4$state
SeasonLen <- RDMoutputbind_disc4$SeasonLen
Bag <- RDMoutputbind_disc4$Bag
MinLen <- RDMoutputbind_disc4$MinLen
SSB <- RDMoutputbind_disc4$SSBcov
LengthSize <- RDMoutputbind_disc4$LengthSize
SumRel <- RDMoutputbind_disc4$SumRel
PropRel <- RDMoutputbind_disc4$PropRel
LenMat <- RDMoutputbind_disc4$LenMat
PropLenMat <- RDMoutputbind_disc4$PropLenMat

#make data frame 
inputdatadisc <- data.frame(tot_rel,
                        state, SeasonLen, Bag, MinLen, SSB, LengthSize, SumRel, PropRel, LenMat, PropLenMat) 


# ------------------------------- DISC GAM 2 SUMMED LENGTHS -------------------------------------------------#
d2 <- gam(tot_rel ~  state + s(SSB, k = 3) + s(SeasonLen, k = 3) + 
            s(Bag, k = 3) + s(MinLen, k = 3) + s(LengthSize, k = 3), data = inputdatadisc, 
          family = Gamma(link = log), method = "REML") 
summary(d2) #summed length bins
#gam.check(d2)
appraise(d2)
draw(d2)

# ------------------------------- DISC GAM 3 SUMMED LENGTHS <MIN SIZE-------------------------------------------------#
d3 <- gam(tot_rel ~  state + s(SSB, k = 3) + s(SeasonLen, k = 3) + 
            s(Bag, k = 3) + s(MinLen, k = 3) + s(SumRel, k = 3), data = inputdatadisc, 
          family = Gamma(link = log), method = "REML") 
summary(d3) #summed length bins ~ =< a given minimum size 
#gam.check(d3)
appraise(d3)
draw(d3)
#saveRDS(d3, "~/Desktop/FlounderMSE/RDMgam/gam_RDM_disc_superscramble12424.rds") #disordered length
#d3 <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDM_disc_superscramble12424.rds") #disordered length

# ------------------------------- DISC GAM 4 PROPORTION <MIN SIZE-------------------------------------------------#
d4 <- gam(tot_rel ~  state + s(SSB, k = 3) + s(SeasonLen, k = 3) + 
           s(Bag, k = 3) + s(MinLen, PropRel), data = inputdatadisc, #interaction seems to reduce confounding between two variables
          family = Gamma(link = log), method = "REML") 
summary(d4) #proportion of length bins ~ =< a given minimum size 
#gam.check(d4)
appraise(d4)
library(ggplot2)
smooth_plot <- draw(d4) +
  scale_x_continuous(trans = "log") 
#interaction between minlen and proprel makes sense, but doesn't fix seasonlen issue

# ------------------------------- DISC GAM 5 SUM <SIZE AT MATURITY -------------------------------------------------#
d5 <- gam(tot_rel ~  state + s(SSB, k = 3) + s(SeasonLen, k = 3) + 
            s(Bag, k = 3) + s(MinLen, k = 3) + s(LenMat, k = 3), data = inputdatadisc, 
          family = Gamma(link = log), method = "REML") 
summary(d5) #proportion of length bins ~ =< size at maturity  
#gam.check(d5)
appraise(d5)
draw(d5)
#this doesn't improve anything -> in fact makes fit worse

# ------------------------------- DISC GAM 6 SUM <SIZE AT MATURITY -------------------------------------------------#
d6 <- gam(tot_rel ~  state + s(SSB, k = 3) + s(SeasonLen, k = 3) + 
            s(Bag, k = 3) + s(MinLen, PropLenMat), data = inputdatadisc, 
          family = Gamma(link = log), method = "REML") 
summary(d6) #proportion of length bins ~ =< size at maturity  
#gam.check(d6)
appraise(d6) #a little better than d5 with and without interaction
draw(d6) +
  scale_x_continuous(trans = "log") 

################### --------------- PREDICTIONS ---------------- #####################

#WITHIN SAMPLE 

#land 
preddata1 <- dplyr::select(inputdata, state, SeasonLen, Bag, MinLen, SSB)
gamfit2 <- predict.gam(g1, newdata = preddata1, type = "link" , se.fit = TRUE)

#disc
preddata2 <- dplyr::select(inputdatadisc, state, SeasonLen, Bag, MinLen, SSB, SumRel)
gamfit3 <- predict.gam(d3, newdata = preddata2, type = "link" , se.fit = TRUE)

#OUT OF SAMPLE

#LOOCV
n <- length(inputdata$Bag)
loo_predictions <- numeric(n)

data <- inputdata

# Perform LOOCV LAND
for (i in 1:n) {
  train_data <- data[-i, ]
  test_data <- data[i, , drop = FALSE]
  gam_model <- gam(tot_keep ~ state + s(SSB, k = 3) + s(SeasonLen, k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
                     s(Bag, k = 3) + s(MinLen, k = 3), data = train_data, 
                   family = Gamma(link = log), method = "REML")
  loo_predictions[i] <- predict(gam_model, newdata = test_data)
}
#saveRDS(loo_predictions, "~/Desktop/FlounderMSE/LOOCV_g1.rds")
loo_predictions <- readRDS("~/Desktop/FlounderMSE/LOOCV_g1.rds") #very similar but not exactly the same as in-sample

# performance metrics 
mse <- mean((loo_predictions - log(data$tot_keep))^2) #keep in logspace since model was fit in logspace
rmse <- sqrt(mse)
mae <- mean(abs(loo_predictions - log(data$tot_keep)))

#residuals and predictions
residuals <- data$tot_keep - exp(loo_predictions)
plot(residuals ~data$tot_keep)

plot(data$tot_keep~exp(gamfit2$fit))
plot(data$tot_keep~exp(loo_predictions))

data <- inputdatadisc

# Perform LOOCV DISC
for (i in 1:n) {
  train_data <- data[-i, ]
  test_data <- data[i, , drop = FALSE]
  gam_model <- gam(tot_rel ~  state + s(SSB, k = 3) + s(SeasonLen, k = 3) + 
                     s(Bag, k = 3) + s(MinLen, k = 3) + s(SumRel, k = 3), data = train_data, 
                   family = Gamma(link = log), method = "REML")
  loo_predictions[i] <- predict(gam_model, newdata = test_data[2:9])
}
#saveRDS(loo_predictions, "~/Desktop/FlounderMSE/LOOCV_d3.rds")
loo_predictions_disc <- readRDS("~/Desktop/FlounderMSE/LOOCV_d3.rds")

# performance metrics 
mse <- mean((loo_predictions_disc - log(data$tot_rel))^2) #keep in logspace since model was fit in logspace
rmse <- sqrt(mse)
mae <- mean(abs(loo_predictions_disc - log(data$tot_rel)))

#residuals and predictions
residuals <- data$tot_rel - exp(loo_predictions_disc)
plot(residuals ~data$tot_rel)

plot(data$tot_rel~exp(gamfit3$fit))
plot(data$tot_rel~exp(loo_predictions_disc))

#DIAGNOSTICS AND PARTIAL EFFECTS 

#land
appraise(g1)
draw(g1)

#disc
appraise(d1)
draw(d1)

#for logged response
#plot(inputdata$tot_keep ~ exp(gamfit2$fit))

############# FOR DETERMINING SCALARS FOR do_recmeasures_hcr.R ##########################################

scalarinput <- dplyr::select(inputdata[100:108,], state, SeasonLen, Bag, MinLen, SSB) # pick number where "CT" is first state in list 
scalaroutput = list()
scalaroutput = vector("list", length = nrow(scalarinput))

for(x in 1:length(scalarinput$SeasonLen)){
scalarinput2 <- scalarinput[x,]
gamfit_scalar <- predict.gam(g1, newdata = scalarinput2, type = "response" , se.fit = TRUE)
scalaroutput[[x]] <- scalarinput[x,] %>% mutate(land = gamfit_scalar$fit)
}
scalaroutput2 <- bind_rows(scalaroutput)
  
CTscalar <- scalaroutput2$land[1]/sum(scalaroutput2$land) #scalarinput states should start with "CT"
DEscalar <- scalaroutput2$land[2]/sum(scalaroutput2$land)
MAscalar <- scalaroutput2$land[3]/sum(scalaroutput2$land)
MDscalar <- scalaroutput2$land[4]/sum(scalaroutput2$land)
NCscalar <- scalaroutput2$land[5]/sum(scalaroutput2$land)
NJscalar <- scalaroutput2$land[6]/sum(scalaroutput2$land)
NYscalar <- scalaroutput2$land[7]/sum(scalaroutput2$land)
RIscalar <- scalaroutput2$land[8]/sum(scalaroutput2$land)
VAscalar <- scalaroutput2$land[9]/sum(scalaroutput2$land)

GAMscalar <- data.frame(CTscalar, DEscalar, MAscalar, MDscalar, NCscalar, NJscalar, NYscalar, RIscalar, VAscalar)
#so far these values have been the same no matter which SSB or regulation scenario we start on 
#saveRDS(GAMscalar, "~/Desktop/FlounderMSE/GAMscalar.rds")
#readRDS("~/Desktop/FlounderMSE/GAMscalar.rds")

#ORIGINAL GAMS FOR COMPARISON

#land
#gamland <- readRDS("~/Desktop/FlounderMSE/gam_land.rds")
#appraise(gamland)
#draw(gamland)

#disc
#gamdisc <- readRDS("~/Desktop/FlounderMSE/gam_disc.rds")
#appraise(gamdisc)
#draw(gamdisc)
