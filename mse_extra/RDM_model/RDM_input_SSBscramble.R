library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)
library(gratia)
library(ggplot2)

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
  
  # READ IN LENGTH DISTRIBUTION FOR EACH SET OF REGULATIONS x<-15
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
#sum(RDMoutputbind_edit$tot_rel)
#View(RDMoutputbind_edit)
#saveRDS(RDMoutputbind_edit, "~/Desktop/FlounderMSE/RDMgam/RDMoutputbind_SSBscramble.rds") #trial 1 <- use this

############################# FIT MODEL ########################################
library(mgcv)
library(gratia)
#library(gamreg)
#RDMoutputbind_edit <- readRDS("~/Desktop/FlounderMSE/RDMgam/RDMoutputbind_SSBscramble.rds") #trial 1 <- use this 

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

reg_option <- readRDS("~/Desktop/FlounderMSE/mafmc-recmeasures/mse/regulations_option1.rds")

# --------------------------------------- GAM -----------------------------------------------#
g1 <- gam(tot_keep ~ state + s(SSB, k = 3) + s(SeasonLen, k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
            s(Bag, k = 3) + s(MinLen, k = 3), data = inputdata, 
          family = Gamma(link = log), method = "REML") #random effect on spline for minlen that introduces  a sort of random effect by state 
summary(g1)
gam.check(g1)
#saveRDS(g1, "~/Desktop/FlounderMSE/RDMgam/gam_RDMland.rds") #disordered length
#g1 <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDMland.rds") #disordered length
#draw(g1)

g1pred <- predict(g1, type = "response") #if not 
plot(inputdata$tot_keep ~ g1pred)
plot(inputdata$tot_keep ~ g1pred, xlim = c(0,1e7), ylim = c(0,1e7)) + abline(b = 1, a = 0, col = "red")

########## FACET RDM GAM 1 BY STATE AND REGULATIONS ################

#for substituting with hierarchical GAM
#inputdata <- inputdata_sampled
#g1pred <- g1.3pred

RDMoutputscramble_facet <- inputdata %>% mutate(Pred = g1.1pred)

#state
ggplot(RDMoutputscramble_facet, mapping=aes(x=Pred, y = tot_keep, col = state)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,2e7) +
  ylim(0,2e7) + 
  xlab("Predicted Harvest") + 
  ylab("Observed Harvest") + 
  ggtitle("Mapping Harvest Estimates to Recreational Regulations") +
  facet_wrap(~state, scales = "free_y")

#Bag
ggplot(RDMoutputscramble_facet, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facet, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~SSB, scales = "free_y")


############ LOOK AT RI and CT RDM GAM 1 FIT ONLY ################## 

#RI only 

RDMoutputscramble_facetRI <- inputdata %>% mutate(Pred = g1.1pred) %>% filter(state == "RI") 

#Bag
ggplot(RDMoutputscramble_facetRI, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facetRI, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facetRI, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facetRI, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~SSB, scales = "free_y")

#CT only 

RDMoutputscramble_facetCT <- inputdata %>% mutate(Pred = g1.3pred) %>% filter(state == "CT") 

#Bag
ggplot(RDMoutputscramble_facetCT, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facetCT, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facetCT, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facetCT, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~SSB, scales = "free_y")

############# ZOOM IN ON STATES WITH SMALL RANGES NOT VERY VISIBLE IN RDM GAM 1 STATE FACET ##############

#NC 

RDMoutputscramble_facetNC <- inputdata %>% mutate(Pred = g1pred) %>% filter(state == "NC") 

#Bag
ggplot(RDMoutputscramble_facetNC, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e5) +
  ylim(0,3e5) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facetNC, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e5) +
  ylim(0,3e5) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facetNC, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e5) +
  ylim(0,3e5) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facetNC, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e5) +
  ylim(0,3e5) +
  facet_wrap(~SSB, scales = "free_y")

#MA

RDMoutputscramble_facetMA <- inputdata %>% mutate(Pred = g1pred) %>% filter(state == "MA") 

#Bag
ggplot(RDMoutputscramble_facetMA, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,2.5e6) +
  ylim(0,2.5e6) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facetMA, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,2.5e6) +
  ylim(0,2.5e6) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facetMA, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,2.5e6) +
  ylim(0,2.5e6) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facetMA, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,2.5e6) +
  ylim(0,2.5e6) +
  facet_wrap(~SSB, scales = "free_y")

#DE

RDMoutputscramble_facetDE <- inputdata %>% mutate(Pred = g1pred) %>% filter(state == "DE") 

#Bag
ggplot(RDMoutputscramble_facetDE, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,2.5e6) +
  ylim(0,2.5e6) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facetDE, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,2.5e6) +
  ylim(0,2.5e6) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facetDE, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,2.5e6) +
  ylim(0,2.5e6) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facetDE, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,2.5e6) +
  ylim(0,2.5e6) +
  facet_wrap(~SSB, scales = "free_y")


############## MEAN SQUARED ERROR ANALYSIS OF RDM GAM 1 OUTPUT ##################

#response space 

RDMoutputscramble_facet_mse <- RDMoutputscramble_facet %>% mutate(mse = (tot_keep - Pred)^2)

upper_quartile_mse <- quantile(RDMoutputscramble_facet_mse$mse, probs = 0.75)

RDMoutputscramble_facet_mse_upperquartile <- RDMoutputscramble_facet_mse %>% filter(mse > upper_quartile_mse)

#state
ggplot(RDMoutputscramble_facet_mse_upperquartile, mapping=aes(y = tot_keep, x = Pred, col = state)) + 
  geom_point() + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

# get state quartiles 
state_quartiles <- RDMoutputscramble_facet_mse %>%
  group_by(state) %>%
  summarise(
    state_quart = quantile(mse, probs = 0.75)
  )

CTstatequart <- state_quartiles[1,]$state_quart
DEstatequart <- state_quartiles[2,]$state_quart
MAstatequart <- state_quartiles[3,]$state_quart
MDstatequart <- state_quartiles[4,]$state_quart
NCstatequart <- state_quartiles[5,]$state_quart
NJstatequart <- state_quartiles[6,]$state_quart
NYstatequart <- state_quartiles[7,]$state_quart
RIstatequart <- state_quartiles[8,]$state_quart
VAstatequart <- state_quartiles[9,]$state_quart

#CT
RDMoutputscramble_facet_mse_CT <- RDMoutputscramble_facet_mse %>% filter(state == "CT") %>%
  filter(mse > CTstatequart)
  
  #Bag
  ggplot(RDMoutputscramble_facet_mse_CT, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) + 
    xlim(0,1e7) +
    ylim(0,1e7) +
    facet_wrap(~Bag, scales = "free_y")
  
  #MinLen
  ggplot(RDMoutputscramble_facet_mse_CT, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) + 
    xlim(0,1e7) +
    ylim(0,1e7) +
    facet_wrap(~MinLen, scales = "free_y")
  
  #SeasonLen
  ggplot(RDMoutputscramble_facet_mse_CT, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) + 
    xlim(0,1e7) +
    ylim(0,1e7) +
    facet_wrap(~SeasonLen, scales = "free_y")
  
  #SSB
  ggplot(RDMoutputscramble_facet_mse_CT, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) + 
    xlim(0,1e7) +
    ylim(0,1e7) +
    facet_wrap(~SSB, scales = "free_y")
  
hist(RDMoutputscramble_facet_mse_CT$Bag)
hist(RDMoutputscramble_facet_mse_CT$MinLen)
hist(RDMoutputscramble_facet_mse_CT$SeasonLen)
hist(RDMoutputscramble_facet_mse_CT$SSB)

#DE
RDMoutputscramble_facet_mse_DE <- RDMoutputscramble_facet_mse %>% filter(state == "DE") %>%
  filter(mse > DEstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_DE, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_DE, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_DE, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facet_mse_DE, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~SSB, scales = "free_y")

hist(RDMoutputscramble_facet_mse_DE$Bag)
hist(RDMoutputscramble_facet_mse_DE$MinLen)
hist(RDMoutputscramble_facet_mse_DE$SeasonLen)
hist(RDMoutputscramble_facet_mse_DE$SSB)

#MA
RDMoutputscramble_facet_mse_MA <- RDMoutputscramble_facet_mse %>% filter(state == "MA") %>%
  filter(mse > MAstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_MA, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_MA, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_MA, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facet_mse_MA, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~SSB, scales = "free_y")

hist(RDMoutputscramble_facet_mse_MA$Bag)
hist(RDMoutputscramble_facet_mse_MA$MinLen)
hist(RDMoutputscramble_facet_mse_MA$SeasonLen)
hist(RDMoutputscramble_facet_mse_MA$SSB)

#MA
RDMoutputscramble_facet_mse_NC <- RDMoutputscramble_facet_mse %>% filter(state == "NC") %>%
  filter(mse > NCstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_NC, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e5) +
  ylim(0,5e5) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_NC, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e5) +
  ylim(0,5e5) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_NC, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e5) +
  ylim(0,5e5) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facet_mse_NC, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e5) +
  ylim(0,5e5) +
  facet_wrap(~SSB, scales = "free_y")

hist(RDMoutputscramble_facet_mse_NC$Bag)
hist(RDMoutputscramble_facet_mse_NC$MinLen)
hist(RDMoutputscramble_facet_mse_NC$SeasonLen)
hist(RDMoutputscramble_facet_mse_NC$SSB)

#NJ
RDMoutputscramble_facet_mse_NJ <- RDMoutputscramble_facet_mse %>% filter(state == "NJ") %>%
  filter(mse > NJstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_NJ, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e7) +
  ylim(0,3e7) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_NJ, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e7) +
  ylim(0,3e7) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_NJ, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e7) +
  ylim(0,3e7) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facet_mse_NJ, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e7) +
  ylim(0,3e7) +
  facet_wrap(~SSB, scales = "free_y")

hist(RDMoutputscramble_facet_mse_NJ$Bag)
hist(RDMoutputscramble_facet_mse_NJ$MinLen)
hist(RDMoutputscramble_facet_mse_NJ$SeasonLen)
hist(RDMoutputscramble_facet_mse_NJ$SSB)

#NY
RDMoutputscramble_facet_mse_NY <- RDMoutputscramble_facet_mse %>% filter(state == "NY") %>%
  filter(mse > NYstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_NY, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e7) +
  ylim(0,3e7) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_NY, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e7) +
  ylim(0,3e7) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_NY, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e7) +
  ylim(0,3e7) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facet_mse_NY, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,3e7) +
  ylim(0,3e7) +
  facet_wrap(~SSB, scales = "free_y")

hist(RDMoutputscramble_facet_mse_NY$Bag)
hist(RDMoutputscramble_facet_mse_NY$MinLen)
hist(RDMoutputscramble_facet_mse_NY$SeasonLen)
hist(RDMoutputscramble_facet_mse_NY$SSB)

#RI
RDMoutputscramble_facet_mse_RI <- RDMoutputscramble_facet_mse %>% filter(state == "RI") %>%
  filter(mse > RIstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_RI, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_RI, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_RI, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facet_mse_RI, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,1e7) +
  ylim(0,1e7) +
  facet_wrap(~SSB, scales = "free_y")

hist(RDMoutputscramble_facet_mse_RI$Bag)
hist(RDMoutputscramble_facet_mse_RI$MinLen)
hist(RDMoutputscramble_facet_mse_RI$SeasonLen)
hist(RDMoutputscramble_facet_mse_RI$SSB)

#RI
RDMoutputscramble_facet_mse_VA <- RDMoutputscramble_facet_mse %>% filter(state == "VA") %>%
  filter(mse > VAstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_VA, mapping=aes(x=Pred, y = tot_keep, col = Bag)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~Bag, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_VA, mapping=aes(x=Pred, y = tot_keep, col = MinLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~MinLen, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_VA, mapping=aes(x=Pred, y = tot_keep, col = SeasonLen)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~SeasonLen, scales = "free_y")

#SSB
ggplot(RDMoutputscramble_facet_mse_VA, mapping=aes(x=Pred, y = tot_keep, col = SSB)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  xlim(0,5e6) +
  ylim(0,5e6) +
  facet_wrap(~SSB, scales = "free_y")

hist(RDMoutputscramble_facet_mse_VA$Bag)
hist(RDMoutputscramble_facet_mse_VA$MinLen)
hist(RDMoutputscramble_facet_mse_VA$SeasonLen)
hist(RDMoutputscramble_facet_mse_VA$SSB)

# logspace 

g1pred_log <- predict(g1, type = "link") #if not 

RDMoutputscramble_facet <- inputdata %>% mutate(Pred = g1pred_log)

RDMoutputscramble_facet_mse <- RDMoutputscramble_facet %>% mutate(mse = (log(tot_keep) - Pred)^2)

upper_quartile_mse <- quantile(RDMoutputscramble_facet_mse$mse, probs = 0.75)

RDMoutputscramble_facet_mse_upperquartile <- RDMoutputscramble_facet_mse %>% filter(mse > upper_quartile_mse)

#state

#all
ggplot(RDMoutputscramble_facet_mse, mapping=aes(y = log(tot_keep), x = Pred, col = state)) + 
  geom_point() + 
  xlim(5,20) +
  ylim(5,20) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

#mse
ggplot(RDMoutputscramble_facet_mse_upperquartile, mapping=aes(y = log(tot_keep), x = Pred, col = state)) + 
  geom_point() + 
  xlim(5,20) +
  ylim(5,20) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

#are any states more represented?
RDMoutputscramble_facet_mse_upperquartileRI <- RDMoutputscramble_facet_mse_upperquartile %>% filter(state == "RI")
nrow(RDMoutputscramble_facet_mse_upperquartileRI) #136
RDMoutputscramble_facet_mse_upperquartileCT <- RDMoutputscramble_facet_mse_upperquartile %>% filter(state == "CT")
nrow(RDMoutputscramble_facet_mse_upperquartileCT) #83
RDMoutputscramble_facet_mse_upperquartileNY <- RDMoutputscramble_facet_mse_upperquartile %>% filter(state == "NY")
nrow(RDMoutputscramble_facet_mse_upperquartileNY) #40
RDMoutputscramble_facet_mse_upperquartileNJ <- RDMoutputscramble_facet_mse_upperquartile %>% filter(state == "NJ")
nrow(RDMoutputscramble_facet_mse_upperquartileNJ) #76
RDMoutputscramble_facet_mse_upperquartileMA <- RDMoutputscramble_facet_mse_upperquartile %>% filter(state == "MA")
nrow(RDMoutputscramble_facet_mse_upperquartileMA) #87
RDMoutputscramble_facet_mse_upperquartileDE <- RDMoutputscramble_facet_mse_upperquartile %>% filter(state == "DE")
nrow(RDMoutputscramble_facet_mse_upperquartileDE) #127
RDMoutputscramble_facet_mse_upperquartileNC <- RDMoutputscramble_facet_mse_upperquartile %>% filter(state == "NC")
nrow(RDMoutputscramble_facet_mse_upperquartileNC) #53
RDMoutputscramble_facet_mse_upperquartileMD <- RDMoutputscramble_facet_mse_upperquartile %>% filter(state == "MD")
nrow(RDMoutputscramble_facet_mse_upperquartileMD) #63
RDMoutputscramble_facet_mse_upperquartileVA <- RDMoutputscramble_facet_mse_upperquartile %>% filter(state == "VA")
nrow(RDMoutputscramble_facet_mse_upperquartileVA) #44

############## FIT GAM TO SUMMED COASTWIDE HARVEST ##################
RDMoutputscramble_facet_coastwide <- RDMoutputscramble_facet %>%
  mutate(group = ceiling(row_number() / 9)) %>%  
  group_by(group) %>%
  summarise(
    across(tot_keep, sum, .names = "tot_keep"), 
  #  across(Pred, sum, .names = "Pred"), 
    .groups = "drop"
  ) 

regstable <- RDMoutputscramble_facet %>% filter(state == "CT") %>% 
  select( Bag, MinLen, SeasonLen, SSB) %>%
  mutate(tot_keep = RDMoutputscramble_facet_coastwide$tot_keep) 

g2 <- gam(tot_keep ~ s(SSB, k = 3) + s(SeasonLen, k = 3) + 
            s(Bag, k = 3) + s(MinLen, k = 3), data = regstable, 
          family = Gamma(link = log), method = "REML") 
summary(g2)
appraise(g2)
g2pred <- predict(g2, type = "response")
plot(regstable$tot_keep ~ g2pred) + abline(b = 1, a = 0, col = "red")

#View(regstable)
#View(RDMoutputscramble_facet)

################# FIT TO ALL MSE RDM RUNS ###################

#FIRST TIME -> ALL OUTPUT 

#base
Bag <- all_results$bag
MinLen <- all_results$minlen
SeasonLen <- all_results$seaslen
tot_keep <- all_results$n_keep
SSB <- all_results$biomass
state <- all_results$state

rel_num <- all_results$n_release

#2BBMSY
BagL <- all_results1$bag
MinS <- all_results1$minlen
SeasL <- all_results1$seaslen
keep_num <- all_results1$n_keep
SSBcov <- all_results1$biomass
state <- all_results1$state

#0.3BBMSY
BagL <- all_results7$bag
MinS <- all_results7$minlen
SeasL <- all_results7$seaslen
keep_num <- all_results7$n_keep
SSBcov <- all_results7$biomass
state <- all_results7$state

inputdata_mseruns <- data.frame(Bag, MinLen, SeasonLen, tot_keep, rel_num, SSB, state)

#for trying other ssb scenarios 
inputdata_mseruns1 <- data.frame(BagL, MinS, SeasL, keep_num, SSBcov, state)
inputdata_mseruns7 <- data.frame(BagL, MinS, SeasL, keep_num, SSBcov, state)

inputdata_mseruns <- na.omit(inputdata_mseruns) #change this depending on which scenario is run
inputdata_mseruns <- inputdata_mseruns %>% filter(tot_keep != 0)

#testing to see if RI reg table issue 
inputdata_mseruns2 <- inputdata_mseruns %>% mutate(Pred = g2pred) %>% mutate(SeasL = ifelse(SeasL > 165 & state == "RI", SeasL - 15, SeasL))
inputdata_mseruns2 <- inputdata_mseruns %>% filter(SeasL <= 150)

#gam
g2 <- gam(tot_keep ~  state + s(SSB, k = 3) + s(SeasonLen, k = 3) + #9,5,7 partial effect of smooths look strange with k any higher
            s(Bag, k = 3) + s(MinLen, k = 3), data = inputdata_mseruns,
          family = Gamma(link = log), method = "REML")
summary(g2)
appraise(g2)
draw(g2)
g2pred <- predict(g2, type = "response") #if not
plot(inputdata_mseruns$tot_keep ~ g2pred, xlim = c(0,2e7), ylim = c(0,2e7)) + abline(b = 1, a = 0, col = "red")
#saveRDS(g2, "~/Desktop/FlounderMSE/RDMgam/gam_RDM_allMSE.rds") #disordered length
#g1 <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDM_allMSE.rds") #disordered length

scalarinput <- dplyr::select(inputdata_mseruns[1:9,], state, SeasonLen, Bag, MinLen, SSB) # pick number where "CT" is first state in list 
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
#saveRDS(GAMscalar, "~/Desktop/FlounderMSE/RDMGam/GAMscalar_allMSE.rds")
#readRDS("~/Desktop/FlounderMSE/RDMGam/GAMscalar_allMSE.rds")

#look at how things change across set of starting regulations
inputdata_mseruns3 <- inputdata_mseruns2 %>% mutate(Pred = g2pred) %>% filter(state == "MA", MinS == 17.5, SeasL == 150, BagL == 4)

ggplot(inputdata_mseruns3, mapping=aes(x=Pred, y = keep_num, col = SSBcov)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,1.5e6) +
 # ylim(0,1.5e6) +
  facet_wrap(~state, scales = "free_y")

inputdata_mseruns2_mse <- inputdata_mseruns2 %>% mutate(Pred = g2pred) %>% mutate(mse = (log(keep_num) - log(Pred))^2)

inputdata_mseruns2_upperquart <- quantile(inputdata_mseruns2_mse$mse, probs = 0.75)

inputdata_mseruns2_mse_upperquartile <- inputdata_mseruns2_mse %>% filter(mse > inputdata_mseruns2_upperquart)

#mse
ggplot(inputdata_mseruns2_mse_upperquartile, mapping=aes(y = log(keep_num), x = log(Pred), col = state)) + 
  geom_point() + 
  xlim(10,20) +
  ylim(10,20) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

#are any states more represented?
inputdata_mseruns2_mse_upperquartileRI <- inputdata_mseruns2_mse_upperquartile %>% filter(state == "RI")
nrow(inputdata_mseruns2_mse_upperquartileRI) #8012
inputdata_mseruns2_mse_upperquartileCT <- inputdata_mseruns2_mse_upperquartile %>% filter(state == "CT")
nrow(inputdata_mseruns2_mse_upperquartileCT) #5722
inputdata_mseruns2_mse_upperquartileNY <- inputdata_mseruns2_mse_upperquartile %>% filter(state == "NY")
nrow(inputdata_mseruns2_mse_upperquartileNY) #2600
inputdata_mseruns2_mse_upperquartileNJ <- inputdata_mseruns2_mse_upperquartile %>% filter(state == "NJ")
nrow(inputdata_mseruns2_mse_upperquartileNJ) #2936
inputdata_mseruns2_mse_upperquartileMA <- inputdata_mseruns2_mse_upperquartile %>% filter(state == "MA")
nrow(inputdata_mseruns2_mse_upperquartileMA) #4611
inputdata_mseruns2_mse_upperquartileDE <- inputdata_mseruns2_mse_upperquartile %>% filter(state == "DE")
nrow(inputdata_mseruns2_mse_upperquartileDE) #4142
inputdata_mseruns2_mse_upperquartileNC <- inputdata_mseruns2_mse_upperquartile %>% filter(state == "NC")
nrow(inputdata_mseruns2_mse_upperquartileNC) #3610
inputdata_mseruns2_mse_upperquartileMD <- inputdata_mseruns2_mse_upperquartile %>% filter(state == "MD")
nrow(inputdata_mseruns2_mse_upperquartileMD) #4276
inputdata_mseruns2_mse_upperquartileVA <- inputdata_mseruns2_mse_upperquartile %>% filter(state == "VA")
nrow(inputdata_mseruns2_mse_upperquartileVA) #3470

##############. SECOND TIME FIT TO OUTPUT -> MORE EQUAL REG DISTRIBUTION ###############
all_results #RDMGAM_hierarchical1 Bin3
#View(all_results)

Bag <- all_results$bag
MinLen <- all_results$minlen
SeasonLen <- all_results$seaslen
tot_keep <- all_results$n_keep
SSB <- all_results$biomass
state <- all_results$state

#BBM
rbctrack <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50BBM2/sim1_bag/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled <- rbctrack %>%
  slice(rep(1:n(), each = 18))
rbctrack2 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50BBM2/sim2_length/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled2 <- rbctrack2 %>%
  slice(rep(1:n(), each = 18))
rbctrack3 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50BBM2/sim3_season/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled3 <- rbctrack3 %>%
  slice(rep(1:n(), each = 18))
rbctrack4 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50BBM2/sim4_all/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled4 <- rbctrack4 %>%
  slice(rep(1:n(), each = 18))
#BRP
rbctrack5 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50BRP/sim1_bag/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled5 <- rbctrack5 %>%
  slice(rep(1:n(), each = 18))
rbctrack6 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50BRP/sim2_length/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled6 <- rbctrack6 %>%
  slice(rep(1:n(), each = 18))
rbctrack7 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50BRP/sim3_season/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled7 <- rbctrack7 %>%
  slice(rep(1:n(), each = 18))
rbctrack8 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50BRP/sim4_all/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled8 <- rbctrack8 %>%
  slice(rep(1:n(), each = 18))
#PCA
rbctrack9 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50PCA/sim1_bag/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled9 <- rbctrack9 %>%
  slice(rep(1:n(), each = 18))
rbctrack10 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50PCA/sim2_length/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled10 <- rbctrack10 %>%
  slice(rep(1:n(), each = 18))
rbctrack11 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50PCA/sim3_season/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled11 <- rbctrack11 %>%
  slice(rep(1:n(), each = 18))
rbctrack12 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50PCA/sim4_all/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled12 <- rbctrack12 %>%
  slice(rep(1:n(), each = 18))
#NCA
rbctrack13 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50NCA/sim1_bag/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled13 <- rbctrack13 %>%
  slice(rep(1:n(), each = 18))
rbctrack14 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50NCA/sim2_length/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled14 <- rbctrack14 %>%
  slice(rep(1:n(), each = 18))
rbctrack15 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50NCA/sim3_season/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled15 <- rbctrack15 %>%
  slice(rep(1:n(), each = 18))
rbctrack16 <- read.table(("~/Desktop/FlounderMSE/kw_sims_test/UnityOutput/RDMGAM/Bin3/50NCA/sim4_all/rbctrack.out"), header = FALSE, skip = 1)
rbctrack_doubled16 <- rbctrack16 %>%
  slice(rep(1:n(), each = 18))

rbctrack_all <- rbind(rbctrack_doubled, rbctrack_doubled2, rbctrack_doubled3, rbctrack_doubled4,
                      rbctrack_doubled5, rbctrack_doubled6, rbctrack_doubled7, rbctrack_doubled8,
                      rbctrack_doubled9, rbctrack_doubled10, rbctrack_doubled11, rbctrack_doubled12,
                      rbctrack_doubled13, rbctrack_doubled14, rbctrack_doubled15, rbctrack_doubled16)
#nrow(rbctrack_all) 

#rbctrack_doubled <- rbctrack_doubled %>% mutate(SSB = rbctrack_doubled$V28)
rbctrack_all <- rbctrack_all %>% mutate(SSB = rbctrack_all$V28)

all_results_test <- all_results_test %>% mutate(SSB = rbctrack_all$V28)
#View(all_results_test)

all_results_single <- all_results_test %>% filter(state == "NJ", 
                                                  bag == 4,
                                                  minlen == 17.5,
                                                  seaslen == 150)
plot(all_results_single$biomass~all_results_single$n_keep)
plot(all_results_single$biomass~all_results_single$SSB)

Bag <- all_results_test$bag
MinLen <- all_results_test$minlen
SeasonLen <- all_results_test$seaslen
tot_keep <- all_results_test$n_keep
SSB <- all_results_test$biomass
state <- all_results_test$state
year <- all_results_test$year

#find unique number of regulations, fit to that 
inputdata_mseruns <- data.frame(Bag, MinLen, SeasonLen, tot_keep, SSB, state, year)

#dont need to run this in rbctrack merged dataframe
#inputdata_mseruns <- na.omit(inputdata_mseruns) #change this depending on which scenario is run
#inputdata_mseruns <- inputdata_mseruns %>% filter(tot_keep != 0)

#find number of unique regulations and keep first 45 of each seasonlen
inputdata_uniqueruns <- inputdata_mseruns %>% 
  distinct(Bag, MinLen, SeasonLen, state, SSB, .keep_all = TRUE) #%>%
 # group_by(Bag) %>%
 # slice((1:45)) %>% 
 # ungroup()

#hist(inputdata_uniqueruns$SeasonLen)

#data frame of unique regulations per state 

#fit to rbctrack.out files as well 

#View(inputdata_sampled)

inputdata_uniqueruns$state <- as.factor(inputdata_uniqueruns$state)
inputdata_uniqueruns$year <- as.factor(inputdata_uniqueruns$year)


#fit gam if necessary 
g1.1 <- gam(tot_keep ~ s(SSB, state, bs = "fs", k = 3) + 
              s(SeasonLen, state, bs = "fs", k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
              s(Bag, state, bs = "fs", k = 3) + s(MinLen, state, bs = "fs", k = 3), 
            data = inputdata_uniqueruns, #either isolated RDM runs or total 
            family = Gamma(link = log), method = "REML") #random effect on spline for minlen that introduces  a sort of random effect by state 
summary(g1.1)
draw(g1.1)
appraise(g1.1)
g1.1pred <- predict(g1.1, type = "response") #if not 
plot(inputdata_uniqueruns$tot_keep ~ g1.1pred, xlim = c(0,5e6), ylim = c(0,5e6)) + abline(b = 1, a = 0, col = "red")

#tensor smooth
g1.3 <- gam(tot_keep ~ t2(SSB, state, bs = c("tp", "re")) + 
              t2(SeasonLen, state, bs = c("tp", "re")) + #9,5,7 partial effect of smooths look strange with k any higher 
              t2(Bag, state, bs = c("tp", "re")) + t2(MinLen, state, bs = c("tp", "re")), 
            data = inputdata_uniqueruns, #either isolated RDM runs or total 
            family = Gamma(link = log), method = "REML")
#g1.3 <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDMland_tensorsmooth_allmse.rds") #disordered length
#saveRDS(g1.3, "~/Desktop/FlounderMSE/RDMgam/gam_RDMland_tensorsmooth_allmse.rds") #disordered length
summary(g1.3)
draw(g1.3) #not sure how to plot partial effects
appraise(g1.3)
g1.3pred <- predict(g1.3, type = "response") #if not 
plot(inputdata_uniqueruns$tot_keep ~ g1.3pred, xlim = c(0,5e6), ylim = c(0,5e6)) + abline(b = 1, a = 0, col = "red")

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("SSB", sm$.smooth), ]
ggplot(sm_fs, aes(x = SSB, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "SSB", color = "state", fill = "state")

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("SeasonLen", sm$.smooth), ]
ggplot(sm_fs, aes(x = SeasonLen, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "SeasonLen", color = "state", fill = "state")

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("MinLen", sm$.smooth), ]
ggplot(sm_fs, aes(x = MinLen, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "MinLen", color = "state", fill = "state")

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("Bag", sm$.smooth), ]
ggplot(sm_fs, aes(x = Bag, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Bag", color = "state", fill = "state")

AIC(g1.1, g1.3) #g1.3 is better


########## FORM PREDICTIONS OF OUTPUT FILE ###############

all_results_test <- all_results %>% filter(
  #scenario %in% c("BTable3", "LTable3", "STable3", "ATable3"), 
  year != 2019)


#predict data frame: same time step

predict_frame <- data.frame(Bag = all_results_test$bag,
                            MinLen = all_results_test$minlen, 
                            SeasonLen = all_results_test$seaslen,
                            state = all_results_test$state,
                            n_keep = all_results_test$n_keep,
                       #     SSB_1 = all_results_test$SSB, #rbctrack
                            SSB = all_results_test$biomass #spawbio
                            )# %>% mutate(Scen = "2BBMSY")

gamfit2 <- predict.gam(g1.1, newdata = predict_frame, type = "response" , se.fit = TRUE)
#gamfit2 <- predict.gam(g1.3, newdata = predict_frame, type = "response" , se.fit = TRUE)

#for plotting (includes n_keep)
predict_frame_gg <- data.frame(Bag = all_results_test$bag,
                            MinLen = all_results_test$minlen, 
                            SeasonLen = all_results_test$seaslen,
                            state = all_results_test$state,
                            n_keep = all_results_test$n_keep,
                         #  SSB_1 = all_results_test$SSB, #rbctrack 
                            SSB = all_results_test$biomass, #spawbio
                           exp_keep = gamfit2$fit) #if doing other two

predict_frame_gg %>% 
  #filter(bag == 4, seaslen == 150) %>% 
  drop_na() %>% 
  ggplot() + 
  aes(y = n_keep, x = exp_keep, col = SSB) + #scaler?
  geom_point(alpha=0.1) + 
  #geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_viridis_c() +
  labs(
    y = "harvest number from RDM",
    x = "expected harvest from GAM"
  ) +
#  facet_wrap(~state) +
  xlim(0,4e+07) + #2e7
  ylim(0,4e+07) +
  NULL

#facet by regulations
predict_frame_gg %>% filter(state == "MA") %>% 
  #filter(bag == 4, seaslen == 150) %>% 
  drop_na() %>% 
  ggplot() + 
  aes(y = n_keep, x = exp_keep, col = SSB) + #scaler?
  geom_point(alpha=0.1) + 
  #geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_viridis_c() +
  labs(
    y = "harvest number from RDM",
    x = "expected harvest from GAM"
  ) +
  facet_wrap(~MinLen) +
  xlim(0,1e+06) + #2e7
  ylim(0,1e+06) +
  NULL

#double spawbio
predict_frame <- data.frame(Bag = all_results_test$bag,
                            MinLen = all_results_test$minlen, 
                            SeasonLen = all_results_test$seaslen,
                            state = all_results_test$state,
                            n_keep = all_results_test$n_keep,
                            #     SSB_1 = all_results_test$SSB, #rbctrack
                            SSB_1 = all_results_test$biomass #spawbio
                            ) %>%
  mutate(SSB = rep(SSB_1[seq(1, n(), by = 18)], each = 18)[seq_len(n())])



#predict using GAM
gamfit2 <- predict.gam(g1.1, newdata = predict_frame, type = "response" , se.fit = TRUE)

#for double spawbio
predict_frame_gg2 <- data.frame(Bag = all_results_test$bag,
                                MinLen = all_results_test$minlen, 
                                SeasonLen = all_results_test$seaslen,
                                state = all_results_test$state,
                                SSB = predict_frame$SSB, #make sure this is different
                                n_keep = all_results_test$n_keep,
                                #SSB_1 = all_results_test$SSB,
                                exp_keep = gamfit2$fit)

predict_frame_gg2 %>% 
  #filter(bag == 4, seaslen == 150) %>% 
  drop_na() %>% 
  ggplot() + 
  aes(y = n_keep, x = exp_keep, col = SSB) + #scaler?
  geom_point(alpha=0.1) + 
  #geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_viridis_c() +
  labs(
    y = "harvest number from RDM",
    x = "expected harvest from GAM"
  ) +
  #facet_wrap(~bag) +
  xlim(0,5e+07) + #2e7
  ylim(0,5e+07) +
  NULL

#preceding time step of spawbio 
predict_frame3 <- data.frame(Bag = all_results$bag,
                            MinLen = all_results$minlen, 
                            SeasonLen = all_results$seaslen,
                            tot_keep = all_results$n_keep,
                            state = all_results$state,
                            SSB = all_results$biomass,
                            year = all_results$year,
                            scenario = all_results$scenario,
                            isim = all_results$isim) 

split_predict_frame3 <- split(predict_frame3, predict_frame3$isim)
  
  all_storeframes = list()
  all_storeframes = vector("list", length = length(split_predict_frame3))
  
for (x in 1:length(split_predict_frame3)){
  predict_frame4 <- split_predict_frame3[[x]]
  
  storeframe = list()
  storeframe = vector("list", length = length(unique(predict_frame4$scenario)))
for (i in unique(predict_frame4$scenario)){
predict_frame5 <- predict_frame4 %>% filter(scenario == i) %>% 
  mutate(spawbio3 = c(NA, rep(SSB[seq(1, n() - 18, by = 18)], each = 18))[seq_len(n())])
storeframe[[i]] <- predict_frame5
}
  all_storeframes[[x]] <- rbindlist(storeframe)
}

bound_storeframes <- rbindlist(all_storeframes) %>% filter(year != 2019)

#View(all_storeframes[[40]])
#View(storeframe[[i]])
#View(predict_frame4)

#only run this if using previous time step approach
predict_frame <- data.frame(Bag = bound_storeframes$Bag,
                            MinLen = bound_storeframes$MinLen, 
                            SeasonLen = bound_storeframes$SeasonLen,
                            state = bound_storeframes$state,
                            SSB = bound_storeframes$spawbio3,
                            SSB_1 = bound_storeframes$SSB) #spawbio

#predict using GAM
gamfit2 <- predict.gam(g1.1, newdata = predict_frame, type = "response" , se.fit = TRUE)

#for prev time step approach
predict_frame_gg3 <- data.frame(Bag = bound_storeframes$Bag,
                                MinLen = bound_storeframes$MinLen, 
                                SeasonLen = bound_storeframes$SeasonLen,
                                state = bound_storeframes$state,
                                n_keep = bound_storeframes$tot_keep,
                                SSB = bound_storeframes$spawbio3,
                                SSB_1 = bound_storeframes$SSB,
                                exp_keep = gamfit2$fit)

predict_frame_gg3 %>% 
  #filter(bag == 4, seaslen == 150) %>% 
  drop_na() %>% 
  ggplot() + 
  aes(y = n_keep, x = exp_keep, col = SSB) + #scaler?
  geom_point(alpha=0.1) + 
  #geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_viridis_c() +
  labs(
    y = "harvest number from RDM",
    x = "expected harvest from GAM"
  ) +
  #facet_wrap(~bag) +
  xlim(0,5e+07) + #2e7
  ylim(0,5e+07) +
  NULL

############### SPLIT UP ALL MSE RDM RUNS BY STATE ###############
all_results

#base
BagL <- all_results$bag
MinS <- all_results$minlen
SeasL <- all_results$seaslen
keep_num <- all_results$n_keep
exp_keep <- all_results$exp_keep
SSBcov <- all_results$biomass
state <- all_results$state


GAMscaler <- readRDS("~/Desktop/FlounderMSE/RDMGam/GAMscalar.rds")

#can't use this for hierarchical GAM
CTscaler <- GAMscaler$CTscalar
DEscaler <- GAMscaler$DEscalar
MAscaler <- GAMscaler$MAscalar
MDscaler <- GAMscaler$MDscalar
NCscaler <- GAMscaler$NCscalar
NJscaler <- GAMscaler$NJscalar
NYscaler <- GAMscaler$NYscalar
RIscaler <- GAMscaler$RIscalar
VAscaler <- GAMscaler$VAscalar

inputdata_mseruns <- data.frame(BagL, MinS, SeasL, keep_num, exp_keep, SSBcov, state)
inputdata_mseruns <- na.omit(inputdata_mseruns) #change this depending on which scenario is run
inputdata_mseruns <- inputdata_mseruns %>% mutate(exp_keep = ifelse(
  state == "CT", exp_keep*CTscaler, ifelse(
    state == "DE", exp_keep*DEscaler, ifelse(
      state == "MA", exp_keep*MAscaler, ifelse(
        state == "MD", exp_keep*MDscaler, ifelse(
          state == "NC", exp_keep*NCscaler, ifelse(
            state == "NJ", exp_keep*NJscaler, ifelse(
              state == "NY", exp_keep*NYscaler, ifelse(
                state == "RI", exp_keep*RIscaler, ifelse(
                  state == "VA", exp_keep*VAscaler, 0))))))))))
#View(inputdata_mseruns)

#state
ggplot(inputdata_mseruns, mapping=aes(y = keep_num, x = exp_keep, col = SeasL)) + 
  geom_point() + 
  xlim(0,2.5e7) +
  ylim(0,2.5e7) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~SeasL, scales = "free_y")

#individual states 

inputdata_mseruns %>% filter(state == "CT") %>%
  ggplot() +
  geom_point(mapping=aes(y = keep_num, x = exp_keep, col = state)) + 
 # xlim(0,2.5e6) + #base
 # ylim(0,2.5e6) +
  xlim(0,5e6) + #lowbbmsy
  ylim(0,5e6) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

inputdata_mseruns %>% filter(state == "DE") %>%
  ggplot() +
  geom_point(mapping=aes(y = keep_num, x = exp_keep, col = state)) + 
 # xlim(0,7.5e5) + #base
 # ylim(0,7.5e5) +
   xlim(0,2e6) + #lowbbmsy
   ylim(0,2e6) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

inputdata_mseruns %>% filter(state == "MA") %>%
  ggplot() +
  geom_point(mapping=aes(y = keep_num, x = exp_keep, col = state)) + 
  #xlim(0,7.5e5) + #base
 # ylim(0,7.5e5) +
  xlim(0,1e6) + #lowbbmsy
  ylim(0,1e6) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

inputdata_mseruns %>% filter(state == "MD") %>%
  ggplot() +
  geom_point(mapping=aes(y = keep_num, x = exp_keep, col = state)) + 
 # xlim(0,6e5) + #base
 # ylim(0,6e5) +
   xlim(0,1.5e6) + #low bbmsy
   ylim(0,1.5e6) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

inputdata_mseruns %>% filter(state == "NC") %>%
  ggplot() +
  geom_point(mapping=aes(y = keep_num, x = exp_keep, col = state)) + 
#  xlim(0,1e5) + #base
#  ylim(0,1e5) +
  xlim(0,2.5e5) + #lowbmsy
  ylim(0,2.5e5) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

inputdata_mseruns %>% filter(state == "NJ") %>%
  ggplot() +
  geom_point(mapping=aes(y = keep_num, x = exp_keep, col = state)) + 
 # xlim(0,1e7) + #base
 # ylim(0,1e7) +
  xlim(0,3e7) + #low bbmsy
  ylim(0,3e7) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

inputdata_mseruns %>% filter(state == "NY") %>%
  ggplot() +
  geom_point(mapping=aes(y = keep_num, x = exp_keep, col = state)) + 
 # xlim(0,1e7) + #base
 # ylim(0,1e7) +
   xlim(0,2.5e7) + #low bbmsy
   ylim(0,2.5e7) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

inputdata_mseruns %>% filter(state == "RI") %>%
  ggplot() +
  geom_point(mapping=aes(y = keep_num, x = exp_keep, col = state)) + 
#  xlim(0,2.5e6) + #base
#  ylim(0,2.5e6) +
  xlim(0,5e6) + #low bbmsy
  ylim(0,5e6) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

inputdata_mseruns %>% filter(state == "VA") %>%
  ggplot() +
  geom_point(mapping=aes(y = keep_num, x = exp_keep, col = state)) + 
 # xlim(0,5e5) + #base
 # ylim(0,5e5) +
  xlim(0,2.5e6) + #low bbmsy
  ylim(0,2.5e6) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

#look into other factors potentially driving this consistent issue
all_results %>% mutate(exp_keep = ifelse(
  state == "CT", exp_keep*CTscaler, ifelse(
    state == "DE", exp_keep*DEscaler, ifelse(
      state == "MA", exp_keep*MAscaler, ifelse(
        state == "MD", exp_keep*MDscaler, ifelse(
          state == "NC", exp_keep*NCscaler, ifelse(
            state == "NJ", exp_keep*NJscaler, ifelse(
              state == "NY", exp_keep*NYscaler, ifelse(
                state == "RI", exp_keep*RIscaler, ifelse(
                  state == "VA", exp_keep*VAscaler, 0)))))))))) %>%
#filter(state == "VA") %>% 
 # filter(isim %in% c(41:50)) %>%
  ggplot() +
  geom_point(mapping=aes(y = n_keep, x = exp_keep, col = scenario)) + 
 # xlim(0,5e5) +
 # ylim(0,5e5) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~scenario, scales = "free_y")

########### MEAN SQUARED ERROR FOR TOTAL IN LOGSPACE AND STATES SEPARATELY FOR ALL MSE RDM RUNS ############

#logspace for all
all_results_mse_all <- all_results %>% filter(complete.cases(.)) %>%
  mutate(exp_keep = ifelse(
    state == "CT", exp_keep*CTscaler, ifelse(
      state == "DE", exp_keep*DEscaler, ifelse(
        state == "MA", exp_keep*MAscaler, ifelse(
          state == "MD", exp_keep*MDscaler, ifelse(
            state == "NC", exp_keep*NCscaler, ifelse(
              state == "NJ", exp_keep*NJscaler, ifelse(
                state == "NY", exp_keep*NYscaler, ifelse(
                  state == "RI", exp_keep*RIscaler, ifelse(
                    state == "VA", exp_keep*VAscaler, 0)))))))))) %>%
  mutate(mse = (log(n_keep) - log(exp_keep))^2)

all_results_all_mse <- quantile(all_results_mse_all$mse, probs = 0.75)

all_results_all_upper_quartile_mse <- all_results_mse_all %>% filter(mse > all_results_all_mse)

#mse
ggplot(all_results_all_upper_quartile_mse, mapping=aes(y = log(n_keep), x = log(exp_keep), col = state)) + 
  geom_point() + 
  xlim(5,15) +
  ylim(5,15) +
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~state, scales = "free_y")

#are any states more represented?
all_results_all_upper_quartile_mseRI <- all_results_all_upper_quartile_mse %>% filter(state == "RI")
nrow(all_results_all_upper_quartile_mseRI) #2532 for base; 5724 for low
all_results_all_upper_quartile_mseCT <- all_results_all_upper_quartile_mse %>% filter(state == "CT")
nrow(all_results_all_upper_quartile_mseCT) #2481 for base; 3944 for low
all_results_all_upper_quartile_mseNY <- all_results_all_upper_quartile_mse %>% filter(state == "NY")
nrow(all_results_all_upper_quartile_mseNY) #3578 for base; 4061 for low 
all_results_all_upper_quartile_mseNJ <- all_results_all_upper_quartile_mse %>% filter(state == "NJ")
nrow(all_results_all_upper_quartile_mseNJ) #6164 for base; 5337 for low
all_results_all_upper_quartile_mseMA <- all_results_all_upper_quartile_mse %>% filter(state == "MA")
nrow(all_results_all_upper_quartile_mseMA) #5419 for base; 5640 for low
all_results_all_upper_quartile_mseDE <- all_results_all_upper_quartile_mse %>% filter(state == "DE")
nrow(all_results_all_upper_quartile_mseDE) #9089 for base; 7560 for low
all_results_all_upper_quartile_mseNC <- all_results_all_upper_quartile_mse %>% filter(state == "NC")
nrow(all_results_all_upper_quartile_mseNC) #3727 for base; 3582 for low
all_results_all_upper_quartile_mseMD <- all_results_all_upper_quartile_mse %>% filter(state == "MD")
nrow(all_results_all_upper_quartile_mseMD) #7484 for base; 6046 for low
all_results_all_upper_quartile_mseVA <- all_results_all_upper_quartile_mse %>% filter(state == "VA")
nrow(all_results_all_upper_quartile_mseVA) #6238 for base; 4881 for low

#states in response space
all_results_mse <- inputdata_mseruns %>% mutate(mse = (keep_num-exp_keep)^2)

state_quartiles <- all_results_mse %>%
  group_by(state) %>%
  summarise(
    state_quart = quantile(mse, probs = 0.75)
  )

CTstatequart <- state_quartiles[1,]$state_quart
DEstatequart <- state_quartiles[2,]$state_quart
MAstatequart <- state_quartiles[3,]$state_quart
MDstatequart <- state_quartiles[4,]$state_quart
NCstatequart <- state_quartiles[5,]$state_quart
NJstatequart <- state_quartiles[6,]$state_quart
NYstatequart <- state_quartiles[7,]$state_quart
RIstatequart <- state_quartiles[8,]$state_quart
VAstatequart <- state_quartiles[9,]$state_quart

#CT
RDMoutputscramble_facet_mse_CT <- all_results_mse %>% filter(state == "CT") %>%
  filter(mse > CTstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_CT, mapping=aes(x=exp_keep, y = keep_num, col = BagL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,2.5e6) + base 
 # ylim(0,2.5e6) +
  xlim(0,5e6) + #lowbbmsy
  ylim(0,5e6) +
  facet_wrap(~BagL, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_CT, mapping=aes(x=exp_keep, y = keep_num, col = MinS)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,2.5e6) + base 
  # ylim(0,2.5e6) +
  xlim(0,5e6) + #lowbbmsy
  ylim(0,5e6) +
  facet_wrap(~MinS, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_CT, mapping=aes(x=exp_keep, y = keep_num, col = SeasL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,2.5e6) + base 
  # ylim(0,2.5e6) +
  xlim(0,5e6) + #lowbbmsy
  ylim(0,5e6) +
  facet_wrap(~SeasL, scales = "free_y")

hist(RDMoutputscramble_facet_mse_CT$BagL)
hist(RDMoutputscramble_facet_mse_CT$MinS)
hist(RDMoutputscramble_facet_mse_CT$SeasL)
hist(RDMoutputscramble_facet_mse_CT$SSBcov)

#DE
RDMoutputscramble_facet_mse_DE <- all_results_mse %>% filter(state == "DE") %>%
  filter(mse > DEstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_DE, mapping=aes(x=exp_keep, y = keep_num, col = BagL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,5e5) +
 # ylim(0,5e5) +
  xlim(0,2e6) + #lowbbmsy
  ylim(0,2e6) +
  facet_wrap(~BagL, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_DE, mapping=aes(x=exp_keep, y = keep_num, col = MinS)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,5e5) +
  # ylim(0,5e5) +
  xlim(0,2e6) + #lowbbmsy
  ylim(0,2e6) +
  facet_wrap(~MinS, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_DE, mapping=aes(x=exp_keep, y = keep_num, col = SeasL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,5e5) +
  # ylim(0,5e5) +
  xlim(0,2e6) + #lowbbmsy
  ylim(0,2e6) +
  facet_wrap(~SeasL, scales = "free_y")

hist(RDMoutputscramble_facet_mse_DE$BagL)
hist(RDMoutputscramble_facet_mse_DE$MinS)
hist(RDMoutputscramble_facet_mse_DE$SeasL)
hist(RDMoutputscramble_facet_mse_DE$SSBcov)

#MA
RDMoutputscramble_facet_mse_MA <- all_results_mse %>% filter(state == "MA") %>%
  filter(mse > MAstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_MA, mapping=aes(x=exp_keep, y = keep_num, col = BagL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,5e5) +
 # ylim(0,5e5) +
  xlim(0,1e6) + #lowbbmsy
  ylim(0,1e6) +
  facet_wrap(~BagL, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_MA, mapping=aes(x=exp_keep, y = keep_num, col = MinS)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,5e5) +
  # ylim(0,5e5) +
  xlim(0,1e6) + #lowbbmsy
  ylim(0,1e6) +
  facet_wrap(~MinS, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_MA, mapping=aes(x=exp_keep, y = keep_num, col = SeasL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,5e5) +
  # ylim(0,5e5) +
  xlim(0,1e6) + #lowbbmsy
  ylim(0,1e6) +
  facet_wrap(~SeasL, scales = "free_y")

hist(RDMoutputscramble_facet_mse_MA$BagL)
hist(RDMoutputscramble_facet_mse_MA$MinS)
hist(RDMoutputscramble_facet_mse_MA$SeasL)
hist(RDMoutputscramble_facet_mse_MA$SSBcov)

#MD
RDMoutputscramble_facet_mse_MD <- all_results_mse %>% filter(state == "MD") %>%
  filter(mse > MDstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_MD, mapping=aes(x=exp_keep, y = keep_num, col = BagL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,5e5) +
 # ylim(0,5e5) +
  xlim(0,1.5e6) + #low bbmsy
  ylim(0,1.5e6) +
  facet_wrap(~BagL, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_MD, mapping=aes(x=exp_keep, y = keep_num, col = MinS)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,5e5) +
  # ylim(0,5e5) +
  xlim(0,1.5e6) + #low bbmsy
  ylim(0,1.5e6) +
  facet_wrap(~MinS, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_MD, mapping=aes(x=exp_keep, y = keep_num, col = SeasL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,5e5) +
  # ylim(0,5e5) +
  xlim(0,1.5e6) + #low bbmsy
  ylim(0,1.5e6) +
  facet_wrap(~SeasL, scales = "free_y")

hist(RDMoutputscramble_facet_mse_MD$BagL)
hist(RDMoutputscramble_facet_mse_MD$MinS)
hist(RDMoutputscramble_facet_mse_MD$SeasL)
hist(RDMoutputscramble_facet_mse_MD$SSBcov)

#NC

RDMoutputscramble_facet_mse_NC <- all_results_mse %>% filter(state == "NC") %>%
  filter(mse > NCstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_NC, mapping=aes(x=exp_keep, y = keep_num, col = BagL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,1e5) +
 # ylim(0,1e5) +
  xlim(0,2.5e5) + #lowbmsy
  ylim(0,2.5e5) +
  facet_wrap(~BagL, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_NC, mapping=aes(x=exp_keep, y = keep_num, col = MinS)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,1e5) +
  # ylim(0,1e5) +
  xlim(0,2.5e5) + #lowbmsy
  ylim(0,2.5e5) +
  facet_wrap(~MinS, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_NC, mapping=aes(x=exp_keep, y = keep_num, col = SeasL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,1e5) +
  # ylim(0,1e5) +
  xlim(0,2.5e5) + #lowbmsy
  ylim(0,2.5e5) +
  facet_wrap(~SeasL, scales = "free_y")

hist(RDMoutputscramble_facet_mse_NC$BagL)
hist(RDMoutputscramble_facet_mse_NC$MinS)
hist(RDMoutputscramble_facet_mse_NC$SeasL)
hist(RDMoutputscramble_facet_mse_NC$SSBcov)

#NJ

RDMoutputscramble_facet_mse_NJ <- all_results_mse %>% filter(state == "NJ") %>%
  filter(mse > NJstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_NJ, mapping=aes(x=exp_keep, y = keep_num, col = BagL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,1e7) +
 # ylim(0,1e7) +
  xlim(0,3e7) + #low bbmsy
  ylim(0,3e7) +
  facet_wrap(~BagL, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_NJ, mapping=aes(x=exp_keep, y = keep_num, col = MinS)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,1e7) +
  # ylim(0,1e7) +
  xlim(0,3e7) + #low bbmsy
  ylim(0,3e7) +
  facet_wrap(~MinS, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_NJ, mapping=aes(x=exp_keep, y = keep_num, col = SeasL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,1e7) +
  # ylim(0,1e7) +
  xlim(0,3e7) + #low bbmsy
  ylim(0,3e7) +
  facet_wrap(~SeasL, scales = "free_y")

hist(RDMoutputscramble_facet_mse_NJ$BagL)
hist(RDMoutputscramble_facet_mse_NJ$MinS)
hist(RDMoutputscramble_facet_mse_NJ$SeasL)
hist(RDMoutputscramble_facet_mse_NJ$SSBcov)

#NY

RDMoutputscramble_facet_mse_NY <- all_results_mse %>% filter(state == "NY") %>%
  filter(mse > NYstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_NY, mapping=aes(x=exp_keep, y = keep_num, col = BagL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,1e7) +
 # ylim(0,1e7) +
  xlim(0,2.5e7) + #low bbmsy
  ylim(0,2.5e7) +
  facet_wrap(~BagL, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_NY, mapping=aes(x=exp_keep, y = keep_num, col = MinS)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,1e7) +
  # ylim(0,1e7) +
  xlim(0,2.5e7) + #low bbmsy
  ylim(0,2.5e7) +
  facet_wrap(~MinS, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_NY, mapping=aes(x=exp_keep, y = keep_num, col = SeasL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,1e7) +
  # ylim(0,1e7) +
  xlim(0,2.5e7) + #low bbmsy
  ylim(0,2.5e7) +
  facet_wrap(~SeasL, scales = "free_y")

hist(RDMoutputscramble_facet_mse_NY$BagL)
hist(RDMoutputscramble_facet_mse_NY$MinS)
hist(RDMoutputscramble_facet_mse_NY$SeasL)
hist(RDMoutputscramble_facet_mse_NY$SSBcov)

#RI

RDMoutputscramble_facet_mse_RI <- all_results_mse %>% filter(state == "RI") %>%
  filter(mse > RIstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_RI, mapping=aes(x=exp_keep, y = keep_num, col = BagL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,2.5e6) +
 # ylim(0,2.5e6) +
  xlim(0,5e6) + #low bbmsy
  ylim(0,5e6) +
  facet_wrap(~BagL, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_RI, mapping=aes(x=exp_keep, y = keep_num, col = MinS)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,2.5e6) +
  # ylim(0,2.5e6) +
  xlim(0,5e6) + #low bbmsy
  ylim(0,5e6) +
  facet_wrap(~MinS, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_RI, mapping=aes(x=exp_keep, y = keep_num, col = SeasL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,2.5e6) +
  # ylim(0,2.5e6) +
  xlim(0,5e6) + #low bbmsy
  ylim(0,5e6) +
  facet_wrap(~SeasL, scales = "free_y")

hist(RDMoutputscramble_facet_mse_RI$BagL)
hist(RDMoutputscramble_facet_mse_RI$MinS)
hist(RDMoutputscramble_facet_mse_RI$SeasL)
hist(RDMoutputscramble_facet_mse_RI$SSBcov)

#VA
RDMoutputscramble_facet_mse_VA <- all_results_mse %>% filter(state == "VA") %>%
  filter(mse > VAstatequart)

#Bag
ggplot(RDMoutputscramble_facet_mse_VA, mapping=aes(x=exp_keep, y = keep_num, col = BagL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
 # xlim(0,1e6) +
 # ylim(0,1e6) +
  xlim(0,2.5e6) + #low bbmsy
  ylim(0,2.5e6) +
  facet_wrap(~BagL, scales = "free_y")

#MinLen
ggplot(RDMoutputscramble_facet_mse_VA, mapping=aes(x=exp_keep, y = keep_num, col = MinS)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,1e6) +
  # ylim(0,1e6) +
  xlim(0,2.5e6) + #low bbmsy
  ylim(0,2.5e6) +
  facet_wrap(~MinS, scales = "free_y")

#SeasonLen
ggplot(RDMoutputscramble_facet_mse_VA, mapping=aes(x=exp_keep, y = keep_num, col = SeasL)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  # xlim(0,1e6) +
  # ylim(0,1e6) +
  xlim(0,2.5e6) + #low bbmsy
  ylim(0,2.5e6) +
  facet_wrap(~SeasL, scales = "free_y")

hist(RDMoutputscramble_facet_mse_VA$BagL)
hist(RDMoutputscramble_facet_mse_VA$MinS)
hist(RDMoutputscramble_facet_mse_VA$SeasL)
hist(RDMoutputscramble_facet_mse_VA$SSBcov)

########  comparing GAM predictions in here to do_recmeasures_hcr.R -> they're the same #############
Bag <- all_results[2:10,]$bag #4
MinLen <- all_results[2:10,]$minlen #17.5
SeasonLen <- all_results[2:10,]$seaslen #150
SSB <- all_results[2:10,]$biomass #26051.37 SSB <- 30155
state <- all_results[2:10,]$state
testdata <- data.frame(Bag, MinLen, SeasonLen, SSB, state)

all_results[2,]$exp_keep

View(all_results)

g2pred <- predict.gam(g1, newdata = testdata, type = "response") 

sum(g2pred)

################ DISCARDS ################

# ------------------------------- DISC GAM 1-------------------------------------------------#
d1 <- gam(tot_rel ~  state + s(SSB, k = 3) + s(SeasonLen, k = 3) + 
            s(Bag, k = 3) + s(MinLen, k = 3), data = inputdata, 
          family = Gamma(link = log), method = "REML") 
summary(d1)
gam.check(d1)
#saveRDS(d1, "~/Desktop/FlounderMSE/RDMgam/gam_RDM_disc.rds") #disordered length
#d1 <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDM_disc.rds") #disordered length

inputdatadisc$state <- as.factor(inputdatadisc$state)

d2 <- gam(tot_rel ~ # s(SSB, state, bs = "fs", k = 3) + 
            s(SeasonLen, state, bs = "fs", k = 3) + 
            s(Bag, state, bs = "fs", k = 3) +
            s(MinLen, state, bs = "fs", k = 3) + 
            s(SumRel, SSB, k = 3), 
          data = inputdatadisc, #run troubleshootdiscardsGAM code for inputdatadisc
          family = Gamma(link = log), method = "REML") 
summary(d2)
appraise(d2)
draw(d2)

########  hierarchical GAM ################

inputdata$state <- as.factor(inputdata$state)
#inputdata_mseruns$state <- as.factor(inputdata_mseruns$state)

#hierarchical GAM

#group-level smoothers that all have the same wiggliness
g1.1 <- gam(tot_keep ~  s(SSB, state, bs = "fs", k = 3) + 
            s(SeasonLen, state, bs = "fs", k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
            s(Bag, state, bs = "fs", k = 3) + s(MinLen, state, bs = "fs", k = 3), 
          data = inputdata, #either isolated RDM runs or total 
          family = Gamma(link = log), method = "REML") #random effect on spline for minlen that introduces  a sort of random effect by state 
#saveRDS(g1.1, "~/Desktop/FlounderMSE/RDMgam/gam_RDMland_factored.rds") #disordered length
#g1.1 <- readRDS("~/Desktop/FlounderMSE/RDMgam/gam_RDMland_factored.rds") #disordered length
summary(g1.1)
draw(g1.1)
appraise(g1.1)
g1.1pred <- predict(g1.1, type = "response") #if not 
plot(inputdata$tot_keep ~ g1.1pred, xlim = c(0,1e7), ylim = c(0,1e7)) + abline(b = 1, a = 0, col = "red")
#looks pretty good

#plot partial effects with legend 
sm <- smooth_estimates(g1.1)
sm_fs <- sm[grep("SSB", sm$.smooth), ]
ggplot(sm_fs, aes(x = SSB, y = .estimate, colour = state)) +
  geom_line() +
 # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "SSB", color = "state", fill = "state")

sm <- smooth_estimates(g1.1)
sm_fs <- sm[grep("SeasonLen", sm$.smooth), ]
ggplot(sm_fs, aes(x = SeasonLen, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "SeasonLen", color = "state", fill = "state")

sm <- smooth_estimates(g1.1)
sm_fs <- sm[grep("MinLen", sm$.smooth), ]
ggplot(sm_fs, aes(x = MinLen, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "MinLen", color = "state", fill = "state")

sm <- smooth_estimates(g1.1)
sm_fs <- sm[grep("Bag", sm$.smooth), ]
ggplot(sm_fs, aes(x = Bag, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Bag", color = "state", fill = "state")

#global smoother, random effect on state
#random effect smooth -> k is always set equal to number of levels in grouping variable
g1.2 <- gam(tot_keep ~  s(state, bs = "re") + s(SSB, bs = "tp", k = 3) + 
              s(SeasonLen, bs = "tp", k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
              s(Bag, bs = "tp", k = 3) + s(MinLen, bs = "tp", k = 3), 
            data = inputdata, #either isolated RDM runs or total 
            family = Gamma(link = log), method = "REML")
summary(g1.2)
draw(g1.2)
appraise(g1.2)
g1.2pred <- predict(g1.2, type = "response") #if not 
plot(inputdata$tot_keep ~ g1.2pred, xlim = c(0,1e7), ylim = c(0,1e7)) + abline(b = 1, a = 0, col = "red")
#does not fix the state deviation

#tensor product of continuous smoothers and random effect for the grouping parameter 
g1.3 <- gam(tot_keep ~ t2(SSB, state, bs = c("tp", "re")) + 
              t2(SeasonLen, state, bs = c("tp", "re")) + #9,5,7 partial effect of smooths look strange with k any higher 
              t2(Bag, state, bs = c("tp", "re")) + t2(MinLen, state, bs = c("tp", "re")), 
            data = inputdata, #either isolated RDM runs or total 
            family = Gamma(link = log), method = "REML")
summary(g1.3)
draw(g1.3) #not sure how to plot partial effects
appraise(g1.3)
g1.3pred <- predict(g1.3, type = "response") #if not 
plot(inputdata$tot_keep ~ g1.3pred, xlim = c(0,1e7), ylim = c(0,1e7)) + abline(b = 1, a = 0, col = "red")

#library(visreg)
#visreg2d(g1.3, xvar = "SSB", yvar = "state")
#visreg2d(g1.3, xvar = "MinLen", yvar = "state")
#visreg2d(g1.3, xvar = "Bag", yvar = "state")
#visreg2d(g1.3, xvar = "SeasonLen", yvar = "state")

#plot partial effects with legend 
sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("SSB", sm$.smooth), ]
ggplot(sm_fs, aes(x = SSB, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "SSB", color = "state", fill = "state")

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("SeasonLen", sm$.smooth), ]
ggplot(sm_fs, aes(x = SeasonLen, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "SeasonLen", color = "state", fill = "state")

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("MinLen", sm$.smooth), ]
ggplot(sm_fs, aes(x = MinLen, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "MinLen", color = "state", fill = "state")

sm <- smooth_estimates(g1.3)
sm_fs <- sm[grep("Bag", sm$.smooth), ]
ggplot(sm_fs, aes(x = Bag, y = .estimate, colour = state)) +
  geom_line() +
  # geom_ribbon(aes(ymin = est - 2 * se, ymax = est + 2 * se, fill = by), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Bag", color = "state", fill = "state")

#allow group-level wiggliness for factor smooth with thin plate  
g1.4 <- gam(tot_keep ~  s(SSB, state, bs = "fs", m = 1,  k = 3) + 
              s(SeasonLen, state, bs = "fs", m = 1,  k = 3) + #9,5,7 partial effect of smooths look strange with k any higher 
              s(Bag, state, bs = "fs", m = 1, k = 3) + s(MinLen, state, bs = "fs", m = 1, k = 3), 
            data = inputdata, #either isolated RDM runs or total 
            family = Gamma(link = log), method = "REML") #random effect on spline for minlen that introduces  a sort of random effect by state 
summary(g1.4)
draw(g1.4)
appraise(g1.4)
g1.4pred <- predict(g1.4, type = "response") #if not 
plot(inputdata$tot_keep ~ g1.4pred, xlim = c(0,1e7), ylim = c(0,1e7)) + abline(b = 1, a = 0, col = "red")
#not sure if this really adds any improvement -> AIC update: does not, is actually worse than common wiggliness

#allow group-level wiggliness for tensor product 
g1.5 <- gam(tot_keep ~ t2(SSB, state, bs = c("tp", "re"), m = 1) + 
              t2(SeasonLen, state, bs = c("tp", "re"), m = 1) + #9,5,7 partial effect of smooths look strange with k any higher 
              t2(Bag, state, bs = c("tp", "re"), m = 1) + t2(MinLen, state, bs = c("tp", "re"), m = 1), 
            data = inputdata, #either isolated RDM runs or total 
            family = Gamma(link = log), method = "REML")
summary(g1.5)
draw(g1.5) 
appraise(g1.5)
g1.5pred <- predict(g1.5, type = "response") #if not 
plot(inputdata$tot_keep ~ g1.5pred, xlim = c(0,1e7), ylim = c(0,1e7)) + abline(b = 1, a = 0, col = "red")
#does not help compared to g1.3

#AIC
AIC(g1.1, g1.2, g1.3, g1.4, g1.5)

#test runtime
input_seas2 <- c( 60,
                  75,
                  90,
                  105,
                  120,
                  135,
                  150, 
                  165,
                  180, 
                  195,
                  210,
                  225,
                  240,
                  255,
                  270, 
                  285,
                  300)
input_bag2 <- c(4,5,6,7,8)
input_minlen2 <- c(14,14.5,15,15.5,16,16.5,17,17.5,18,18.5,19,19.5,20,20.5,21)
state2 <- c("NC", "VA", "MA", "RI", "NY", "NJ", "CT", "DE", "MD")
SSB <- inputdata$SSB[1]
comb_input2 <- expand.grid(SeasonLen = unique(input_seas2), Bag = unique(input_bag2),
                           MinLen = unique(input_minlen2), state = unique(state2))
comb_input2 <- comb_input2 %>% mutate(SSB = SSB)

#newer way
gamfit2 <- predict.gam(g1.1, newdata = comb_input2, type = "response" , se.fit = TRUE)
flukecatch1 <- comb_input2 %>% mutate(land = gamfit2$fit)
View(flukecatch)
View(flukecatch1)
# I can't believe I didn't just do this in the first place 

################### --------------- PREDICTIONS ---------------- #####################

#WITHIN SAMPLE 

#land 
preddata1 <- dplyr::select(inputdata[1:9,], state, SeasonLen, Bag, MinLen, SSB)
#preddata1 <- preddata1 %>% mutate(SeasonLen = 150, Bag = 4, MinLen = 17.5, SSB = 30155)

gamfit2 <- predict.gam(g1, newdata = preddata1, type = "link" , se.fit = TRUE)
exp(gamfit2$fit)

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
mse <- mean((loo_predictions - log(data$tot_keep))^2) #keep in logspace since model was fit in logspace 0.1376026
rmse <- sqrt(mse) #0.3709482
mae <- mean(abs(loo_predictions - log(data$tot_keep))) #0.2902894

#residuals and predictions
residuals <- data$tot_keep - exp(loo_predictions)
plot(residuals ~ data$tot_keep)

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


############# FOR DETERMINING SCALARS FOR do_recmeasures_hcr.R: NO LONGER USED ##########################################

scalarinput <- dplyr::select(inputdata[10:18,], state, SeasonLen, Bag, MinLen, SSB) # pick number where "CT" is first state in list 
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
#saveRDS(GAMscalar, "~/Desktop/FlounderMSE/RDMGam/GAMscalar.rds")
#readRDS("~/Desktop/FlounderMSE/RDMGam/GAMscalar.rds")

#ORIGINAL GAMS FOR COMPARISON

#land
#gamland <- readRDS("~/Desktop/FlounderMSE/gam_land.rds")
#appraise(gamland)
#draw(gamland)

#disc
#gamdisc <- readRDS("~/Desktop/FlounderMSE/gam_disc.rds")
#appraise(gamdisc)
#draw(gamdisc)