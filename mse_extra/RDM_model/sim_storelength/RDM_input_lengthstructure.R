library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)

setwd("~/Desktop/FlounderMSE")

blankinput <- readRDS("blankinputtables.rds") # here is the blank input table to populate with new regs

#LOOPING THROUGH RDM METHOD ###########################################################################

#Alternative = "scen1"
#Alternative = "scen2"
#Alternative = "scen3"

Alternative1 <- c("scen1", "scen2", "scen3", "scen4", "scen5", "scen6", "scen7", "scen8", "scen9")

for(x in 1:9){
Alternative = Alternative1[x]
#extract regulations for each time step 
if(Alternative == "scen1"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength")}
if(Alternative == "scen1"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength/"}
if(Alternative == "scen2"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal")}
if(Alternative == "scen2"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal/"}
if(Alternative == "scen3"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative")}
if(Alternative == "scen3"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative/"}
if(Alternative == "scen4"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength1")}
if(Alternative == "scen4"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength1/"}
if(Alternative == "scen5"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal1")}
if(Alternative == "scen5"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal1/"}
if(Alternative == "scen6"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative1")}
if(Alternative == "scen6"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative1/"}
if(Alternative == "scen7"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength7")}
if(Alternative == "scen7"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength7/"}
if(Alternative == "scen8"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal7")}
if(Alternative == "scen8"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal7/"}
if(Alternative == "scen9"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative7")}
if(Alternative == "scen9"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative7/"}


  rbctrack <- read.table(paste0(scendir, "rbctrack.out"), header = FALSE, skip = 1)
  nsim = length(unique(rbctrack[,1]))
  rbc2 <- rbctrack %>% 
    tibble() %>% 
    select(1,2,4,15,9,10,11,12,18,22) %>% 
    I()
  rbc2 <- rbctrack %>% 
    tibble() %>% 
    select(1,2,4,8,9,11,12,18,15:17) %>% 
    I()
  names(rbc2) <- c("isim","year","abc","fmsy","est_ofl","rhl","bmsy","exp_keep","seaslen","bag","minlen") #,"msy")
  rbc2 <- rbc2 %>% 
    mutate(exp_keep = as.numeric(ifelse(exp_keep == "******************",NA,exp_keep))) %>% 
    mutate(year = 2019 + 2*year - 1) %>% 
    I()
  
rbc3 <- rbc2[rep(1:nrow(rbc2), each = 2),]
rbc3$year[seq(2, nrow(rbc3), 2)] <- rbc3$year[seq(1, nrow(rbc3)-1, 2)] + 1
rbc4 <- rbc3[rep(1:nrow(rbc3), each = 9),]
rbc4 <- rbc4 %>% mutate(State = rep(c("NC", "VA", "MD", "DE", "NJ", "NY", "CT", "RI", "MA"), times = 26))

rbc4 <- select(rbc4, year, bag, seaslen, minlen, State)
colnames(rbc4) <- c("Year", "Bag", "SeasonLen", "MinLen", "State")
#View(rbc4)
#starting regs

#Bag <- rep(4, times = 9)
#MinLen <- rep(17.5, times = 9)
#SeasonLen <- rep(150, times = 9)
#State <- c("NC", "VA", "MD", "DE", "NJ", "NY", "CT", "RI", "MA")
#Year <- rep(2019, times = 9)

n = 1

## -----------------------------------------------------------------------------

NJtableHCR <- rbc4 %>% filter(State == "NJ")

NJdatalist = list()
NJdatalist = vector("list", length = n)

for (x in 1:length(NJtableHCR$Year)){
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
  NJdatalist[[x]] <- NJinputmiddleman %>% mutate(Nsim = NJtableHCR$Year[x])
}
NJbig_data = do.call(rbind, NJdatalist)

## -----------------------------------------------------------------------------

NYtableHCR <- rbc4 %>% filter(State == "NY")

NYdatalist = list()
NYdatalist = vector("list", length = n)

for (x in 1:length(NYtableHCR$Year)){
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
  NYdatalist[[x]] <- NYinputmiddleman %>% mutate(Nsim = NYtableHCR$Year[x])
}
NYbig_data = do.call(rbind, NYdatalist)


## -----------------------------------------------------------------------------

NCtableHCR <- rbc4 %>% filter(State == "NC")

NCdatalist = list()
NCdatalist = vector("list", length = n)

for (x in 1:length(NCtableHCR$Year)){
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
  NCdatalist[[x]] <- NCinputmiddleman %>% mutate(Nsim = NCtableHCR$Year[x])
}
NCbig_data = do.call(rbind, NCdatalist)


## -----------------------------------------------------------------------------

RItableHCR <- rbc4 %>% filter(State == "RI")

RIdatalist = list()
RIdatalist = vector("list", length = n)

for (x in 1:length(RItableHCR$Year)){
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
  RIdatalist[[x]] <- RIinputmiddleman %>% mutate(Nsim = RItableHCR$Year[x])
}
RIbig_data = do.call(rbind, RIdatalist)


## -----------------------------------------------------------------------------

MDtableHCR <- rbc4 %>% filter(State == "MD")

MDdatalist = list()
MDdatalist = vector("list", length = n)

for (x in 1:length(MDtableHCR$Year)){
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
  MDdatalist[[x]] <- MDinputmiddleman %>% mutate(Nsim = MDtableHCR$Year[x])
}
MDbig_data = do.call(rbind, MDdatalist)


## -----------------------------------------------------------------------------

VAtableHCR <- rbc4 %>% filter(State == "VA")

VAdatalist = list()
VAdatalist = vector("list", length = n)

for (x in 1:length(VAtableHCR$Year)){
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
  VAdatalist[[x]] <- VAinputmiddleman %>% mutate(Nsim = VAtableHCR$Year[x])
}
VAbig_data = do.call(rbind, VAdatalist)


## -----------------------------------------------------------------------------

CTtableHCR <- rbc4 %>% filter(State == "CT")

CTdatalist = list()
CTdatalist = vector("list", length = n)

for (x in 1:length(CTtableHCR$Year)){
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
  CTdatalist[[x]] <- CTinputmiddleman %>% mutate(Nsim = CTtableHCR$Year[x])
}
CTbig_data = do.call(rbind, CTdatalist)


## -----------------------------------------------------------------------------

DEtableHCR <- rbc4 %>% filter(State == "DE")

DEdatalist = list()
DEdatalist = vector("list", length = n)

for (x in 1:length(DEtableHCR$Year)){
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
  DEdatalist[[x]] <- DEinputmiddleman %>% mutate(Nsim = DEtableHCR$Year[x])
}
DEbig_data = do.call(rbind, DEdatalist)


## -----------------------------------------------------------------------------

MAtableHCR <- rbc4 %>% filter(State == "MA")

MAdatalist = list()
MAdatalist = vector("list", length = n)

for (x in 1:length(MAtableHCR$Year)){
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
  MAdatalist[[x]] <- MAinputmiddleman %>% mutate(Nsim = MAtableHCR$Year[x])
}
MAbig_data = do.call(rbind, MAdatalist)

Big_Data <- rbind(CTbig_data, DEbig_data, MAbig_data, MDbig_data, NCbig_data, NJbig_data, NYbig_data, 
                  RIbig_data, VAbig_data)

Big_Data <- Big_Data %>% arrange(Big_Data$Nsim)
#View(Big_Data)

n = 26

bigdatalist = list()
bigdatalist = vector("list", length = n)

for (x in 1:26){
  Big_Data2 <- Big_Data %>% filter(Nsim == rbc3$year[x] )
 # Big_Data2$Nsim <- NULL
  bigdatalist[[x]] <- Big_Data2
}

#View(bigdatalist[[1]])
#View(directed_trips_table)

#RUN RDM for each scen

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

n = 26

RDMoutput_edit = list()
RDMoutput_edit = vector("list", length = n)

#WITHOUT PERIOD/WAVE RESOLUTION

for (x in 1:26){
  
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
  
  # Read-in current population length composition (from sinatra output)
  Nlen <- 42
  iyr <- bigdatalist[[x]]$Nsim[1]
  om_length_cm <- if(iyr == 2020){scan("om-length2020.dat",n=Nlen+1)} else
    if(iyr == 2021){scan("om-length2021.dat",n=Nlen+1)} else
      if(iyr == 2022){scan("om-length2022.dat",n=Nlen+1)} else
        if(iyr == 2023){scan("om-length2023.dat",n=Nlen+1)} else
          if(iyr == 2024){scan("om-length2024.dat",n=Nlen+1)} else
            if(iyr == 2025){scan("om-length2025.dat",n=Nlen+1)} else
              if(iyr == 2026){scan("om-length2026.dat",n=Nlen+1)} else
                if(iyr == 2027){scan("om-length2027.dat",n=Nlen+1)} else
                  if(iyr == 2028){scan("om-length2028.dat",n=Nlen+1)} else
                    if(iyr == 2029){scan("om-length2029.dat",n=Nlen+1)} else
                      if(iyr == 2030){scan("om-length2030.dat",n=Nlen+1)} else
                        if(iyr == 2031){scan("om-length2031.dat",n=Nlen+1)} else
                          if(iyr == 2032){scan("om-length2032.dat",n=Nlen+1)} else
                            if(iyr == 2033){scan("om-length2033.dat",n=Nlen+1)} else
                              if(iyr == 2034){scan("om-length2034.dat",n=Nlen+1)} else
                                if(iyr == 2035){scan("om-length2035.dat",n=Nlen+1)} else
                                  if(iyr == 2036){scan("om-length2036.dat",n=Nlen+1)} else
                                    if(iyr == 2037){scan("om-length2037.dat",n=Nlen+1)} else
                                      if(iyr == 2038){scan("om-length2038.dat",n=Nlen+1)} else
                                        if(iyr == 2039){scan("om-length2039.dat",n=Nlen+1)} else
                                          if(iyr == 2040){scan("om-length2040.dat",n=Nlen+1)} else
                                            if(iyr == 2041){scan("om-length2041.dat",n=Nlen+1)} else
                                              if(iyr == 2042){scan("om-length2042.dat",n=Nlen+1)} else
                                                if(iyr == 2043){scan("om-length2043.dat",n=Nlen+1)} else
                                                  if(iyr == 2044){scan("om-length2044.dat",n=Nlen+1)} else
                                                    if(iyr == 2045){scan("om-length2045.dat",n=Nlen+1)} else
                                                      if(iyr == 2046){scan("om-length2046.dat",n=Nlen+1)} else
                                                        if(iyr == 2047){scan("om-length2047.dat",n=Nlen+1)} else
                                                          if(iyr == 2048){scan("om-length2048.dat",n=Nlen+1)} else
                                                            if(iyr == 2049){scan("om-length2049.dat",n=Nlen+1)} else
                                                              if(iyr == 2050){scan("om-length2050.dat",n=Nlen+1)}
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
  RDMoutput_edit[[x]] <- extra_output %>% mutate(Nsim = x, SeasonLen = NJtableHCR$SeasonLen[x], Bag = NJtableHCR$Bag[x],
                                                 MinLen = NJtableHCR$MinLen[x], Year = NJtableHCR$Year[x])
 write.table(extra_output,file = "rec-catch.out", append = TRUE, row.names = FALSE, col.names = FALSE)
}

RDMoutputbind_edit <- bind_rows(RDMoutput_edit)
#if(Alternative == "scen1"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen1.rds")}
if(Alternative == "scen1"){RDMoutputbind_scen1 <- RDMoutputbind_edit}
#if(Alternative == "scen2"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen2.rds")}
if(Alternative == "scen2"){RDMoutputbind_scen2 <- RDMoutputbind_edit}
#if(Alternative == "scen3"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen3.rds")}
if(Alternative == "scen3"){RDMoutputbind_scen3 <- RDMoutputbind_edit}
#if(Alternative == "scen4"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen4.rds")}
if(Alternative == "scen4"){RDMoutputbind_scen4 <- RDMoutputbind_edit}
#if(Alternative == "scen5"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen5.rds")}
if(Alternative == "scen5"){RDMoutputbind_scen5 <- RDMoutputbind_edit}
#if(Alternative == "scen6"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen6.rds")}
if(Alternative == "scen6"){RDMoutputbind_scen6 <- RDMoutputbind_edit}
#if(Alternative == "scen7"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen7.rds")}
if(Alternative == "scen7"){RDMoutputbind_scen7 <- RDMoutputbind_edit}
#if(Alternative == "scen8"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen8.rds")}
if(Alternative == "scen8"){RDMoutputbind_scen8 <- RDMoutputbind_edit}
#if(Alternative == "scen9"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen9.rds")}
if(Alternative == "scen9"){RDMoutputbind_scen9 <- RDMoutputbind_edit}
#RDMoutputbind_edit <- readRDS("RDMoutputbind_scen1.rds")
}
#View(RDMoutputbind_scen1)
RDMoutput_edit2 <- select(as.data.frame(rbind(RDMoutputbind_scen1, RDMoutputbind_scen2,
                                              RDMoutputbind_scen3, RDMoutputbind_scen4,
                                              RDMoutputbind_scen5, RDMoutputbind_scen6,
                                              RDMoutputbind_scen7, RDMoutputbind_scen8,
                                              RDMoutputbind_scen9)), state, tot_keep, tot_rel, SeasonLen, Bag, MinLen, Year)
saveRDS(RDMoutput_edit2, "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/RDMoutputbind_all.rds")
#View(RDMoutputbind_scen4)


#LOOPING THROUGH RDM METHOD NUMBER LENGTH STRUCTURE ###########################################################################

#Alternative = "scen1"
#Alternative = "scen2"
#Alternative = "scen3"

Alternative1 <- c("scen1", "scen2", "scen3", "scen4", "scen5", "scen6", "scen7", "scen8", "scen9")

for(x in 1:9){
  Alternative = Alternative1[x]
  #extract regulations for each time step 
  if(Alternative == "scen1"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength")}
  if(Alternative == "scen1"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength/"}
  if(Alternative == "scen2"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal")}
  if(Alternative == "scen2"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal/"}
  if(Alternative == "scen3"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative")}
  if(Alternative == "scen3"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative/"}
  if(Alternative == "scen4"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength1")}
  if(Alternative == "scen4"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength1/"}
  if(Alternative == "scen5"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal1")}
  if(Alternative == "scen5"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal1/"}
  if(Alternative == "scen6"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative1")}
  if(Alternative == "scen6"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative1/"}
  if(Alternative == "scen7"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength7")}
  if(Alternative == "scen7"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength7/"}
  if(Alternative == "scen8"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal7")}
  if(Alternative == "scen8"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal7/"}
  if(Alternative == "scen9"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative7")}
  if(Alternative == "scen9"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative7/"}
  
  
  rbctrack <- read.table(paste0(scendir, "rbctrack.out"), header = FALSE, skip = 1)
  nsim = length(unique(rbctrack[,1]))
  rbc2 <- rbctrack %>% 
    tibble() %>% 
    select(1,2,4,15,9,10,11,12,18,22) %>% 
    I()
  rbc2 <- rbctrack %>% 
    tibble() %>% 
    select(1,2,4,8,9,11,12,18,15:17) %>% 
    I()
  names(rbc2) <- c("isim","year","abc","fmsy","est_ofl","rhl","bmsy","exp_keep","seaslen","bag","minlen") #,"msy")
  rbc2 <- rbc2 %>% 
    mutate(exp_keep = as.numeric(ifelse(exp_keep == "******************",NA,exp_keep))) %>% 
    mutate(year = 2019 + 2*year - 1) %>% 
    I()
  
  rbc3 <- rbc2[rep(1:nrow(rbc2), each = 2),]
  rbc3$year[seq(2, nrow(rbc3), 2)] <- rbc3$year[seq(1, nrow(rbc3)-1, 2)] + 1
  rbc4 <- rbc3[rep(1:nrow(rbc3), each = 9),]
  rbc4 <- rbc4 %>% mutate(State = rep(c("NC", "VA", "MD", "DE", "NJ", "NY", "CT", "RI", "MA"), times = 26))
  
  rbc4 <- select(rbc4, year, bag, seaslen, minlen, State)
  colnames(rbc4) <- c("Year", "Bag", "SeasonLen", "MinLen", "State")
  #View(rbc4)
  #starting regs
  
  #Bag <- rep(4, times = 9)
  #MinLen <- rep(17.5, times = 9)
  #SeasonLen <- rep(150, times = 9)
  #State <- c("NC", "VA", "MD", "DE", "NJ", "NY", "CT", "RI", "MA")
  #Year <- rep(2019, times = 9)
  
  n = 1
  
  ## -----------------------------------------------------------------------------
  
  NJtableHCR <- rbc4 %>% filter(State == "NJ")
  
  NJdatalist = list()
  NJdatalist = vector("list", length = n)
  
  for (x in 1:length(NJtableHCR$Year)){
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
    NJdatalist[[x]] <- NJinputmiddleman %>% mutate(Nsim = NJtableHCR$Year[x])
  }
  NJbig_data = do.call(rbind, NJdatalist)
  
  ## -----------------------------------------------------------------------------
  
  NYtableHCR <- rbc4 %>% filter(State == "NY")
  
  NYdatalist = list()
  NYdatalist = vector("list", length = n)
  
  for (x in 1:length(NYtableHCR$Year)){
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
    NYdatalist[[x]] <- NYinputmiddleman %>% mutate(Nsim = NYtableHCR$Year[x])
  }
  NYbig_data = do.call(rbind, NYdatalist)
  
  
  ## -----------------------------------------------------------------------------
  
  NCtableHCR <- rbc4 %>% filter(State == "NC")
  
  NCdatalist = list()
  NCdatalist = vector("list", length = n)
  
  for (x in 1:length(NCtableHCR$Year)){
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
    NCdatalist[[x]] <- NCinputmiddleman %>% mutate(Nsim = NCtableHCR$Year[x])
  }
  NCbig_data = do.call(rbind, NCdatalist)
  
  
  ## -----------------------------------------------------------------------------
  
  RItableHCR <- rbc4 %>% filter(State == "RI")
  
  RIdatalist = list()
  RIdatalist = vector("list", length = n)
  
  for (x in 1:length(RItableHCR$Year)){
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
    RIdatalist[[x]] <- RIinputmiddleman %>% mutate(Nsim = RItableHCR$Year[x])
  }
  RIbig_data = do.call(rbind, RIdatalist)
  
  
  ## -----------------------------------------------------------------------------
  
  MDtableHCR <- rbc4 %>% filter(State == "MD")
  
  MDdatalist = list()
  MDdatalist = vector("list", length = n)
  
  for (x in 1:length(MDtableHCR$Year)){
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
    MDdatalist[[x]] <- MDinputmiddleman %>% mutate(Nsim = MDtableHCR$Year[x])
  }
  MDbig_data = do.call(rbind, MDdatalist)
  
  
  ## -----------------------------------------------------------------------------
  
  VAtableHCR <- rbc4 %>% filter(State == "VA")
  
  VAdatalist = list()
  VAdatalist = vector("list", length = n)
  
  for (x in 1:length(VAtableHCR$Year)){
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
    VAdatalist[[x]] <- VAinputmiddleman %>% mutate(Nsim = VAtableHCR$Year[x])
  }
  VAbig_data = do.call(rbind, VAdatalist)
  
  
  ## -----------------------------------------------------------------------------
  
  CTtableHCR <- rbc4 %>% filter(State == "CT")
  
  CTdatalist = list()
  CTdatalist = vector("list", length = n)
  
  for (x in 1:length(CTtableHCR$Year)){
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
    CTdatalist[[x]] <- CTinputmiddleman %>% mutate(Nsim = CTtableHCR$Year[x])
  }
  CTbig_data = do.call(rbind, CTdatalist)
  
  
  ## -----------------------------------------------------------------------------
  
  DEtableHCR <- rbc4 %>% filter(State == "DE")
  
  DEdatalist = list()
  DEdatalist = vector("list", length = n)
  
  for (x in 1:length(DEtableHCR$Year)){
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
    DEdatalist[[x]] <- DEinputmiddleman %>% mutate(Nsim = DEtableHCR$Year[x])
  }
  DEbig_data = do.call(rbind, DEdatalist)
  
  
  ## -----------------------------------------------------------------------------
  
  MAtableHCR <- rbc4 %>% filter(State == "MA")
  
  MAdatalist = list()
  MAdatalist = vector("list", length = n)
  
  for (x in 1:length(MAtableHCR$Year)){
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
    MAdatalist[[x]] <- MAinputmiddleman %>% mutate(Nsim = MAtableHCR$Year[x])
  }
  MAbig_data = do.call(rbind, MAdatalist)
  
  Big_Data <- rbind(CTbig_data, DEbig_data, MAbig_data, MDbig_data, NCbig_data, NJbig_data, NYbig_data, 
                    RIbig_data, VAbig_data)
  
  Big_Data <- Big_Data %>% arrange(Big_Data$Nsim)
  #View(Big_Data)
  
  n = 26
  
  bigdatalist = list()
  bigdatalist = vector("list", length = n)
  #x=1
  for (x in 1:26){
    Big_Data2 <- Big_Data %>% filter(Nsim == rbc3$year[x] )
    # Big_Data2$Nsim <- NULL
    bigdatalist[[x]] <- Big_Data2
  }
  
  #View(bigdatalist[[1]])
  #View(directed_trips_table)
  
  #RUN RDM for each scen
  
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
  
  n = 26
  
  RDMoutput_edit = list()
  RDMoutput_edit = vector("list", length = n)
  
  #WITHOUT PERIOD/WAVE RESOLUTION
  
  for (x in 1:26){
   # x = 20
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
    
    # Read-in current population length composition (from sinatra output)
    Nlen <- 42
    iyr <- bigdatalist[[x]]$Nsim[1]
    om_length_cm <- if(iyr == 2020){scan("om-length2020.dat",n=Nlen+1)} else
      if(iyr == 2021){scan("om-length2021.dat",n=Nlen+1)} else
        if(iyr == 2022){scan("om-length2022.dat",n=Nlen+1)} else
          if(iyr == 2023){scan("om-length2023.dat",n=Nlen+1)} else
            if(iyr == 2024){scan("om-length2024.dat",n=Nlen+1)} else
              if(iyr == 2025){scan("om-length2025.dat",n=Nlen+1)} else
                if(iyr == 2026){scan("om-length2026.dat",n=Nlen+1)} else
                  if(iyr == 2027){scan("om-length2027.dat",n=Nlen+1)} else
                    if(iyr == 2028){scan("om-length2028.dat",n=Nlen+1)} else
                      if(iyr == 2029){scan("om-length2029.dat",n=Nlen+1)} else
                        if(iyr == 2030){scan("om-length2030.dat",n=Nlen+1)} else
                          if(iyr == 2031){scan("om-length2031.dat",n=Nlen+1)} else
                            if(iyr == 2032){scan("om-length2032.dat",n=Nlen+1)} else
                              if(iyr == 2033){scan("om-length2033.dat",n=Nlen+1)} else
                                if(iyr == 2034){scan("om-length2034.dat",n=Nlen+1)} else
                                  if(iyr == 2035){scan("om-length2035.dat",n=Nlen+1)} else
                                    if(iyr == 2036){scan("om-length2036.dat",n=Nlen+1)} else
                                      if(iyr == 2037){scan("om-length2037.dat",n=Nlen+1)} else
                                        if(iyr == 2038){scan("om-length2038.dat",n=Nlen+1)} else
                                          if(iyr == 2039){scan("om-length2039.dat",n=Nlen+1)} else
                                            if(iyr == 2040){scan("om-length2040.dat",n=Nlen+1)} else
                                              if(iyr == 2041){scan("om-length2041.dat",n=Nlen+1)} else
                                                if(iyr == 2042){scan("om-length2042.dat",n=Nlen+1)} else
                                                  if(iyr == 2043){scan("om-length2043.dat",n=Nlen+1)} else
                                                    if(iyr == 2044){scan("om-length2044.dat",n=Nlen+1)} else
                                                      if(iyr == 2045){scan("om-length2045.dat",n=Nlen+1)} else
                                                        if(iyr == 2046){scan("om-length2046.dat",n=Nlen+1)} else
                                                          if(iyr == 2047){scan("om-length2047.dat",n=Nlen+1)} else
                                                            if(iyr == 2048){scan("om-length2048.dat",n=Nlen+1)} else
                                                              if(iyr == 2049){scan("om-length2049.dat",n=Nlen+1)} else
                                                                if(iyr == 2050){scan("om-length2050.dat",n=Nlen+1)}
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
   # View(xx[[6]]$result)
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
    #View(prediction_output_by_period[[4]])
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
      mutate_at(vars(contains("length")), replace_na, replace = 0) #%>%
   #   mutate(Period = c(10:20, 9:19,21, 9:20, 6:20, 8:22, 6:21, 7:21, 7:22, 8:17,20)) %>%
    #  mutate(Wave = ifelse(Period >= 1 & Period <= 4, 1, ifelse(Period >= 5 & Period <= 8, 2, ifelse(Period >= 9 & Period <= 12, 3,
     #                ifelse(Period >= 13 & Period <= 16, 4, ifelse(Period >= 17 & Period <= 20, 5, ifelse(Period >= 21 & Period <= 24, 6, 0)))))))
   
  #  extra_output2 <- extra_output[rep(1:nrow(extra_output), each = 16),] %>% mutate(Length = rep(c(10:24,27)), each = 123)
    #View(extra_output) 
    #View(extra_output2)
    pred_len2 <- tibble(extra_output) %>% 
      dplyr::select(state, contains("length")) %>% 
      pivot_longer(cols = 2:ncol(.), names_to = "bin",values_to = "num") %>% 
      separate(bin, into =c("type","len"),sep = "_length_") %>% 
      mutate(len = as.numeric(len))
    #View(pred_len2)
   # pred_len <- pred_len2 %>% 
   #   group_by(state, type) #%>%
  #   summarize(mulen = sum(len*num)/sum(num), 
     #         .groups = "drop") %>% 
   #   pivot_wider(names_from = type,
      #            names_glue = "mulen_{type}",
       #           values_from = mulen)
    #View(pred_len) 
    MAstring <- rep(10:20, each = 26)
    RIstring <- rep(c(9:19,21), each = 26)
    CTstring <- rep(9:20, each = 26)
    DEstring <- rep(6:21, each = 26)
    NYstring <- rep(6:20, each = 26)
    NJstring <- rep(8:22, each = 26)
    VAstring <- rep(7:22, each = 26)
    NCstring <- rep(c(8:17,20), each = 26)
    MDstring <- rep(7:21, each = 26)
      
    biglen <- pred_len2 %>% 
      filter(type == "keep") %>% 
      arrange(state) %>%
        mutate(Period = c(CTstring, DEstring, MAstring, MDstring,
                          NCstring, NJstring, NYstring, RIstring, VAstring),
               Wave = ifelse(Period >= 1 & Period <= 4, 1, ifelse(Period >= 5 & Period <= 8, 2,
               ifelse(Period >= 9 & Period <= 12, 3, ifelse(Period >= 13 & Period <= 16, 4, 
               ifelse(Period >= 17 & Period <= 20, 5, ifelse(Period >= 21 & Period <= 24, 6, 0)))))))
    
    #View(biglen)
  #    mutate(numbig = ifelse(len>=28,num,0))  %>% 
   #  group_by(state) %>% 
   #   summarize(fracbig = sum(numbig)/sum(num))
  #  pred_len <- left_join(pred_len, biglen, by = c("state"))  
    
 #   extra_output <- extra_output %>% 
     # select(state, tot_keep, tot_rel, observed_trips, n_choice_occasions, change_CS, cost, keep_one) %>% 
    #  group_by(state) %>% 
     # summarize_if(is.numeric, .funs = sum,na.rm=TRUE) %>% 
   #   left_join(pred_len, by = c("state"))
    RDMoutput_edit[[x]] <- biglen %>% mutate(Nsim = x, SeasonLen = NJtableHCR$SeasonLen[x], Bag = NJtableHCR$Bag[x],
                                                 MinLen = NJtableHCR$MinLen[x], Year = NJtableHCR$Year[x])
    #write.table(extra_output,file = "rec-catch.out", append = TRUE, row.names = FALSE, col.names = FALSE)
  }
  
  RDMoutputbind_edit <- bind_rows(RDMoutput_edit)
  #View(RDMoutputbind_edit)
  #if(Alternative == "scen1"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen1.rds")}
  if(Alternative == "scen1"){RDMoutputbind_scen1 <- RDMoutputbind_edit}
  #if(Alternative == "scen2"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen2.rds")}
  if(Alternative == "scen2"){RDMoutputbind_scen2 <- RDMoutputbind_edit}
  #if(Alternative == "scen3"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen3.rds")}
  if(Alternative == "scen3"){RDMoutputbind_scen3 <- RDMoutputbind_edit}
  #if(Alternative == "scen4"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen4.rds")}
  if(Alternative == "scen4"){RDMoutputbind_scen4 <- RDMoutputbind_edit}
  #if(Alternative == "scen5"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen5.rds")}
  if(Alternative == "scen5"){RDMoutputbind_scen5 <- RDMoutputbind_edit}
  #if(Alternative == "scen6"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen6.rds")}
  if(Alternative == "scen6"){RDMoutputbind_scen6 <- RDMoutputbind_edit}
  #if(Alternative == "scen7"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen7.rds")}
  if(Alternative == "scen7"){RDMoutputbind_scen7 <- RDMoutputbind_edit}
  #if(Alternative == "scen8"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen8.rds")}
  if(Alternative == "scen8"){RDMoutputbind_scen8 <- RDMoutputbind_edit}
  #if(Alternative == "scen9"){saveRDS(RDMoutputbind_edit, "RDMoutputbind_scen9.rds")}
  if(Alternative == "scen9"){RDMoutputbind_scen9 <- RDMoutputbind_edit}
  #RDMoutputbind_edit <- readRDS("RDMoutputbind_scen1.rds")
}
#View(RDMoutputbind_scen1)
RDMoutput_edit4 <- select(as.data.frame(rbind(RDMoutputbind_scen1, RDMoutputbind_scen2,
                                              RDMoutputbind_scen3, RDMoutputbind_scen4,
                                              RDMoutputbind_scen5, RDMoutputbind_scen6,
                                              RDMoutputbind_scen7, RDMoutputbind_scen8,
                                              RDMoutputbind_scen9)),
                                 Nsim, state, num, len, SeasonLen, Bag, MinLen, Year, Period, Wave)
RDMoutput_edit5 <- RDMoutput_edit4 %>% mutate(SeasLen = 
                                                    case_when(Wave == 4 ~ 60,
                                                              Wave == 3 & SeasonLen == 75 & num != 0 ~ 15,
                                                              Wave == 3 & SeasonLen == 90 & num != 0 ~ 30,
                                                              Wave == 3 & SeasonLen == 105 & num != 0 ~ 45,
                                                              Wave == 3 & SeasonLen >= 120 & num != 0 ~ 60,
                                                              Wave == 5 & SeasonLen == 135 & num != 0 ~ 15,
                                                              Wave == 5 & SeasonLen == 150 & num != 0 ~ 30,
                                                              Wave == 5 & SeasonLen == 165 & num != 0 ~ 45,
                                                              Wave == 5 & SeasonLen >= 180 & num != 0 ~ 60,
                                                              Wave == 2 & SeasonLen == 195 & num != 0 ~ 15,
                                                              Wave == 2 & SeasonLen == 210 & num != 0 ~ 30,
                                                              Wave == 2 & SeasonLen == 225 & num != 0 ~ 45,
                                                              Wave == 2 & SeasonLen >= 240 & num != 0 ~ 60,
                                                              Wave == 6 & SeasonLen == 255 & num != 0 ~ 15,
                                                              Wave == 6 & SeasonLen == 270 & num != 0 ~ 30,
                                                              Wave == 6 & SeasonLen == 285 & num != 0 ~ 45,
                                                              Wave == 6 & SeasonLen == 300 & num != 0 ~ 60))
RDMoutput_edit5$SeasLen[is.na(RDMoutput_edit5$SeasLen)] <- 0

#saveRDS(RDMoutput_edit5, "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/RDMoutputbind_lengthstructure.rds")
#View(RDMoutput_edit5)


#DIRECTLY FROM recoutput2.out METHOD ###########################################################################

#Alternative = "scen1"
#Alternative = "scen2"
#Alternative = "scen3"

Alternative1 <- c("scen1", "scen2", "scen3","scen4", "scen5", "scen6","scen7", "scen8", "scen9")

for(x in 1:9){
  Alternative <- Alternative1[x]
if(Alternative == "scen1"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength")}
if(Alternative == "scen1"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength/"}
if(Alternative == "scen2"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal")}
if(Alternative == "scen2"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal/"}
if(Alternative == "scen3"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative")}
if(Alternative == "scen3"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative/"}
if(Alternative == "scen4"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength1")}
if(Alternative == "scen4"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength1/"}
if(Alternative == "scen5"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal1")}
if(Alternative == "scen5"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal1/"}
if(Alternative == "scen6"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative1")}
if(Alternative == "scen6"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative1/"}
if(Alternative == "scen7"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength7")}
if(Alternative == "scen7"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelength7/"}
if(Alternative == "scen8"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal7")}
if(Alternative == "scen8"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVliberal7/"}
if(Alternative == "scen9"){setwd("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative7")}
if(Alternative == "scen9"){scendir = "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/sim_storelengthVconservative7/"}
  

rbctrack <- read.table(paste0(scendir, "rbctrack.out"), header = FALSE, skip = 1)
nsim = length(unique(rbctrack[,1]))
rbc2 <- rbctrack %>% 
  tibble() %>% 
  select(1,2,4,15,9,10,11,12,18,22) %>% 
  I()
rbc2 <- rbctrack %>% 
  tibble() %>% 
  select(1,2,4,8,9,11,12,18,15:17) %>% 
  I()
names(rbc2) <- c("isim","year","abc","fmsy","est_ofl","rhl","bmsy","exp_keep","seaslen","bag","minlen") #,"msy")
rbc2 <- rbc2 %>% 
  mutate(exp_keep = as.numeric(ifelse(exp_keep == "******************",NA,exp_keep))) %>% 
  mutate(year = 2019 + 2*year - 1) %>% 
  I()

rbc3 <- rbc2[rep(1:nrow(rbc2), each = 2),]
rbc3$year[seq(2, nrow(rbc3), 2)] <- rbc3$year[seq(1, nrow(rbc3)-1, 2)] + 1
rbc4 <- rbc3[rep(1:nrow(rbc3), each = 9),]
rbc4 <- rbc4 %>% mutate(State = rep(c("CT", "DE", "MA", "MD", "NC", "NJ", "NY", "RI", "VA"), times = 26))
rbc4 <- as.data.table(rbc4)
rbc4 <- as.data.frame(rbc4)

rbc4 <- select(rbc4, year, bag, seaslen, minlen, State)
colnames(rbc4) <- c("Year", "Bag", "SeasonLen", "MinLen", "State")

recoutput2 <- read.table(paste0(scendir, "recoutput2.out"), header = FALSE, skip = 1) #TRUE)
nyrs <- length(unique(recoutput2[,2]))
recoutput2[,1] <- rep(1:nsim, each = 10*nyrs)
names(recoutput2) <- c("isim","year","state","n_keep","n_release","ntrips","nchoice","change_cs","cost",
                       "keep_one", "mulen_keep","mulen_release","trophy")
#names(recoutput2) <- c("isim","year","state","ntrips","nchoice","change_cs","cost",
#                       "keep_one", "mulen_keep","mulen_release","trophy")
coast_recoutput <- recoutput2 %>% 
  filter(state != 1)

RDMoutput_edit1.1 <- cbind(coast_recoutput, rbc4)
RDMoutput_edit1.1 <- select(RDMoutput_edit1.1, State, Year, Bag, MinLen, SeasonLen, n_keep, n_release)

if(Alternative == "scen1"){RDMoutputbind_scen1.1 <- RDMoutput_edit1.1}
if(Alternative == "scen2"){RDMoutputbind_scen2.2 <- RDMoutput_edit1.1}
if(Alternative == "scen3"){RDMoutputbind_scen3.3 <- RDMoutput_edit1.1}
if(Alternative == "scen4"){RDMoutputbind_scen4.4 <- RDMoutput_edit1.1}
if(Alternative == "scen5"){RDMoutputbind_scen5.5 <- RDMoutput_edit1.1}
if(Alternative == "scen6"){RDMoutputbind_scen6.6 <- RDMoutput_edit1.1}
if(Alternative == "scen7"){RDMoutputbind_scen7.7 <- RDMoutput_edit1.1}
if(Alternative == "scen8"){RDMoutputbind_scen8.8 <- RDMoutput_edit1.1}
if(Alternative == "scen9"){RDMoutputbind_scen9.9 <- RDMoutput_edit1.1}
}
RDMoutput_edit3 <- rbind(RDMoutputbind_scen1.1, RDMoutputbind_scen2.2, RDMoutputbind_scen3.3,
                         RDMoutputbind_scen4.4, RDMoutputbind_scen5.5, RDMoutputbind_scen6.6,
                         RDMoutputbind_scen7.7, RDMoutputbind_scen8.8, RDMoutputbind_scen9.9)
#View(RDMoutput_edit3)
#saveRDS(RDMoutput_edit3, "~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/RDMoutputbind_recoutputdirect.rds")


#fit gam #######################################################################################################

########### GAM FROM DIRECTLY RUNNING THROUGH RDM ##########################
RDMoutput_edit2 <- readRDS("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/RDMoutputbind_all.rds")
Harvest <- RDMoutput_edit2$tot_keep
Discards <- RDMoutput_edit2$tot_rel
State <- RDMoutput_edit2$state
Season <- RDMoutput_edit2$SeasonLen
Bag <- RDMoutput_edit2$Bag
MinimumSize <- RDMoutput_edit2$MinLen
Year <- RDMoutput_edit2$Year
#Wave <- RDMoutput_edit2$Wave

inputgam1 <- data.table(Harvest, Discards, State, Season, Bag, MinimumSize, Year)#, Wave)
#View(inputgam1)

gam1 <- gam(Harvest ~ s(MinimumSize, k = 10, bs = "tp") + State + Year + s(Season, k=10, bs = "tp") + s(Bag, k=5, bs = "tp"), 
            data = inputgam1, family = Gamma(link = log), method = "REML")
summary(gam1)

#predictions

preddata1 <- select(inputgam1, State, Season, Bag, MinimumSize, Year)
#preddata3 <- data.frame(Season, Bag, MinimumSize, State)

gamfit2 <- predict.gam(gam1, newdata = preddata1, type = "link" , se.fit = TRUE)
plot(gamfit2$fit)
sum(inputgam1$Harvest)
sum(exp(gamfit2$fit))
#sum(exp(gamfit$fit))

#overall
MinSize2 <- inputgam1$MinimumSize+0.2
Season2 <- inputgam1$Season+5
Bag2 <- inputgam1$Bag+0.2

plot(inputgam1$Harvest ~ inputgam1$MinimumSize)
points(exp(gamfit2$fit) ~ MinSize2, col = "blue")

plot(inputgam1$Harvest ~ inputgam1$Bag, xlim = c(4,8.5))
points(exp(gamfit2$fit) ~ Bag2, col = "red")

plot(inputgam1$Harvest ~ inputgam1$Season)
points(exp(gamfit2$fit) ~ Season2, col = "green")

#diagnostics
library(gratia)
#plot.gam(gam1)
#gam.check(gam1)

draw(gam1, residuals = TRUE)
appraise(gam1)

#draw(gamland, residuals = TRUE)
#appraise(gamland)

#calibglm <- readRDS("~/Desktop/FlounderMSE/calib_glm.rds")


########### GAMS FROM DIRECTLY RUNNING THROUGH RDM WITH NUMBERS AT LENGTH ##########################

RDMoutput_edit5 <- readRDS("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/RDMoutputbind_lengthstructure.rds")

Harvest <- RDMoutput_edit5$num
State <- RDMoutput_edit5$state
Season <- RDMoutput_edit5$SeasLen
Length <- RDMoutput_edit5$len
Wave <- RDMoutput_edit5$Wave
Bag <- RDMoutput_edit5$Bag
MinimumSize <- RDMoutput_edit5$MinLen
Year <- RDMoutput_edit5$Year

inputgam5 <- data.table(Harvest, State, Season, Bag, MinimumSize, Year, Wave, Length)

inputgam6 <- inputgam5[inputgam5$Harvest != 0,]
#View(inputgam5)

gam_length <- gam(Harvest ~ s(Length, MinimumSize) + State + s(Wave, k = 3, bs = "tp") + s(Season, k=4, bs = "tp") + s(Bag, k=4, bs = "tp"), 
            data = inputgam6, family = Gamma(link = log), method = "REML")
summary(gam_length)

#predictions

preddata5 <- select(inputgam6, Length, MinimumSize, State, Wave, Season, Bag)

gamfit4 <- predict.gam(gam_length, newdata = preddata5, type = "link" , se.fit = TRUE)

sum(inputgam6$Harvest)
sum(exp(gamfit4$fit))

#overall
MinSize2 <- inputgam6$MinimumSize+0.2
Season2 <- inputgam6$Season+5
Bag2 <- inputgam6$Bag+0.2

plot(inputgam6$Harvest ~ inputgam6$MinimumSize)
points(exp(gamfit4$fit) ~ MinSize2, col = "blue")

plot(inputgam6$Harvest ~ inputgam6$Bag)
points(exp(gamfit4$fit) ~ Bag2, col = "red")

plot(inputgam6$Harvest ~ inputgam6$Season)
points(exp(gamfit4$fit) ~ Season2, col = "green")

#diagnostics
library(gratia)
gam.check(gam_length)
#draw(gam_length)
appraise(gam_length)


########### GAM FROM recoutput2.out #################################
RDMoutput_edit3 <- readRDS("~/Desktop/FlounderMSE/kw_sims_test/sinatra_split_saveOM/RDMoutputbind_recoutputdirect.rds")
Harvest2 <- RDMoutput_edit3$n_keep
Discards2 <- RDMoutput_edit3$n_release
State2 <- RDMoutput_edit3$State
Season2 <- RDMoutput_edit3$SeasonLen
Bag2 <- RDMoutput_edit3$Bag
MinimumSize2 <- RDMoutput_edit3$MinLen
Year2 <- RDMoutput_edit3$Year
#Wave <- RDMoutput_edit2$Wave

inputgam2 <- data.table(Harvest2, Discards2, State2, Season2, Bag2, MinimumSize2, Year2)#, Wave)

#View(inputgam2)
gam2 <- gam(Harvest2 ~ s(MinimumSize2, k = 10, bs = "tp") + State2 + Year2 + s(Season2, k=10, bs = "tp") + s(Bag2, k=5, bs = "tp"), 
            data = inputgam2, family = Gamma(link = log), method = "REML")
summary(gam2)

#predictions

preddata2 <- select(inputgam2, State2, Season2, Bag2, MinimumSize2, Year2)
#preddata3 <- data.frame(Season, Bag, MinimumSize, State)

gamfit3 <- predict.gam(gam2, newdata = preddata2, type = "link" , se.fit = TRUE)

sum(inputgam2$Harvest)
sum(exp(gamfit3$fit))
#sum(exp(gamfit$fit))

#overall
MinSize3 <- inputgam2$MinimumSize+0.2
Season3 <- inputgam2$Season+5
Bag3 <- inputgam2$Bag+0.2

plot(inputgam2$Harvest ~ inputgam2$MinimumSize)
points(exp(gamfit3$fit) ~ MinSize3, col = "blue")

plot(inputgam2$Harvest ~ inputgam2$Bag, xlim = c(4,8.5))
points(exp(gamfit3$fit) ~ Bag3, col = "red")

plot(inputgam2$Harvest ~ inputgam2$Season)
points(exp(gamfit3$fit) ~ Season3, col = "green")

#diagnostics
library(gratia)
gam.check(gam2)
draw(gam2, residuals = TRUE)
appraise(gam2)

