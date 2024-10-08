args = commandArgs(trailingOnly=TRUE)

# This is the modeling wrapper 

# Steps in the process

  #2) Simulate the fishery under alternative regulations and a new catch-at-length distribution for summer flounder
        # a) Create new catch-at-length/catch-per-trip distributions for summer flounder based on population numbers at length. 
        # a) Calcualte angler welfare/fishing effort changes and changes in catch
# Modeling wrapper test
#profvis::profvis({
#load needed packages and install if not currently installed.
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
### 

#year of the prediction
iyr <- 2019
iyr <- as.integer(args[1])
#hcr_output <- args[2]

# Input the data set containing alternative regulations and directed trips (directed_trips_region - alternative regs test.xlsx)
mgmt_scen <- 1
if (iyr == 2019) directed_trips_table <- readRDS(paste0("regulations_option",mgmt_scen,".rds"))
if (iyr != 2019) {
  directed_trips_table <- readRDS("new_regs_table.rds") %>% 
    tibble() %>% 
    rename(dtrip_2019 = dtrip2019) %>% 
  mutate(state = factor(state, levels = c("MA","RI","CT","NY","NJ","DE","MD","VA","NC"))) 
  directed_trips_table <- split(directed_trips_table, directed_trips_table$state)
}
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
om_length_cm <- scan("om-length.dat",n=Nlen+1)
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

write.table(extra_output,file = "rec-catch.out", append = TRUE, row.names = FALSE, col.names = FALSE)


# print("keep")
# print(keep)
# print("release")
# print(release)
#compare.results[[jsim]] <- NULL
# simkeep <- rbind(simkeep,keep)
# simrel <- rbind(simrel,release)
# simagg <- rbind(simagg, dplyr::select(aggregate_prediction_output, -starts_with("keep_"), -starts_with("release_")))
# }
# #####
# # Stop the clock
# #proc.time() - ptm
# results <- list(simkeep = as.data.frame(simkeep),
#                 simrel = as.data.frame(simrel),
#                 simagg = simagg)
# #saveRDS(results,file="sim-prediction-comparison-2019.rds")
# write_xlsx(results,path = "sim-prediction-comparison-2019.xlsx")

# ###
# # Calculate ouput statisitics for calibration and prediction year
# source("simulation output stats.R")

