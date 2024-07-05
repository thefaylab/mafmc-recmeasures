
#projection plot
projection.plot <- function(project.results) {
  project.results <- project.results %>% 
    #filter(type %in% c("biomass","catch")) %>%
    I()
  project.results %>% 
    #group_by(scenario, type, year) %>% 
    #median_qi(value, .width = c(.5, .8, .95)) %>%
    ggplot() +  
    stat_lineribbon(aes(x = year, y = value, fill = mp2),
                    #show.legend = FALSE,
                    alpha = 0.35) +
    # geom_lineribbon(aes(x = year, y = value, ymin = .lower, ymax = .upper, fill = scenario),
    #                 show.legend = FALSE) +
    scale_fill_brewer(type = "qual", palette = 2) +
    facet_wrap(~mp2, scale = "free") + 
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==1), lty=1,lwd=0.5,col=gray(0.7),alpha=0.75) +
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==2), lty=1,lwd=0.5,col=gray(0.7),alpha=0.75) +
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==3), lty=1,lwd=0.5,col=gray(0.7),alpha=0.75) +
    geom_line(aes(y=value,x=year,group=scenario),data = subset(project.results, type != "index" & isim==4), lty=1,lwd=0.5,col=gray(0.7),alpha=0.75) +
    ylim(0,NA) + 
    ylab("") + 
    theme_bw() +
    theme(legend.position= "none") + #,
    #axis.text.y = element_blank()) +
    labs(fill = "",
         subtitle = "expected harvest (1000s)")
}


all_results %>% 
  ggplot() + 
  aes(y = 1000*keep_num, x = 0.39*exp_keep, col = biomass) + 
  geom_point(alpha=0.1) + 
  geom_abline(slope = 1, intercept = 0) + 
  scale_color_viridis_c() +
  labs(
    y = "harvest number from RDM",
    x = "expected harvest from GAM"
  ) +
  ylim(0,NA)

all_results %>% 
  mutate(ratio = 1000*keep_num / (0.39*exp_keep),
         mp2 = fct_collapse(scenario,
                            "rhl" = c("MP 4","MP 8", "MP 12"),
                            "perc_change" = c("MP 1","MP 5","MP 9"), 
                            "refpt" = c("MP 2", "MP 6","MP 10"),
                            "bmatrix" = c("MP 3","MP 7","MP 11")),
         mp3 = fct_collapse(scenario,
                            "bag" = c("MP 4","MP 1","MP 2", "MP 3"),
                            "minlen" = c("MP 8","MP 5","MP 6","MP 7"),
                            "season" = c("MP 12","MP 9","MP 10","MP 11"))) %>% 
  ggplot() + 
  aes(x = biomass, y = ratio, col = minlen) + #col = mulen_keep) + 
  geom_point(alpha=0.1) + 
  #geom_abline(slope = 1, intercept = 0) + 
  geom_hline(yintercept = 1, lty=2) +
  geom_hline(yintercept = 2.564, lty=2) +
  scale_color_viridis_c() +
  ylim(0,NA) +
  facet_wrap(~mp3) +
  NULL


all_results %>% 
  ggplot() +
  aes(x = seaslen, y = keep_num, col = biomass) +
  geom_point(alpha=0.1) +
  scale_color_viridis_c() +
  ylim(0,NA) +
  #facet_wrap(~scenario) +
  labs(x = "minimum size",
       y = "RDM harvest (1000s)",
       shape = "bag limit",
       size = "season (days)") +
  NULL

all_results %>% 
  drop_na() %>% 
  ggplot() +
  aes(x = factor(minlen), y = keep_num) +
  geom_boxplot() +
  scale_color_viridis_c() +
  ylim(0,NA) +
  #facet_wrap(~scenario) +
  labs(x = "bag limit",
       y = "RDM harvest (1000s)",
       #shape = "bag limit",
       #size = "season (days)",
       ) +
  NULL

all_results %>% 
  ggplot() +
  aes(x = factor(bag), y = keep_num) +
  geom_boxplot() +
  scale_color_viridis_c() +
  ylim(0,12000) +
  #facet_wrap(~scenario) +
  labs(x = "season length (days)",
       y = "RDM harvest (1000s)",
       #shape = "bag limit",
       ) +
  theme_bw() +
  NULL


diag_ts %>% 
  mutate(value = ifelse(type == "exp_keep", 0.39*value/1000, value),
         mp2 = fct_collapse(scenario,
                            "rhl" = c("MP 4","MP 8", "MP 12"),
                            "perc_change" = c("MP 1","MP 5","MP 9"), 
                            "refpt" = c("MP 2", "MP 6","MP 10"),
                            "bmatrix" = c("MP 3","MP 7","MP 11")),
         mp3 = fct_collapse(scenario,
                            "bag" = c("MP 4","MP 1","MP 2", "MP 3"),
                            "minlen" = c("MP 8","MP 5","MP 6","MP 7"),
                            "season" = c("MP 12","MP 9","MP 10","MP 11"))) %>% 
  filter(year != 2019) %>% 
#  mutate(type = fct_relevel(type,c("spawning biomass", "total catch"))) %>% 
  filter(type == "exp_keep",
         mp3 == "minlen") %>% 
  projection.plot()




tempcat <- flukecatch %>% 
  janitor::clean_names() %>% 
  group_by(bag, min_len, season_len) %>% 
  summarize(land = sum(land),
            disc = sum(disc)) %>% 
  ungroup()

catuse <- tempcat %>%
  filter(min_len == max(min_len),
         bag == 4,
         season_len == 150)

tempcat <- tempcat %>% 
  mutate(land = land/catuse$land[1],
         disc = disc/catuse$disc[1])

tempcat %>% 
  filter(season_len == 150) %>% 
  ggplot() +
  aes(x = land) +
  geom_histogram(col="white") +
  theme_bw() +
  facet_wrap(~bag) +
  geom_vline(xintercept = 1.05, lty=2) +
  geom_vline(xintercept = 1.1, lty=2) +
  geom_vline(xintercept = 1.2, lty=2) +
  geom_vline(xintercept = 1.3, lty=2) +
  geom_vline(xintercept = 1.4, lty=2) +
  geom_vline(xintercept = 1.6, lty=2) +
  labs(x = "expected harvest compared to minimum") +
  NULL

# tempcat %>% 
#   filter(min_len == 17.5) %>% 
#   ggplot() +
#   aes(x = land) +
#   geom_histogram(col="white") +
#   theme_bw() +
#   facet_wrap(~bag) +
#   NULL

tempcat %>% 
  filter(season_len == 150) %>% 
  ggplot() +
  aes(x = land) +
  geom_histogram(col="white") +
  theme_bw() +
  #facet_wrap(~bag) +
  geom_vline(xintercept = 1.05, lty=2) +
  geom_vline(xintercept = 1.1, lty=2) +
  geom_vline(xintercept = 1.2, lty=2) +
  geom_vline(xintercept = 1.3, lty=2) +
  geom_vline(xintercept = 1.4, lty=2) +
  geom_vline(xintercept = 1.6, lty=2) +
  labs(x = "expected harvest compared to minimum") +
  NULL


tempcat %>% 
  #filter(season_len == 150) %>% 
  ggplot() +
  aes(x = land) +
  geom_histogram(col="white") +
  theme_bw() +
  #facet_wrap(~bag) +
  geom_vline(xintercept = 1.05, lty=2) +
  geom_vline(xintercept = 1.1, lty=2) +
  geom_vline(xintercept = 1.2, lty=2) +
  geom_vline(xintercept = 1.3, lty=2) +
  geom_vline(xintercept = 1.4, lty=2) +
  geom_vline(xintercept = 1.6, lty=2) +
  labs(x = "expected harvest compared to minimum") +
  NULL


#######

all_results %>% 
  mutate(ratio = 1000*keep_num / (0.39*exp_keep),
         mp2 = fct_collapse(scenario,
                            "rhl" = c("MP 4","MP 8", "MP 12"),
                            "perc_change" = c("MP 1","MP 5","MP 9"), 
                            "refpt" = c("MP 2", "MP 6","MP 10"),
                            "bmatrix" = c("MP 3","MP 7","MP 11")),
         mp3 = fct_collapse(scenario,
                            "bag" = c("MP 4","MP 1","MP 2", "MP 3"),
                            "minlen" = c("MP 8","MP 5","MP 6","MP 7"),
                            "season" = c("MP 12","MP 9","MP 10","MP 11"))) %>% 
  ggplot() + 
  aes(x = biomass, y = ratio, col = minlen) + #col = mulen_keep) + 
  geom_point(alpha=0.1) + 
  #geom_abline(slope = 1, intercept = 0) + 
  geom_hline(yintercept = 1, lty=2) +
  geom_hline(yintercept = 2.564, lty=2) +
  scale_color_viridis_c() +
  ylim(0,NA) +
  facet_wrap(~mp3) +
  NULL



all_results %>% 
  mutate(ratio = 1000*keep_num / (0.39*exp_keep),
         mp2 = fct_collapse(scenario,
                            "rhl" = c("MP 4","MP 8", "MP 12"),
                            "perc_change" = c("MP 1","MP 5","MP 9"), 
                            "refpt" = c("MP 2", "MP 6","MP 10"),
                            "bmatrix" = c("MP 3","MP 7","MP 11")),
         mp3 = fct_collapse(scenario,
                            "bag" = c("MP 4","MP 1","MP 2", "MP 3"),
                            "minlen" = c("MP 8","MP 5","MP 6","MP 7"),
                            "season" = c("MP 12","MP 9","MP 10","MP 11"))) %>% 
  ggplot() + 
  aes(x = biomass, y = ratio, col = seaslen) + #col = mulen_keep) + 
  geom_point(alpha=0.1) + 
  #geom_abline(slope = 1, intercept = 0) + 
  geom_hline(yintercept = 1, lty=2) +
  geom_hline(yintercept = 2.564, lty=2) +
  scale_color_viridis_c() +
  ylim(0,NA) +
  facet_wrap(~mp3) +
  NULL



all_results %>% 
  mutate(ratio = 1000*keep_num / (0.39*exp_keep),
         mp2 = fct_collapse(scenario,
                            "rhl" = c("MP 4","MP 8", "MP 12"),
                            "perc_change" = c("MP 1","MP 5","MP 9"), 
                            "refpt" = c("MP 2", "MP 6","MP 10"),
                            "bmatrix" = c("MP 3","MP 7","MP 11")),
         mp3 = fct_collapse(scenario,
                            "bag" = c("MP 4","MP 1","MP 2", "MP 3"),
                            "minlen" = c("MP 8","MP 5","MP 6","MP 7"),
                            "season" = c("MP 12","MP 9","MP 10","MP 11"))) %>% 
  ggplot() + 
  aes(x = biomass, y = ratio, col = bag) + #col = mulen_keep) + 
  geom_point(alpha=0.1) + 
  #geom_abline(slope = 1, intercept = 0) + 
  geom_hline(yintercept = 1, lty=2) +
  geom_hline(yintercept = 2.564, lty=2) +
  scale_color_viridis_c() +
  ylim(0,NA) +
  facet_wrap(~mp3) +
  NULL


####

all_results %>% 
  mutate(ratio = 1000*keep_num / (0.39*exp_keep),
         mp2 = fct_collapse(scenario,
                            "rhl" = c("MP 4","MP 8", "MP 12"),
                            "perc_change" = c("MP 1","MP 5","MP 9"), 
                            "refpt" = c("MP 2", "MP 6","MP 10"),
                            "bmatrix" = c("MP 3","MP 7","MP 11")),
         mp3 = fct_collapse(scenario,
                            "bag" = c("MP 4","MP 1","MP 2", "MP 3"),
                            "minlen" = c("MP 8","MP 5","MP 6","MP 7"),
                            "season" = c("MP 12","MP 9","MP 10","MP 11"))) %>% 
  ggplot() + 
  aes(x = 0.39*exp_keep, y = 1000*keep_num, size = biomass, col = biomass) + #col = mulen_keep) + 
  geom_point(alpha=0.1) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "glm", formula = y ~ x, method.args = list(family = "poisson"), col ="blue") +
  #geom_hline(yintercept = 1, lty=2) +
  #geom_hline(yintercept = 2.564, lty=2) +
  scale_color_viridis_c() +
  ylim(0,NA) +
  facet_wrap(~mp3) +
  labs(    y = "harvest number from RDM",
           x = "expected harvest from GAM") +
  NULL

all_results %>% 
  mutate(ratio = 1000*keep_num / (0.39*exp_keep),
         mp2 = fct_collapse(scenario,
                            "rhl" = c("MP 4","MP 8", "MP 12"),
                            "perc_change" = c("MP 1","MP 5","MP 9"), 
                            "refpt" = c("MP 2", "MP 6","MP 10"),
                            "bmatrix" = c("MP 3","MP 7","MP 11")),
         mp3 = fct_collapse(scenario,
                            "bag" = c("MP 4","MP 1","MP 2", "MP 3"),
                            "minlen" = c("MP 8","MP 5","MP 6","MP 7"),
                            "season" = c("MP 12","MP 9","MP 10","MP 11"))) %>% 
  ggplot() + 
  aes(x = 0.39*exp_keep, y = 1000*keep_num, col = bag) + #col = mulen_keep) + 
  geom_point(alpha=0.1) + 
  geom_abline(slope = 1, intercept = 0) + 
  #geom_hline(yintercept = 1, lty=2) +
  #geom_hline(yintercept = 2.564, lty=2) +
  scale_color_viridis_c() +
  ylim(0,NA) +
  facet_wrap(~mp3) +
  NULL

all_results %>% 
  mutate(ratio = 1000*keep_num / (0.39*exp_keep),
         mp2 = fct_collapse(scenario,
                            "rhl" = c("MP 4","MP 8", "MP 12"),
                            "perc_change" = c("MP 1","MP 5","MP 9"), 
                            "refpt" = c("MP 2", "MP 6","MP 10"),
                            "bmatrix" = c("MP 3","MP 7","MP 11")),
         mp3 = fct_collapse(scenario,
                            "bag" = c("MP 4","MP 1","MP 2", "MP 3"),
                            "minlen" = c("MP 8","MP 5","MP 6","MP 7"),
                            "season" = c("MP 12","MP 9","MP 10","MP 11"))) %>% 
  ggplot() + 
  aes(x = 0.39*exp_keep, y = 1000*keep_num, col = seaslen) + #col = mulen_keep) + 
  geom_point(alpha=0.1) + 
  geom_abline(slope = 1, intercept = 0) + 
  #geom_hline(yintercept = 1, lty=2) +
  #geom_hline(yintercept = 2.564, lty=2) +
  scale_color_viridis_c() +
  ylim(0,NA) +
  facet_wrap(~mp3) +
  NULL


glm1 <- all_results %>% 
  mutate(kept = 1000*keep_num,
         pred = 0.39*exp_keep) %>% 
  #glm(formula = log(kept) ~ pred + biomass + bag + seaslen,data =., family = "poisson") # 
  glm(formula = log(kept) ~ pred + biomass + bag + seaslen,data =.) #, family = "poisson") # 
library(broom)
bob <- augment(glm1, type.predict = "response")
bob %>% 
  janitor::clean_names() %>% 
  select(-biomass,-bag,-seaslen) %>% 
  bind_cols(all_results %>% slice(as.integer(bob$.rownames))) %>% 
  ggplot() +
  aes(x = log_kept, y = fitted, col = seaslen) + #biomass) +
  geom_point(alpha = 0.2) +
  scale_color_viridis_c() +
  facet_wrap(~minlen, scales = "free") +
  geom_abline(slope = 1, intercept = 0) + 
  NULL


glm2 <- all_results %>% 
  mutate(kept = 1000*keep_num,
         pred = 0.39*exp_keep) %>% 
  #glm(formula = log(kept) ~ pred + biomass + bag + seaslen,data =., family = "poisson") # 
  lm(formula = log(kept) ~ log(pred)*minlen + biomass + seaslen, data =.) #, family = "poisson") # 
library(broom)
bob <- augment(glm2, type.predict = "response")
p2 <- bob %>% 
  janitor::clean_names() %>% 
  select(-biomass,-minlen, -seaslen) %>% 
  bind_cols(all_results %>% slice(as.integer(bob$.rownames))) %>% 
  ggplot() +
  #aes(x = log_kept, y = fitted, col = biomass) +
  aes(x = exp(log_kept), y = exp(fitted), col = biomass) +
  geom_point(alpha = 0.2) +
  scale_color_viridis_c() +
  #facet_wrap(~minlen, scales = "free") +
  geom_abline(slope = 1, intercept = 0) + 
  #ylim(0, 6e+07) +
  NULL

p1 <- bob %>% 
  janitor::clean_names() %>% 
  select(-biomass,-minlen, -seaslen) %>% 
  bind_cols(all_results %>% slice(as.integer(bob$.rownames))) %>% 
  ggplot() +
  aes(x = exp(log_kept), y = exp(log_pred), col = biomass) +
  #aes(x = exp(log_kept), y = exp(fitted), col = biomass) +
  geom_point(alpha = 0.2) +
  scale_color_viridis_c() +
  #facet_wrap(~minlen, scales = "free") +
  geom_abline(slope = 1, intercept = 0) + 
  #ylim(0, 6e+07) +
  NULL

p1+p2

df_use <-  all_results %>% 
  mutate(kept = 1000*keep_num,
         pred = 0.39*exp_keep)
df_use %>% 
  ggplot() +
  aes(x = pred, y = kept, col = minlen, group = as.factor(minlen)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  NULL


glm3 <- all_results %>% 
  mutate(kept = 1000*keep_num,
         pred = 0.39*exp_keep) %>% 
  #glm(formula = log(kept) ~ pred + biomass + bag + seaslen,data =., family = "poisson") # 
  lm(formula = log(kept) ~ log(pred) + biomass, data =.) #, family = "poisson") # 
bob <- augment(glm3, type.predict = "response")
p3 <- bob %>% 
  janitor::clean_names() %>% 
  select(-biomass) %>% 
  bind_cols(all_results %>% slice(as.integer(bob$.rownames))) %>% 
  ggplot() +
  #aes(x = log_kept, y = fitted, col = biomass) +
  aes(x = exp(log_kept), y = exp(fitted), col = biomass) +
  geom_point(alpha = 0.2) +
  scale_color_viridis_c() +
  #facet_wrap(~minlen, scales = "free") +
  geom_abline(slope = 1, intercept = 0) + 
  #ylim(0, 6e+07) +
  NULL

p2 + p3  
  
glm4 <- all_results %>% 
  mutate(kept = 1000*keep_num,
         pred = exp_keep) %>% 
  glm(formula = kept ~ pred + minlen + biomass + seaslen + bag, data =., family = "poisson") # 
  #lm(formula = log(kept) ~ log(pred) + biomass, data =.) #, family = "poisson") # 
bob <- augment(glm4, type.predict = "response")
p4 <- bob %>% 
  janitor::clean_names() %>% 
  select(-biomass, -seaslen, -minlen, -bag) %>% 
  bind_cols(all_results %>% slice(as.integer(bob$.rownames))) %>% 
  ggplot() +
  #aes(x = log_kept, y = fitted, col = biomass) +
  aes(x = kept, y = fitted, col = biomass) +
  geom_point(alpha = 0.2) +
  scale_color_viridis_c() +
  #facet_wrap(~minlen, scales = "free") +
  #facet_wrap(~bag, scales = "free") +
  geom_abline(slope = 1, intercept = 0) + 
  ylim(0, 5000000) +
  xlim(0, 5000000) +
  labs(x = "RDM",
       y = "fitted from GAM") +
  NULL
p4
100*with(summary(glm4), 1 - deviance/null.deviance)


  
  
#bag + seaslen 
glm5 <- all_results %>% 
  mutate(kept = 1000*keep_num,
         pred = 0.39*exp_keep) %>% 
  glm(formula = kept ~ pred + minlen + seaslen + bag, data =., family = "poisson") # 
#lm(formula = log(kept) ~ log(pred) + biomass, data =.) #, family = "poisson") # 
bob <- augment(glm5, type.predict = "response")
p5 <- bob %>% 
  janitor::clean_names() %>% 
  select(-seaslen, -minlen, -bag) %>% 
  #select(-biomass, -minlen) %>% 
  bind_cols(all_results %>% slice(as.integer(bob$.rownames))) %>% 
  ggplot() +
  #aes(x = log_kept, y = fitted, col = biomass) +
  aes(x = kept, y = fitted, col = biomass) +
  geom_point(alpha = 0.2) +
  scale_color_viridis_c() +
  #facet_wrap(~minlen, scales = "free") +
  #facet_wrap(~bag, scales = "free") +
  geom_abline(slope = 1, intercept = 0) + 
  #ylim(0, 6e+07) +
  NULL
p5
100*with(summary(glm5), 1 - deviance/null.deviance)
100*with(summary(glm4), 1 - deviance/null.deviance)

