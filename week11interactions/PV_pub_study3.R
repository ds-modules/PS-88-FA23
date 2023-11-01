## Partisan Violence Study 3
## Replicable code starts at line 52

### STEP 1: ANONYMIZE THE DATA -----------

library(tidyverse)
setwd("")

# download as csv from qualtrics, header rows 2 and 3 deleted, added columns for MID and IP duplicates
addr <- "2020_12_7_PV_exp_full_fielding+-+JS_January+5,+2021_12.11.csv"
d0 <- read_csv(addr)



d1 <-
  d0 %>%
  # select participants who began the survery
  filter(Progress > 10) %>%
  # remove MID duplicates, independents/others, and people who failed attention check
  filter(MID_dup == 0,
         screen_reason != "party",
         screen_reason != "attn") %>%
  # remove people who did not complete the main DV items
  filter(
    !is.na(SPV_1_2), !is.na(SPV_2_2), !is.na(SPV_3_1), !is.na(SPV_4_2),
    !is.na(ESPV_1_2), !is.na(ESPV_2_2), !is.na(ESPV_3_1)
  ) %>% 
  dplyr::select(ResponseId,
                `Duration (in seconds)`, party, party_4_TEXT, anes_rep, anes_dem, anes_ind, anes_ind_force,
                SPV_1_meta_out_2, SPV_2_meta_out_2, SPV_3_meta_out_1, SPV_4_meta_out_2,
                `out_corr_time_Page Submit`, `out_cont_time_Page Submit`,
                manip_ck_1,
                SPV_1_2, SPV_2_2, SPV_3_1, SPV_4_2,
                ESPV_1_2, ESPV_2_2, ESPV_3_1,
                ft_rep_1, ft_dem_1,
                CSV_1_1, CSV_2_1, CSV_3_1,
                ancodi_1, ancodi_2, ancodi_3,
                rep_violence_1, dem_violence_1,
                sd_marriage_1, sd_friend_1, sd_work_1, sd_carpool_1,
                YOB, Gender, race, education, polit, religious, region,
                employed, income, ladder,
                screen_reason, condition, Inparty, Outparty,
                IP_dup, MID_dup, gc
                
  )

# 652 participants were not duplicates, passed atten checks, and copleted all surveys

write_csv(d1, "PV_study_3_anon.csv")


## STEP 2: CLEAN THE DATA ------ 

# enter working directory
setwd("")

library(tidyverse)
library(Rmisc)

d0 <- read_csv("PV_study_3_anon.csv")

d1 <-
  d0 %>%
  mutate(
    # create indeces of variables
    SPV_self = (SPV_1_2 + SPV_2_2 + SPV_3_1 + SPV_4_2)/4,
    SPV_meta_out = (SPV_1_meta_out_2 + SPV_2_meta_out_2 + SPV_3_meta_out_1 + SPV_4_meta_out_2) / 4,
    ESPV_avg = (ESPV_1_2 + ESPV_2_2 + ESPV_3_1) / 3,
    CSV_avg = (CSV_1_1 + CSV_2_1 + CSV_3_1) / 3,
    ancodi = (ancodi_1 + ancodi_2 + ancodi_3) / 3,
    sd_avg = (sd_marriage_1 + sd_friend_1 + sd_work_1 + sd_carpool_1) / 4,
    ft_out = if_else(Inparty == "Democrat", ft_rep_1, ft_dem_1),
    ft_in = if_else(Inparty == "Democrat", ft_dem_1, ft_rep_1),
    ft_diff = ft_in - ft_out,
    out_violence = if_else(Inparty == "Democrat", rep_violence_1, dem_violence_1),
    in_violence = if_else(Inparty == "Democrat", dem_violence_1, rep_violence_1)
  ) %>%
  mutate(
    gender = if_else(Gender == "2", "Female", "Male"),
    age = 2020 - YOB,
    stren = case_when(
      anes_rep == 1  ~"Strong",
      anes_dem == 1 ~"Strong",
      anes_rep ==2 ~ "Not strong",
      anes_dem == 2 ~ "Not strong",
      TRUE ~ NA_character_
    )
  )  %>%
  mutate(
    age = if_else(age >= 1900, 2020-age, age),
    Education = case_when(
      education %in% c(1, 2) ~ "HS or less",
      education == 3 ~ "Some college",
      education == 4 ~ "Bachelors",
      education == 5 ~ "Graduate",
      TRUE ~ NA_character_
    ),
    education = factor(Education, levels = c("HS or less", "Some college", "Bachelors", "Graduate")),
    Race = case_when(
      race == 1 ~ "white",
      race == 2 ~ "black",
      race == 3 ~ "hispanic",
      race == 4 ~ "asian",
      race == 5 ~ "other"
    ),
    race = factor(Race, levels = c("white", "black", "hispanic", "asian", "other")),
  ) %>%
  mutate(
    SPV_meta_over = if_else(Inparty == "Republican", SPV_meta_out - 9.3, SPV_meta_out - 10.3)
  )

s1 <-
  d1 %>% filter(stren == "Strong") %>%
  mutate(
    SPV_scaled = scale(SPV_self)
  )

## ANALYSIS -------


# means by party condition
s1 %>%
  filter(gc == 1) %>%
  group_by(condition) %>%
  dplyr::summarise(
    SPV_meta= mean(SPV_meta_out, na.rm = TRUE),
    SPV_self = mean(SPV_self, na.rm = TRUE),
    count = n()
  )



# outcome: SPV
# strong partisans (pre-registered)
lm_out_strong <- lm(SPV_self ~ condition + gender + age + Inparty + income + race + education, 
                    data = s1)
summary(lm_out_strong)


# outcome: SPV
# full sample
lm_out_full <- lm(SPV_self ~ condition + gender + age + Inparty + income + race + education, 
                  data = d1)
summary(lm_out_full)

library(stargazer)
stargazer(lm_out_strong, lm_out_full,
          column.labels = c("Strong partisans (pre-registered)", "Full sample"),
          out="linear_model_study_3.htm")





# MODERATION ANALYSIS, CENTERING ON THE TRUE VALUES  -------


# rescale SPV
s2 <- s1 %>%
  mutate(
    SPV_meta_scale = scale(SPV_meta_out),
    SPV_meta_s2 = SPV_meta_scale + 1,
    SPV_meta_s3 = SPV_meta_scale - 1,
    SPV_meta_s4 = SPV_meta_scale + 0.25
    
  ) %>%
  mutate(
    SPV_meta_shift = SPV_meta_over - 21,
    SPV_over_scale = scale(SPV_meta_out),
    SPV_over_2 = SPV_meta_scale + 1,
    SPV_over_3 = SPV_meta_scale - 1,
    SPV_over_4 = SPV_meta_scale + 0.25
  )


m1 <- lm(SPV_self ~ condition + SPV_meta_over + condition*SPV_meta_over + gender + age + Inparty + income + race + education, 
         data = s2)
summary(m1)

library(sjPlot)
library(sjmisc)

# moderation plot 
plot_model(m1, type = "pred", terms = c("SPV_meta_over", "condition"), ci.lvl = .9) +
  theme_bw() +
  labs(
    x = "Overestimate of outpartisans' metaperceptions of\nsupport for partisan violence",
    y = "Support for partisan violence",
    title = "",
    color = "Condition"
  ) +
  scale_color_manual(values=c("red", "blue"), labels = c("Control", "Correction")) +
  scale_x_continuous(limits = c(-10, 90)) +
  #scale_y_continuous(limits = c(0, 25)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "grey")

ggsave(filename = "fig3_over.png", width = 7, height = 5)

# showing that the positive effect starts for participants with metaperceptions 21 points above the true value
m2 <- lm(SPV_self ~ condition + SPV_meta_shift + condition*SPV_meta_shift + gender + age + Inparty + income + race + education, 
         data = s2)
summary(m2)



# no significiant effect for overestimates 1SD below the mean
m3 <- lm(SPV_self ~ 1 + condition*SPV_over_2 + gender + age + Inparty + income + as.factor(race) + as.factor(education) + SPV_over_2, 
         data = s2)
summary(m3)
# 1.651e+00  1.547e+00   1.067  0.28633

# significant effect for average overestimates
m4 <- lm(SPV_self ~ 1 + condition*SPV_over_scale + gender + age + Inparty + income + as.factor(race) + as.factor(education) + SPV_over_scale, 
         data = s2)
summary(m4)
#  -2.938e+00  1.092e+00  -2.691  0.00735 ** 


# very significiant effect for overestimates 1SD above the mean
m5 <- lm(SPV_self ~ 1 + condition*SPV_over_3 + SPV_over_3 + gender + age + Inparty + income + as.factor(race) + as.factor(education), 
         data = s2)
summary(m5)
# -7.527e+00  1.539e+00  -4.892 1.32e-06 ***




## PLOTS ---------

plot1 <-
  s1 %>% 
  filter(gc == 1) %>%
  mutate(condition = if_else(condition == "out_control", "Control", "Correction")) %>%
  group_by(condition) %>%
  dplyr::summarise(
    mean = mean(SPV_self, na.rm = TRUE),
    sd = sd(SPV_self, na.rm = TRUE),
    ci_lower = CI(SPV_self, ci = 0.95)[3],
    ci_upper = CI(SPV_self, ci = 0.95)[1]

  ) %>% 
  ungroup() %>%
  ggplot(aes(condition, mean)) +
  coord_cartesian(ylim=c(0,11)) +
  geom_col(width = .7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .1) +
  
  #facet_grid(. ~ gender) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size= 14)
  ) +
  labs(
    title = "Study 3: Average support for partisan violence",
    y = "Avg. support for partisan violence",
    x = "Condition"
  )

plot1
ggsave(plot = plot1, filename = "condition_bar_1.png", width = 6.5, height = 4.5)

# library(gridExtra)
# only use this code to combine this plot with study 4 plot
#two_plots <- grid.arrange(plot1, plot2, nrow = 1)
#ggsave(two_plots, filename = "two_exp_panelv3.png", width = 10, height = 5)

## EFFECT OF CONDITION ON AFFECTIVE POLARIZATION MEASURES -------


# outcome: feeling thermometer diff
# strong partisans
lm_ft<- lm(ft_diff ~ condition + gender + age + Inparty + income + race + education, 
           data = s1)
summary(lm_ft)


# outcome: social distancing
# strong partisans
lm_sd <- lm(sd_avg ~ condition + gender + age + Inparty + income + race + education, 
            data = s1)
summary(lm_sd)

library(stargazer)

stargazer(lm_ft, lm_sd,
          column.labels = c("Feeling thermometer diff", "Social distancing"),
          out="exp1_affpol_regression_v2.htm")



## STANDARDIZED REGRESSION ------

lm_out_strong <- lm(SPV_scaled ~ condition + gender + scale(age) + Inparty + scale(income) + race + education, 
                    data = s1)
summary(lm_out_strong)


## SENSITIVITY ANALYSIS (no controls) -----


lm_out_strong <- lm(SPV_self ~ condition, 
                    data = s1)
summary(lm_out_strong)

stargazer(lm_out_strong)



## MODERATION BY PARTY AFFILIATION --------

# model with just democrats
lm_out_dem <- lm(SPV_self ~ condition + gender + age + income + race + education, 
                  data = d1 %>% filter(Inparty == "Democrat"))
summary(lm_out_dem)

# model with just republicans
lm_out_rep <- lm(SPV_self ~ condition + gender + age + income + race + education, 
                  data = d1 %>% filter(Inparty == "Republican"))
summary(lm_out_rep)

# interaction effect
lm_out_inter <- lm(SPV_self ~ condition + gender + age + income + race + education + Inparty + condition:Inparty, 
                  data = d1)
summary(lm_out_inter)

stargazer(lm_out_rep, lm_out_dem, lm_out_inter)


## CALCULATING COHEN'S D OF EFFECT SIZE ------

lm_out_strong <- lm(SPV_self ~ condition + gender + age + Inparty + income + race + education, 
                    data = s1)
summary(lm_out_strong)

library(esc)

esc::esc_B(b = -2.825, sdy = sd(s1$SPV_self, na.rm = TRUE), 
          grp1n = nrow(s1[s1$condition == "out_correct", ]),
          grp2n = nrow(s1[s1$condition == "out_control", ]),
          es.type = "d"
          )


lm_out_all <- lm(SPV_self ~ condition + gender + age + Inparty + income + race + education, 
                    data = d1)
summary(lm_out_all)

library(esc)

esc::esc_B(b = -3.231, sdy = sd(d1$SPV_self, na.rm = TRUE), 
           grp1n = nrow(d1[d1$condition == "out_correct", ]),
           grp2n = nrow(d1[d1$condition == "out_control", ]),
           es.type = "d"
)




# MODERATION ANALYSIS, without centering on true values -------


# interaction effect
m1 <- lm(SPV_self ~ 1 + condition +SPV_meta_scale + condition*SPV_meta_scale + gender + age + Inparty + income + race + education, 
         data = s2)
summary(m1)

m1 <- lm(SPV_self ~ condition + SPV_meta_out + condition*SPV_meta_out + gender + age + Inparty + income + race + education, 
         data = s2)
summary(m1)

library(sjPlot)
library(sjmisc)

plot_model(m1, type = "pred", terms = c("SPV_meta_out", "condition")) +
  theme_bw() +
  labs(
    x = "Metaperceptions of outparty SPV",
    y = "SPV",
    title = "",
    color = "Condition"
  )



# no significiant effect 1SD below the mean
m2 <- lm(SPV_self ~ 1 + condition*SPV_meta_s2 + gender + age + Inparty + income + as.factor(race) + as.factor(education) + SPV_meta_s2, 
         data = s2)
summary(m2)

#
m2 <- lm(SPV_self ~ 1 + condition*SPV_meta_scale + gender + age + Inparty + income + as.factor(race) + as.factor(education) + SPV_meta_scale, 
         data = s2)
summary(m2)
# -2.938e+00  1.092e+00  -2.691  0.00735 ** 

# very significiant effect 1SD above the mean
m3 <- lm(SPV_self ~ 1 + condition*SPV_meta_s3 + SPV_meta_s3 + gender + age + Inparty + income + as.factor(race) + as.factor(education), 
         data = s2)
summary(m3)
# -7.527e+00  1.539e+00  -4.892 1.32e-06 ***





