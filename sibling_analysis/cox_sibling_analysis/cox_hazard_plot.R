library(survival)
library(survminer)
library(NlsyLinks)
library(ggplot2)

df1 <- Links97PairExpanded %>% 
  distinct(ExtendedID, .keep_all = T)
df2 <- categories %>% 
  rename(ExtendedID = SIDCODE_1997,
         first_arrest = ARREST_FIRST_XRND,
         first_inc = INCARC_FIRST_XRND,
         asvab = ASVAB_MATH_VERBAL_SCORE_PCT_1999) %>% 
  mutate(first_arrest = first_arrest %/% 100,
         first_inc = first_inc %/% 100,
         asvab_score = qnorm(asvab/100000))
dat_cox_analysis <- merge(df1, df2, by = "ExtendedID")

# run code in sibling_fe_analysis here

cox_data <- merge(dat_og_analysis, dat_cox_analysis, by = "PUBID_1997")
View(cox_data)

# change name and create some new variables
cox_data <- cox_data %>% 
  rename(year_born = KEY_BDATE_Y_1997) %>% 
  mutate(time_first_inc = first_inc - year_born,
         time_first_arrest = first_inc - year_born)

# select relevant variables
cox_data <- cox_data %>% 
  select(ExtendedID.x, PUBID_1997, time_first_inc, time_first_arrest, asvab_score, year_born, ever_inc)

# rename the household ID
cox_data <- cox_data %>% 
  rename(fam_id = ExtendedID.x)

# convert z-scores to percentiles
cox_data <- cox_data %>% 
  mutate(asvab_score = pnorm(asvab_score))

# arrange the data so that the 
cox_data_grouped <- cox_data %>%
  group_by(fam_id)

cox_data <- cox_data_grouped %>% 
  arrange(desc(asvab_score), .by_group = TRUE)

# make sure we only are comparing two siblings in household
cox_data <- cox_data %>% 
  group_by(fam_id) %>% 
  filter(n() == 2)

# view the data
head(cox_data)
dim(cox_data)

# cox hazard model
cox_model <- coxph(Surv(time_first_inc, ever_inc) ~ asvab_score + strata(fam_id)
                   ,data = cox_data, robust = T)
summary(cox_model)

cox_data <- cox_data %>% 
  mutate(IQ = rep(c("Higher", "Lower")))

cox_data <- subset(cox_data, select = -c(IQ))

dat <- cox_data %>% 
  filter(!is.na(asvab_score))

dat <- dat %>% 
  group_by(fam_id) %>% 
  filter(n() >= 2)

dat <- dat %>% 
  mutate(IQ = rep(c("Higher", "Lower")))

# fit
fit <- survfit(Surv(time_first_inc, ever_inc) ~ IQ, data = dat)
# plot
ggsurvplot(fit, data = dat, pval = TRUE,
           ggtheme = theme_minimal())



