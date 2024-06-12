df <- categories %>% 
  rename(year_born = `KEY!BDATE M/Y (SYMBOL) 1997`,
         highest_degree = `CV_HIGHEST_DEGREE_0607 2006`,
         gov_prov_jobs = `GOVT RESPONSIBILITY - PROVIDE JOBS 2006`,
         gov_ind_help = `GOVT RESPNSBLTY -PROV IND HELP 2006`,
         gov_red_inc = `GOVT RESPNSBLTY -REDUCE INC DIFF 2006`,
         gov_pro_env = `GOVT RESPNSBLTY -PROTECT ENVIRONMENT 2006`) %>% 
  mutate(age_06 = 2006 - year_born)

glimpse(df)

df %>% 
  mutate(highest_degree = as.character(highest_degree)) %>% 
  mutate(highest_degree = ifelse(highest_degree=="High school diploma (Regular 12 year program)", "High school", highest_degree)) %>%
  filter(!is.na(highest_degree), !is.na(gov_pro_env),
         !(highest_degree %in% c("Professional degree (DDS, JD, MD)", "PhD"))) %>%
  group_by(highest_degree) %>% 
  count(gov_pro_env) %>% 
  reframe(pct = n / sum(n), gov_protect_env = gov_pro_env) %>%
  arrange(desc(pct)) %>% 
  filter(gov_protect_env == "Definitely should be")


library(kableExtra)


         