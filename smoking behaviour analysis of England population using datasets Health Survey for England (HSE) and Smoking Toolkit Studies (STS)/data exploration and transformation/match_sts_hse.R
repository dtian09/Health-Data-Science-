#match STS variables with HSE variables

hse <- readRDS("Z:/Data/eng_nat_tob_alc_data/intermediate_data/HSE_2013_to_2018_imputed.rds")#missing values imputed data
hse0 <- readRDS("Z:/Data/eng_nat_tob_alc_data/intermediate_data/HSE_2013_to_2018.rds")#original data

dim(hse)
names(hse)
dim(hse0)
l<-names(hse0)
write.csv(l,file="hse_variables.csv",quote = FALSE, sep = ",")
  
# Sampling probabilities
# wt_int are the interview sampling weights supplied with the HSE data
hse[, probs := wt_int / sum(wt_int)]
nrow(hse) # 101,264 individuals
# Number of individuals in the synthetic population
n <- 200000
# Generate the synthetic population from the population survey data
# this can later be augmented with other variables
sim_data <- stapmr::SampleData(
  data_sample = hse,
  option = "sample",
  probs_name = "probs",
  sample_size = n,
  sample_seed = 1,
  pop_keepvars = c("age", "sex", "imd_quintile", "smk.state", "drinker_cat"))
