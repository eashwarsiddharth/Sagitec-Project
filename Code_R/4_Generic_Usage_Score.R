
# Generic Usage score

# MultiBrand Drugs
length(unique(presc_detail$generic_name))
require(dplyr)
grouped_unique_generic_name <- select(presc_detail, drug_name, generic_name) %>% split(f = presc_detail$generic_name) %>% lapply(FUN = unique)
length(grouped_unique_generic_name)
x_generic_unique_name <- sapply(grouped_unique_generic_name, nrow)
length(x_generic_unique_name)
multibrand_generic_vector <- x_generic_unique_name[x_generic_unique_name > 1]
length(multibrand_generic_vector)
#Verification
#c('drug_name', 'generic_name') %in% colnames(presc_detail)
#colnames(presc_detail) %in% c('drug_name', 'generic_name')
sum(presc_detail$drug_name %in% names(multibrand_generic_vector))

multibrand_generic_names <- names(x_generic_unique_name)[x_generic_unique_name > 1]
length(multibrand_generic_names)
single_or_generic_drugs <- names(x_generic_unique_name)[x_generic_unique_name == 1]
length(single_or_generic_drugs)

# Unique Generic Names
unique_generic_name <- unique(presc_detail$generic_name)

# Generic Usage Score

score_top_1 <- vector()
for(i in 1:length(PUF_conso$top1_drug_prescribed)) {
  if(PUF_conso$top1_drug_prescribed[i] %in% unique_generic_name) {
    score_top_1[i] <- 1
  } else {
    score_top_1[i] <- 0
  }
}
PUF_conso$score_top_1 <- score_top_1

score_top_2 <- vector()
for(i in 1:length(PUF_conso$top2_drug_prescribed)) {
  if(PUF_conso$top2_drug_prescribed[i] %in% unique_generic_name) {
    score_top_2[i] <- 1
  } else {
    score_top_2[i] <- 0
  }
}
PUF_conso$score_top_2 <- score_top_2

score_top_3 <- vector()
for(i in 1:length(PUF_conso$top3_drug_prescribed)) {
  if(PUF_conso$top3_drug_prescribed[i] %in% unique_generic_name) {
    score_top_3[i] <- 1
  } else {
    score_top_3[i] <- 0
  }
}
PUF_conso$score_top_3 <- score_top_3

score_top_4 <- vector()
for(i in 1:length(PUF_conso$top4_drug_prescribed)) {
  if(PUF_conso$top4_drug_prescribed[i] %in% unique_generic_name) {
    score_top_4[i] <- 1
  } else {
    score_top_4[i] <- 0
  }
}
PUF_conso$score_top_4 <- score_top_4

score_top_5 <- vector()
for(i in 1:length(PUF_conso$top5_drug_prescribed)) {
  if(PUF_conso$top5_drug_prescribed[i] %in% unique_generic_name) {
    score_top_5[i] <- 1
  } else {
    score_top_5[i] <- 0
  }
}
PUF_conso$score_top_5 <- score_top_5

PUF_conso$total_score_top <- score_top_1 + score_top_2 + score_top_3 + score_top_4 + score_top_5

# Most doc_ids prescribe more branded drugs than generic ones
barplot(table(PUF_conso$total_score_top))
use_generic <- PUF_conso %>% select(doc_id,total_score_top) %>%
                group_by(doc_id) %>% summarise(total_score_top = mean(total_score_top))

# Nearly, 51 % of doc_ids don't prescribe generic drugs at all
100 - (mean(use_generic$total_score_top > 0)*100)
