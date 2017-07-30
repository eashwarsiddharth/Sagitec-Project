
# Getting top_hcpcs codes from PUF_detailed based on amount received per submitted charge
# getting switch_likelihood from Code(5) doc_id_proportion_diff_brand_generic
# Creating new PUF_consolidated/detailed files

# PUF_smry
# All records are unique by (id city, state) combination !
head(PUF_sumry)
dim(PUF_sumry)[[1]] - dim(PUF_sumry %>% select(doc_id, nppes_provider_city, nppes_provider_state) %>% unique)[[1]]

# Get top 3 hcpcs_codes for each doc_id
head(new_PUF_detail)
get_hcpcs_1 <- new_PUF_detail %>% mutate(received_per_submitted_charge = average_medicare_payment_amt/average_submitted_chrg_amt) %>%
                split(f = as.factor(new_PUF_detail$doc_id))
get_hcpcs_2 <- get_hcpcs_1 %>% lapply(function(x){
                                        x_1 <- x %>% select(hcpcs_code, received_per_submitted_charge) %>%
                                          arrange(desc(received_per_submitted_charge)) %>% head(3)
                                        x_2 <- t(x_1)
                                        x_3 <- c(x_2[1,], x_2[2,])
                                        if(length(x_3) == 2) {
                                          x_w1 <- c(x_2[1,], NA, NA, x_2[2,], NA, NA)
                                          return(trimws(x_w1))
                                        } else if(length(x_3) == 4) {
                                          x_w2 <- c(x_2[1,], NA, x_2[2,], NA)
                                          return(trimws(x_w2))
                                        } else if(length(x_3) == 6) {
                                          return(trimws(x_3))
                                        }
                                      })
head(get_hcpcs_2) # Each record is a list
get_hcpcs_3 <- as.data.frame(matrix(unlist(get_hcpcs_2, use.names = F), ncol = 6, byrow = T), stringsAsFactors = F)
colnames(get_hcpcs_3) <- c('top_hcpcs_code_1', 'top_hcpcs_code_2', 'top_hcpcs_code_3',
                           'top_hcpcs_code_received_per_submitted_charge_1', 'top_hcpcs_code_received_per_submitted_charge_2',
                           'top_hcpcs_code_received_per_submitted_charge_3')
get_hcpcs_3$doc_id <- names(get_hcpcs_2)
get_hcpcs_4 <- select(get_hcpcs_3, 7, 1:6)
str(get_hcpcs_4)
get_hcpcs_4$top_hcpcs_code_received_per_submitted_charge_1 <- as.numeric(get_hcpcs_4$top_hcpcs_code_received_per_submitted_charge_1)
get_hcpcs_4$top_hcpcs_code_received_per_submitted_charge_2 <- as.numeric(get_hcpcs_4$top_hcpcs_code_received_per_submitted_charge_2)
get_hcpcs_4$top_hcpcs_code_received_per_submitted_charge_3 <- as.numeric(get_hcpcs_4$top_hcpcs_code_received_per_submitted_charge_3)
str(get_hcpcs_4)

# Get mean_received_per_submitted_charge
get_received_per_submitted_charge_1 <- new_PUF_detail %>% select(doc_id, average_medicare_payment_amt, average_submitted_chrg_amt) %>%
                                        mutate(received_per_submitted_charge = average_medicare_payment_amt/average_submitted_chrg_amt) %>%
                                        select(doc_id, received_per_submitted_charge) %>% group_by(doc_id) %>%
                                        summarise(mean_received_per_submitted_charge = mean(received_per_submitted_charge))
get_hcpcs_5 <- inner_join(get_hcpcs_4, get_received_per_submitted_charge_1)
summary(get_hcpcs_5)

# Get likelihood to switch from proportion of drugs for which different brands were prescribed
switch_likelihood <- vector()
for(i in 1:length(get_hcpcs_5$doc_id)) {
  if(get_hcpcs_5$doc_id[i] %in% names(doc_id_proportion_diff_brand_generic)) {
    switch_likelihood[i] <- doc_id_proportion_diff_brand_generic[names(doc_id_proportion_diff_brand_generic) == get_hcpcs_5$doc_id[i]]
  } else {
    switch_likelihood[i] <- NA # NA implies that the given doc_id does not prescribe meds (or) it's missing in pres_detail
  }
}

head(switch_likelihood)
length(switch_likelihood)

# Create new PUF_smry to include top 3 services
new_PUF_smry <- inner_join(x = PUF_sumry, y = get_hcpcs_5)
dim(new_PUF_smry) # [[1]] same as before
head(new_PUF_smry)
str(new_PUF_smry)
write.csv(x = new_PUF_smry, file = 'new_PUF_smry.csv', row.names = F)

# Create new_PUF_conso to include switch_likelihood
# new_PUF_conso_2 <- cbind(new_PUF_conso_1, switch_likelihood) # arguments imply differing number of rows: 11314, 11321
switch_likelihood <- as.data.frame(switch_likelihood)
switch_likelihood$doc_id <- get_hcpcs_5$doc_id
head(switch_likelihood)
new_PUF_conso <- inner_join(x = PUF_conso, y = switch_likelihood)
head(new_PUF_conso)
str(new_PUF_conso)
colnames(new_PUF_conso)[c(2,3, 42:47)] <- c('nppes_provider_city', 'nppes_provider_state',
                                            'generic_usage_score_top_1', 'generic_usage_score_top_2', 'generic_usage_score_top_3',
                                            'generic_usage_score_top_4', 'generic_usage_score_top_5', 'generic_usage_total_score_top')
str(new_PUF_conso)
write.csv(x = new_PUF_conso, file = 'new_PUF_conso.csv', row.names = F)
