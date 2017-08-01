
# Avonex vs Copaxone
# Build using create_df_all function
avocop <- create_df_all(drug_name_1 = 'Avonex', drug_name_2 = 'Copaxone')
avocop <- droplevels(avocop) # Since this is a subset of the original full dataset !
write.csv(x = avocop, file = 'Avo_Cop.csv', row.names = F)

# Calculations based on Flags !
# bene_count_ge65_suppress_flag
find_desc(att_name = 'bene_count_ge65_suppress_flag', match = 1)
# Given Clue ! View(avocop) !
# bene_count_ge65 = 5, iff, bene_count_ge65_suppress_flag = '*'
# And, for all ge65_suppress_flag = '#', total_claim_count_ge65 = total_claim_count - 5

# ge65_suppress_flag # Revisit for other cost & supply related attribute value imputation !!
find_desc(att_name = 'ge65_suppress_flag', match = 1)
avocop$total_claim_count_ge65[avocop$ge65_suppress_flag == '*'] <- 5
avocop$total_claim_count_ge65[avocop$ge65_suppress_flag == '#'] <- avocop$total_claim_count[avocop$ge65_suppress_flag == '#'] - 5

# brand_suppress_flag # Revisit for other cost related attribute value imputation !!
find_desc(att_name = 'brand_suppress_flag', match = 1)
brand_flag_asterix <- avocop$brand_suppress_flag == '*'
brand_flag_pound_generic <- avocop$brand_suppress_flag == '#' &
                              avocop$generic_suppress_flag == '' &
                              avocop$other_suppress_flag == '*' # for, brand_claim = total_claim - generic_claim - 5
brand_flag_pound_other <- avocop$brand_suppress_flag == '#' &
                            avocop$generic_suppress_flag == '*' &
                            avocop$other_suppress_flag == '' # for, brand_claim = total_claim - 5 - other_claim
avocop$brand_claim_count[brand_flag_asterix] <- 5
avocop$brand_claim_count[brand_flag_pound_generic] <- avocop$total_claim_count[brand_flag_pound_generic] -
                                                        avocop$generic_claim_count[brand_flag_pound_generic] - 5
avocop$brand_claim_count[brand_flag_pound_other] <- avocop$total_claim_count[brand_flag_pound_other] - 5 -
                                                      avocop$other_claim_count[brand_flag_pound_other]

# generic_suppress_flag # Revisit for other cost related attribute value imputation !!
find_desc(att_name = 'generic_suppress_flag', match = 1)
generic_flag_asterix <- avocop$generic_suppress_flag == '*'
generic_flag_pound_brand <- avocop$generic_suppress_flag == '#' &
                              avocop$brand_suppress_flag == '' &
                              avocop$other_suppress_flag == '*' # for, generic_claim = total_claim - brand_claim - 5
generic_flag_pound_other <- avocop$generic_suppress_flag == '#' &
                              avocop$brand_suppress_flag == '*' &
                              avocop$other_suppress_flag == '' # for, generic_claim = total_claim - 5 - other_claim
avocop$generic_claim_count[generic_flag_asterix] <- 5
avocop$generic_claim_count[generic_flag_pound_brand] <- avocop$total_claim_count[generic_flag_pound_brand] -
                                                          avocop$brand_claim_count[generic_flag_pound_brand] - 5
avocop$generic_claim_count[generic_flag_pound_other] <- avocop$total_claim_count[generic_flag_pound_other] - 5 -
                                                          avocop$other_claim_count[generic_flag_pound_other]

# other_suppress_flag # Revisit for other cost related attribute value imputation !!
find_desc(att_name = 'other_suppress_flag', match = 1)
other_flag_asterix <- avocop$other_suppress_flag == '*'
other_flag_pound_brand <- avocop$other_suppress_flag == '#' &
                            avocop$brand_suppress_flag == '' &
                            avocop$generic_suppress_flag == '*' # for, other_claim = total_claim - brand_claim - 5
other_flag_pound_generic <- avocop$other_suppress_flag == '#' &
                              avocop$brand_suppress_flag == '*' &
                              avocop$generic_suppress_flag == '' # for, other_claim = total_claim - 5 - generic_claim
avocop$other_claim_count[other_flag_asterix] <- 5
avocop$other_claim_count[other_flag_pound_brand] <- avocop$total_claim_count[other_flag_pound_brand] -
                                                      avocop$brand_claim_count[other_flag_pound_brand] - 5
avocop$other_claim_count[other_flag_pound_generic] <- avocop$total_claim_count[other_flag_pound_generic] - 5 -
                                                        avocop$generic_claim_count[other_flag_pound_generic]

# Dropping attributes with insufficient description, as advised ! # Issue#29 @GitHub
avocop <- avocop %>% select(-which(colnames(avocop) %in% c('number_of_drug_hcpcs','total_drug_services', 'total_drug_medicare_allowed_amt',
                                                           'number_of_med_hcpcs', 'total_med_services', 'total_med_medicare_allowed_amt')))

# Converting factors to chararcters for easier substitutions
avocop <- avocop %>% mutate_if(is.factor, as.character)

# Cleaning up top_hcpcs_code !
avocop$top_hcpcs_code_2[is.na(avocop$top_hcpcs_code_2)] <- 'Not_Applicable'
avocop$top_hcpcs_code_3[is.na(avocop$top_hcpcs_code_3)] <- 'Not_Applicable'
avocop$top_hcpcs_code_received_per_submitted_charge_2[avocop$top_hcpcs_code_2 == 'Not_Applicable'] <- 0
avocop$top_hcpcs_code_received_per_submitted_charge_3[avocop$top_hcpcs_code_3 == 'Not_Applicable'] <- 0

# Cleaning up hcpcs class counts !
avocop$diagnostic[is.na(avocop$diagnostic)] <- 0
avocop$other[is.na(avocop$other)] <- 0
avocop$therapeutic[is.na(avocop$therapeutic)] <- 0
sum((avocop$diagnostic + avocop$other + avocop$therapeutic) != avocop$total_services_count) # Successfull !

# Cleaning up hcpcs class count percentages !
avocop$diagnostic_services_percentage[avocop$diagnostic == 0] <- 0
avocop$other_services_percentage[avocop$other == 0] <- 0
avocop$therapeutic_services_percentage[avocop$therapeutic == 0] <- 0
sum(round((avocop$diagnostic_services_percentage + avocop$other_services_percentage +
             avocop$therapeutic_services_percentage), digits = 0) != 100) # Successful !

# Cleaning up speciality ranks !
avocop$diagnostic_services_rank_in_his_speciality[avocop$diagnostic == 0] <- 0
avocop$other_services_rank_in_his_speciality[avocop$other == 0] <- 0
avocop$therapeutic_services_rank_in_his_speciality[avocop$therapeutic == 0] <- 0
df_services_rank_verification_1 <- cbind(avocop$diagnostic_services_rank_in_his_speciality != 0,
                                         avocop$other_services_rank_in_his_speciality != 0,
                                         avocop$therapeutic_services_rank_in_his_speciality != 0)
df_services_rank_verification_2 <- cbind(avocop$diagnostic != 0, avocop$other != 0, avocop$therapeutic != 0)
# If doc_id has performed a hcpcs_class service, then they have a rank in the specific hcpcs_class
mean(rowSums(df_services_rank_verification_1) == rowSums(df_services_rank_verification_2)) # Successfull !

# Cleaning up top_drug_prescribed
avocop$top3_drug_prescribed[is.na(avocop$top3_drug_prescribed)] <- 'Not_Applicable'
avocop$top4_drug_prescribed[is.na(avocop$top4_drug_prescribed)] <- 'Not_Applicable'
avocop$top5_drug_prescribed[is.na(avocop$top5_drug_prescribed)] <- 'Not_Applicable'

# Cleaning up top_drug_cost_prescribed
avocop$top3_drug_cost_prescribed[is.na(avocop$top3_drug_cost_prescribed)] <- 0
avocop$top4_drug_cost_prescribed[is.na(avocop$top4_drug_cost_prescribed)] <- 0
avocop$top5_drug_cost_prescribed[is.na(avocop$top5_drug_cost_prescribed)] <- 0

# Looking for NAs # Revisit for imputing missing values !
(sum(complete.cases(avocop))/dim(avocop)[[1]])*100
sum(complete.cases(avocop))
sort(colMeans(is.na(avocop))[colMeans(is.na(avocop)) > 0], decreasing = T)
sort(table(colMeans(is.na(avocop))), decreasing = T) # other_drug_cost, odd one out ! Others are multiple of 2
find_desc(att_name = 'other_drug_cost', match = 1)
all_names[which(all_names %like% 'cost_percentage')] # other_drug_cost_percentage not given !

# Cleaning up empty strings !
avocop_flags <- avocop[,which(colnames(avocop) %like% 'flag')]
str(avocop_flags)
sum(avocop_flags == '') == sum(avocop == '', na.rm = T) # Successfull ! # Empty Strings exist only in the flags, not in any other attribute !
avocop[avocop == ''] <- 'Not_Suppressed'


# Visulaising missing values - ggplot
require(ggplot2)
require(reshape)
head(avocop %>% is.na %>% melt)
ggplot_missing <- function(x){
                    x %>% is.na %>% melt %>%
                      ggplot(data = ., aes(x = X1, y = X2)) +
                      geom_raster(aes(fill = value)) +
                      scale_fill_grey(name = "", labels = c("Present","Missing")) +
                      labs(y = "Variables in Dataset", x = "Rows / observations")
                  }
ggplot_missing(avocop)

#write.csv(x = avocop, file = 'Avo_Cop_partial_tidy.csv', row.names = F)

# Missing values in each category
avocop_Y_split <- avocop %>% split(f = as.factor(.$y_drug_name))
lapply(X = avocop_Y_split, FUN = is.na) %>% lapply(FUN = table)
lapply(X = avocop_Y_split, FUN = complete.cases) %>% lapply(FUN = sum)

# Consider only complete cases
avocop_cc <- avocop[complete.cases(avocop),]
str(avocop_cc)
write.csv(x = avocop_cc, file = 'Avo_Cop_tidy_complete_cases.csv', row.names = F)