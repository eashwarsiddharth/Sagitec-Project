# Analysing Prescriber_Detail File
# Comparing suppressed bene counts vs non-suppressed counts using boxplots
# Function to plot side-by-side boxplots to inspect variations/outliers

# New Feature
presc_detail$cost_per_day_per_claim <- round(presc_detail$total_drug_cost/presc_detail$total_day_supply/presc_detail$total_claim_count, digits = 3)

# Plotting by drug_name for bene_count based split
drug_generic_bene_NA <- select(presc_detail, doc_id:cost_per_day_per_claim) %>% filter(is.na(bene_count))
drug_generic_bene <- select(presc_detail, doc_id:cost_per_day_per_claim) %>% filter(!is.na(bene_count))
length(unique(drug_generic_bene_NA$drug_name))
length(unique(drug_generic_bene$drug_name))
intersect_bene <- intersect(drug_generic_bene_NA$drug_name, drug_generic_bene$drug_name)

split_drug_generic_bene_NA <- split(drug_generic_bene_NA, as.factor(drug_generic_bene_NA$drug_name))
length(split_drug_generic_bene_NA)
split_drug_generic_bene <- split(drug_generic_bene, as.factor(drug_generic_bene$drug_name))
length(split_drug_generic_bene)

boxplot_drug_bene_NA <- function(drug_name) {
                          if (any(names(split_drug_generic_bene_NA) %in% drug_name)) {
                            drug_index_NA <- which(names(split_drug_generic_bene_NA) %in% drug_name)
                            feature_index_NA <- which(colnames(split_drug_generic_bene_NA[[drug_index_NA]]) %in% 'cost_per_day_per_claim')
                            boxplot(split_drug_generic_bene_NA[[drug_index_NA]][[feature_index_NA]], xlab = 'bene_NA', ylab = 'cost/day/claim')
                          } else {
                            stop('Please enter a valid drug_name.')
                          }
                        }


boxplot_drug_bene <- function(drug_name) {
                      if (any(names(split_drug_generic_bene) %in% drug_name)) {
                        drug_index <- which(names(split_drug_generic_bene) %in% drug_name)
                        feature_index <- which(colnames(split_drug_generic_bene[[drug_index]]) %in% 'cost_per_day_per_claim')
                        boxplot(split_drug_generic_bene[[drug_index]][[feature_index]], xlab = 'bene', ylab = 'cost/day/claim')
                      } else {
                        stop('Please enter a valid drug_name')
                      }
                    }

# The range and outliers for those in bene_NA seem to be greater than the other
# Can we claim that these records could be further scrutinized ? Drill Down to Location ?

boxplot_drug_bene_split <- function(drug_name) {
                            if (any(intersect_bene %in% drug_name)) {
                              par(mfrow=c(1,2))
                              boxplot_drug_bene_NA(drug_name)
                              boxplot_drug_bene(drug_name)
                              par(mfrow=c(1,1))
                            } else {
                              stop('Please enter drug_name that has both NA and non-NA values under bene_count')
                            }
                          }

# Testing
#unique(drug_generic_bene$drug_name)[which(!(unique(drug_generic_bene$drug_name) %in% intersect_bene))]
boxplot_drug_bene_split(drug_name = 'ZEMAIRA') # works !!
boxplot_drug_bene_split(drug_name = 'CARBIDOPA-LEVODOPA')