# Function to create combined data.frame for 2 given drug_names
create_df_all <- function(drug_name_1, drug_name_2) {
                  if(toupper(drug_name_1) %in% unique(presc_detail$drug_name) & toupper(drug_name_2) %in% unique(presc_detail$drug_name)) {
                    
                    doc_id_drug_name_1 <- presc_detail %>% select(doc_id, drug_name) %>%
                      filter(drug_name %like% toupper(drug_name_1)) %>% select(doc_id) %>% unique()
                    
                    doc_id_drug_name_2 <- presc_detail %>% select(doc_id, drug_name) %>%
                      filter(drug_name %like% toupper(drug_name_2)) %>% select(doc_id) %>% unique()
                    
                    doc_id_drug_name_both <- intersect(doc_id_drug_name_1, doc_id_drug_name_2)
                    
                    df_only_drug_name_1 <- presc_detail %>% select(doc_id, drug_name, cost_per_day_per_claim) %>%
                      filter(!(doc_id %in% doc_id_drug_name_both$doc_id) & (presc_detail$drug_name %like% toupper(drug_name_1))) %>%
                      mutate(y_drug_name = drug_name_1) %>% select(doc_id, y_drug_name, cost_per_day_per_claim) %>%
                      group_by(doc_id, y_drug_name) %>% summarise(mean_cost_per_day_per_claim = mean(cost_per_day_per_claim))
                    
                    df_only_drug_name_2 <- presc_detail %>% select(doc_id, drug_name, cost_per_day_per_claim) %>%
                      filter(!(doc_id %in% doc_id_drug_name_both$doc_id) & (presc_detail$drug_name %like% toupper(drug_name_2))) %>%
                      mutate(y_drug_name = drug_name_2) %>% select(doc_id, y_drug_name, cost_per_day_per_claim) %>%
                      group_by(doc_id, y_drug_name) %>% summarise(mean_cost_per_day_per_claim = mean(cost_per_day_per_claim))
                    
                    df_drug_name_both <- presc_detail %>% select(doc_id, drug_name, cost_per_day_per_claim) %>%
                      filter((doc_id %in% doc_id_drug_name_both$doc_id) &
                               ((presc_detail$drug_name %like% toupper(drug_name_1)) | (presc_detail$drug_name %like% toupper(drug_name_2)))) %>%
                      mutate(y_drug_name = paste(drug_name_1,'/',drug_name_2)) %>% select(doc_id, y_drug_name, cost_per_day_per_claim) %>%
                      group_by(doc_id, y_drug_name) %>% summarise(mean_cost_per_day_per_claim = mean(cost_per_day_per_claim))
                    
                    drugs_cost_day_claim_df <- rbind(df_only_drug_name_1, df_only_drug_name_2, df_drug_name_both)
                    
                    all_presc <- suppressWarnings(inner_join(drugs_cost_day_claim_df, presc_sumry))
                    
                    all_PUF <- suppressWarnings(inner_join(new_PUF_smry, new_PUF_conso))
                    
                    all_df <- suppressWarnings(inner_join(all_presc, all_PUF))
                    
                    lut <- c('NY' = 'New York', 'CA' = 'California', 'FL' = 'Florida', 'SC' = 'South Carolina', 'WA' = 'Washington',
                             'IL' = 'Illinois', 'DE' = 'Delaware', 'PA' = 'Pennsylvania', 'TN' = 'Tennessee', 'NC' = 'North Carolina',
                             'MO' = 'Missouri', 'NM' = 'New Mexico', 'TX' = 'Texas', 'NH' = 'New Hampshire', 'LA' = 'Louisiana',
                             'NJ' = 'New Jersey', 'MA' = 'Massachusetts', 'MD' = 'Maryland', 'AL' = 'Alabama', 'OH' = 'Ohio',
                             'OR' = 'Oregon', 'MI' = 'Michigan', 'SD' = 'South Dakota', 'IN' = 'Indiana', 'IA' = 'Iowa',
                             'MT' = 'Montana', 'KY' = 'Kentucky', 'VA' = 'Virginia', 'CO' = 'Colorado', 'MS' = 'Mississippi',
                             'CT' = 'Connecticut', 'KS' = 'Kansas', 'HI' = 'Hawaii', 'GA' = 'Georgia', 'OK' = 'Oklahoma',
                             'AZ' = 'Arizona', 'WY' = 'Wyoming', 'DC' = 'District of Columbia', 'NV' = 'Nevada', 'MN' = 'Minnesota',
                             'WI' = 'Wisconsin', 'UT' = 'Utah', 'ND' = 'North Dakota', 'RI' = 'Rhode Island', 'ME' = 'Maine',
                             'PR' = 'Puerto Rico', 'WV' = 'West Virginia', 'AR' = 'Arkansas', 'NE' = 'Nebraska', 'ID' = 'Idaho',
                             'VT' = 'Vermont', 'AK' = 'Alaska', 'ZZ' = 'Foreign Country', 'VI' = 'Virgin Islands', 'GU' = 'Guam')
                    
                    all_df$nppes_provider_state <- lut[all_df$nppes_provider_state]
                    
                    as.data.frame(all_df %>% select(1,3:76,2))
                    
                  } else {
                    stop('Please make sure both the drug_name are part of the presc_detail data.frame')
                  }
                }

trial_create_df_all <- create_df_all(drug_name_1 = 'asbkjdbfil', drug_name_2 = 'Copaxone') # Works !!
gi_cop <- create_df_all(drug_name_1 = 'Gilenya', drug_name_2 = 'Copaxone') # Works !!
str(gi_cop)
ga_aman <- create_df_all(drug_name_1 = 'GABAPENTIN', drug_name_2 = 'AMANTADINE') # Works !!
str(ga_aman)
head(unique(presc_detail$drug_name))

# To make it work on other systems the following objects are necessary !
# Make sure the current working directory has these files...
presc_detail <- read.csv('prescriber.detailed.csv')
presc_detail$cost_per_day_per_claim <- presc_detail$total_drug_cost/presc_detail$total_day_supply/presc_detail$total_claim_count
presc_sumry <- read.csv('prescriber.summary.csv')
new_PUF_smry <- read.csv('new_PUF_smry.csv')
new_PUF_conso <- read.csv('new_PUF_conso.csv')