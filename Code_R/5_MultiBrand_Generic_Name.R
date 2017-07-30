
# Analysing prescription of different brand for the same generic name by a doc_id
# Function to get Top N candidates, who will likely try out new drug for a given generic name
# based on cost/day/claim, generic prescription, likelihood to switch

# Grouping by Doc_ID
length(unique(presc_detail$doc_id))
grouped_unique_doc_id <- select(presc_detail, doc_id:nppes_provider_state) %>% split(f = presc_detail$doc_id) %>% lapply(FUN = unique)
length(grouped_unique_doc_id)
x_doc_id <- sapply(grouped_unique_doc_id, nrow)
head(x_doc_id)

# doc_ids in presc_detail provide services only in 1 location
length(x_doc_id[x_doc_id >1])

# Doctors: Different Brands for a Drug
grouped_unique_doc_id_generic_name <- select(presc_detail, doc_id, drug_name:generic_name) %>%
                                        split(f = presc_detail$doc_id) %>%
                                        lapply(FUN = unique)
head(grouped_unique_doc_id_generic_name, 1)
grouped_unique_doc_id_generic_name[[1]][[3]] # need to drop unused levels
table_grouped_unique_doc_id_generic_name <- lapply(grouped_unique_doc_id_generic_name,
                                                   function(x){
                                                     select(x, generic_name) %>% droplevels() %>% table()
                                                   })
head(table_grouped_unique_doc_id_generic_name, 1)
table_grouped_unique_doc_id_generic_name[[2]] == 1
doc_id_diff_brand_generic <- sapply(table_grouped_unique_doc_id_generic_name,
                                    function(x){
                                      sum(x > 1)
                                    })
head(doc_id_diff_brand_generic)

# 64.3% of the doc_ids don't seem to prescribe the same brand for a drug... they tend to juggle b/w brands for each prescription ?!
(mean(doc_id_diff_brand_generic > 0))*100

# proportion of drugs for which different brand has been prescribed for the same generic name
doc_id_proportion_diff_brand_generic <- sapply(table_grouped_unique_doc_id_generic_name,
                                               function(x){
                                                 (mean(x > 1))
                                               })
hist(doc_id_proportion_diff_brand_generic)

# Function to identify top candidates to target based on cost/day/claim, generic prescription, likelihood to switch
find_top_candidates <- function(generic_drug_name, n = 10) {
                        if(!(generic_drug_name %in% unique(presc_detail$generic_name))) {
                          stop('Please enter valid generic_name')
                        } else {
                          x <- which(names(table_grouped_unique_doc_id_generic_name) %in% jk)
                          x_1 <- table_grouped_unique_doc_id_generic_name[x]
                          x_2 <- lapply(x_1, names)
                          require(data.table)
                          x_3 <- names(x_2)[which(x_2 %like% generic_drug_name)]
                          require(plotrix)
                          y_1 <- presc_detail %>%
                            select(doc_id,generic_name,cost_per_day_per_claim)  %>%
                            filter((presc_detail$doc_id = presc_detail$doc_id %in% x_3) & presc_detail$generic_name == generic_drug_name) %>%
                            group_by(doc_id, generic_name) %>% # needed to address condition where the doc_id prescribes diff brands
                            summarise(median(cost_per_day_per_claim))
                          y_1$cost_score <- rescale(y_1$`median(cost_per_day_per_claim)`, range(1:100))
                          y_2 <- y_1[,-3]
                          switch_chance <- vector()
                          for(i in 1:length(y_2$doc_id)) {
                            switch_chance[i] <- doc_id_proportion_diff_brand_generic[names(doc_id_proportion_diff_brand_generic) == y_2$doc_id[i]]
                          }
                          switch_chance <- rescale(switch_chance, range(1:100))
                          y_3 <- cbind(as.data.frame(y_2),switch_chance)
                          x_4 <- PUF_conso %>% select(doc_id, total_score_top) %>% # Useful >> iff, duplicated records present
                            filter(doc_id %in% x_3) %>% group_by(doc_id) %>%
                            summarise(total_score_top = mean(total_score_top))
                          generic_usage_score <- vector()
                          for(i in 1:length(y_3$doc_id)) {
                            generic_usage_score[i] <- x_4$total_score_top[which(as.character(x_4$doc_id) ==  as.character(y_3$doc_id[i]))]
                          }
                          generic_usage_score <- rescale(generic_usage_score, range(1:100))
                          y_4 <- cbind(y_3,generic_usage_score)
                          y_4$Final_Score <- (y_4$cost_score + y_4$switch_chance + y_4$generic_usage_score)/3
                          y_5 <- y_4[,-2] %>% arrange(desc(Final_Score))
                          head(y_5, n)
                        }
                      }

head(unique_generic_name)
find_top_candidates('zadkanso', 5) # works !! :D
find_top_candidates('GABAPENTIN', 5) # works !! :D