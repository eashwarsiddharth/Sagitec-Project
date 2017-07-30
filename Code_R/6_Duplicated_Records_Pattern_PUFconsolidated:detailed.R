
# Ducplicated records in PUF_consolidated/detailed
# Underlying Pattern ?

# PUF_conso
# duplicated records ?
list_1[[3]] %>% select(doc_id,city,state) %>% unique()
length(PUF_conso$doc_id)
PUF_conso$doc_id[which(duplicated(PUF_conso$doc_id))] # Duplicated doc_id
list_1[[3]] %>% select(everything()) %>% filter(doc_id == "FK5280266")
PUF_conso <- PUF_conso[-which(duplicated(PUF_conso$doc_id)),]
dim(PUF_conso %>% select(doc_id,city,state) %>% unique())[[1]] == length(PUF_conso$doc_id)

# PUF_detail
# duplicated records ?
head(PUF_detail)
PUF_detail_anomaly_1 <- (PUF_detail %>% select(doc_id, nppes_provider_city, nppes_provider_state,
                                               hcpcs_code, hcpcs_description, hcpcs_drug_indicator, # hcpcs_description included because they are truncated versions
                                               hcpcs_chapter, hcpcs_class) %>% unique())
dim(PUF_detail_anomaly_1)[[1]] - dim(PUF_detail)[[1]] # No. of records doesn't match the count of unique combinations
sum(PUF_detail %>% select(everything()) %>% duplicated()) # Records seem to be unique
sum(PUF_detail %>% select(doc_id, nppes_provider_city, nppes_provider_state,
                          hcpcs_code, hcpcs_description, hcpcs_drug_indicator,
                          hcpcs_chapter, hcpcs_class) %>% duplicated())
PUF_detail_anomaly_2 <- PUF_detail[which(PUF_detail %>% select(doc_id, nppes_provider_city, nppes_provider_state,
                                                               hcpcs_code, hcpcs_description, hcpcs_drug_indicator,
                                                               hcpcs_chapter, hcpcs_class) %>% duplicated()),]
dim(PUF_detail_anomaly_2)[[1]]
head(PUF_detail_anomaly_2)
row.names(head(PUF_detail_anomaly_2))
# Findings
# All amount based columns have different values
PUF_detail %>% select(everything()) %>% filter(doc_id == 'XZ8826672' & hcpcs_code == '99205')
# All numeric columns have different values
PUF_detail %>% select(everything()) %>% filter(doc_id == 'OH4026307' & hcpcs_code == '99215')
# Barplot for HCPCS_code with duplicates, proportion
# How do you handle this ? # Should we simply aggregate them ? Is there a reason behind such instances ?
require(ggplot2)
require(grid)
row.names(head(PUF_detail_anomaly_2))
PUF_detail$anomaly_hcpcs_code <- 0
PUF_detail$anomaly_hcpcs_code[row.names(PUF_detail) %in% row.names(PUF_detail_anomaly_2)] <- 1
mean(PUF_detail$anomaly_hcpcs_code)
colnames(PUF_detail)
PUF_detail_anomaly_plot_1 <- PUF_detail[,c(4,16)] %>%
                              group_by(hcpcs_code) %>%
                              summarise(proportion_anomaly = (mean(anomaly_hcpcs_code))*100) %>%
                              filter(proportion_anomaly > 0) %>%
                              ggplot(aes(x = hcpcs_code, y = proportion_anomaly)) +
                              geom_bar(stat = 'identity') +
                              ggtitle('HCPCS_code Duplication Proportion') +
                              theme(plot.title = element_text(size=22),
                                    axis.title.x = element_blank(),
                                    axis.text.x = element_blank(),
                                    axis.ticks.x=element_blank())
PUF_detail_anomaly_plot_2 <- PUF_detail[,c(4,16)] %>%
                              group_by(hcpcs_code) %>%
                              summarise(proportion_anomaly = (mean(anomaly_hcpcs_code))*100) %>%
                              filter(proportion_anomaly > 10) %>%
                              ggplot(aes(x = hcpcs_code, y = proportion_anomaly)) +
                              geom_bar(stat = 'identity') +
                              ggtitle('HCPCS_code Duplication Proportion > 10%') +
                              theme(plot.title = element_text(size=15),
                                    text = element_text(size=10),
                                    axis.text.x = element_text(angle=90, hjust=1))
PUF_detail_anomaly_plot_3 <- PUF_detail[,c(4,16)] %>%
                              group_by(hcpcs_code) %>%
                              summarise(proportion_anomaly = (mean(anomaly_hcpcs_code))*100) %>%
                              arrange(desc(proportion_anomaly))
PUF_detail_anomaly_plot_3$hcpcs_code <- factor(PUF_detail_anomaly_plot_3$hcpcs_code,
                                               levels = PUF_detail_anomaly_plot_3$hcpcs_code[order(PUF_detail_anomaly_plot_3$proportion_anomaly,
                                                                                                   decreasing = T)])
PUF_detail_anomaly_plot_3_1 <- PUF_detail_anomaly_plot_3 %>% head(5) %>% arrange(desc(proportion_anomaly)) %>%
                                ggplot(aes(x = hcpcs_code, y = proportion_anomaly)) +
                                geom_bar(stat = 'identity') +
                                ggtitle('HCPCS_code Duplication Proportion Top 5') +
                                theme(plot.title = element_text(size=15),
                                      text = element_text(size=10),
                                      axis.text.x = element_text(angle=90, hjust=1))
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
define_region <- function(row, col){
                  viewport(layout.pos.row = row, layout.pos.col = col)
                }
print(PUF_detail_anomaly_plot_1, vp = define_region(1,1:2))
print(PUF_detail_anomaly_plot_2, vp = define_region(2,1))
print(PUF_detail_anomaly_plot_3_1, vp = define_region(2,2))

# Advised to Aggregate by procedure for (doc_id,city,state,hcpcs_code) # Issue#20 @GitHub
new_PUF_detail <- PUF_detail %>% select(-16) %>%
                    group_by(doc_id, nppes_provider_city, nppes_provider_state, hcpcs_code, hcpcs_description,
                             hcpcs_drug_indicator, hcpcs_chapter, hcpcs_class) %>%
                    summarise_if(.predicate = function(x){!is.factor(x)},
                                 .funs = funs("mean"))
dim(new_PUF_detail)[[1]] - dim(PUF_detail)[[1]]
head(new_PUF_detail)
str(new_PUF_detail)
new_PUF_detail <- as.data.frame(new_PUF_detail)
