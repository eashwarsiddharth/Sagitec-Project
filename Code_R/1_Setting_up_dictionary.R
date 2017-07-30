# Setting up Dictionary
# Function to get description of specified attribute(s)

# Set Desired Working Directory
#setwd('Documents/PGCBA/Placement/Sagitec/SSN_SACE_2017_Jan-master/CSV')
require(data.table)

# reading data
file_names <- c('prescriber.detailed.csv', 'prescriber.summary.csv',
                'PUF_pres_conso.csv', 'PUF.detailed.csv', 'PUF.summary.csv')
list_1 <- lapply(file_names, fread, stringsAsFactors = T) # easier to search for attribute
str(list_1)

# variables in each data table
dt_names <- lapply(list_1, names)

# get all unique variable names
all_names <- vector()
for (i in 1:length(list_1)) {
  a <- names(list_1[[i]])
  all_names <- unique(c(a, all_names))
}
length(all_names)


# Setting up Data Dictionary
require(gdata)
full_att_desc <- read.xls('Data_Desc.xlsx') # ignore warnings !
length(full_att_desc$Attribute)
require(stringr)
str_count(full_att_desc$Attribute[1]) # needs to be trimmed, since, npi is only 3 characters long
full_att_desc$Attribute <- str_trim(full_att_desc$Attribute)
require(dplyr)
attr_desc <- select(full_att_desc, Attribute,Description) %>% filter(full_att_desc$Attribute %in% all_names)
dim(attr_desc)
length(all_names)
# Redundant Operation !
#missing_attr_desc <- all_names[which(!(all_names %in% attr_desc$Attribute))]
#missing_attr_desc_which_dataSet <- list()
#for (i in 1:length(missing_attr_desc)) {
#  missing_attr_desc_which_dataSet[[i]] <- which(dt_names %like% missing_attr_desc[i])
#}
#names(missing_attr_desc_which_dataSet) <- missing_attr_desc
#missing_attr_desc_which_dataSet

find_desc <- function(att_name, match = 2) {
              require(data.table)
              if (match == 0) {
                if (length(att_name) > 1) {
                  stop('\n', 'Have you specified the following;', '\n\t','match = 0 for desc of all attributes with similar name',
                       '\n\t', 'match = 1 for desc of attributes in a specific data table', 
                       '\n', 'If you are searching for more than 1 attribute, please use lapply(<names_vector>,<function>, <match>)')
                } else if (any(attr_desc$Attribute %like% att_name)) {
                  # Useful >> when you wish to look at desc of all attributes with similar name
                  attr_desc[which(attr_desc$Attribute %like% att_name),]
                } else {
                  warning(att_name,' not found !')
                }
              } else if (match == 1) {
                # Useful >> when you wish to look at desc of attributes in a specific data table
                if (length(att_name) > 1) {
                  stop('\n', 'Have you specified the following;', '\n\t','match = 0 for desc of all attributes with similar name',
                       '\n\t', 'match = 1 for desc of attributes in a specific data table', 
                       '\n', 'If you are searching for more than 1 attribute, please use lapply(<names_vector>,<function>, <match>)')
                } else if (any(attr_desc$Attribute %like% att_name)) {
                  attr_desc[match(att_name,attr_desc$Attribute),]
                } else {
                  warning(att_name,' not found !')
                }
              } else if (match != 0 & match != 1) {
                stop('\n', 'Have you specified the following;', '\n\t','match = 0 for desc of all attributes with similar name',
                     '\n\t', 'match = 1 for desc of attributes in a specific data table', 
                     '\n', 'If you are searching for more than 1 attribute, please use lapply(<names_vector>,<function>, <match>)')
              }
            }

# Testing
find_desc('generic_name')
find_desc(c('npi', 'drug_name')) # works !!
lapply(c('bene_count_ge65', 'total_claim_count'), find_desc, match = 0)
lapply(dt_names[[5]], find_desc, match = 1)

# split into tables
presc_detail <- list_1[[1]] # 'prescriber.detailed.csv'
presc_sumry <- list_1[[2]] # 'prescriber.summary.csv'
PUF_conso <- list_1[[3]] # 'PUF_pres_conso.csv'
PUF_detail <- list_1[[4]] # 'PUF.detailed.csv'
PUF_sumry <- list_1[[5]] # 'PUF.summary.csv

names(dt_names) <- c('presc_detail', 'presc_sumry', 'PUF_conso', 'PUF_detail', 'PUF_sumry')

# Finding tables corresponding to attribute (pattern)
names(dt_names)[which(dt_names %like% 'total_claim_count')]