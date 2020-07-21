
library(dplyr)
# Data Table
library(data.table)

# Counter
library(tictoc)

library(vroom)

# #Import Patent data
# col_types <- list(
#   id = col_character(),
#   type = col_character(),
#   number = col_character(),
#   country = col_character(),
#   date = col_date("%Y-%m-%d"),
#   abstract = col_character(),
#   title = col_character(),
#   kind = col_character(),
#   num_claims = col_double(),
#   filename = col_character(),
#   withdrawn = col_double()
# )
# 
# patent_tbl <- vroom(
#   file       = "00_data/Patents/patent.tsv", 
#   delim      = "\t", 
#   col_names  = names(col_types),
#   col_types  = col_types,
#   na         = c("", "NA", "NULL")
# )

#setDT(patent_tbl)



# #Import uspc main class description

col_types <- list(
  id = col_character(),
  title = col_character())
  
mainclass_description <- vroom(
  file       = "00_data/Patents/mainclass_current.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
setnames(mainclass_description, "id", "mainclass_id")
setDT(mainclass_description)

  
#load uspc data

col_types <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_double()
)

uspc_tbl <- vroom(
  file       = "00_data/Patents/uspc.tsv",
  delim      = "\t",
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
setDT(uspc_tbl)

#import assignee Data
col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
  )

assignee_tbl <- vroom(
  file       = "00_data/Patents/assignee.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)



#import patent assignee
col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "00_data/Patents/patent_assignee.tsv", 
  delim      = "\t", 
  col_names  = names(col_types),
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)



setDT(assignee_tbl)

setDT(patent_assignee_tbl)


#change column name of patent_assignee to id
setnames(patent_assignee_tbl, "assignee_id", "id")

#Joining / Merging Data


combined_data <-
  merge(x = assignee_tbl, y = patent_assignee_tbl, 
                       by    = "id", 
                       all.x = TRUE, 
                       all.y = FALSE)

rm(assignee_tbl, patent_assignee_tbl)
gc()


combined_data <-
  merge(x = combined_data, y = uspc_tbl, 
        by    = "patent_id", 
        all.x = TRUE, 
        all.y = FALSE)

rm(uspc_tbl)
gc()
combined_data <-
  merge(x = combined_data, y = mainclass_description, 
        by    = "mainclass_id", 
        all.x = TRUE, 
        all.y = FALSE)

rm(mainclass_description)
gc()

#Question 1
#What US company has the most patents? List the 10 US companies with the most assigned/granted patents

# tic()
# combined_data %>%
# select(id, organization)%>%
# count(organization, sort = TRUE)%>%
#   head(n = 10)
# toc()


combined_data %>%
group_by(id)%>%
  filter(!is.na(organization))%>%
count(organization, sort = TRUE)%>%
  head(n = 10)


#question 2
#can not do because patents file is to large



#Question 3
#What is the most innovative tech sector? For the top 10 companies with the most patents, 
#what are the top 5 USPTO tech main classes?



combined_data %>%
  select(id, organization, mainclass_id, title)%>%
  group_by(id)%>%
  filter(!is.na(organization))%>%
  filter(!is.na(mainclass_id))%>%
  filter(title != "DOES NOT EXIST") %>%
  filter(title != "unclassified") %>%
  count(organization, title, sort = TRUE)%>%
    
  head(n = 10)




