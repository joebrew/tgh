library(RISmed)
# http://amunategui.github.io/pubmed-query/

# Define search topic
search_topic <- 'leishmaniasis'

# Define search range
start_year <- 2000
end_year <- 2015
years <- start_year:end_year

# Create an empty list for sticking results
results_list <- list()
# Create another empty list for sticking abstracts
abstracts_list <- list()

# Loop through each year to get results
for (year in 1:length(years)){
  # Perform search
  search_query <- 
    EUtilsSummary(search_topic, 
                  db="pubmed", 
                  mindate=years[year], 
                  maxdate=years[year],
                  retmax = 500)
  
  # See results summary
  # summary(search_query)
  
  # See number of results
  # QueryCount(search_query)
  
  # Get IDs of the articles returned in our query
  # Qids <- queryId(search_query)
  
  # Actually fetch data
  message(paste0('Fetching records for year ', years[year]))
  records <- EUtilsGet(search_query)
  
  # Turn into a data.frame
  pubmed_data <- 
    data.frame('title' = ArticleTitle(records),
               # 'Abstract' = AbstractText(records),
               'language' = Language(records),
               'country' = Country(records),
               'id' = ArticleId(records),
               'year' = years[year])
  
  # Create separate dataframe for abstracts
  abstracts <- data.frame('id' = ArticleId(records),
                          'abstract' = AbstractText(records))
  
  # Add authors separately
  temp <- Author(records)
  first_authors <- lapply(temp,
                          function(x){
                            x[1,]
                          })
  last_authors <- lapply(temp,
                         function(x){
                           x[nrow(x),]
                         })
  for (i in c('first', 'last')){
    # Last name
    pubmed_data[,paste(i, '_author_last_name')] <- 
      unlist(lapply(get(paste0(i, '_authors')), function(x){x['LastName']}))
    
    # First name
    pubmed_data[,paste(i, '_author_first_name')] <- 
      unlist(lapply(get(paste0(i, '_authors')), function(x){x['ForeName']}))
    
    # Initials
    pubmed_data[,paste(i, '_author_initials')] <- 
      unlist(lapply(get(paste0(i, '_authors')), function(x){x['Initials']}))
  }
  
  # Add results to results_list
  results_list[[year]] <- pubmed_data
  
  # Add abstract to abstracts list
  abstracts_list[[year]] <- abstracts
  
  # Remove unecessary objects
  rm(pubmed_data, abstracts)
}

# Bind together the results
results <- do.call('rbind', results_list)

# Bind together the abstracts
abstracts <- do.call('rbind', abstracts_list)



