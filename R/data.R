# PROBLEM 1a
all_processed_data <- function() {
  dat <- list.files('../raw_data', full.names = T) %>% 
    map_df(function(.thisFile) {
      thisDF <- fread(.thisFile)
      thisDF$country <- str_sub(.thisFile, -12, -11)
      return(thisDF)
    })
  
  return(dat)
}