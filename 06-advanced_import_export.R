##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Advanced data reading and writing
##########################################################################


require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
require(data.table) # for fread()


# Reading data ------------------------------------------------------------



data_path <- file.path("data", "flights")
files <- dir(data_path, pattern = "*.csv")


flights <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>% 
  reduce(rbind)
flights

# The same using an anonymous function
flights <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) %>%  
  reduce(rbind)
flights


system.time( flights <- data_frame(filename = files) %>% # create a data frame
               # holding the file names
               mutate(file_contents = map(filename,          # read files into
                                          ~ data.table::fread(file.path(data_path, .), showProgress=T, nThread=4)) # a new data column
               )  )

flights

flights <- unnest(flights)

flights

print(object.size(get('flights')), units='auto')



# Exporting data ----------------------------------------------------------

flights %>% 
  sample_n(1000) %>% 
  write_csv(., file.path("exports", "flights.csv"))


flights %>% 
  group_by(Month) %>% 
  do(tail(., 2))

dir.create('exports')

flights %>% 
  sample_n(1000) %>% 
  group_by(Year, Month) %>%
  do(write_csv(., file.path("exports", paste0(unique(.$Year),"_",unique(.$Month), "_flights.csv"))))
