##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Tidyr
##########################################################################

gapminder <- read_csv("data/gapminder.csv")

gap_wide <- read_csv("https://docs.google.com/spreadsheets/d/1NTXQNoY8V0H_EZ_peFmnH1ZcGlxCPhwl2VmJNpiACMU/pub?output=csv")
# gap_wide <- read.csv('data/gapminder_wide.csv')
head(gap_wide)

gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values)
head(gap_long)
tail(gap_long)

# Debemos indicar las columnas a transformar
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         3:38)  # ó -1:-2
head(gap_long)
tail(gap_long)

# Alternativa
gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         dplyr::starts_with('pop'),
         dplyr::starts_with('lifeExp'),
         dplyr::starts_with('gdpPercap'))
head(gap_long)
tail(gap_long)

# ¿Que ocurre con la variable 'obstype_year'?

gap_long <- gap_wide %>% 
  gather(key   = obstype_year,
         value = obs_values,
         -continent, -country) %>%
  separate(obstype_year,
           into = c('obs_type','year'),
           sep="_")
head(gap_long)
tail(gap_long)

# spread()
gap_normal <- gap_long %>% 
  spread(obs_type, obs_values)

head(gap_normal)
head(gapminder)
