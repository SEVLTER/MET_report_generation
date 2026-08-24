# This script is for subsetting the hourly MET data in yearly files.

library(tidyverse)

# Working directory should be set using Session -> Set Working Directory. 
# Not hard coded. 
# Better practices suggest your file structure look like this:
# .
# └── Project name/
#   ├── data/
#   │   ├── external
#   │   ├── fake
#   │   ├── interim
#   │   ├── processed
#   │   └── raw
#   ├── docs
#   ├── models
#   └── reports/
#       ├── images
#       └── graphs

# This sets a global theme for all my plots. 
theme_set(theme_bw() +
            theme(
              plot.background = element_blank()
              ,panel.grid.major = element_blank()
              ,panel.grid.minor = element_blank()
              ,panel.background = element_blank()
              ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
            ))


hourly_dat <- read_csv("data_output_files/hourly_raw.csv")
hourly_dat

hourly_dat %>% filter(year==2024) %>% 
  write_csv("data_output_files/hourly_data_2024.csv")

hourly_dat %>% filter(year==2025) %>% 
  write_csv("data_output_files/hourly_data_2025.csv")
