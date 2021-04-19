# non-MER Munge Project Setup

library(glamr)
library(usethis)
library(here)



#set up project folders
si_setup() 

#initialize a new R project
project_path <- here("non-MER")

create_project(path = project_path, open = TRUE, rstudio = TRUE)
