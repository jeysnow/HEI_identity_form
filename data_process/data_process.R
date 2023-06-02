# Set up functions----------
source("data_process/functions/read_write.R")



# clean data------

# Clean functions
source("data_process/functions/clean_faculty.R")
source("data_process/functions/clean_HEI.R")
source("data_process/functions/clean_program.R")
source("data_process/functions/clean_identity_HEI.R")

# HEI data
read_data(path = "/Data/CENSUP/Raw_Data/HEI/SUP_IES_2019.CSV",
          encoding = "Latin-1") %>%
  HEI_cleaning() %>% 
  assign("HEI", ., envir = .GlobalEnv)  %T>%
  write_data(file_name = "HEI_clean", dir_path = "./clean_data/")

# Faculty data
read_data(path = "/Data/CENSUP/Raw_Data/Faculty/SUP_DOCENTE_2019.CSV",
          encoding = "Latin-1") %>%
  faculty_cleaning() %>% 
  assign("faculty", ., envir = .GlobalEnv)  %T>%
  write_data(file_name = "faculty_clean", dir_path = "./clean_data/")

# Program data
read_data(path = "/Data/CENSUP/Raw_Data/Program/SUP_CURSO_2019.CSV",
          encoding = "Latin-1") %>%
  program_cleaning() %>% 
  assign("program", ., envir = .GlobalEnv)  %T>%
  write_data(file_name = "program_clean", dir_path = "./clean_data/")

# HEI identity data
read_data(path = "/Data/CENSUP/Raw_Data/PDA_Lista_Instituicoes_Ensino_Superior_do_Brasil_EMEC.csv") %>% 
  identity_HEI_cleaning() %>% 
  assign("identity_HEI", ., envir = .GlobalEnv)  %T>%
  write_data(file_name = "identity_clean", dir_path = "./clean_data/")


clean_data <- list()
clean_data$HEI <- HEI
clean_data$program <- program
clean_data$faculty <- faculty
clean_data$identity_HEI <- identity_HEI

rm(HEI, program, faculty, identity_HEI)

rm(faculty_cleaning,
   HEI_cleaning, 
   program_cleaning, 
   identity_HEI_cleaning)
# Merge data----

source("./data_process/functions/merge_faculty_HEI.R")
source("./data_process/functions/merge_program_HEI.R")
source("./data_process/functions/merge_identity_HEI.R")

faculty_merging(clean_data$faculty, clean_data$HEI) %>% 
  program_merging(program_clean = clean_data$program) %>% 
  identity_HEI_merging(identity_clean = clean_data$identity_HEI) %>% 
  assign("merged", ., envir = .GlobalEnv) %T>%
  write_data(file_name = "merged_clean", dir_path = "./clean_data/")

clean_data$merged <- merged

rm(merged, faculty_merging, program_merging, identity_HEI_merging)
