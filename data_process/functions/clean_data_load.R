load_clean_data <- function(){
  clean_data <- list()
  
  # HEI-----
  clean_data$HEI <- read_data("./clean_data/HEI_clean.csv")
  
  setkey(clean_data$HEI, HEI_code)
  
  clean_data$faculty[,c("administrative_structure",
                        "academic_status")] <- 
      lapply(clean_data$faculty[,c("administrative_structure", 
                                   "academic_status")], factor)
  
  # faculty-----
  clean_data$faculty <- read_data("./clean_data/faculty_clean.csv")
  
  setkey(clean_data$faculty,faculty_code)
  
  
  clean_data$faculty[, c("active", 
                         "employment_type",
                         "qualification")] <- 
    lapply(clean_data$faculty[, c("active",
                                  "employment_type",
                                  "qualification")], factor)
  
  # program-----
  
  clean_data$program <- read_data("./clean_data/program_clean.csv")
  
  setkey(clean_data$program$program_code)
 
  
  clean_data$program[, c("active",
                         "program_type",
                         "degree_type")] <- 
    lapply(clean_data$program[, c("active",
                                  "program_type",
                                  "degree_type")], factor)
  
  # identity ------
  
  clean_data$identity_HEI <- read_data("./clean_data/identity_clean.csv")
  
  setkey(clean_data$identity_HEI,HEI_code)
  
  
  clean_data$identity_HEI[,  c("active",
                               "confessional",
                               "communitary",
                               "filantropic")] <- 
    lapply(clean_data$identity_HEI[,c("active",
                                      "confessional",
                                     "communitary",
                                     "filantropic")], as.booltype)
  
  # merged ------
  
  clean_data$merged <- read_data("./clean_data/merged_clean.csv")
  
  setkey(clean_data$merged, HEI_code)
  
  clean_data$merged[,c("administrative_structure",
                        "academic_status")] <- 
    lapply(clean_data$merged[,c("administrative_structure", 
                                 "academic_status")], factor)
  
  clean_data$merged[,  c("active",
                               "confessional",
                               "communitary",
                               "filantropic")] <- 
    lapply(clean_data$merged[,c("active",
                                      "confessional",
                                      "communitary",
                                      "filantropic")], as.booltype)
  
  
  assign("clean_data",clean_data,envir = .GlobalEnv)
}
