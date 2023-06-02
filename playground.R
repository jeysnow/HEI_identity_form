

clean_data <- list()

clean_data$HEI <- read_data("./clean_data/HEI_clean.csv")



clean_data$faculty <- read_data("./clean_data/faculty_clean.csv")
clean_data$program <- read_data("./clean_data/program_clean.csv")
clean_data$identity_HEI <- read_data("./clean_data/identity_clean.csv")
clean_data$merged <- read_data("./clean_data/merged_clean.csv")


clean_data$merged$administrative_structure %>% is.factor()

clean_data$merged$confessional %>% is.booltype()

names(clean_data$merged)

#next step: correct the error in subsetting collumns in j on the lapply 
#call in load clean data, and then go back to variable definition.

clean_data$merged[,lapply(.SD, factor),.SDcols = c("academic_status","administrative_structure")]

clean_data$merged[,c("academic_status","administrative_structure")] <- 
  lapply(clean_data$merged[,c("academic_status","administrative_structure")], factor)

load_clean_data()


temp <- data.table(x=c(1,2),y=c("a","b"))
temp <- temp[,.(
  z= c(3,4)
)]
temp <- clean_data$merged
temp$test <- grepl(" tec| prof" , clean_data$merged$HEI_name, ignore.case = T)
temp[test == TRUE, HEI_name]
warnings()
load_clean_data()
clean_data$merged[HEI_code %in% c(13630, 17613, 18880, 17617, 17614, 20342, 17161,
                    22020, 17969, 21508, 17615, 23291, 17612, 19215, 
                    17616, 15705, 25590, 26575, 21800, 25413, 26909, 
                    26489, 26840, 26990, 25817), .N]


clean_data$merged$HEI_code[100:110]
