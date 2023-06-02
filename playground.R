

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



multi_analysis$data[,.(logic_pattern = 
                         paste(..logics[
                           as.logical(
                             str_split_1(
                               logic_pattern))]))]


logics[multi_analysis$data$logic_pattern[1] %>%
         str_split_1(", ") %>% 
         as.logical()]


?strsplit_1()
