load_analysis_data <-function(){
  data <- read_data("./clean_data/variables.csv")
  
  setkey(data,HEI_code)
  
  data[,c("legal_system","acad_form")] <- 
    lapply(data[,c("legal_system", "acad_form")],
           factor)
  
  if(exists("multi_analysis",envir = .GlobalEnv)){
    .GlobalEnv$multi_analysis$data <- data
  } else{
    .GlobalEnv$multi_analysis <- list()
    .GlobalEnv$multi_analysis$data <- data
  }
}
