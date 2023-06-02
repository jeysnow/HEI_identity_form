read_data<- function(path, encoding = "unknown"){
  check_file(path, origin = "import_data")
  
  DT <- fread(path,header = TRUE, encoding = encoding)
  
  return(DT)
}

write_data<- function(DT,file_name, dir_path){
  check_data_table(DT, origin = "write_data")
  check_directory(dir_path, origin = "write_data")
  
  
  file_path<- paste(dir_path,file_name,".csv",sep = "")
  fwrite(DT,file = file_path)
  
}
