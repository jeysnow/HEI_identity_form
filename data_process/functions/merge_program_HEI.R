program_merging <- function(program_clean, HEI_clean){
  check_program_clean(program_clean, origin = "program_merging")
  check_HEI_clean(HEI_clean, origin = "program_merging")
  
  
  # Sum # of students, except 
  
  DT <- program_clean[,
    .(students_total = sum(students_enrolled)),
    by = HEI_code]
  
  # Sum of students per degree type, becomes 5 variables
  
  temp <- program_clean[,
    .(students_bachelor = sum(as.numeric("bachelor" == degree_type)),
      students_license = sum(as.numeric("license" == degree_type)),
      students_technology = sum(as.numeric("technology" == degree_type)),
      students_base_cycle = sum(as.numeric("base cycle" == degree_type)),
      students_certification = sum(as.numeric("certification" == degree_type))),
    by = HEI_code]
  
  
  DT <- DT[temp, on = .(HEI_code = HEI_code)]
  
  
  
  # sum of students per program type
  
  temp <- program_clean[,
    .(students_classroom = sum(as.numeric("classroom" == program_type)),
      students_distance = sum(as.numeric("distance" == program_type))),
    by = HEI_code]
  
  DT <- DT[temp, on = .(HEI_code = HEI_code)]
  
  
  # average percent dist, divided by 100 to make percentages
  
  temp <- program_clean[program_type == "classroom",
                .(dist_load = mean(percent_dist)/100),
                by = HEI_code]
  
  DT <- DT[temp, on = .(HEI_code = HEI_code)]
  
  
  # Different campi for programs. Note that there were no HEIs
  # for whose distance programs were in more than one location.
  
  temp <- program_clean[,
                        .(campi = length(unique(campus_code))),
                        by = HEI_code]
  
  DT <- DT[temp, on = .(HEI_code = HEI_code)]
  
  DT <- DT[HEI_clean, on = .(HEI_code = HEI_code)]
  
  return(DT)
}
