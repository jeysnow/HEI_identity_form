faculty_merging <- function(faculty_clean, HEI_clean){
  check_faculty_clean(faculty_clean, origin = "faculty_merging")
  check_HEI_clean(HEI_clean, origin = "faculty_merging")

  
  # counting average participation of active teacher 
  # per activity per HEI.
  DT <- faculty_clean[
    active=="active",
    .(teaching_bac_dist = mean(teaching_bac_dist),
      teaching_bac_pres = mean(teaching_bac_pres),
      teaching_post_dist = mean(teaching_post_dist),
      teaching_post_pres = mean(teaching_post_pres),
      service_external = mean(service_external),
      service_management = mean(service_management),
      research          = mean(research),
      research_w_funding = mean(research_w_funding)),
    by = HEI_code]
  
  #Employment becomes 4 variables, % summing 100%
  
  employment <- faculty_clean[
    active=="active",
    .(full_time_ex = mean(as.numeric(("full time exclusive" == employment_type))),
      full_time_nex = mean(as.numeric(("full time non exclusive" == employment_type))),
      part_time = mean(as.numeric("part time" == employment_type)),
      hourly_contract = mean(as.numeric(("hourly contract" == employment_type)))),
    by= HEI_code]
  
  DT <- DT[employment, on = .(HEI_code = HEI_code)]
  rm(employment)
  
  #qualification becomes 5 variables, % summing 100%
  qualification <- faculty_clean[
    active=="active",
    .(doctor = mean(as.numeric(("doctor" == qualification))),
      master = mean(as.numeric("master" == qualification)),
      specialist = mean(as.numeric(("specialist" == qualification))),
      bachelor = mean(as.numeric(("bachelor" == qualification))),
      none = mean(as.numeric(("none" == qualification)))),
    by= HEI_code]
  
  DT <- DT[qualification, on = .(HEI_code = HEI_code)]
  rm(qualification)
  
  # merging the new data with HEI Clean
  
  DT <- HEI_clean[DT, on = .(HEI_code = HEI_code)]
  
  
  
  return(DT)
}
