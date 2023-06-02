identity_HEI_merging <- function(identity_clean, HEI_clean){
  check_identity_HEI_clean(identity_clean)
  check_HEI_clean(HEI_clean)
  
  DT <- HEI_clean[identity_clean, 
                  on = .(HEI_code = HEI_code)][maintainer_code != ""]
  
  return(DT)
}
