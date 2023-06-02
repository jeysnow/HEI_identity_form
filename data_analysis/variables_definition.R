multi_analysis <-list()

multi_analysis$data <- data.table()

# Variables ready:-----

# Identification: HEI_code, HEI_name

multi_analysis$data <- clean_data$merged[,
  .(#identification
    HEI_code,
    HEI_name,
    
    # Structure categories:    
    fac_qual = doctor,
    sci_work = research,
    acad_manag = service_management,
    spat_dist = campi,
    train_offer = students_technology,
    employment = full_time_nex + full_time_ex,
    
    #identity categories:
    acad_form = academic_status,
    
    #stuff to be derived
    legal_system = administrative_structure,
    communitary = communitary,
    confessional = confessional)]

#Variables to derive:-----

# Identity categories: 

# Ownership (conf, Comm, family/corporate, municipal)
multi_analysis$data[communitary == TRUE,
                   ownership := "communitary"]


multi_analysis$data[confessional == TRUE,
                   ownership := "confessional"]

multi_analysis$data[legal_system == "municipally_owned",
                   ownership := "municipal"]

multi_analysis$data$confessional <- NULL
multi_analysis$data$communitary <- NULL

# Legal system (private, public)

multi_analysis$data[legal_system == "federally_owned" |
    legal_system == "municipally_owned" |
    legal_system == "state_owned",
  legal_system := "public"]

multi_analysis$data[legal_system != "public",
                           legal_system := "private"]


# Mission (tec or prof)
multi_analysis$data$tec_prof <- 
  grepl(" tec| prof" ,clean_data$merged$HEI_name,
        ignore.case = T)



# Logic variables:-----
# , , , Family
# Corporation, , , , multiplicity 

#training (no gvt schools in this census)
multi_analysis$data[acad_form != "university" | 
                     tec_prof == TRUE,
                   logic_train := TRUE]

multi_analysis$data[is.na(logic_train) , logic_train := FALSE]

# Scholarship

multi_analysis$data[acad_form != "faculty", logic_schol := TRUE]

multi_analysis$data[is.na(logic_schol) , logic_schol := FALSE]

#Religious
multi_analysis$data[ ownership == "confessional",
                    logic_relig := TRUE]

multi_analysis$data[is.na(logic_relig) , logic_relig := FALSE]

# Community
multi_analysis$data[ ownership == "communitary",
                    logic_comm := TRUE]

multi_analysis$data[is.na(logic_comm) , logic_comm := FALSE]

# Market
multi_analysis$data[ legal_system == "private" |
                       ownership == "municipal",
                     logic_market := TRUE]

multi_analysis$data[is.na(logic_market) , logic_market := FALSE]

# state
multi_analysis$data[ legal_system == "public", logic_state := TRUE]

multi_analysis$data[is.na(logic_state) , logic_state := FALSE]

# multiplicity
multi_analysis$data[, logic_number := logic_train + logic_schol +
                     logic_relig+ logic_comm + logic_market +
                      logic_state]

write_data(multi_analysis$data,"variables", "./clean_data/")
lapply(multi_analysis$data, class)
