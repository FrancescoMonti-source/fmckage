process_prescriptions = function(data){

    data = data %>%
        map_dfr(as.data.frame) %>%
        mutate(across(everything(), function(x) as.character(x))) %>%
        select(PATID,EVTID,ELTID,everything())

    return(data)
}
