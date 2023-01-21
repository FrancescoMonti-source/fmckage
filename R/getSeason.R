getSeason <- function(input.date){
    numeric.date <- 100*month(input.date)+day(input.date)
    ## input Seasons upper limits in the form MMDD in the "break =" option:
    cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231))
    # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
    levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
    return(cuts)
}
