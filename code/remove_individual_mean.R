remove_individual_mean <- function(){
  
  dat <- here("data/raw/loss_aversion.csv") %>% 
    read_csv()

  # identify the means to be removed
  means_to_be_removed <- 
    dat %>% 
    # get observation number, report number, and estimation type
    select(obs, i_report, la_type) %>% 
    # filter out aggregate estimation
    filter(la_type != "Aggregate") %>% 
    # group by report number and count the number of occurrences of each
    group_by(i_report) %>% 
    mutate(n = n()) %>% 
    # identify those reporting both individual mean and median, get the mean cases
    filter(n >= 2 & la_type == "Ind. mean") 

 return(anti_join(dat, means_to_be_removed, by = "obs"))

}
