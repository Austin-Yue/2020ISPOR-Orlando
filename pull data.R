con <- dbConnect(odbc::odbc(), driver = "MySQL ODBC 5.3 ANSI Driver", 
                 database = "NHWS", uid = "austiny",               
                 pwd = "austiny", 
                 server = "10.176.50.61",
                 port = 3306)

query <-"Select * from num_all_US_2019 where variable IN('DPDX') or variable like 'DPSY%' or variable like 'DPRX%'"

dat <- lapply(query, function(x){dbGetQuery(con, x)})

arrange_and_transpose <- function(dat){
  dat %>% arrange(zKey) %>% spread(variable, numvalue)
}

dat <- lapply(dat, arrange_and_transpose)

dat <- dat[[1]]

save(dat, file = "./2020ISPOR_Orlando_depression.RData")