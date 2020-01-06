#PHQ9 score segment from internet: 
#minimum (0–4), mild (5–9), moderate (10–14), moderately severe (15–19) and severe (20–27)
#PHQ9 score categories in NHWS: 
#None(1), mild (2), moderate (3), moderately severe (4) and severe (5)

con <- dbConnect(odbc::odbc(), driver = "MySQL ODBC 5.3 ANSI Driver", 
                 database = "NHWS", uid = "austiny",               
                 pwd = "austiny", 
                 server = "10.176.50.61",
                 port = 3306)

query <-"Select * from num_all_US_2019 where variable IN('PHQ9SCRESEG', 'DPVAS') or variable like 'DPRX%'"

dat <- lapply(query, function(x){dbGetQuery(con, x)})

arrange_and_transpose <- function(dat){
  dat %>% arrange(zKey) %>% spread(variable, numvalue)
}

dat <- lapply(dat, arrange_and_transpose)

dat <- dat[[1]]

cols <- c('adherence_scale', 'depression_status')
dat1 <- dat %>% 
  dplyr::rename(rx = DPRX,
                abilify =  DPRXAB,
                amitriptyline =  DPRXAM,
                aplenzin =  DPRXAP,
                brintellix =  DPRXBNX,
                bupropion_hcl =  DPRXBRN,
                bupropion_hcl_sr =  DPRXBRS,
                bupropion_xl =  DPRXBRX,
                celexa =  DPRXCX,
                citalopram =  DPRXCT,
                clomipramine_hcl =  DPRXCME,
                cymbalta =  DPRXCY,
                duloxetine =  DPRXDU,
                duloxetine_hcl =  DPRXDXE,
                effexor_xr =  DPRXFX,
                elavil =  DPRXEV,
                emsam =  DPRXES,
                escitalopram_oxalate =  DPRXELM,
                fetzima =  DPRXFZA,
                fluoxetine =  DPRXFL,
                fluvoxamine_maleate =  DPRXFLU,
                lamictal =  DPRXLI,
                lexapro =  DPRXLX,
                oleptro =  DPRXOPO,
                paroxetine =  DPRXPT,
                paxil =  DPRXPX,
                paxil_cr =  DPRXPC,
                pexeva =  DPRXPXV,
                pristiq =  DPRXPQ,
                prozac =  DPRXPZ,
                remeron =  DPRXRM,
                seroquel =  DPRXSQ,
                seroquel_xr =  DPRXSX,
                sertraline =  DPRXSL,
                symbyax =  DPRXSY,
                trazodone =  DPRXTZ,
                venlafaxine =  DPRXVE,
                venlafaxine_hcl_er =  DPRXVEX,
                viibryd_ =  DPRXVI,
                wellbutrin =  DPRXWL,
                wellbutrin_xl =  DPRXWLX,
                zoloft =  DPRXZL,
                zyprexa =  DPRXZY,
                rexulti =  DPRXREX,
                savella =  DPRXSA,
                other =  DPRXOT,
                abilify_mycite =  DPRXAT,
                desvenlafaxine =  DPRXDV,
                quetiapine_fumarate =  DPRXQF,
                depression_status = PHQ9SCRESEG,
                adherence_scale = DPVAS) %>% 
  filter(., depression_status != 1) %>% 
  mutate(depression_status = case_when(depression_status == 2 ~ 'mild',
                                       depression_status == 3 ~ 'moderate',
                                       depression_status == 4 ~ 'moderately severe',
                                       depression_status == 5 ~ 'severe')) %>% 
  mutate(adherence_scale = case_when(adherence_scale == 0 ~ 'take none of the medication',
                                     adherence_scale == 10 ~ 'take 10% of the medication',
                                     adherence_scale == 20 ~ 'take 20% of the medication',
                                     adherence_scale == 30 ~ 'take 30% of the medication',
                                     adherence_scale == 40 ~ 'take 40% of the medication',
                                     adherence_scale == 50 ~ 'take 50% of the medication',
                                     adherence_scale == 60 ~ 'take 60% of the medication',
                                     adherence_scale == 70 ~ 'take 70% of the medication',
                                     adherence_scale == 80 ~ 'take 80% of the medication',
                                     adherence_scale == 90 ~ 'take 90% of the medication',
                                     adherence_scale == 100 ~ 'take all of the medication')) %>% 
  mutate_at(cols, funs(factor(.)))

save(dat1, file = "./2020ISPOR_Orlando_depression.RData")