library(psych)
library(corrr)

# FCC-MS
# Get data from ms_models.R
datadesc <- discr %>% filter(!is.na(yearblt), !is.na(rntbrdn)) %>%
                      select(availability_adv, usage, dis_rel_fcc_ms,
                             RUCC_2013, ru_binary, 
                             hs_r_ls, poverty, ag_65_l, hispanc, black, landarea, popultn, 
                             family, foreign,wrkfrmh, lngcmmt, assstnc, labrfrc, vacant, renters, yearblt, rntbrdn, nontrnt) %>%
                      mutate(availability_adv = availability_adv, 
                             usage = usage, 
                             dis_rel_fcc_ms = dis_rel_fcc_ms,
                             RUCC_2013 = RUCC_2013,
                             ru_binary = ru_binary, 
                             hs_r_ls = hs_r_ls*100, 
                             poverty = poverty*100, 
                             ag_65_l = ag_65_l*100, 
                             hispanc = hispanc*100, 
                             black = black*100, 
                             landarea = landarea, 
                             popultn = popultn,
                             family = family*100, 
                             foreign = foreign*100,
                             wrkfrmh = wrkfrmh*100, 
                             lngcmmt = lngcmmt*100, 
                             assstnc = assstnc*100, 
                             labrfrc = labrfrc*100, 
                             vacant = vacant*100, 
                             renters = renters*100, 
                             yearblt = yearblt,
                             rntbrdn = rntbrdn, 
                             nontrnt = nontrnt*100)

describeBy(datadesc, digits = 2)
describeBy(datadesc, datadesc$ru_binary, digits = 2)

datadesc %>% select(-ru_binary) %>% correlate() %>% network_plot(min_cor = 0.2)

# FCC-ACS --> Josh has to update
# Get data from predictFCC_ACS.R
datadesc_acs <- rf_data_full %>% select(-state, -population) %>%
                                 mutate(availability = availability*100,
                                        subscription = subscription*100,
                                        hs_or_less = hs_or_less*100,
                                        poverty = poverty*100,
                                        age_65_older = age_65_older*100,
                                        hispanic = hispanic*100,
                                        black = black*100,
                                        family = family*100,
                                        foreign = foreign*100)
describeBy(datadesc_acs, digits = 2)
describeBy(datadesc_acs, datadesc_acs$rural, digits = 2)