library(dplyr)
library(electionsBR)
library(lubridate)
library(stringr)
library(foreign)
library(tidyr)
library(openxlsx)

# Registry data processing ------------------------------------------------
aea <- read.csv("../../data/raw/aea_trials_electoral.csv") %>%
  filter(EE == 1) %>%
  rename(duplicate = `repeat.`) %>%
  filter(is.na(duplicate)) %>%
  select(-IRB)

aea_supp <- read.csv("../../data/raw/PAP_contents - aea.csv")

aea <- left_join(aea, aea_supp) %>%
  mutate(Outcomes = NA)

egap <- read.csv("../../data/raw/egap_merged.csv")


aea_desc <- aea %>%
  dplyr::select(Treatment:Office, 
                Start = Start.date,
                Registered = First.registered.on,
                IRB,
                Ethics_not_IRB,
                Outcomes) %>%
  mutate(dataset = "AEA",
         Registered = as_date(Registered, format = "%m/%d/%y"))

egap_desc <- egap %>%
  dplyr::select(Treatment:Office,
                Start = Start_of_study,
                registered = ID,
                IRB,
                Ethics_not_IRB,
                Outcomes) %>%
  mutate(dataset = "EGAP",
         Registered = as_date(paste0(substr(registered, 1, 4), "-",
                                     substr(registered, 5, 6), "-",
                                     substr(registered, 7, 8)), 
                              format = "%Y-%m-%d")) %>%
  select(-registered)


trials <- rbind(egap_desc, aea_desc) %>% 
  mutate(region = ifelse(Country %in% c("Benin", "Burkina Faso", "Ghana", "Kenya", "Liberia",
                                        "Malawi", "Sierra Leone", "South Africa", "Uganda", "Senegal", "Tunisia"), "Africa",
                         ifelse(Country %in% c("Canada", "USA", "Dominican Republic", "Mexico"), "North\nAmerica",
                                ifelse(Country %in% c("Brazil", "Colombia", "Argentina", "Chile"), "South\nAmerica",
                                       ifelse(Country %in% c("India", "Philippines", "Afghanistan", "Bangladesh", "Indonesia",
                                                             "Israel, Palestine", "Pakistan", "China"), "Asia", "Europe")))),
         region = ifelse(Country == "", NA, region),
         Treatment = ifelse(grepl(Treatment, pattern = "infor"), "information", as.character(Treatment)),
         Treatment = ifelse(Treatment == "party worker", "other", 
                            ifelse(Treatment == "registration", "voter registration", 
                                   ifelse(Treatment == "candidate level", "candidate-level", 
                                          ifelse(Treatment == "electoral institution", "electoral institutions", Treatment)))),
         trt = str_to_title(Treatment),
         start_date = as.Date(Start, format = "%m/%d/%y"),
         yr = year(start_date),
         US = ifelse(Country == "USA", "US", "Outside US"))

save(trials, file = "../../results/pap_data.Rdata")



# Colorado simulation -----------------------------------------------------

r2010 <- read.xlsx("../../data/raw/2010GeneralPrecinctResults.xlsx")
r2012 <- read.xlsx("../../data/raw/2012GeneralPrecinctLevelResults.xlsx")
r2014 <- read.xlsx("../../data/raw/2014GeneralPrecinctResults.xlsx")
r2016 <- read.csv("../../data/raw/2016_General_Election_Precinct-Level_Results.csv")
r2018 <- read.xlsx("../../data/raw/2018GEPrecinctLevelResults.xlsx")

names(r2010)[grep(names(r2010), pattern = "Office")] <- "Office"
names(r2012)[grep(names(r2012), pattern = "Office")] <- "Office"
names(r2014)[grep(names(r2014), pattern = "Office")] <- "Office"
names(r2016)[grep(names(r2016), pattern = "Office")] <- "Office"
names(r2018)[grep(names(r2018), pattern = "Office")] <- "Office"


extract_office2010 <- function(data, office, year){
  races <- unique(data$`Office`)[grepl(unique(data$`Office`), pattern = office)]
  out <- filter(data, `Office` %in% races) %>%
    mutate(TotalVotes = PollVotes + MailVotes + EarlyVotes) %>%
    group_by(`Office`, Precinct) %>%
    summarize(Total = sum(TotalVotes),
              REP = ifelse(sum(Party == "REP") == 1, TotalVotes[Party == "REP"], 0),
              DEM = ifelse(sum(Party == "DEM") == 1, TotalVotes[Party == "DEM"], 0)) %>%
    mutate(District = str_remove(string = Office, pattern = "STATE REPRESENTATIVE - DISTRICT "),
           year = year) %>% ungroup() %>%
    select(-Office) 
  return(out)
  
}

extract_office <- function(data, office, year){
  races <- unique(data$`Office`)[grepl(unique(data$`Office`), pattern = office, ignore.case = T)]
  out <- filter(data, `Office` %in% races) %>%
    mutate(TotalVotes = `Candidate.Votes`) %>%
    group_by(`Office`, Precinct) %>%
    summarize(Total = sum(TotalVotes),
              REP = ifelse(sum(Party == "Republican Party") == 1, TotalVotes[Party == "Republican Party"], 0),
              DEM = ifelse(sum(Party == "Democratic Party") == 1, TotalVotes[Party == "Democratic Party"], 0)) %>%
    mutate(District = str_remove(string = Office, pattern = "State Representative - District "),
           year = year) %>% ungroup() %>%
    select(-Office) 
  return(out)
}

out <- rbind(extract_office2010(r2010, "STATE REP", 2010),
             extract_office(r2012, "STATE REP", 2012),
             extract_office(r2014, "STATE REP", 2014),
             extract_office(r2016, "STATE REP", 2016),
             extract_office(r2018, "STATE REP", 2018))

out2 <- extract_office(r2016, "United States Representative", 2016)
# Set up for US House
races <- unique(r2018$`Office`)[grepl(unique(r2018$`Office`), pattern = "United States Representative", ignore.case = T)]
us_h_precincts <- dplyr::filter(r2018, `Office` %in% races)

ush <- us_h_precincts %>% 
  group_by(Office, Precinct) %>%
  summarize() %>%
  mutate(USH_District = str_remove(string = Office, pattern = "United States Representative - District "))


reg1 <- read.xlsx("../../data/raw/partisan_reg_sh.xlsx")
reg2 <- read.xlsx("../../data/raw/partisan_reg_sh_2016_on.xlsx")

sum1 <- select(reg1, Year, District, ADEM, AREP, UDEM, UREP, Atotal, Utotal, TOTAL) %>%
  mutate(dem_marg = (ADEM +UDEM - AREP - UREP)/TOTAL) %>%
  select(Year, District, dem_marg, Atotal, TOTAL)

sum2 <- select(reg2, Year, District, ADEM, AREP, UDEM, UREP, PDEM, PREP, Atotal, Utotal, Ptotal, TOTAL) %>%
  mutate(dem_marg = (ADEM + PDEM + UDEM - AREP - PREP - UREP)/(TOTAL)) %>%
  select(Year, District, dem_marg, Atotal, TOTAL)

reg <- rbind(sum1, sum2)

# aggregate to district level ---------------------------------------------

dist <- group_by(out, District, year) %>%
  summarize(DEM = sum(DEM, na.rm = T),
            REP = sum(REP, na.rm = T),
            total = sum(DEM + REP, na.rm = T)) %>%
  mutate(vote_marg = DEM - REP)

dat <- merge(dist, reg, by.x = c("year", "District"), by.y = c("Year", "District")) %>%
  mutate(vote_prop = vote_marg/TOTAL)

# dataset used for naive predictions
dat <- group_by(dat, District) %>%
  arrange(District, year) %>%
  mutate(lag = lag(vote_prop, 1)) %>%
  ungroup()

# Morris predictions for US house races --- subset those from colorado
indiv <- read.csv("../../data/raw/indiv_seats.csv") %>%
  filter(grepl(Code, pattern = "CO-"))



# Create precinct-level data for bounding

# registered voters as of 2018
reg2018 <- read.csv("../../data/raw/co2018_reg.csv")
# state house results in 2016
sh2016 <- filter(out, year == 2016) 
sh2016p <- merge(sh2016, reg2018, by.x = "Precinct", by.y = "PRECINCT")
sh2016p <- mutate(sh2016p, 
                  ratio = DEM/TOTAL, # dem two-party vote share
                  b1 = ratio * TOTAL,
                  b2 = (1-ratio) * TOTAL)

# us house results in 2016
ush2016p <- merge(out2, reg2018, by.x = "Precinct", by.y = "PRECINCT")
ush2016p <- mutate(ush2016p, 
                   ratio = DEM/TOTAL, # dem two-party vote share
                   b1 = ratio * TOTAL,
                   b2 = (1-ratio) * TOTAL,
                   District = substr(District, start = nchar(District), stop = nchar(District)))

save(dat, indiv, sh2016, sh2016p, ush2016p, file = "../../results/colorado.Rdata")



# Gerber and Green application --------------------------------------------

# loads replication data
gg <- read.dta("../../data/raw/NHrep_household.dta")

# calculates probability of assignment to (any) canvassing intervention 
nc <- sum(gg$PERSONS[gg$MAILGRP == 0 & gg$PERSNGRP == 0 & gg$PHONGOTV == 0 & gg$V98_2 !=99 & gg$V98_1 != 99])
nt <- sum(gg$PERSONS[gg$MAILGRP != 0 | gg$PERSNGRP != 0 | gg$PHONGOTV != 0 & gg$V98_2 !=99 & gg$V98_1 != 99])
pr_t <- nt/(nc + nt)

# loads 1998 election results in CT
res <- read.csv("../../data/raw/ct9698.csv")


length(gg$ID1[gg$MAILGRP == 0 & gg$PERSNGRP == 0 & gg$PHONGOTV == 0 & gg$V98_2 !=99 & gg$V98_1 != 99]) 
length(gg$ID1[gg$MAILGRP != 0 | gg$PERSNGRP != 0 | gg$PHONGOTV != 0 & gg$V98_2 !=99 & gg$V98_1 != 99])

# number of registered voters per office from CT secretary of state data
nd_office <- c(rep(1860750, 6), 310723, rep(1860750/36, 2), rep(1860750/133, 7), 443414, 52517)
maei <- nt/nd_office
maei[8:16] <- NA
res <- res[1:18,] %>% 
  mutate(nd = nd_office,
         psid = Dif/(nd), 
         pred = ifelse(Race == "Governor", .24 *.429, ifelse(Race == "US Senate", .2*.429, NA)), 
         maeid =maei,
         uncontested = ifelse(Second == 0, "Uncontested", "Contested"),
         lb = ifelse(is.na(maei) & nd > nt + nc, pr_t * (nd_office - nt - nc)/nd_office, ifelse(is.na(maei), 0, NA)),
         ub = ifelse(is.na(maei), pr_t, NA)) %>%
  mutate(Race = factor(Race, levels = rev(unique(Race))))

gg_electoral <- select(res, Race, uncontested, pred, psid, maeid)

save(gg_electoral, file = "../../results/gg.Rdata")

# Boas, Hidalgo, and Melo application -------------------------------------

# municipal identifiers dataset
muni_key <- read.csv("../../data/raw/Brazil+Municipal+Identifiers.csv")

# bhm replication data
bhm <- read.csv("../../data/raw/bhm_2019.csv") %>%
  mutate(ibge7_code = codeibge) 

# Registration in 2016 --> downloads data using electionsBR package
vr <- voter_profile(year = 2016)
# aggregate at municipal level for state of Pernambuco
vr_tot <- vr %>%
  group_by(ANO_ELEICAO, SG_UF, CD_MUNICIPIO) %>%
  summarize(tot = sum(QT_ELEITORES_PERFIL, na.rm = T)) %>%
  filter(SG_UF == "PE") %>%
  mutate(tse_code = CD_MUNICIPIO) %>%
  ungroup()

vr_tot1 <- left_join(vr_tot, muni_key)

# Collapse to municipal level
bhm_n <- group_by(bhm, ibge7_code) %>%
  summarize(n = n(),
            nt = n- sum(grepl(group, pattern = "Control")))

# Merge with electoral data
bhm_n <- left_join(bhm_n, vr_tot1)

# Get previous electoral data using electionsBR package
p2004 <- party_mun_zone_local(year = 2004, uf = "PE")
p2008 <- party_mun_zone_local(year = 2008, uf = "PE")
p2012 <- party_mun_zone_local(year = 2012, uf = "PE")
p2016 <- party_mun_zone_local(year = 2016, uf = "PE")

# Note that columns and column names do not align for 2004, 2008, and 2016 with 
#current version of elections BR...

correct04 <- select(p2004, 
             tse_code = DESCRICAO_CARGO,
             cargo = NOME_PARTIDO)
correct04$votes <- unlist(p2004[,27])


correct08 <- select(p2008, 
                    tse_code = DESCRICAO_CARGO,
                    cargo = NOME_PARTIDO)
correct08$votes <- unlist(p2008[,27])


correct12 <- select(p2012, 
                    tse_code = DESCRICAO_CARGO,
                    cargo = NOME_PARTIDO)
correct12$votes <- unlist(p2012[,27])

# The column names are correct here
correct16 <- select(p2016, 
                    tse_code = SIGLA_UE,
                    cargo = DESCRICAO_CARGO,
                    votes = QTDE_VOTOS_NOMINAIS)



get_margins <- function(dat, yr){
  return(filter(dat, cargo == "Prefeito") %>%
           group_by(tse_code) %>%
           arrange(tse_code, desc(votes)) %>%
           mutate(first = votes[1],
                  second = votes[2],
                  second = ifelse(is.na(second), 0, second),
                  ncand = n(),
                  dif = first-second,
                  nvotes = sum(votes)) %>%
           summarize(dif = mean(dif, na.rm = T),
                     nvotes = mean(nvotes, na.rm = T),
                     ncand = mean(ncand)) %>%
           mutate(year = yr))
}



panel <- do.call("rbind",
                 list(get_margins(correct04, 2004), get_margins(correct08, 2008),
                      get_margins(correct12, 2012 ), get_margins(correct16, 2016)))

panel <- left_join(panel, vr_tot)

panel %<>% 
  group_by(tse_code) %>%
  arrange(year) %>% 
  mutate(lead_dif = lead(dif, 1))

mod <- lm(I(lead_dif)/tot ~ I(dif/tot), data = panel)
panel$pred <- c(predict(mod), rep(NA, 184))
e <- quantile(resid(mod), probs = .05)
panel$psid <- panel$pred + e

save(bhm, bhm_n, panel, file = "../../results/bhm.Rdata")

