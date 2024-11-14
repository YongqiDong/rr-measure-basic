# Performs pre-processing  

# load data from Hardwicke et al. (2018)
# NB - the OSF code below can be used to download the original data file directly from the authors' OSF repository
# There's no need to run this code as are going to work with a local copy to ensure internal reproducibility (e.g., to protect against the file being lost at the original source)
# osfFile <- osf_retrieve_file("https://osf.io/hvytz/") %>%
#   osf_download(path = 'data/raw/')
data_hardwicke2018 <- read_csv(here("../data", "raw", "Hardwicke2018_data.csv"))

# load data from Kidwell et al. 2016
# NB - the OSF code below can be used to download the original data file directly from the authors' OSF repository
# There's no need to run this code as are going to work with a local copy to ensure internal reproducibility (e.g., to protect against the file being lost at the original source)
# osfFile <- osf_retrieve_file("https://osf.io/u6g7t/") %>%
#   osf_download(path = 'data/raw/')
data_kidwell <- read_csv(here('../data','raw','Kidwell_data.csv'))
data_kidwell_bib <- read_csv(here('../data','raw','Kidwell_bibliographic.csv'))

# load study data
data_values <- read_csv(here('../data', 'raw', 'data_values.csv')) # load data
data_articles <- read_csv(here('../data', 'raw', 'data_articles.csv')) # load data

# add bibligographic info to main Kidwell data
data_kidwell <- data_kidwell %>% rename("Article ID" = `Article ID number`) # rename article ID column to allow merge (below)
data_kidwell <- data_kidwell_bib %>%
  select(`Article ID`, Journal, DOI, Year, Title, Authors) %>% # select relevant bibliographic info
  merge(data_kidwell, by = "Article ID") # add the info

# record total number of values so we can check against this later
totalValues <- nrow(data_values)

# give each row in data_values a unique ID
data_values <- data_values %>%
  mutate(dataID = seq(1,nrow(data_values))) 

# handle eyeball checks

# Values that required eyeball checks (visual comparison) need special attention
# In these cases, the reportedValue column contains either 'fig' if the values were examined in a figure, or a thresholded value, like p < .05

# Firstly, we need to handle two cases (dataIDs 510 and 515) where there was a MATCH eyeball check and the obtained value is reported as a threshold because we had to extract the numbers from SPSS output
# in these cases we know the obtained value was 0.001
data_values <- data_values %>% 
  mutate(obtainedValue = case_when(
    dataID %in% c(510, 515) ~ "0.001", # make manual adjustment for these IDs
    TRUE ~ obtainedValue # otherwise retain obtained value
  ))

# If the eyeball check was TRUE (a match), then we can copy the obtained value to the reported column
# Let's do that now:
data_values <- data_values %>% 
  mutate(reportedValue = case_when(
    eyeballCheck == TRUE ~ obtainedValue, # when eyeball check is TRUE make reported value the same as the obtained value
    TRUE ~ reportedValue # otherwise retain the reported value
  ))

# If the eyeball check was FALSE (no match), then we will use the threshold value as a proxy for the reported value
# We need to make these changes manually:
data_values <- data_values %>% 
  mutate(reportedValue = case_when(
    dataID == 13 ~ "1", # for these data IDs make manual adjustments to reported value
    dataID == 452 ~ "0.01",
    dataID == 461 ~ "0.9",
    dataID == 462 ~ "0.9",
    dataID == 597 ~ "0.01",
    dataID %in% c(772, 786, 788) ~ "0.23",
    TRUE ~ reportedValue)) # otherwise retain reported value

# Now let's change reportedValue and obtainedValue to numeric variables and comparison outcome to a factor
data_values <- data_values %>%
  mutate(
    reportedValue = as.numeric(reportedValue),
    obtainedValue = as.numeric(obtainedValue),
    comparisonOutcome = factor(comparisonOutcome))

# Relabel p-value 'eyeball errors' that were decision errors as decision errors
# First identify cases where >= .05 was reported bu <.05 was obtained
pEyeballDecisionError <- data_values %>%
  filter(valueType == 'p', comparisonOutcome == 'EYEBALL_ERROR',
         reportedValue >= .05, obtainedValue <.05) %>% 
  # leave out these two data IDs because they are from a case (3-10-2014) where the alpha threshold could not be determined (see Vignette 25)
  filter(dataID %notin% c(786,788)) %>%
  pull(dataID)
  
# Now identify cases where < .0 was reported bu <.05 was obtained
pEyeballDecisionError2 <- data_values %>%
  filter(valueType == 'p', comparisonOutcome == 'EYEBALL_ERROR',
         reportedValue < .05, obtainedValue >=.05) %>%
  pull(dataID)

# combine the two types of decision error
pEyeballDecisionError <- c(pEyeballDecisionError,pEyeballDecisionError2)

# now do the relabeling
data_values <- data_values %>%
  mutate(comparisonOutcome = replace(
    comparisonOutcome,dataID %in% pEyeballDecisionError,"DECISION_ERROR"
  ))

# Relabel other 'eyeball errors' as major errors
data_values <- data_values %>%
  mutate(comparisonOutcome =
           fct_collapse(comparisonOutcome, 
             "MAJOR_ERROR" = c("MAJOR_ERROR","EYEBALL_ERROR")))

# change factor order
data_values <- data_values %>%
  mutate(comparisonOutcome = fct_relevel(
    comparisonOutcome, "MATCH","MINOR_ERROR","MAJOR_ERROR"))

# Rename 
data_values <- data_values %>%
  mutate(comparisonOutcome =
           fct_recode(comparisonOutcome, 
                        "MAJOR NUMERICAL DISCREPANCY" = "MAJOR_ERROR",
                        "MINOR NUMERICAL DISCREPANCY" = "MINOR_ERROR"))

# check we still have all of the values we started with
stopifnot(nrow(data_values) == totalValues)

# do some renaming in data_articles for display purposes
data_articles <- data_articles %>%
  mutate(finalOutcome = 
   fct_recode(finalOutcome, 
    "Reproducible\nwithout author\ninvolvement" = 
      "Success without author assistance",
    "Not reproducible\nwith author\ninvolvement" = 
      "Failure despite author assistance",
    "Not reproducible\nwithout author\ninvolvement" = 
      "Failure without author assistance",
    "Reproducible\nwith author\ninvolvement" = 
      "Success with author assistance"))

# create two new columns to store repro outcome before and after author involvement
data_articles <- data_articles %>%
  mutate(reproOutcomeBeforeAssistance = 
    case_when(
      finalOutcome == 
        "Reproducible\nwithout author\ninvolvement" ~ TRUE,
      TRUE ~ FALSE
    ),
    reproOutcomeAfterAssistance = case_when(
      finalOutcome %in% c(
        "Reproducible\nwithout author\ninvolvement",
        "Reproducible\nwith author\ninvolvement"
      ) ~ TRUE,
      TRUE ~ FALSE
    ))

# add in information about which articles were accompanied by analysis scripts

data_articles <- data_articles %>%
  mutate(analysisScript = case_when(
    articleID %in% c("8-12-2014_PS", "2-2-2015_PS", "2-10-2014_PS", "16-9-2014_PS", "8-5-2015_PS", "16-2-2015_PS") ~ TRUE,
    TRUE ~ FALSE
  ))

# save the data 
save(data_values, file = here('../data', 'processed', 'data_values.RData'))
save(data_articles, file = here('../data', 'processed', 'data_articles.RData'))
save(data_kidwell, file = here('../data', 'processed', 'data_kidwell.RData'))
save(data_hardwicke2018, file = here('../data', 'processed', 'data_hardwicke2018.RData'))