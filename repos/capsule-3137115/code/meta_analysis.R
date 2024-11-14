# performs meta-analysis of present study and Hardwicke et al. (2018)
# the code below will perform two meta-analyses for the article-level reproducibility outcomes.
# one meta-analysis is for reproducibility outcomes before contacting authors
# the other meta-analysis is for reproducibility outcomes after contacting authors
# we will synthesize data from Hardwicke et al. (2018) with data from the present study
# because both studies had similar goals and designs.

# prepare Hardwicke et al. data

## get the total number of articles
hardwicke2018_totalN <- data_hardwicke2018 %>%
  nrow()

## get the number of successful cases prior to author assistance
hardwicke2018_preAuthorSuccess_n <- data_hardwicke2018 %>%
  count(outcome) %>%
  filter(outcome == "Success") %>%
  pull(n)

## get the number of successful cases after author assistance
hardwicke2018_postAuthorSuccess_n <- data_hardwicke2018 %>%
  count(outcome) %>%
  filter(outcome %in% c("Success", "Success with author assistance")) %>%
  pull(n) %>%
  sum()

# prepare present study data

## get the total number of articles
presentStudy_totalN <- articleLevelSummary %>%
  pull(n) %>%
  sum()

## get the number of successful cases prior to author assistance
presentStudy_preAuthorSuccess_n <- articleLevelSummary %>% 
  filter(finalOutcome == "Reproducible\nwithout author\ninvolvement") %>%
  pull(n)

## get the number of successful cases after author assistance
presentStudy_postAuthorSuccess_n <- articleLevelSummary %>% 
  filter(finalOutcome %in% 
           c("Reproducible\nwithout author\ninvolvement",
             "Reproducible\nwith author\ninvolvement")) %>%
  pull(n) %>%
  sum()

# combine the data from the two studies into data frames...

## ...one for pre-author assistance
ma_data_pre <- data.frame(
  type = c("Before author contact", "Before author contact"),
  study = c("Hardwicke et al. (2018)", "Present study"),
  cases = c(hardwicke2018_preAuthorSuccess_n, presentStudy_preAuthorSuccess_n), 
  total = c(hardwicke2018_totalN,presentStudy_totalN))

## ...one for post-author assistance
ma_data_post <- data.frame(
  type = c("After author contact", "After author contact"),
  study = c("Hardwicke et al. (2018)", "Present study"),
  cases = c(hardwicke2018_postAuthorSuccess_n, presentStudy_postAuthorSuccess_n), 
  total = c(hardwicke2018_totalN, presentStudy_totalN))

## ...and one for both together
ma_data_combined <- rbind(ma_data_pre, ma_data_post)

## run meta-analysis
ma <- metaprop(
  data = ma_data_combined, # specify data frame
  event = cases, # specify column of reproducible cases
  n = total, # specify column of total cases
  studlab = study, # specify column of study identity
  method.ci = 'WSCC', # compute Wilson confidence intervals
  comb.fixed = F, # don't used fixed effects model
  comb.random = T, # use random effects model
  sm = "PRAW", # specify summary measure as raw proportions
  method = "Inverse", # use inverse-variance weighting method
  byvar = type, # specify subgroup
  overall = F, # turn off overall summary statistic
  overall.hetstat = F, # turn of heterogeneity statistics
  print.byvar = F # don't print subgroup label
)


## build forest plot

#pdf(file="forestPlot.pdf", width = 12, height = 6) # only if saving to pdf

forest(ma, # supply meta-analysis object from above
       leftlabs = c("Study", "Reproducible\ncases", "Total\ncases"),
       rightlabs = c("Proportion", "[95% CI]"),
       xlab = 'Proportion',
       xlim = c(0,1),
       col.square = 'black',
       col.diamond = 'black',
       col.by = 'black',
       hetstat = F, # turn off heterogeneity stats
       spacing = 1.25,
       fs.heading = 12.5,
       fontsize = 11,
       weight.study = 'random',
       weight.subgroup = 'weight',
       colgap.forest.left = unit(0.5, 'cm')
)

#dev.off() # only if saving to pdf