# This script produces Figures A2 and A3; part of Table A2; and Table A3

load("../../results/pap_data.Rdata")

library(patchwork)
library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)

# Figure A2 ---------------------------------------------------------------
first_trials <- group_by(trials, dataset) %>%
       summarize(start = min(Registered))

figa2_a <- trials %>%
  ggplot(aes(x = start_date)) + 
  stat_ecdf() + 
  geom_vline(xintercept = first_trials$start, col = c("#41b6c4","#253494")) + 
  annotate("text", label = c("First AEA registration", "First EGAP registration"),
            x = first_trials$start + c(160,-160), y = .5, col = c("#41b6c4","#253494"), angle = 90) +
  scale_x_date("Start of intervention") + 
  scale_y_continuous("ECDF") + 
  theme_minimal()

figa2_b <- trials %>%
  mutate(lab = substr(dataset, 1, 1)) %>% 
  ggplot(aes(x = start_date, y = Registered, col = lab)) + 
  geom_text(aes(label=lab)) + 
  scale_color_manual(values = c("#41b6c4","#253494")) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = 1, intercept = -365.25, lty = 3) + 
  geom_abline(slope = 1, intercept = 365.25, lty = 3) + 
  scale_x_date("Start of intervention") + 
  scale_y_date("Date of origina pre-registration") + 
  theme_minimal() +
  theme(legend.position = "n")

ggsave(figa2_a + figa2_b, file = "../../results/Figure_A2.pdf", width = 10, height = 4)

# Figure A3 ---------------------------------------------------------------

figa3 <- trials %>%
        group_by(Treatment, US, .drop = FALSE) %>%
        summarize(n = n()) %>%
        group_by(US, .drop = F) %>%
        mutate(prop = n/sum(n),
               n_studies = sum(n),
               label = paste0(US, ", n = ", n_studies)) %>%
        mutate(Treatment = str_to_sentence(Treatment),
               Treatment = str_replace(Treatment, pattern = " ", "\n")) %>%
        ggplot(aes(y = prop, x = reorder(Treatment, n), fill = label)) +
        scale_fill_manual("Location of experiment", values = c("#41b6c4","#253494")) + 
        geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) + 
        scale_y_continuous("Proportion of 131 pre-registered experiments\nin AEA and EGAP registries") +
        scale_x_discrete("Intervention class") + 
        theme_minimal() + 
        theme(legend.position = "bottom") 

ggsave(figa3, file = "../../results/Figure_A3.pdf", width = 9, height = 5)


# Table A2 ----------------------------------------------------------------

# Note that this table contains description of treatment classification in 
# addition to the rates at which these treatments are used. This produces a 
# csv with the rates, which is manually combined with the descriptions. 

trials %<>% mutate(US = factor(US, levels = c("US", "Outside US")))
tab <- round(prop.table(table(trials$Treatment, trials$US), margin = 2), 3) * 100
table_A2 <- data.frame(`Intervention type` = rownames(tab),
                       US = tab[,1],
                       `Outside US` = tab[,2])
write.table(table_A2,
            file = "../../results/Table_A2.csv", sep = ";")
# Table A3 --- proportions ----------------------------------------------------------------

prop.table(table(trials$dataset, !is.na(trials$IRB)), margin = 1)
prop.table(table(trials$dataset, !is.na(trials$Ethics_not_IRB)), margin = 1)

