library(dplyr)
library(ggplot2)
library(estimatr)
library(mvtnorm)
library(gridExtra)
library(patchwork)

set.seed(416)

load("../../results/colorado.Rdata")
# Figure 2 ----------------------------------------------------------------

# naive predictive model for state house races
mod <- (lm_robust(vote_prop ~ lag + dem_marg, data = dat, subset = year != 2018))
resid <- resid(lm(vote_prop ~ lag + dem_marg, data = dat, subset = year != 2018))

betas <- rmvnorm(10000, mean = coef(mod), sigma = vcov(mod))
pred <- filter(dat, year == 2018)
X <- model.matrix(~lag + dem_marg, dat = pred)
sims <- function(x){
  psi_hat <- X %*% betas[x,]
  pred_p <- psi_hat + sample(resid, size = 65)
  return(pred_p)
}
dist <- sapply(X = 1:10000, sims)

# 90% predictive intervals
sh_bounds <- data.frame(lb = apply(dist, MAR = 1, FUN = quantile, probs = .05),
                        ub = apply(dist, MAR = 1, FUN = quantile, probs = 0.95),
                        dist = 1:65)

fig2_a <- data.frame(ub = apply(dist, MAR = 1, FUN = quantile, probs = .05),
                lb = apply(dist, MAR = 1, FUN = quantile, probs = 0.95),
                dist = 1:65) %>%
  arrange(lb) %>%
  mutate(rank = 1:65,
         col = ifelse(sign(ub) != sign(lb), 1,  2)) %>% 
  ggplot(aes(x = rank)) +
  geom_errorbar(aes(ymin =lb, ymax = ub, col = as.factor(col)), width = 0) + 
  geom_hline(yintercept = 0, col = "red") +
  scale_color_manual(values = c("gray", "black")) + 
  scale_x_continuous("State House District \n(Ranked by Predicted Democratic Vote Margin)", 
                     breaks = c(0, 20, 40, 60), labels = c("", "", "", "")) +
  scale_y_continuous("90% Predictive Interval: \nDemocratic Vote Margin/Registered Voters") +
  theme_minimal() +
  theme(legend.position = "none")

# use Morris forecast for US house races
crit_vals <- c(qnorm(p = .95, lower.tail = F), qnorm(p = .05, lower.tail = F))

ush_bounds <- data.frame(lb = (indiv$forecast_dem_margin + crit_vals[1] * indiv$forecast_dem_sigma)/100, 
                         ub = (indiv$forecast_dem_margin + crit_vals[2] * indiv$forecast_dem_sigma)/100,
                         district = 1:7) %>%
  arrange(lb)

fig2_b <- data.frame(ub = (indiv$forecast_dem_margin + crit_vals[1] * indiv$forecast_dem_sigma)/100, 
                 lb = (indiv$forecast_dem_margin + crit_vals[2] * indiv$forecast_dem_sigma)/100) %>%
  arrange(lb) %>%
  mutate(rank = 1:7,
         col = ifelse(sign(ub) != sign(lb), 1,  2)) %>% 
  ggplot(aes(x = rank)) +
  geom_errorbar(aes(ymin =lb, ymax = ub, col = as.factor(col)), width = 0) + 
  geom_hline(yintercept = 0, col = "red") +
  scale_color_manual(values = c("gray", "black")) + 
  scale_x_continuous("US House District \n(Ranked by Predicted Democratic Vote Margin)", 
                     breaks = c(0, 4, 8), labels = c("", "", "")) +
  scale_y_continuous("90% Predictive Interval: \nDemocratic Vote Margin/Registered Voters") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(fig2_a + fig2_b, filename = "../../results/Figure_2.pdf", width = 8, height = 4)



# Figure 3 ----------------------------------------------------------------

cols <- c("Best Case"="#fee0d2","Random Sample"="#fc9272", "Worst Case"= "#de2d26")

# If individual randomization, collapse to the district. Use 2016 vote share as a measure
# of E[a_c(0)]. 

sh2016 <- sh2016p %>%
  dplyr::group_by(District) %>%
  dplyr::summarize(D2016 = sum(DEM),
                   R2016 = sum(REP),
                   T2016 = sum(Total),
                   Reg2018 = sum(TOTAL)) %>%
  mutate(marg2016 = (D2016 - R2016)/Reg2018)

sh2016 <- merge(sh2016, sh_bounds, by.x = "District", by.y = "dist")

sh2016 <- mutate(sh2016,
                 max = ifelse(ub < 0, ub, ifelse(lb > 0, lb, 0)),
                 maei2 = max/2, 
                 nrs = abs(maei2 * Reg2018),
                 mb = ifelse(abs(marg2016) < .5, 1- abs(marg2016), abs(marg2016)),
                 rs = nrs/mb,
                 hh = abs(maei2 *Reg2018)*2) %>%
  arrange(lb) %>%
  mutate(order = 1:65)

p1 <- ggplot(sh2016, aes(x = order)) +
  geom_bar(aes(y = hh,fill = "Best Case"), stat = "identity", alpha = 1) +
  geom_bar(aes(y = rs, fill = "Random Sample"), stat = "identity", alpha = 1) + 
  geom_bar(aes(y = nrs, fill = "Worst Case"), stat = "identity", alpha = 1) +
  scale_fill_manual("Subjects comprise...", 
                    values = cols) +
  theme_minimal() +
  geom_vline(xintercept = 23.5, lty = 2)+
  geom_vline(xintercept = 56.5, lty = 2) +
  ylab("Max. # of Treated Subjects:\nIndividual Random Assignment") +
  xlab("State House District\n(Ranked by Democratic Vote Margin)") +
  theme(legend.position = "bottom")


p2 <- ggplot(sh2016, aes(x = order)) +
  geom_bar(aes(y = hh/Reg2018,fill = "Best Case"), stat = "identity", alpha = 1) +
  geom_bar(aes(y = rs/Reg2018, fill = "Random Sample"), stat = "identity", alpha = 1) + 
  geom_bar(aes(y = nrs/Reg2018, fill = "Worst Case"), stat = "identity", alpha = 1) +
  scale_fill_manual("Subjects comprise...", 
                    values = cols) +
  theme_minimal() +
  geom_vline(xintercept = 23.5, lty = 2)+
  geom_vline(xintercept = 56.5, lty = 2) +
  ylab("Max. Share of Treated Subjects:\nIndividual Random Assignment") +
  xlab("State House District,\n(Ranked by Democratic Vote Margin)") +
  theme(legend.position = "none")

ush2016 <- ush2016p  %>%
  dplyr::group_by(District) %>%
  dplyr::summarize(D2016 = sum(DEM),
                   R2016 = sum(REP),
                   T2016 = sum(Total),
                   Reg2018 = sum(TOTAL)) %>%
  mutate(marg2016 = (D2016 - R2016)/Reg2018)

ush2016 <- merge(ush2016, ush_bounds, by.x = "District", by.y = "district")

ush2016 <- mutate(ush2016,
                  max = ifelse(ub < 0, ub, ifelse(lb > 0, lb, 0)),
                  maei2 = max/2, 
                  nrs = abs(maei2 * Reg2018),
                  mb = ifelse(abs(marg2016) < .5, 1- abs(marg2016), abs(marg2016)),
                  rs = nrs/mb,
                  hh = abs(maei2 *Reg2018)*2) %>%
  arrange(lb) %>%
  mutate(order = 1:7)



p3 <- ggplot(ush2016, aes(x = order)) +
  geom_bar(aes(y = hh,fill = "Best Case"), stat = "identity", alpha = 1) +
  geom_bar(aes(y = rs, fill = "Random Sample"), stat = "identity", alpha = 1) + 
  geom_bar(aes(y = nrs, fill = "Worst Case"), stat = "identity", alpha = 1) +
  scale_fill_manual("Subjects comprise...", 
                    values = cols) +
  theme_minimal() +
  geom_vline(xintercept = 2.5, lty = 2)+
  geom_vline(xintercept = 4.5, lty = 2) +
  ylab("Max. # of Treated Subjects:\nIndividual Random Assignment") +
  xlab("US House District\n(Ranked by Democratic Vote Margin)") +
  theme(legend.position = "none")


p4 <- ggplot(ush2016, aes(x = order)) +
  geom_bar(aes(y = hh/Reg2018,fill = "Best Case"), stat = "identity", alpha = 1) +
  geom_bar(aes(y = rs/Reg2018, fill = "Random Sample"), stat = "identity", alpha = 1) + 
  geom_bar(aes(y = nrs/Reg2018, fill = "Worst Case"), stat = "identity", alpha = 1) +
  scale_fill_manual("Subjects comprise...", 
                    values = cols) +
  theme_minimal() +
  geom_vline(xintercept = 2.5, lty = 2)+
  geom_vline(xintercept = 4.5, lty = 2) +
  ylab("Max. Share of Treated Subjects:\nIndividual Random Assignment") +
  xlab("US House District,\n(Ranked by Democratic Vote Margin)") +
  theme(legend.position = "none")


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


fig3 <- grid.arrange(p1 + theme(legend.position = "none"), p2, p3, p4, get_legend(p1), 
                       ncol=2, nrow = 3,
                       layout_matrix = rbind(c(1,2), c(3, 4), c(5, 5)),
                       widths = c(2.7, 2.7), heights = c(3, 3, 0.2))

ggsave(fig3, file = "../../results/Figure_3.pdf", width = 7, height = 8)


# Figure A11 --------------------------------------------------------------



sh_avg_precinct <- group_by(sh2016p, District) %>%
  dplyr::summarize(avg_precinct = mean(Total)/sum(Total),
                   n_precincts = n())


ush_avg_precinct <- group_by(ush2016p, District) %>%
  dplyr::summarize(avg_precinct = mean(Total)/sum(Total),
                   n_precincts = n())



sh_df <- merge(select(sh_bounds, District = dist, lb, ub), sh_avg_precinct) %>%
  mutate(Eclusts = floor(ifelse(sign(lb) != sign(ub), 0,
                        ifelse(lb > 0, lb/avg_precinct, abs(ub)/avg_precinct))),
         share_precincts = Eclusts/n_precincts) %>%
  arrange(lb) %>%
  mutate(order = 1:65) 

ush_df <- merge(select(ush_bounds, District = district, lb, ub), ush_avg_precinct) %>%
  mutate(Eclusts = floor(ifelse(sign(lb) != sign(ub), 0,
                                ifelse(lb > 0, lb/avg_precinct, abs(ub)/avg_precinct))),
         share_precincts = Eclusts/n_precincts) %>%
  arrange(lb) %>%
  mutate(order = 1:7) 

q1 <- ggplot(sh_df, aes(x = order)) +
  geom_bar(aes(y = Eclusts), fill = "#fc9272", stat = "identity", alpha = 1) +
  theme_minimal() +
  geom_vline(xintercept = 23.5, lty = 2)+
  geom_vline(xintercept = 56.5, lty = 2) +
  ylab("E[# of Treated Precincts]:\nClustered Random Assignment") +
  xlab("State House District\n(Ranked by Democratic Vote Margin)") +
  theme(legend.position = "none")

q2 <- ggplot(sh_df, aes(x = order)) +
  geom_bar(aes(y = share_precincts), fill = "#fc9272", stat = "identity", alpha = 1) +
  theme_minimal() +
  geom_vline(xintercept = 23.5, lty = 2)+
  geom_vline(xintercept = 56.5, lty = 2) +
  ylab("E[Proportion of Treated Precincts]:\nClustered Random Assignment") +
  xlab("State House District\n(Ranked by Democratic Vote Margin)") +
  theme(legend.position = "none")

q3 <- ggplot(ush_df, aes(x = order)) +
  geom_bar(aes(y = Eclusts), fill = "#fc9272", stat = "identity", alpha = 1) +
  theme_minimal() +
  geom_vline(xintercept = 2.5, lty = 2)+
  geom_vline(xintercept = 4.5, lty = 2) +
  ylab("E[# of Treated Precincts]:\nClustered Random Assignment") +
  xlab("US House District\n(Ranked by Democratic Vote Margin)") +
  theme(legend.position = "none")

q4 <- ggplot(ush_df, aes(x = order)) +
  geom_bar(aes(y = share_precincts), fill = "#fc9272", stat = "identity", alpha = 1) +
  theme_minimal() +
  geom_vline(xintercept = 2.5, lty = 2)+
  geom_vline(xintercept = 4.5, lty = 2) +
  ylab("E[Proportion of Treated Precincts]:\nClustered Random Assignment") +
  xlab("State House District\n(Ranked by Democratic Vote Margin)") +
  theme(legend.position = "none")


ggsave(q1 + q2 + q3 + q4, file = "../../results/Figure_A11.pdf")
