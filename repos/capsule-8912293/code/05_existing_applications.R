# This script produces Figures A4-A10 and Table A6

library(xtable)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(foreign)

set.seed(416)


# Boas, Hidalgo, and Melo -------------------------------------------------

load("../../results/bhm.Rdata")
figa4 <- bhm %>% 
  group_by(codeibge) %>%
  mutate(n_sample = n()) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(muni, -n_sample), fill = group))+
  geom_histogram(stat = "count") +
  scale_x_discrete("Municipality (electoral district)", labels = 1:47) +
  ylab("Number of subjects") +
  scale_fill_manual("Treatment", values = c("gray80", "gray40", "gray10"), labels = c("Control", "Financial Management", "Education"))  +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank(),axis.text.x = element_text(angle = 90))

ggsave(figa4, file = "../../results/Figure_A4.pdf", width = 7, height = 4)


figa5 <- ggplot(bhm_n, aes(x = nt/tot)) +
  geom_histogram(col = "white") +
  xlab(expression(paste(MAEI[d], " (assuming E[",a[i](0),"]=0)"))) +
  ylab("Number of districts") + 
  theme_minimal()

ggsave(figa5, file = "../../results/Figure_A5.pdf", width = 7, height = 3)

figa6_a <- ggplot(bhm_n) +
  geom_histogram(aes(x =  nt/tot), col = "white", alpha = .25) +
  geom_histogram(aes(x = 0.5 * nt/tot), col = "white", alpha = .75) +
  xlab(expression(paste("Min. (dark) and max. (light) ", MAEI[d], " varying E[", a[i](0),"]"))) +
  ylab("Number of districts") + 
  theme_minimal()

figa6_b <- ggplot(bhm_n) +
  geom_histogram(aes(x =  nt/tot), col = "white", alpha = .25) +
  geom_histogram(aes(x = nt/tot + .01 * (tot-nt)/tot), col = "white", alpha = .75) +
  xlab(expression(paste(MAEI[d], " under SUTVA (light) or with interference of ", pi[c], "= 0.01 (dark)"))) +
  ylab("Number of districts") + 
  theme_minimal()

ggsave(figa6_a + figa6_b, file = "../../results/Figure_A6.pdf", width = 9, height = 4)



preds <- filter(panel, year == 2012 & tse_code %in% bhm_n$tse_code) 
figa7 <- left_join(bhm_n, preds) %>%
  ggplot(aes(x = 2 * nt/tot, y = psid)) +
  geom_point(cex = 2) +
  geom_abline(intercept = 0, slope = 1, col = "red") + 
  xlab(expression(paste("2 ", MAEI[d]))) +
  ylab(expression(frac(psi[d]))) + 
  theme_minimal() +
  geom_text(x = .015, y = .02, label = "Passes decision rule", cex =2) +
  geom_text(x = .015, y = .01, label = "Does not pass decision rule", cex = 2) 

ggsave(figa7, file = "../../results/Figure_A7.pdf", width = 5, height = 3.5)

results_2016 <- filter(panel, year == 2016 & tse_code %in% bhm_n$tse_code)

figa8 <- left_join(bhm_n, results_2016) %>%
  arrange(dif/tot) %>%
  mutate(margin = 1:n(),
         maei = ifelse(dif < nt, "A", "B")) %>%
  ggplot(aes(y = margin, col = maei, yend = margin, x = nt/tot, xend = dif/tot)) +
  geom_point(aes(x = nt/tot), pch = 15, cex = 2) +
  geom_point(aes(x = dif/tot), pch = 2, cex = 2) +
  geom_segment() +
  scale_color_manual(expression(MAEI[d]), values = c("dark red", "dark blue"), labels = c(expression(paste(">",psi[d])), expression(paste("<", psi[d]))))+
  xlab(expression(paste(MAEI[d], "(squares) vs. margin between top two candidates (triangles)"))) +
  ylab("District (ranked in increasing order of margin)") + 
  theme_minimal()

ggsave(figa8, file = "../../results/Figure_A8.pdf", width = 7, height = 4)



# Gerber and Green --------------------------------------------------------

# loads replication data
gg <- read.dta("../../data/raw/NHrep_household.dta")

# calculates probability of assignment to (any) canvassing intervention 
nc <- sum(gg$PERSONS[gg$MAILGRP == 0 & gg$PERSNGRP == 0 & gg$PHONGOTV == 0 & gg$V98_2 !=99 & gg$V98_1 != 99])
nt <- sum(gg$PERSONS[gg$MAILGRP != 0 | gg$PERSNGRP != 0 | gg$PHONGOTV != 0 & gg$V98_2 !=99 & gg$V98_1 != 99])
pr_t <- nt/(nc + nt)

# calculates plausible range of MAEIs as a function of district size (registered voters)
# see appendix for data sources on counts of registered voters
text3 = data.frame(nd = c(1860750/133), lab = c("CT State House"))
text4 = data.frame(nd = 25000, y = .7, lab = "Pr(Assigned\nto any treatment)")
text5 = data.frame(nd = 15000, y = .35)

figa9_a <- data.frame(nd = seq(from = 0, to = 1/pr_t *nt))  %>%
  mutate(upper = pr_t, 
         lower = ifelse(nd < 52761-nt-nc, 0, pr_t * (nd - (52761-nt-nc))/nd)) %>%
  ggplot(aes(x = nd)) +
  geom_ribbon(alpha = .25, aes(ymin = lower, ymax = upper)) +
  theme_minimal() +
  geom_hline(yintercept  = pr_t, col = "red", lty = 2, alpha = .5) + 
  geom_vline(xintercept = 1860750/133, lty = 3, col = "dark blue") +
  scale_y_continuous(expression(MAEI[d]), lim = c(0,1)) +
  scale_x_continuous(expression(n[d])) +
  geom_text(data = text3, aes(x = nd-500, label = lab), y = 0.85, angle = 90, col = "dark blue") +
  geom_text(data = text5, aes(x = nd, y= y), label = expression(paste("Plausible range of ", MAEI[d]))) +
  geom_text(data = text4, aes(x = nd, y = y, label = lab), col = "red")  


# calculates MAEI as a function of district size
# see appendix for data sources on counts of registered voters
maei_d_gg <- function(nd){ifelse(nd < 1/pr_t *(nt), pr_t, nt/nd)}
text = data.frame(nd = c(52517, 443414, 310723), lab = c("New Haven City", "New Haven County", "CT-3"))
text2 = data.frame(nd = 450000, y = .575, lab = "Pr(Assigned\nto any treatment)")
options(scipen=10000)
figa9_b <- data.frame(nd = seq(from = 31000, to = 500000)) %>%
  ggplot(aes(x = nd)) +
  geom_hline(yintercept  = pr_t, col = "red", lty = 2, alpha = .5) + 
  geom_vline(xintercept = 52517, lty = 3, col = "dark blue") +
  geom_vline(xintercept = 443414, lty = 3, col = "dark blue") +
  geom_vline(xintercept = 310723, lty = 3, col = "dark blue") +
  stat_function(fun = maei_d_gg) +
  scale_y_continuous(expression(MAEI[d]), lim = c(0, 1)) +
  scale_x_continuous(expression(n[d])) +
  geom_text(data = text, aes(x = nd-10000, label = lab), y = 0.85, angle = 90, col = "dark blue") +
  geom_text(data = text2, aes(x = nd, y = y, label = lab), col = "red") + 
  theme_minimal()

ggsave(figa9_a + figa9_b, file = "../../results/Figure_A9.pdf", width = 10, height = 5)


load("../../results/gg.Rdata")

pts <- select(gg_electoral, Race, maeid, pred) %>%
  pivot_longer(cols = c(maeid, pred)) %>%
  mutate(value = ifelse(name == "maeid", 2 * value, value))


figa10 <- ggplot() +
  geom_bar(data = gg_electoral, aes(y = Race, x = psid, alpha = uncontested), stat = "identity") +
  geom_point(data = pts, aes(x =value, y = Race,  pch = name), col = "orchid", cex = 3) +
  scale_shape_manual("Quantity", values = c(15, 5), labels = c(expression(paste("2 ", MAEI[d])), expression(frac(psi[d])))) +
  scale_alpha_manual("Race was...", values = c(1, .5)) +
  xlab(expression(paste(psi[d], " (election outcome)"))) +
  ylab("") +
  theme_minimal()

ggsave(figa10, file = "../../results/Figure_A10.pdf", width = 7, height = 4)

# recreate Mason-Dixon polling data for governor and senate polls --
sen <- c(rep(1, 346), rep(0, 195), rep(NA, 629-346-195))
gov <- c(rep(1, 317), rep(0, 148), rep(NA, 629-317-148))

bs <- function(vec){
  s <- sample(vec, size = 691, replace = T)
  p <- (sum(s == 1, na.rm = T)-sum(s == 0, na.rm = T))/629
}

psi_bar <- c(quantile(replicate(n = 10000, expr= bs(gov)), probs = 0.05),
             quantile(replicate(n = 10000, expr= bs(sen)), probs = 0.05))

print(xtable(data.frame(Race = c("Governor", "Senator"),
                       `Favored party` = c("R", "D"),
                        Survey = "Mason-Dixon PMR (Oct 9-12, 1998)",
                        n = 629,
                        psi_bar),
             type = "latex",
             include.rownames = F), file = "../../results/Table_A6.tex")
