## This code reproduces Figure 1. Note that the back-of-the-envelope calculations
## are described in Table A8. The results of those calculations are in studies_maei.csv. 

library(ggplot2)

maei <- read.csv("../../data/raw/studies_maei.csv")

fig1 <- ggplot(maei, aes(y = Study)) +
  geom_point(aes(x = lwr, col = rand_type, shape = type), size = 2) +
  geom_point(aes(x = upr, col = rand_type, shape = type), size = 2) +
  geom_errorbarh(aes(xmin = lwr, xmax = upr, col = rand_type), height = .4, lwd = .9) +
  scale_color_manual("Random\nassignment", values = c(c("#fc9272", "#de2d26"))) +
  scale_shape_manual("Estimate", values = c(16, 17)) +
  xlab(expression(paste("Average ", MAEI[d]))) + 
  scale_y_continuous("Study", breaks = 1:6) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_vline(xintercept = 0.5) + 
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank()) 

ggsave(fig1, filename = "../../results/Figure_1.pdf", width =6, height = 3)
