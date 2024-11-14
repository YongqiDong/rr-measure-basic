fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
stopifnot(identical(sleepstudy$Reaction,fm1@resp$y))