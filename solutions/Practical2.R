#'Practical 2
#'===========

#' Step 1
#' ------
library(gnm)
yaish <- yaish[,,-7]
yaish <- aperm(yaish, c("orig", "dest", "educ"))


#' Step 2
#' ------
library(logmult)
mod1 <- unidiff(yaish)
mod1

#' Step 3
#' ------
plot(mod1, se.type = "se")


#' Step 4
#' ------
mod2 <- gnm(Freq ~ educ*orig + educ*dest +
                Mult(Exp(educ), orig:dest), family = poisson,
            data = yaish, constrain = "[.]educ1")
gamma <- pickCoef(mod2, "[.]educ", value = TRUE)
exp(gamma)


#' Step 5
#' ------
prof <- profile(mod2, pickCoef(mod2, "[.]educ"), trace = TRUE)
plot(prof)


#' Step 6
#' ------
conf <- confint(prof)
plot(mod1, se.type = "se")
segments(1:5+0.1, exp(conf[,1]), 1:5+0.1, exp(conf[,2]), col = "red")


