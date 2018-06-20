#'Practical 4b
#'============

#' Step 1
#' ------
Richard <- function(time){
    list(predictors = list(A = 1, K = 1, B = 1, M = 1, v = 1),
       variables = list(substitute(time)),
       term = function(predLabels, varLabels){
         sprintf("%s + (%s - %s)/((1 + exp(-%s * (%s - %s)))^(1/%s))",
                 predLabels[1], predLabels[2],predLabels[1],
                 predLabels[3], varLabels[1], predLabels[4],
                 predLabels[5])
       })
}
class(Richard)<- "nonlin"

Richard(time)$term(c("A", "K", "B", "M", "v"), c("t"))


#' Step 2
#' ------
dat <- read.table("data/Richard.txt")
plot(y ~ t, data = dat)
mod1 <- gnm(y ~ Richard(t) - 1, data = dat,
            start = c(A = 0, K = 100, B = -0.5, M = 10, v = 1))

#' Step 3
#' ------
plot(y ~ t, data = dat)
lines(fitted(mod1) ~ t, data = dat)
summary(mod1)

mod2 <- update(mod1, constrain = "v", constrainTo = 1)
summary(mod2)

mod3 <- update(mod1, constrain = c("A", "v"), constrainTo = c(0, 1))
summary(mod3)

mod4 <- update(mod1, constrain = c("A", "K", "v"), constrainTo = c(0, 100, 1))
summary(mod4)

#' P.S.
#' ----
#'
#' If assuming data are Gaussian, using nls makes more sense. Need to be more
#' careful with starting values in this example.
mod1nls <- nls(y ~ A + (K - A)/(1 + exp(-B*(t - M)))^(1/v), data = dat,
               start = c(A = 0, K = 100, B = -0.5, M = 10, v = 1))

mod1nls <- nls(y ~ A + (K - A)/(1 + exp(-B*(t - M)))^(1/v), data = dat,
               start = c(A = 0, K = 100, B = -0.1, M = 15, v = 1))
