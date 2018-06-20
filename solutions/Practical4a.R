#'Practical 4a
#'============

#' Step 1
#' ------
count <- with(voting, percentage/100 * total)
yvar <- cbind(count, voting$total - count)


#' Step 2
#' ------
dref <- gnm(yvar ~ -1 + Dref(origin, destination),
            family = binomial, data = voting)
summary(dref)
mosaic(dref, ~ origin + destination)

#' Step 3
#' ------
origin <- as.numeric(as.character(voting$origin))
destination <- as.numeric(as.character(voting$destination))
upward <- origin > destination
drefUpward <- gnm(yvar ~ -1 + Dref(origin, destination,
                                   delta = ~ 1 + upward),
                  family = binomial, data = voting)
DrefWeights(drefUpward)

#' Step 4
#' ------
in1 <- with(voting, origin != 1 & destination == 1)
out1 <- with(voting, origin == 1 & destination != 1)

drefInOut1 <- gnm(yvar ~ -1 + Dref(origin, destination,
                                 delta = ~ 1 + in1 + out1),
                family = binomial, data = voting)

#' Step 5
#' ------
DrefWeights(drefInOut1)
drefOut1<- gnm(yvar ~ -1 + Dref(origin, destination,
                                         delta = ~ 1 + out1),
                        family = binomial, data = voting)
DrefWeights(drefOut1)
anova(dref, drefOut1, test = "Chisq")



