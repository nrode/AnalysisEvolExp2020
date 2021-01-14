#' @title CI compute
#'
#' @description  Compute 95% confidence interval of a mixed model
#'
#'
#'
#' @param boo01 bootMer object
#'
#' @return
#' @export
#'
#' @examples
#'boo01 <- lme4::bootMer(m2, mySumm, nsim = 2, re.form = NA)
#'bCI.tab(boo01)
#'
bCI_tab <- function(b,ind=length(b$t0), type="perc", conf=0.95) {
btab0 <- t(sapply(as.list(seq(ind)),
                  function(i)
                    boot::boot.ci(b,index=i,conf=conf, type=type)$percent))
btab <- btab0[,4:5]
rownames(btab) <- names(b$t0)
a <- (1 - conf)/2
a <- c(a, 1 - a)
pct <- stats:::format.perc(a, 3)
colnames(btab) <- pct
return(btab)
}
