# I adapted code from Richard for the one-sample t-test.
# The suggestion to adapt the code from the one-sample to the two-sample t test
#   is in Rouder et al. (2009), p. 234, 2nd paragraph.

# Notation:
# n1 = sample size group 1
# n2 = sample size group 2
# x  = binary predictor   (length n1+n2)
# y  = continuous outcome (length n1+n2)

# Priors:
cauchy.prior <- function(delta, location = 0, scale = sqrt(2)/2) 
{
  dcauchy(delta, location = location, scale = scale) 
}
normal.prior <- function(delta, location = 0, scale  = sqrt(2)/2) 
{
  dnorm  (delta, mean = location, sd = scale) 
}
tstude.prior <- function(delta, location = 0, scale = sqrt(2)/2, df = 1) 
{
  gamma((df+1)/2) * ((df+((delta-location)/scale)^2)/df)^(-((df+1)/2)) / (scale*sqrt(df*pi)*gamma(df/2))
}

# marginal likelihood p(D|H1):
p.y.alt <- function(t.stat, n1, n2, prior.dens, type.H1, point.H1, ...) {
  switch(type.H1, 
         "H1.diff0" = {
           py        <- integrate(function(delta,t.stat,...) dnct(t.stat, n1+n2-2, delta*sqrt(n1*n2/(n1+n2))) * prior.dens(delta, ...),
                                  lower = -Inf, upper = Inf, t.stat = t.stat, ...)[[1]]
           normalize <- integrate(function(delta,...) prior.dens(delta, ...), lower = -Inf, upper = Inf, ...)[[1]]
           py/normalize
         }, 
         "H1.larger0" = {
           py        <- integrate(function(delta,t.stat,...) dnct(t.stat, n1+n2-2, delta*sqrt(n1*n2/(n1+n2))) * prior.dens(delta, ...),
                                  lower = 0, upper = Inf, t.stat = t.stat, ...)[[1]]
           normalize <- integrate(function(delta,...) prior.dens(delta, ...), lower = 0, upper = Inf, ...)[[1]]
           py/normalize
         }, 
         "H1.smaller0" = {
           py        <- integrate(function(delta,t.stat,...) dnct(t.stat, n1+n2-2, delta*sqrt(n1*n2/(n1+n2))) * prior.dens(delta, ...),
                                  lower = -Inf, upper = 0, t.stat = t.stat, ...)[[1]]
           normalize <- integrate(function(delta,...) prior.dens(delta, ...), lower = -Inf, upper = 0, ...)[[1]]
           py/normalize
         }, 
         "H1.point" = {
           dnct(t.stat, n1+n2-2, point.H1*sqrt(n1*n2/(n1+n2)))
         }
  )
} 

# B01 <- function(t.stat, n1, n2, prior.dens, type.H1, point.H1, ...) {
#   dnct(t.stat, n1+n2-2, 0) / p.y.alt(t.stat, n1, n2, prior.dens, type.H1, point.H1, ...)
# }
B01 <- function(t.stat, n1, n2, prior.dens, type.H1, point.H1, ...) {
  den <- p.y.alt(t.stat, n1, n2, prior.dens, type.H1, point.H1, ...)
  switch(type.H1, 
         "H1.diff0"    = dnct(t.stat, n1+n2-2, 0) / den, 
         "H1.larger0"  = p.y.alt(t.stat, n1, n2, prior.dens, "H1.smaller0", point.H1, ...) / den, 
         "H1.smaller0" = p.y.alt(t.stat, n1, n2, prior.dens, "H1.larger0", point.H1, ...) / den, 
         "H1.point"    = dnct(t.stat, n1+n2-2, 0) / den
  )
}

# posterior p(delta|D, H1):
post.dlt.H1 <- function(t.stat, n1, n2, prior.dens, type.H1, point.H1, dlt.supp, ...) {
  dlt.supp.ind <- switch(type.H1, 
                         "H1.diff0"    = 1, 
                         "H1.larger0"  = (dlt.supp > 0), 
                         "H1.smaller0" = (dlt.supp < 0), 
                         "H1.point"    = NULL)
  delta.supp.ind <- function(delta, type.H1) {
    switch(type.H1, 
           "H1.diff0"    = 1, 
           "H1.larger0"  = (delta > 0), 
           "H1.smaller0" = (delta < 0), 
           "H1.point"    = NULL)
  }
  integral.range <- switch(type.H1, 
                           "H1.diff0"    = range(dlt.supp), 
                           "H1.larger0"  = c(0, max(dlt.supp)), 
                           "H1.smaller0" = c(min(dlt.supp), 0), 
                           "H1.point"    = NULL)
  
  if (type.H1 != "H1.point")
  {
    py        <- dlt.supp.ind * dnct(t.stat, n1+n2-2, dlt.supp*sqrt(n1*n2/(n1+n2))) * prior.dens(dlt.supp, ...)
    normalize <- integrate(function(delta) delta.supp.ind(delta, type.H1) * dnct(t.stat, n1+n2-2, delta*sqrt(n1*n2/(n1+n2))) * prior.dens(delta, ...), lower = -Inf, upper = Inf)[[1]]
    post      <- py/normalize
    CI95.LB.f <- function(q)
    {
      integrate(function(delta) delta.supp.ind(delta, type.H1) * dnct(t.stat, n1+n2-2, delta*sqrt(n1*n2/(n1+n2))) * prior.dens(delta, ...) / normalize, lower = -Inf, upper = q)[[1]] - .025
    }
    CI95.LB <- uniroot(CI95.LB.f, integral.range)$root
    CI95.UB.f <- function(q)
    {
      integrate(function(delta) delta.supp.ind(delta, type.H1) * dnct(t.stat, n1+n2-2, delta*sqrt(n1*n2/(n1+n2))) * prior.dens(delta, ...) / normalize, lower = -Inf, upper = q)[[1]] - .975
    }
    CI95.UB <- uniroot(CI95.UB.f, integral.range)$root
  } else
  {
    NULL
  }
  
  if (type.H1 != "H1.point") list(posterior = post, CI95.LB = CI95.LB, CI95.UB = CI95.UB) else NULL
}




