rriskFitdist.GJC <- function (data, distr, method = c("mle", "mme"), start, chisqbreaks,
                              meancount, ...)
{
  
  rriskMLEdistGJC <- function (data, distr, start = NULL, optim.method = "default",
                               lower = -Inf, upper = Inf, custom.optim = NULL, ...)
  {
    #if (distr == "triang")require(mc2d)
    #if (distr == "gompertz")require(eha)
    #if (distr == "gumbel")require(reliaR)
    #if (distr == "gompertz")require(eha)
    #if (distr == "gumbel")require(actuar)
    if (!is.character(distr)) {
      distname <- substring(as.character(match.call()$distr),2)
    }
    else distname <- distr
    ddistname <- paste("d", distname, sep = "")
    if (!exists(ddistname, mode = "function"))
      stop(paste("The ", ddistname, " function must be defined"))
    if (distname == "unif")
      stop("Maximum likelihood estimation is not available for the uniform distribution")
    if (is.vector(data)) {
      cens <- FALSE
      if (!(is.numeric(data) & length(data) > 1))
        stop("data must be a numerical vector of length greater than 1 for non censored data\n            or a dataframe with two columns named left and right and more than one line for censored data")
    }
    else {
      cens <- TRUE
      censdata <- data
      if (!(is.vector(censdata$left) & is.vector(censdata$right) &
            length(censdata[, 1]) > 1))
        stop("data must be a numerical vector of length greater than 1 for non censored data\n        or a dataframe with two columns named left and right and more than one line for censored data")
      pdistname <- paste("p", distname, sep = "")
      if (!exists(pdistname, mode = "function"))
        stop(paste("The ", pdistname, " function must be defined to apply maximum likelihood to censored data"))
    }
    if (cens) {
      lcens <- censdata[is.na(censdata$left), ]$right
      if (any(is.na(lcens)))
        stop("An observation cannot be both right and left censored, coded with two NA values")
      rcens <- censdata[is.na(censdata$right), ]$left
      ncens <- censdata[censdata$left == censdata$right & !is.na(censdata$left) &
                          !is.na(censdata$right), ]$left
      icens <- censdata[censdata$left != censdata$right & !is.na(censdata$left) &
                          !is.na(censdata$right), ]
      data <- c(rcens, lcens, ncens, (icens$left + icens$right)/2)
    }
    if (is.null(start)) {
      if (distname == "norm") {
        n <- length(data)
        sd0 <- sqrt((n - 1)/n) * sd(data)
        mx <- mean(data)
        start <- list(mean = mx, sd = sd0)
      }
      else if (distname == "lnorm") {
        if (any(data <= 0))
          stop("values must be positive to fit a lognormal distribution")
        n <- length(data)
        ldata <- log(data)
        sd0 <- sqrt((n - 1)/n) * sd(ldata)
        ml <- mean(ldata)
        start <- list(meanlog = ml, sdlog = sd0)
      }
      else if (distname == "pois") {
        start <- list(lambda = mean(data))
      }
      else if (distname == "exp") {
        if (any(data < 0))
          stop("values must be positive to fit a exponential distribution")
        start <- list(rate = 1/mean(data))
      }
      else if (distname == "gamma") {
        if (any(data < 0))
          stop("values must be positive to fit a Gamma distribution")
        n <- length(data)
        m <- mean(data)
        v <- (n - 1)/n * var(data)
        start <- list(shape = m^2/v, rate = m/v)
        lower <- c(0,0)+1e-6
        upper <- c(Inf,Inf)
        optim.method <- "L-BFGS-B"
      }
      else if (distname == "nbinom") {
        n <- length(data)
        m <- mean(data)
        v <- (n - 1)/n * var(data)
        size <- if (v > m)
          m^2/(v - m)
        else 100
        start <- list(size = size, mu = m)
      }
      else if (distname == "geom") {
        m <- mean(data)
        prob <- if (m > 0)
          1/(1 + m)
        else 1
        start <- list(prob = prob)
      }
      else if (distname == "t") {
        df.start <- 2 * sd(data)^2/(sd(data)^2 - 1)
        start <- list(df = df.start)
      }
      else if (distname == "beta") {
        if (any(data < 0) | any(data > 1))
          stop("values must be in [0-1] to fit a beta distribution")
        n <- length(data)
        m <- mean(data)
        v <- (n - 1)/n * var(data)
        aux <- m * (1 - m)/v - 1
        start <- list(shape1 = m * aux, shape2 = (1 - m) *
                        aux)
      }
      else if (distname == "weibull") {
        if (any(data < 0))
          stop("values must be positive to fit a Weibull distribution")
        m <- mean(log(data))
        v <- var(log(data))
        shape <- 1.2/sqrt(v)
        scale <- exp(m + 0.572/shape)
        start <- list(shape = shape, scale = scale)
        lower <- c(0,0)+1e-6
        upper <- c(Inf,Inf)
        optim.method <- "L-BFGS-B"
      }
      else if (distname == "logis") {
        n <- length(data)
        m <- mean(data)
        v <- (n - 1)/n * var(data)
        start <- list(location = m, scale = sqrt(3 * v)/pi)
      }
      else if (distname == "gumbel.evd") {
        #https://en.wikipedia.org/wiki/Gumbel_distribution
        n <- length(data)
        m <- mean(data)
        sigStart <- sd(data)*sqrt(6)/pi
        start <- list(loc = m-sigStart*0.57721, scale = sigStart)
        lower <- c(-Inf,1e-12)
        upper <- c(Inf,Inf)
        optim.method <- "L-BFGS-B"
      }
      else if (distname == "chisq") {
        if (any(data < 0))
          stop("values must be positive to fit a Chi-square distribution")
        start <- list(df = mean(data))
      }
      else if (distname == "f") {
        if (any(data < 0))
          stop("values must be positive to fit a F distribution")
        df2.start <- 2 * mean(data)/(mean(data) - 1)
        start <- list(df1 = 3, df2 = df2.start)
      }
      else if (distname == "gompertz.fs") {
        if (any(data < 0))
          stop("values must be positive to fit a Gompertz distribution")
        scale.start <- sd(data) * sqrt(6)/pi
        gamma.const <- 0.577221566
        shape.start <- (scale.start * exp(mean(data)/scale.start +
                                            gamma.const))^(-1)
        rate.start <- 1/mean(data)
        start <- list(shape = shape.start, rate = rate.start)
        lower <- c(-Inf,1e-6)
        upper <- c(Inf,Inf)
        optim.method <- "L-BFGS-B"
      }
      else if (distname == "cauchy") {
        start <- list(location = median(data), scale = IQR(data)/2)
      }
      else stop("Fitting procedure for given distribution is not implemented",
                call. = FALSE)
      if (!is.list(start))
        stop("'start' must be defined as a named list for this distribution")
    }
    vstart <- unlist(start)
    argddistname <- names(formals(ddistname))
    m <- match(names(start), argddistname)
    if (any(is.na(m)))
      stop("'start' must specify names which are arguments to 'distr'")
    if (!cens) {
      if ("log" %in% argddistname) {
        fnobj <- function(par, obs, ddistnam) {
          -sum(do.call(ddistnam, c(list(obs), par, log = TRUE)))
        }
      }
      else {
        fnobj <- function(par, obs, ddistnam) {
          -sum(log(do.call(ddistnam, c(list(obs), par))))
        }
      }
    }
    else {
      argpdistname <- names(formals(pdistname))
      if (("log" %in% argddistname) & ("log.p" %in% argpdistname))
        fnobjcens <- function(par, rcens, lcens, icens, ncens,
                              ddistnam, pdistnam) -sum(do.call(ddistnam, c(list(x = ncens),
                                                                           as.list(par), list(log = TRUE)))) - sum(do.call(pdistnam,
                                                                                                                           c(list(q = lcens), as.list(par), list(log = TRUE)))) -
          sum(do.call(pdistnam, c(list(q = rcens), as.list(par),
                                  list(lower.tail = FALSE), list(log = TRUE)))) -
          sum(log(do.call(pdistnam, c(list(q = icens$right),
                                      as.list(par))) - do.call(pdistnam, c(list(q = icens$left),
                                                                           as.list(par)))))
      else fnobjcens <- function(par, rcens, lcens, icens,
                                 ncens, ddistnam, pdistnam) -sum(log(do.call(ddistnam,
                                                                             c(list(x = ncens), as.list(par))))) - sum(log(do.call(pdistnam,
                                                                                                                                   c(list(q = lcens), as.list(par))))) - sum(log(1 -
                                                                                                                                                                                   do.call(pdistnam, c(list(q = rcens), as.list(par))))) -
          sum(log(do.call(pdistnam, c(list(q = icens$right),
                                      as.list(par))) - do.call(pdistnam, c(list(q = icens$left),
                                                                           as.list(par)))))
    }
    if (optim.method == "default") {
      if (length(vstart) > 1) {
        meth <- "Nelder-Mead"
        if (ddistname == "dtriang")
          meth = "SANN"
      }
      else meth <- "BFGS"
    }
    else meth = optim.method
    if (is.null(custom.optim)) {
      if (!cens) {
        print("Fitting")
        print(c(method=meth))
        print(c(lower=lower))
        print(c(upper=upper))
        print(ddistname)
        print(start)
        
        opttryerror <- try(opt <- optim(par = vstart, fn = fnobj,
                                        obs = data, ddistnam = ddistname, hessian = TRUE,
                                        method = meth, lower = lower, upper = upper,
                                        ...), silent = TRUE)
      }
      else opttryerror <- try(opt <- optim(par = vstart, fn = fnobjcens,
                                           rcens = rcens, lcens = lcens, icens = icens, ncens = ncens,
                                           ddistnam = ddistname, pdistnam = pdistname, hessian = TRUE,
                                           method = meth, lower = lower, upper = upper, ...),
                              silent = TRUE)
      if (inherits(opttryerror, "try-error")) {
        warnings("The function optim encountered an error and stopped")
        return(list(estimate = rep(NA, length(vstart)), convergence = 100,
                    loglik = NA, hessian = NA))
      }
      if (opt$convergence > 0) {
        warnings("The function optim failed to converge, with the error code ",
                 opt$convergence)
        return(list(estimate = rep(NA, length(vstart)), convergence = opt$convergence,
                    loglik = NA, hessian = NA))
      }
      hessian2 <- numDeriv::hessian(func = fnobj,x=opt$par,obs = data, ddistnam = ddistname)
      return(list(estimate = opt$par, convergence = opt$convergence,
                  loglik = -opt$value, hessian = opt$hessian, optim.function = "optim",hessian2=hessian2))
    }
    else {
      print("bin da...")
      if (!cens) {
        opttryerror <- try(opt <- custom.optim(fn = fnobj,
                                               obs = data, ddistnam = ddistname, par = vstart,
                                               ...), silent = TRUE)
      }
      else opttryerror <- try(opt <- custom.optim(fn = fnobjcens,
                                                  rcens = rcens, lcens = lcens, icens = icens, ncens = ncens,
                                                  ddistnam = ddistname, pdistnam = pdistname, par = vstart,
                                                  ...), silent = TRUE)
      if (inherits(opttryerror, "try-error")) {
        print(opttryerror)
        warnings("The customized optimization function encountered an error and stopped")
        return(list(estimate = rep(NA, length(vstart)), convergence = 100,
                    loglik = NA, hessian = NA))
      }
      if (opt$convergence > 0) {
        warnings("The customized optimization function failed to converge, with the error code ",
                 opt$convergence)
        return(list(estimate = rep(NA, length(vstart)), convergence = opt$convergence,
                    loglik = NA, hessian = NA))
      }
      return(list(estimate = opt$par, convergence = opt$convergence,
                  loglik = -opt$value, hessian = opt$hessian, optim.function = custom.optim))
    }
  }
  
  
  
  if (!is.character(distr))
    distname <- substring(as.character(match.call()$distr),2)
  else distname <- distr
  ddistname <- paste("d", distname, sep = "")
  if (!exists(ddistname, mode = "function"))
    stop(paste("The ", ddistname, " function must be defined"))
  pdistname <- paste("p", distname, sep = "")
  if (!exists(pdistname, mode = "function"))
    stop(paste("The ", pdistname, " function must be defined"))
  if (any(method == "mom"))
    warning("the name \"mom\" for matching moments is NO MORE used and is replaced by \"mme\".")
  method <- match.arg(method)
  if (!missing(start) & method == "mme")
    warnings("Starting values for parameters will not be used with matching moments")
  if (!(is.vector(data) & is.numeric(data) & length(data) >
        1))
    stop("data must be a numerical vector of length greater than 1")
  n <- length(data)
  if (method == "mme") {
    estimate <- rriskMMEdist(data, distname)
    sd <- NULL
    loglik <- NULL
    aic <- NULL
    bic <- NULL
    correl <- NULL
  }
  else {
    if (missing(start))
      mle <- rriskMLEdistGJC(data, distname, ...)
    else mle <- rriskMLEdistGJC(data, distname, start, ...)
    if (mle$convergence > 0){
      print(mle)
      stop("the function mle failed to estimate the parameters, \n                with the error code ",
           mle$convergence, "\n")
    }
    estimate <- mle$estimate
    if (!is.null(mle$hessian)) {
      if (all(!is.na(mle$hessian))) {
        print(mle$hessian)
        print(mle$hessian2)
        varcovar <- try(solve(mle$hessian))
        print(varcovar)
        if(class(varcovar)[1]!="try-error"){
          sd <- sqrt(diag(varcovar))
          correl <- cov2cor(varcovar)
        }else
        {
          varcovar <- NA
          sd <- NA
          correl <- NA
        }
      }
      else {
        varcovar <- NA
        sd <- NA
        correl <- NA
      }
    }
    else {
      varcovar <- NA
      sd <- NA
      correl <- NA
    }
    loglik <- mle$loglik
    npar <- length(estimate)
    aic <- -2 * loglik + 2 * npar
    bic <- -2 * loglik + log(n) * npar
  }
  if (is.element(distname, c("binom", "nbinom", "geom", "hyper",
                             "pois")))
    discrete <- TRUE
  else discrete <- FALSE
  if (missing(chisqbreaks)) {
    if (missing(meancount))
      meancount <- round(n/((4 * n)^(2/5)))
    sdata <- sort(data)
    if (length(sdata) > ceiling(1.5 * meancount)) {
      limit <- sdata[meancount]
      sdata <- sdata[sdata > limit]
      chisqbreaks <- limit
    }
    else {
      warnings("The sample is too small to automatically define chisqbreaks")
      chisq <- NULL
      chisqbreaks <- NULL
      chisqpvalue <- NULL
      chisqtable <- NULL
      chisqdf <- NULL
    }
    while (length(sdata) > ceiling(1.5 * meancount)) {
      limit <- sdata[meancount]
      sdata <- sdata[sdata > limit]
      chisqbreaks <- c(chisqbreaks, limit)
    }
  }
  if (!is.null(chisqbreaks)) {
    if (!is.numeric(chisqbreaks))
      stop("chisqbreaks must be a numerical vector defining the cell boundaries")
    nbreaks <- length(chisqbreaks)
    pbreaks <- do.call(pdistname, c(list(q = chisqbreaks),
                                    as.list(estimate)))
    Fobsbreaks <- ecdf(data)(chisqbreaks)
    Fobsunder <- c(0, Fobsbreaks[1:nbreaks - 1])
    punder <- c(0, pbreaks[1:nbreaks - 1])
    if (pbreaks[nbreaks] == 1 & Fobsbreaks[nbreaks] == 1) {
      p <- pbreaks - punder
      Fobs <- Fobsbreaks - Fobsunder
    }
    else {
      p <- c(pbreaks - punder, 1 - pbreaks[nbreaks])
      Fobs <- c(Fobsbreaks - Fobsunder, 1 - Fobsbreaks[nbreaks])
    }
    obscounts <- round(Fobs * n)
    theocounts <- p * n
    chisq <- sum(((obscounts - theocounts)^2)/theocounts)
    chisqdf <- length(obscounts) - 1 - length(estimate)
    if (chisqdf > 0) {
      chisqpvalue <- pchisq(chisq, df = chisqdf, lower.tail = FALSE)
    }
    else chisqpvalue <- NULL
    chisqtable <- as.table(cbind(obscounts, theocounts))
    for (i in 1:length(obscounts) - 1) rownames(chisqtable)[i] <- paste("<=",
                                                                        signif(chisqbreaks[i], digits = 4))
    rownames(chisqtable)[length(obscounts)] <- paste(">",
                                                     signif(chisqbreaks[i], digits = 4))
  }
  if (!discrete) {
    s <- sort(data)
    obspu <- seq(1, n)/n
    obspl <- seq(0, n - 1)/n
    theop <- do.call(pdistname, c(list(q = s), as.list(estimate)))
    ks <- max(pmax(abs(theop - obspu), abs(theop - obspl)))
    Dmod <- ks * (sqrt(n) + 0.12 + 0.11/sqrt(n))
    if (n >= 30)
      kstest <- ifelse(Dmod > 1.358, "rejected", "not rejected")
    else kstest <- NULL
    ad <- -n - sum((2 * (1:n) - 1) * log(theop) + (2 * n +
                                                     1 - 2 * (1:n)) * log(1 - theop))/n
    if ((distname == "norm" | distname == "lnorm") & n >=
        5) {
      a2mod <- ad * (1 + 0.75/n + 2.25/n^2)
      adtest <- ifelse(a2mod > 0.752, "rejected", "not rejected")
    }
    else if (distname == "exp" & n >= 5) {
      a2mod <- ad * (1 + 0.6/n)
      adtest <- ifelse(a2mod > 1.321, "rejected", "not rejected")
    }
    else if (distname == "gamma" & n >= 5) {
      m <- as.list(estimate)$shape
      interp <- approxfun(c(1, 2, 3, 4, 5, 6, 8, 10, 12,
                            15, 20), c(0.786, 0.768, 0.762, 0.759, 0.758,
                                       0.757, 0.755, 0.754, 0.754, 0.754, 0.753), yright = 0.752)
      adtest <- ifelse(ad > interp(m), "rejected", "not rejected")
    }
    else if (distname == "weibull" & n >= 5) {
      a2mod <- ad * (1 + 0.2/sqrt(n))
      adtest <- ifelse(a2mod > 0.757, "rejected", "not rejected")
    }
    else if (distname == "logis" & n >= 5) {
      a2mod <- ad * (1 + 0.25/n)
      adtest <- ifelse(a2mod > 0.66, "rejected", "not rejected")
    }
    else if (distname == "cauchy" & n >= 5) {
      interp <- approxfun(c(5, 8, 10, 12, 15, 20, 25, 30,
                            40, 50, 60, 100), c(1.77, 3.2, 3.77, 4.14, 4.25,
                                                4.05, 3.57, 3.09, 2.48, 2.14, 1.92, 1.52), yright = 1.225)
      adtest <- ifelse(ad > interp(n), "rejected", "not rejected")
    }
    else adtest <- NULL
    if (length(table(data)) != length(data))
      warnings("Kolmogorov-Smirnov and Anderson-Darling statistics may not be correct with ties")
  }
  else {
    ks <- NULL
    kstest <- NULL
    ad <- NULL
    adtest <- NULL
  }
  return(structure(list(estimate = estimate, method = method,
                        sd = sd, cor = correl, loglik = loglik, aic = aic, bic = bic,
                        n = n, data = data, distname = distname, chisq = chisq,
                        chisqbreaks = chisqbreaks, chisqpvalue = chisqpvalue,
                        chisqdf = chisqdf, chisqtable = chisqtable, ad = ad,
                        adtest = adtest, ks = ks, kstest = kstest)))
}
