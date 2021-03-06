% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xtable_goodies.R
\name{xtable.summary.coxph}
\alias{xtable.summary.coxph}
\title{xtable method for summary.coxph}
\usage{
\method{xtable}{summary.coxph}(
  x,
  digits = 3,
  print.coef = FALSE,
  print.se.coef = FALSE,
  print.z = FALSE,
  print.exp.coef = TRUE,
  print.exp.coef.lci = TRUE,
  print.exp.coef.uci = TRUE,
  print.p = TRUE,
  ...
)
}
\arguments{
\item{x}{a \code{summary.coxph} object}

\item{digits}{digits}

\item{print.coef}{print beta?}

\item{print.se.coef}{print se(beta)?}

\item{print.z}{print z?}

\item{print.exp.coef}{print HR}

\item{print.exp.coef.lci}{print HR lower confint}

\item{print.exp.coef.uci}{print HR upper confint}

\item{print.p}{print p?}

\item{...}{other things passed to \code{xtable::xtable}}
}
\description{
xtable method for summary.coxph
}
