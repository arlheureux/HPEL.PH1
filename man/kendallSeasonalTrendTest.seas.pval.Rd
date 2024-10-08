% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kendall_seas_mod.R
\name{kendallSeasonalTrendTest.seas.pval}
\alias{kendallSeasonalTrendTest.seas.pval}
\title{Fonction qui fait les résultats du mann kendall saisonnier (pareil que dans package EnvStats avec en plus les pval saisonnières)}
\usage{
\method{kendallSeasonalTrendTest}{seas.pval}(
  y,
  season,
  year,
  alternative = "two.sided",
  correct = TRUE,
  Dates,
  ci.slope = TRUE,
  conf.level = 0.95,
  independent.obs = TRUE,
  data.name = NULL,
  season.name = NULL,
  year.name = NULL,
  parent.of.data = NULL,
  subset.expression = NULL,
  ...
)
}
\arguments{
\item{y}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{season}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{year}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{alternative}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{correct}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{Dates}{Vecteur de dates (en plus de EnvStats)}

\item{ci.slope}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{conf.level}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{independent.obs}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{data.name}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{season.name}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{year.name}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{parent.of.data}{cf EnvsStats::kendallSeasonalTrendTest()}

\item{subset.expression}{cf EnvsStats::kendallSeasonalTrendTest()}
}
\value{
Les résultats du mann kendall saisonnier
}
\description{
Fonction qui fait les résultats du mann kendall saisonnier (pareil que dans package EnvStats avec en plus les pval saisonnières)
}
