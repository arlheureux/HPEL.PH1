% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TS.agreg.UT.R.r
\name{TS.agreg.UT}
\alias{TS.agreg.UT}
\title{Fonction mère de l'agrégation temporelles par moyenne normale des UT}
\usage{
TS.agreg.UT(
  Data_TS,
  agg.func,
  year.min,
  year.max,
  season,
  nb.cores = parallel::detectCores()/2
)
}
\arguments{
\item{Data_TS}{Data : must have columns :
\itemize{
 \item Year : integer for year
 \item Month : interger for season (could be day, week, month etc)
 \item Site : Sites at which data is available
 \item Param : Parameter for which data is available
 \item Val : value of parameter at site at year y at month m
 \item Latitude : coordinates in decimal
 \item Longitude : coordinates in decimal
}}

\item{agg.func}{Texte : fonction pour aggréger}

\item{year.min}{Numérique : Année min}

\item{year.max}{Numérique : Année max}

\item{season}{Numérique : Numéro de saisons. Par défaut 1:12}

\item{nb.cores}{Numérique : Nombre de coeurs sur lesquels paralléliser}
}
\value{
Les données agrégées
}
\description{
Fonction mère de l'agrégation temporelles par moyenne normale des UT
}
\examples{
TS.agreg.UT(Data_TS = data, agg.func = "mean", year.min = 1980, year.max = 2020, season = 1:12, nb.cores = 1)
}
