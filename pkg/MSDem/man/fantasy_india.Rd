\name{fantasy_india}
\alias{fantasy_india}
\docType{data}
\title{
A dataset for a fantasy India consisting of just two states.
}
\description{
The dataset is used in the examples section of \code{msproj()}. It includes all the 
necessary information to run a multi-state population projection for the two Indian 
states Andaman and Nicobar Islands (ISO code: IN.AN) and Andhra Pradesh (ISO code: IN.AP).
The dataset is limited to these two states because this keeps the file small and the 
time needed to run the model short.
}
\usage{data("fantasy_india")}
\format{
  A list with the following 4 components.
  \describe{
    \item{\code{India_AGESR_Baseline_mig_dom}}{a data table containing domestic migration data}
    \item{\code{India_AGESR_Baseline_mig_int}}{a data table containing (mock) international migration data}
    \item{\code{India_AGESR_Baseline_state_space}}{a data table containing the state space}
    \item{\code{India_AGESR_Baseline_var_def}}{a data table containing the variable definition}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
K.C., S., Speringer, M., & Wurzer, M. (2017). Population projection by age, sex, and educational attainment in rural and urban regions of 35 provinces of India, 2011-2101: Technical report on projecting the regionally explicit socioeconomic heterogeneity in India. \emph{Laxenburg: International Institute for Applied Systems Analysis.}
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(fantasy_india)
str(fantasy_india)
}
\keyword{datasets}
