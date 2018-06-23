\name{msproj}
\alias{msproj}
\title{
Running a multi-state population projection
}
\description{
\code{msproj} is used to run multi-state population projections. The dimensions of the simulation must include age and gender, and may be expanded by any combination of education, region, and residence.
}
\usage{
msproj(data = NULL, patt = NULL, country = NULL,
       SSP = "SSP2", fert = NULL, mort = NULL, 
       dom.mig = NULL, recl = NULL, edu = NULL, 
       detail.out = FALSE, nSx = "axmx", iPr = 18, 
       reclass.model = NULL, reclass.period = 5, 
       mig.adj = FALSE, maxeapr5 = c(urban = 0.7, 
       rural = 0.5, total = 0.6))
}

\arguments{
  \item{data}{
a list containing the data needed to run the simulation. It has to include sublists containing the state space, the variable definitions, and (optionally) the flows of migration as defined by the \code{state.space} function. Defaults to NULL.
}
  \item{patt}{
the pattern of the model to be run. Used to create meaningful file names (especially when detailed output is generated). Defaults to NULL.
}
  \item{country}{
the name of the country (or more general, the highest level in the hierarchy of geographic units) the projections are done for. Used to annotate plots and tables that are created automatically when argument \code{detail.out} is set to \code{TRUE}. Defaults to NULL. If no country is specified, the variable definition file is checked for an entry. If no country is given there, too, the generic name \code{country} will be used.
}
  \item{SSP}{
the Shared Socioeconomic Pathway that forms the basis for the simulation assumptions concerning fertility, mortality, domestic migration, reclassification, and education. Defaults to SSP2 (middle of the road scenario). Not used at the moment.
}
  \item{fert}{
an optional vector specifying changes in the fertility assumptions for the whole simulation horizon. Should be \code{NULL} (the default) if the middle of the road fertility assumptions of SSP2 are used, a vector of length \code{iPr}, i.e., corresponding to the number of simulation periods (e.g., years or 5-year-periods) or a single number that is than replicated \code{iPr} times. The values of these vectors represent the relative changes in comparison to the SSP2 assumptions (see examples). Not used at the moment.
}
  \item{mort}{
an optional vector specifying absolute changes in comparison to the base mortality assumptions. In contrast to \code{fert}, the vector values specify absolute changes in comparison to SSP2 for the life expectancy at age 0. Not used at the moment.
}
  \item{dom.mig}{
an optional vector specifying relative changes in comparison to the base domestic migration assumptions, comparable to \code{fert}. Not used at the moment.
}
  \item{recl}{
an optional vector specifying relative changes in comparison to the base reclassification assumptions, comparable to \code{fert}. Not used at the moment.
}
  \item{edu}{
an optional vector specifying relative changes in comparison to the base educational transition assumptions, comparable to \code{fert}. Not used at the moment.
}
  \item{detail.out}{
logical. Should detailed output be written to the "output_data" directory, which includes frequency tables and PDF's? Defaults to \code{FALSE}.
}
  \item{nSx}{
the information used to compute the nSx's. Defaults to \code{"axmx"}, i.e., the nSx's are computed using ax and mx. Alternatively, the computation can be based on \code{"le0"}.
}
  \item{iPr}{
the number of simulation periods. Must be smaller or equal to the number of periods in the simulation horizon. Defaults to 18 in compliance with the default simulation horizon (90 years = 18 five-year periods).
}
  \item{reclass.model}{
an R object of class "glm" containing the reclassification model. Needed to estimate the proportion of the rural population that is reclassified depending on the current population distribution between urban and rural areas. Defaults to NULL.
}
  \item{reclass.period}{
an integer number specifying the space of time for the reclassification data, i.e., the number of years it takes to observe the specified proportion of reclassification. Defaults to 5.
}
  \item{mig.adj}{
logical. Should the domestic migration rates be adjusted to account for the changing proportions of rural and urban population that occur when the simulation is run? Defaults to \code{FALSE}.
}
  \item{maxeapr5}{
a vector specifying the upper limits for the education transition rate to post-secondary attainment among upper-secondary attained. The total value is used if there is no differentiation between urban and rural areas in the model. In contrast, if residence is considered, separate values may be used for rural and urban areas. The possible values are bounded between 0 and 1.
}
}
\details{
Although the users could generate the list containing the data needed to run the simulation themselves, it is advisable to use function \code{fread.msproj} to read the data sets generated by \code{state.space} since the successful operation of \code{msproj} heavily depends on the structure of the data and the naming of the list elements.
}
\value{
A list with the following 11 components.
  \item{pop}{A contingency table in array respresentation of S3 class \code{c("xtabs", "table")} containing the total population numbers. The dimensions are period, sex and (if included in the model) region and/or residence.}
  \item{births}{A contingency table in array respresentation of S3 class \code{c("xtabs", "table")} containing the number of births. The dimensions are period, sex and (if included in the model) region and/or residence.}
  \item{deaths}{A contingency table in array respresentation of S3 class \code{c("xtabs", "table")} containing the number of deaths. The dimensions are period, sex and (if included in the model) region and/or residence.}
  \item{net.mig}{A contingency table in array respresentation of S3 class \code{c("xtabs", "table")} containing the net domestic migration numbers, i.e., the difference between in-migration and out-migration. Thus, the numbers are negative if the out-migration is higher. The dimensions are period, sex and (if included in the model) region and/or residence.}
  \item{prop.pop}{A table containing the proportion of urban population for each year of the simulation period and (if included in the model) subdivided by region.}
  \item{prop.reclass}{A table containing the proportion of reclassified population (rural to urban) for each year of the simulation period and (if included in the model) subdivided by region.}
  \item{fert}{Relative changes of fertility in comparison to the SSP2 assumptions throughout the simulation horizon (see above for details).}
  \item{mort}{Absolute changes of mortality in comparison to the SSP2 assumptions (see above) throughout the simulation horizon (see above for details).}
  \item{dom.mig}{Relative changes of domestic migration in comparison to the SSP2 assumptions throughout the simulation horizon (see above for details).}
  \item{recl}{Relative changes of reclassification in comparison to the SSP2 assumptions throughout the simulation horizon (see above for details).}
  \item{edu}{Relative changes of educational transition in comparison to the SSP2 assumptions throughout the simulation horizon (see above for details).}
}
\references{
K.C., S., Speringer, M., & Wurzer, M. (2017). Population projection by age, sex, and educational attainment in rural and urban regions of 35 provinces of India, 2011-2101: Technical report on projecting the regionally explicit socioeconomic heterogeneity in India. \emph{Laxenburg: International Institute for Applied Systems Analysis.}
}
\author{
Marcus Wurzer, Samir K.C., & Markus Speringer
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{state.space}, \link{fread.msproj}, \link{fantasy_india}
}
\examples{
## Using Indian data, but only for two states (IN.AN and IN.AP).
data(fantasy_india) #input data files
fantasy_india 

##A full model using age, gender, region, residence and education 
##is run. We use country name "FantasyIndia" since India only
##consists of two regions here:
res <- msproj(data = fantasy_india, reclass.period = 10, 
              country = "FantasyIndia")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ state space }% use one of  RShowDoc("KEYWORDS")
