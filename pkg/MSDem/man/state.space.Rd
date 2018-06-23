\name{state.space}
\alias{state.space}
\title{
Creating data files for multi-state population projections
}
\description{
\code{state.space} is used to generate the data files needed as input for function \code{msproj} that runs a multi-state population projections.
}
\usage{
state.space(period = c(2010, 2100), by = 5, region = NULL, 
            residence = c("rural", "urban"), 
            sex = c("male", "female"), age = c(0, 100), 
            edu = NULL, dom = "biregional", 
            dom.var = "mrate", int.emi.var = "mrate",
            country = "Country", scen = "SSP2", 
            data.dir = "input_data/")
}

\arguments{
  \item{period}{
a vector with two values used to define the time horizon of the simulation, i.e., start year and end year. By default, simulations start in 2010 and end in 2100.
}
  \item{by}{
a number specifying the time increment for the simulation, and consequently, the definition of the age groups. Defaults to 5, i.e., period lengths of five years and five-year age groups. May also be set to 1, but please note that \code{msproj()} doesn't provide the functionality to deal with period lengths of one year yet.
}
  \item{region}{
an optional vector of region names that has to be entered by the users if they want to do a sub-national simulation (region here stands for any administrative level below the national one, this could be regions, states, districts, municipalities etc.). Should be \code{NULL} (if no regions are used) or a character vector.
}
  \item{residence}{
an optional vector with the two values \code{urban} and \code{rural} used to specify the distinction between urban and rural areas in the model. Should be \code{NULL} or the default character vector.
}
  \item{sex}{
a vector containing the two values \code{male} and \code{female}. The usage of \code{sex} is mandatory in any of the models, and thus, it is part of every state space.
}
  \item{age}{
a vector containing two values: Minimum age and maximum age of the population. Defaults to 0 years and 100 years, respectively. The width of each age category is taken from the \code{by} argument and thus, defaults to 5. In keeping with the structure of standard life tables, two age groups (0 and 1-4) are created internally instead of using one 0-4 age group if five-year age groups are chosen. Just like \code{sex}, it is mandatory to use this variable.
}
  \item{edu}{
an optional vector specifying the number of educational levels to be used in the simulation. Should be \code{NULL} (the default) if education is not used in the simulation or a single number.
}
  \item{dom}{
the type of domestic migration used in the model. Should be either \code{biregional} (the default) or \code{bilateral}. Biregional means that for a given geographical unit, it is not of interest where the migrants come from or where they go to, i.e., there is only one outgoing (to the rest of the country) and one incoming stream (from the rest of the country). When using bilateral migration, the exact origins and destinations of the flows are specified. Migration only makes sense if either \code{region} or \code{residence} or both are used in the model. If this is not the case, no file containing the domestic migration information (see above) will be produced.
}
  \item{dom.var}{
the type of domestic migration data used in the state space. Should be either \code{mrate} if migration rates are to be used or \code{mabs} in the case of absolute migration numbers. Note: At the moment, the model doesn't provide the functionality to work with absolute numbers.
}
  \item{int.emi.var}{
the type of international emigration data used in the state space. Should be either \code{mrate} if rates are to be used for emigration or \code{mabs} in the case of absolute numbers. Immigration doesn't need this specification, as it explicitely assumes total numbers because of the infeasibility of immigration rates. Note: At the moment, the model doesn't provide the functionality to work with absolute numbers for emigration.
}
  \item{country}{
the name of the country the simulation is ran for. This information is used in the output generation of the model (e.g., to annotate plots and tables) and to create file names. If no value is provided by the user, the default (\code{World}) is used.
}
  \item{scen}{
the name of the scenario that is ran. Just like \code{country}, this information is used to create meaningful file names.
}
  \item{data.dir}{
the name of the directory the empty file should be written to. By default, a subfolder called "input_data" is created automatically within the current working directory, and the files are then saved into this folder. If the user wants to use a different directory, he has to specify the whole path instead.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\code{state.space()} returns a list consisting of five elements if domestic migration is considered or four elements otherwise:

\item{state.space}{A data frame containing the state space itself. It contains all the data needed to run the simulation (population, fertility, mortality etc.) for all possible combinations of sex, age, region etc. There is only one exception: Migration data has a different structure and thus, is written into a separate list element.}
\item{variable.definitions}{A two-dimensional vector containing an overview of the variables used in the simulation and the values they may take. This information is partly used in the simulation itself, but also can be seen as metadata that provides useful information about the specific simulation settings.}
\item{mig.dom}{A data frame containing the rates or flows of domestic migration for each possible combination of origin and destination. Only created when domestic migration is part of the model.}
\item{mig.int}{A data frame containing the rates and flows of international migration for each possible combination of origin and destination.}
\item{edu.trans}{An indicator matrix showing the education transitions. The entries are 1 for the transitions that are possible and 0 for the ones that are not. The rows of the matrix correspond to the educational level before the transition, the columns refer to the level after the transition. Currently, only transitions to higher levels are possible, and within a five-year period, it is only possible to reach one of the next two higher levels, e.g., transitions from level 1 to level 2 or from level 1 to level 3 are possible, but going from level 1 to level 4 (or higher) is not.}

The first three or four elements (depending on the consideration of domestic migration, see above) of this list are written to the directory specified by the \code{data.dir} argument. The matrix containing the education transition is not saved to an external file, but the possible transitions are included in the variable definition file.
}
%\references{
%% ~put references to the literature/web site here ~ -> Vignette
%}
\author{
Marcus Wurzer
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{msproj}, \link{fread.msproj}
}
\examples{
## Creating a state space using the default settings:
st.sp <- state.space()
str(st.sp)

##creating the state space for a sub-national projection for 
##India, with education differentials considered and a 
##scenario name given

##vector of ISO codes for the Indian states:
reg <- c("IN.AD", "IN.AP", "IN.AR", "IN.AS", "IN.BR", "IN.CH", 
         "IN.CT", "IN.DD", "IN.DL", "IN.DN", "IN.GA", "IN.GJ", 
         "IN.HP", "IN.HR", "IN.JH", "IN.JK", "IN.KA", "IN.KL", 
         "IN.LD", "IN.MH", "IN.ML", "IN.MN", "IN.MP", "IN.MZ", 
         "IN.NL", "IN.OR", "IN.PB", "IN.PY", "IN.RJ", "IN.SK", 
         "IN.TN", "IN.TR", "IN.UP", "IN.UT", "IN.WB")
st.sp2 <- state.space(region = reg, edu = 6, country = "India", 
                      scen = "AGESR_Const")
str(st.sp2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ state space }% use one of  RShowDoc("KEYWORDS")
