% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inspect_apsimx_json.R
\name{inspect_apsimx}
\alias{inspect_apsimx}
\title{Inspect an .apsimx (JSON) file}
\usage{
inspect_apsimx(
  file = "",
  src.dir = ".",
  node = c("Clock", "Weather", "Soil", "SurfaceOrganicMatter", "MicroClimate", "Crop",
    "Manager", "Report", "Operations", "Other"),
  soil.child = c("Metadata", "Water", "InitialWater", "Chemical", "Physical", "Analysis",
    "SoilWater", "InitialN", "CERESSoilTemperature", "Sample", "Nutrient", "Organic"),
  parm = NULL,
  digits = 3,
  print.path = FALSE,
  root
)
}
\arguments{
\item{file}{file ending in .apsimx to be inspected (JSON)}

\item{src.dir}{directory containing the .apsimx file to be inspected; defaults to the current working directory}

\item{node}{specific node to be inspected either \sQuote{Clock}, \sQuote{Weather}, 
\sQuote{Soil}, \sQuote{SurfaceOrganicMatter}, \sQuote{MicroClimate}, \sQuote{Crop},
 \sQuote{Manager}, \sQuote{Operations} or \sQuote{Other}}

\item{soil.child}{specific soil component to be inspected. The options vary depending on what is available (see details)}

\item{parm}{parameter to refine the inspection of the \sQuote{manager} list(\sQuote{parm},\sQuote{position}), use \sQuote{NA} for all the positions. \sQuote{parm} can be a regular expression for partial matching.}

\item{digits}{number of decimals to print (default 3). Not used now because everything is a character.}

\item{print.path}{whether to print the path to the specific parameter. Useful to give the later editing. (Also returned as \sQuote{invisible})}

\item{root}{root node label. In simulation structures such as factorials there will be multiple possible nodes. This can be specified by supplying an appropriate character.}
}
\value{
prints a table with inspected parameters and values (and \sQuote{parm path} when \sQuote{print.path} = TRUE).
}
\description{
inspect a JSON apsimx file. It does not replace the GUI, but it can save time by quickly checking parameters and values.
}
\details{
In general, this function is used to edit one parameter at a time. There are some exceptions. \cr

 - For the Clock, both the \sQuote{Start} and \sQuote{End} can be edited in one call.

This is simply a script that prints the relevant parameters which are likely to need editing. It does not print all information from an .apsimx file.
         To investigate the available \sQuote{soil.childs} specify \sQuote{Soil} for \sQuote{node} and do not specify the \sQuote{soil.child}.
}
\examples{
\donttest{
extd.dir <- system.file("extdata", package = "apsimx")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Clock") 
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Weather")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "Metadata") 
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "Physical") 
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "SoilWater") 
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "Organic")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "Chemical")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "InitialWater")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Soil", soil.child = "InitialN")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "SurfaceOrganicMatter")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "MicroClimate")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Crop")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Manager")
inspect_apsimx("Wheat.apsimx", src.dir = extd.dir, node = "Report")

## Manager folder present
extd.dir <- system.file("extdata", package = "apsimx")
inspect_apsimx("maize-manager-folder.apsimx", node = "Other", src.dir = extd.dir,
               parm = list("Manager", "Fertiliser", "Amount"))
               
}

}
