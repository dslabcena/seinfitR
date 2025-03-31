#' Glasshouse Experiment Dataset
#'
#' This dataset originates from a greenhouse experiment that assessed the effect of nematode population density on plant yield.
#' One cultivar was used, and 14 different nematode population densities (`p_i`), including zero, were tested.
#' Each density was replicated five times. The dataset provides the nematode densities and the corresponding average plant yield.
#'
#' @format A data frame with 14 rows and 2 columns:
#' \describe{
#'   \item{p_i}{Nematode population density (initial population).}
#'   \item{y}{Average crop yield at the given population density.}
#' }
#'
#' @usage data(glasshouse, package = "seinfitR")
#'
#' @references Schomaker, C., & Been, T. (2013). Plant growth and population dynamics. *Plant Nematology*, 301-330. \doi{10.1079/9781780641515.0301}
"glasshouse"
