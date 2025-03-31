#' Jambu Dataset
#'
#' This dataset is based on the results of a pre-modeled raw dataset. The original data was generated using
#' seven repetitions for five initial nematode population densities (`p_i`): 0, 500, 1000, 2500, and 5000.
#' The model parameters `t`, `m`, and `z` obtained from the raw dataset were then used to predict and extend
#' `p_i` values across the range from 0 to 5001.
#'
#' This dataset is used in the `seinfitR` package to study the relationship between nematode populations and plant growth.
#'
#' @format A data frame with 5,002 rows and 2 columns:
#' \describe{
#'   \item{p_i}{Nematode population density (initial population).}
#'   \item{y}{Crop yield, another plant growth parameter, or the ratio of the estimated variable for plant growth at an initial nematode population density.}
#' }
#'
#' @usage data(jambu, package = "seinfitR")
#'
#' @source <https://osf.io/pm94t>
#'
#' @references Silva, M.F., Faccioli, F.C., Honório, A.P. et al. (2024). First report of angular leaf spot in *Acmella oleracea* caused by the foliar nematode *Aphelenchoides pseudobesseyi*. *J Plant Dis Prot*, 131, 1707–1720. \doi{10.1007/s41348-024-00982-2}
"jambu"
