seinfitR <- function(dataset, start){
  nlsLM(
    y ~ ifelse(pi <= t,
               mean(dataset$y[dataset$pi <= t]),
               (mean(dataset$y[dataset$pi <= t]) * m) + (mean(dataset$y[dataset$pi <= t]) * (1 - m) * z^(pi - t))),
    start = list(m = start$m, t = start$t, z = start$z),
    control = nls.lm.control(maxiter = 600 ),
    lower = c(0, 0, 0),
    upper = c(max(dataset$y), max(dataset$pi), 1),
    algorithm = "LM",
    trace = TRUE,
    data = dataset
  )
}
