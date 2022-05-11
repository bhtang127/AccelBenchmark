# Dictionary for different algorithms
alg_dict = list(
  raw = function(...) fpiter(...),
  squarem = function(...) squarem(...),
  daarem = function(...) daarem(...),
  qn = function(...) quasi_newton(...),
  pem = function(...) parabolic_em(...),
  nes = function(...) reNesterov(...)
)

# Dictionary for different tasks
task_dict = list(
  poissmix = function(...) poissmix_task(...),
  mvt_mmf = function(...) mvt_mmf_task(...),
  lasso = function(...) lasso_task(...),
  tsne = function(...) tsne_task(...),
  bvs = function(...) bvs_task(...),
  sinkhorn = function(...) sinkhorn_task(...)
)


#' Benchmarking Different Accelerating Methods for Various Fixed-Point Iteration Problems
#'
#' A benchmarking function used to test the performance of different accelerating methods on various fixed-point interation problems.
#'
#' @param task A list or function or string for the problem to be benchmarked. Explained in Details below.
#' @param algorithm An string array of algorithms that will be run in the benchmarking. "raw" for the original algorithm in the task. "squarem" for SQUAREM. "daarem" for DAAREM. "qn" for Quasi-Newton, "pem" for parabolic-EM and "nes" for restart Nesterov.
#' @param ntimes Integer, number of repetitions for the task in benchmarking.
#' @param control A list for control parameters that will be shared to all the algorithm tried.
#' @param control.spec A named list to pass control parameters for specific algorithms. For example \code{list(squarem=listA, qn=listB)} will pass parameters in \code{listA} to SQUAREM and those in \code{listB} to Quasi-Newton.
#' @param verbose A bool value indicating whether to print out informating when the function is running.
#' @param sample_every_time A bool value indicating whether we should create a new task list in every repetition. This is only usable when \code{task} is a function.
#' @param seed An integer for random seed
#' @param store_tasks A bool value indicating whether to store all tasks in the result.
#' @param ... Other arguments required by \code{task} if it is a function.
#'
#' @details The \code{task} argument above indicates the problem to be benchmarked. In the simplest case, it will be a list containing \code{initfn} for parameter initialization function; \code{fixptfn} for fixed-point updating function; \code{objfn} for objective function and other arguments required for \code{fixptfn} and \code{objfn}.
#'          But \code{task} can also be a function taking parameters and return such list.
#'          We also register some character names for specific tasks: "poissmix" for Poisson mixture; "mvt_mmf" for Multivariate t-distribution; "lasso" for LASSO logistic regression; "bvs" for variational Bayes variable selection; "tsne" for tSNE in COIL-20 and "sinkhorn" for Sinkhorn iteration in matrix balancing.
#'
#' @return A list of results in format of "benchmark" class.
#'  \item{result_table}{A dataframe containing necessary information for each repetition.}
#'  \item{all_results}{A list combining all results lists from all methods in all repetitions.}
#'  \item{all_tasks}{A list containing all task lists in every repetition. If \code{store_task} is \code{FALSE} this will be empty.}
#'  \item{task}{A string for task name.}
#'
#' @examples
#' \dontrun{
#' set.seed(54321)
#' benchmark("poissmix", c("raw", "squarem", "daarem", "pem", "qn", "nes"),  ntimes=100)
#' }
#'
#' @importFrom utils modifyList
#'
#' @export benchmark
benchmark = function(
  task="poissmix",
  algorithm=c("raw"),
  ntimes=100,
  control = list(tol=1e-7, convtype="parameter", maxiter=2000),
  control.spec = list(),
  verbose = TRUE,
  sample_every_time = TRUE,
  seed = NULL,
  store_tasks = FALSE,
  ...
){
  # if(class(algorithm) == "character") algorithm = c(algorithm)
  if(inherits(algorithm, "character")) algorithm = c(algorithm)
  # if(class(task) == "character") {
  if(inherits(task, "character")) {
    if(verbose) cat("Task:", task, "Begin\n")
    task_name = task
    task = task_dict[[task]]
  } else{
    task_name = "task"
  }
  sample_ready = FALSE
  # if(class(task) == "function"){
  if(inherits(task, "function")){
    sample_ready = TRUE
    task_func = task
    task = task_func(...)
  }
  result_table = data.frame(
    run_id=NA, method=NA, objval=NA, niter=NA,
    fpevals=NA, convergence=NA, user_time=NA, elapsed_time=NA
  )
  all_results = list(); all_tasks = list()
  if(!is.null(seed))
    set.seed(seed)
  for(it in 1:ntimes){
    if(sample_every_time & sample_ready)
      task = task_func(...)
    if(store_tasks) all_tasks[[it]] = task
    all_results[[it]] = list()
    if(verbose) cat("Repeating ", it, " times\n")
    p0 = task$initfn()
    if(verbose) cat("Processing Algorithm: ")
    for(alg in algorithm){
      if(verbose) cat(alg, "  ")
      control.alg = control
      if(!is.null(control.spec[[alg]]))
        control.alg = modifyList(control.alg, control.spec[[alg]])
      # print(alg)
      # print(control.alg)
      param_alg = list(par=p0, control=control.alg)
      for(parameter in names(task)){
        if(parameter != "initfn"){
          param_alg[[parameter]] = task[[parameter]]
        }
      }
      res = suppressWarnings(try({
        ti = system.time(pfi <- do.call(alg_dict[[alg]], param_alg))[c(1,3)]
        pfi$elapsed_time = ti[2]
        pfi$user_time = ti[1]
        if(is.null(pfi$iter))
          pfi$iter = pfi$fpevals
        result_table = rbind(result_table,
                             c(it, alg, pfi$value.objfn, pfi$iter,
                               pfi$fpevals, pfi$convergence, ti))
      }, TRUE))
      if(inherits(res, "try-error")){
        result_table = rbind(result_table, c(it, alg, NA, NA, NA, FALSE, NA, NA))
        pfi = list()
      }

      all_results[[it]][[alg]] = pfi
    }
    if(verbose) cat("\n")
  }
  res = list(result_table=result_table[-1,], all_results=all_results,
             all_tasks=all_tasks, task=task_name)
  attr(res, "class") = "benchmark"
  res
}

#' Plot Method for \code{benchmark} Class
#'
#' @param x benchmark class output by benchmark function
#' @param start_from A integer indicating which repetition should be plot out. If that has NA values it will search the following repetitions and plot out the first one without NA.
#' @param min_loss A real number indicating the minimum loss that should be subtracted out when doing plots. If NULL, the minimum loss will be subtracted out.
#' @param type A string indicating the x-axis. "iter" for fpevals and "elapsed" for elapsed time.
#' @param xlog A bool value indicating whether x should be plotted in log scale
#' @param ylog A bool value indicating whether y should be plotted in log scale
#' @param eps A small positive real number setting a lower bound for the standardized loss. Useful when doing log scale plots.
#' @param ... Other arguments for plot function.
#'
#' @examples
#' \dontrun{
#' set.seed(54321)
#' out = benchmark("poissmix", c("raw", "squarem", "daarem", "pem", "qn", "nes"),  ntimes=100)
#' plot(out)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line ylab xlab ggtitle scale_x_log10 scale_y_log10 theme_bw
#'
#' @export
plot.benchmark = function(x, start_from=1, min_loss=NULL,
                          type="iter", eps=1e-10,
                          xlog=TRUE, ylog=TRUE, ...){
  all_results = x$all_results
  for(i in start_from:length(all_results)){
    current = all_results[[i]]
    for(alg in names(current)){
      if(any(is.null(current[[alg]]$objfn.track)))
        next
      if(length(current[[alg]]$objfn.track) < 2)
        next
    }
    break
  }
  data = c()
  for(alg in names(current)){
    method = alg
    data_alg = data.frame(method=alg, objfn.track=current[[alg]]$objfn.track[-1])
    if(type == "iter"){
      xaxis = seq(1, current[[alg]]$fpevals, length.out=length(current[[alg]]$objfn.track))[-1]
      data_alg[["xaxis"]] = xaxis
      xname = "fpeval"
    }
    else if(type == "elapsed"){
      xaxis = seq(0, current[[alg]]$elapsed_time, length.out=length(current[[alg]]$objfn.track))[-1]
      data_alg[["xaxis"]] = xaxis
      xname = "elapsed time"
    }
    else{
      xaxis = seq(0, current[[alg]]$user_time, length.out=length(current[[alg]]$objfn.track))[-1]
      data_alg[["xaxis"]] = xaxis
      xname = "user time"
    }
    data = rbind(data, data_alg)
  }
  if(is.null(min_loss)){
    objval = data$objfn.track - min(data$objfn.track, na.rm=TRUE) + eps
    data[["objval"]] = objval
  } else {
    objval = data$objfn.track - min_loss + eps
    data[["objval"]] = objval
  }

  p1 = ggplot(data, aes(x=xaxis, y=objval, color=method)) +
    geom_line() + ylab("Loss") + xlab(xname) +
    ggtitle(paste("Task:", x$task))
  if(xlog) p1 = p1 + scale_x_log10()
  if(ylog) p1 = p1 + scale_y_log10()
  p1 = p1 + theme_bw()
  p1
}


#' Summary Method for \code{benchmark} Class
#'
#' @param object benchmark class output by benchmark function
#' @param rate_tol A value in (0, 1). All repetitions with loss bigger than \eqn{(1+rate_tol) * minimum_loss} would be considered as a failure.
#' @param loss_tol A real value. All repetitions with loss bigger than \code{loss_tol} would be considered as a failure.
#' @param ci_level A value in (0, 1) for confidence level of confidence intervals in the result
#' @param ... Other arguments for summary function.
#'
#' @details If \code{loss_tol} is not \code{NULL}, \code{rate_tol} will not be used.
#'
#' @return A data frame containing useful values illustrating the performance of different algorithms.
#'
#' @examples
#' \dontrun{
#' set.seed(54321)
#' out = benchmark("poissmix", c("raw", "squarem", "daarem", "pem", "qn", "nes"),  ntimes=100)
#' summary(out)
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise arrange
#' @importFrom stats median quantile var
#'
#' @export
summary.benchmark = function(object, rate_tol=0.1, loss_tol=NULL, ci_level=0.95, ...){
  results = object$result_table
  user_time = as.numeric(results$user_time)
  results$user_time = user_time
  elapsed_time = as.numeric(results$elapsed_time)
  results$elapsed_time = elapsed_time
  fpevals = as.numeric(results$fpevals)
  results$fpevals = fpevals
  objval = as.numeric(results$objval)
  results$objval = objval
  convergence = as.logical(results$convergence)
  results$convergence = convergence

  ql = (1-ci_level)/2; qu = 1 - ql

  min_loss = min(results$objval, na.rm=TRUE)
  if(is.null(loss_tol)){
    failed = !results$convergence | is.na(results$objval) | results$objval > (min_loss + rate_tol * abs(min_loss))
    results$failed = failed
  } else {
    failed = !results$convergence | is.na(results$objval) | results$objval > loss_tol
    results$failed = failed
  }
  mean_elapsed_time = c(); method = results$method
  results %>% group_by(method) %>%
    summarise(conv_rate = mean(1-failed, na.rm=TRUE),
              mean_user_time = mean(user_time, na.rm=TRUE),
              mean_elapsed_time = mean(elapsed_time, na.rm=TRUE),
              mean_fpevals = mean(fpevals, na.rm=TRUE),
              median_fpevals = median(fpevals, na.rm=TRUE),
              std_fpevals = sqrt(var(fpevals, na.rm=TRUE)),
              std_user_time = sqrt(var(user_time, na.rm=TRUE)),
              std_elapsed_time = sqrt(var(elapsed_time, na.rm=TRUE)),
              user_time_low = quantile(user_time, ql, na.rm=TRUE),
              user_time_up = quantile(user_time, qu, na.rm=TRUE),
              elapsed_time_low = quantile(elapsed_time, ql, na.rm=TRUE),
              elapsed_time_up = quantile(elapsed_time, qu, na.rm=TRUE),
              fpevals_low = quantile(fpevals, ql, na.rm=TRUE),
              fpevals_up = quantile(fpevals, qu, na.rm=TRUE)) %>%
    arrange(mean_elapsed_time)
}
