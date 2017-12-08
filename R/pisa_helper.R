#' Creates a helpful view from student Pisa data.
#'
#' \code{create_disposition_view} Creates a view for student disposition
#' @param students A data.table modified from student2012
#' @return A data.table prepared to be passed into a GBM.
#' @examples
#' create_disposition_view(student2012)
create_disposition_view <- function(students) {
  require(data.table)
  # Create a list of column names to be included
  #interest_cols <- paste0("ST29Q0", 1:8)
  efficacy_cols <- paste0("ST37Q0", 4:8)
  #disposition_cols <- append(interest_cols, efficacy_cols)
  anxiety_cols <- paste0("ST42Q0", 1:9)
  disposition_cols <- append(efficacy_cols, anxiety_cols)
  perception_cols <- paste0("ST43Q0", 1:6)
  disposition_cols <- append(disposition_cols, perception_cols)
  disposition_cols <- append(c("math_score", "W_FSTUWT"), disposition_cols)

  # Create view and remove NAs
  aus_disp <- na.omit(students[, .SD, .SDcols = disposition_cols])
}

#' Trains a model for student dispositions effects on math scores.
#'
#' \code{train_gbm} Uses a gbm to determine a model for math scores and student dispositon.
#' @param students A data.table modified to contain student disposition columns
#' @return A trained supervised model created from h2o
#' @examples
#' train_gbm(australian_disposition)
train_gbm <- function(students) {
  require(data.table)
  require(h2o)
  require(magrittr)
  h2o.init(nthreads = -1, max_mem_size = "2G")
  h2o.removeAll() ## clean slate - just in case the cluster was already running

  h2o_data <- students %>%
    as.h2o %>%
    h2o.splitFrame(ratios = c(0.8),  #partition data into 80% and 20% chunks
                   destination_frames = c("train", "test"),
                   seed = 1234) %>% #setting a seed will guarantee reproducibility
    `names<-`(c("train", "test"))

  y <- "math_score"
  x <- setdiff(names(h2o_data$train), c(y, "W_FSTUWT"))  # x is all the cols except price and id
  print(x)

  hyperparams <- list(ntrees = c(50),
                      max_depth = c(2, 5, 10),
                      learn_rate = c(0.01, 0.005, 0.001))

  gbm_grid <- h2o.grid("gbm",
                       y = y,
                       x = x,
                       training_frame = h2o_data$train,
                       grid_id = "gbm_models",
                       nfolds = 5,
                       hyper_params = hyperparams,
                       min_split_improvement = 1e-04,
                       stopping_rounds = 10,
                       col_sample_rate = 0.5,
                       seed = 1234)

  sorted_grid <- h2o.getGrid(grid_id = "gbm_models", sort_by = "mae")

  best_model <- h2o.getModel(sorted_grid@model_ids[[1]])
  best_model
}
