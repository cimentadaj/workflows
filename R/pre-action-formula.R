#' Add formula terms to a workflow
#'
#' @description
#' - `add_formula()` specifies the terms of the model through the usage of a
#'   formula.
#'
#' - `remove_formula()` removes the formula as well as any downstream objects
#'   that might get created after the formula is used for preprocessing, such as
#'   terms. Additionally, if the model has already been fit, then the fit is
#'   removed.
#'
#' - `update_formula()` first removes the formula, then replaces the previous
#'   formula with the new one. Any model that has already been fit based on this
#'   formula will need to be refit.
#'
#' @details
#' To fit a workflow, one of `add_formula()` or `add_recipe()` _must_ be
#' specified, but not both.
#'
#' @includeRmd man/rmd/add-formula.Rmd details
#'
#' @param x A workflow
#'
#' @param formula A formula specifying the terms of the model. It is advised to
#'   not do preprocessing in the formula, and instead use a recipe if that is
#'   required.
#'
#' @param ... Not used.
#'
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'
#'   If `NULL`, [hardhat::default_formula_blueprint()] is used and is passed
#'   arguments that best align with the model present in the workflow.
#'
#'   Note that preprocessing done here is separate from preprocessing that
#'   might be done by the underlying model. For example, if a blueprint with
#'   `indicators = "none"` is specified, no dummy variables will be created by
#'   hardhat, but if the underlying model requires a formula interface that
#'   internally uses [stats::model.matrix()], factors will still be expanded to
#'   dummy variables by the model.
#'
#' @return
#' `x`, updated with either a new or removed formula preprocessor.
#'
#' @export
#' @examples
#' workflow <- workflow()
#' workflow <- add_formula(workflow, mpg ~ cyl)
#' workflow
#'
#' remove_formula(workflow)
#'
#' update_formula(workflow, mpg ~ disp)
add_formula <- function(x, formula, ..., blueprint = NULL) {
  ellipsis::check_dots_empty()
  action <- new_action_formula(formula, blueprint)
  add_action(x, action, "formula")
}

#' @rdname add_formula
#' @export
remove_formula <- function(x) {
  validate_is_workflow(x)

  if (!has_preprocessor_formula(x)) {
    rlang::warn("The workflow has no formula preprocessor to remove.")
  }

  new_workflow(
    data = x$data,
    pre = new_stage_pre(purge_action_formula(x), mold = x$data),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_formula
#' @export
update_formula <- function(x, formula, ..., blueprint = NULL) {
  ellipsis::check_dots_empty()
  x <- remove_formula(x)
  add_formula(x, formula, blueprint = blueprint)
}

# ------------------------------------------------------------------------------

fit.action_formula <- function(object, workflow) {
  formula <- object$formula
  blueprint <- object$blueprint
  
  # TODO - Strip out the formula environment at some time?
  workflow$pre$mold <- hardhat::mold(formula,
                                     workflow$pre$mold,
                                     blueprint = blueprint)
  
  # All pre steps return the `workflow`
  workflow
}

# ------------------------------------------------------------------------------

check_conflicts.action_formula <- function(action, x) {
  pre <- x$pre

  if (has_action(pre, "recipe")) {
    abort("A formula cannot be added when a recipe already exists.")
  }

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_formula <- function(formula, blueprint) {
  if (!is_formula(formula)) {
    abort("`formula` must be a formula.")
  }

  # `NULL` blueprints are finalized at fit time
  if (!is_null(blueprint) && !is_formula_blueprint(blueprint)) {
    abort("`blueprint` must be a hardhat 'formula_blueprint'.")
  }

  new_action_pre(
    formula = formula,
    blueprint = blueprint,
    subclass = "action_formula"
  )
}

is_formula_blueprint <- function(x) {
  inherits(x, "formula_blueprint")
}
