test_that("can add a split to a workflow", {
  workflow <- workflow()
  workflow <- add_split(workflow, rsample::initial_split)

  expect_is(workflow$pre$actions$split, "action_split")
})

test_that("split is validated", {
  expect_error(add_split(workflow(), 1),
               "`.f` must be a function for splitting the dataset")
})

test_that("remove a split specification", {
  workflow_no_split <- workflow()
  workflow_with_split <- add_split(workflow_no_split, rsample::initial_split)
  workflow_removed_split <- remove_split(workflow_with_split)

  expect_equal(workflow_no_split$pre, workflow_removed_split$pre)
})

# TODO
# After you define a fit method for split, adapt this
# example to remove the fit object after split
## test_that("remove a recipe after model fit", {
##   lm_model <- parsnip::linear_reg()
##   lm_model <- parsnip::set_engine(lm_model, "lm")

##   rec <- recipes::recipe(mpg ~ cyl, mtcars)

##   workflow_no_recipe <- workflow()
##   workflow_no_recipe <- add_model(workflow_no_recipe, lm_model)

##   workflow_with_recipe  <- add_recipe(workflow_no_recipe, rec)
##   workflow_with_recipe <- fit(workflow_with_recipe, data = mtcars)

##   workflow_removed_recipe <- remove_recipe(workflow_with_recipe)

##   expect_equal(workflow_no_recipe$pre, workflow_removed_recipe$pre)
## })

test_that("update a split specification", {
  workflow <- workflow()
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- update_split(workflow, rsample::initial_time_split)

  expect_equal(workflow$pre$actions$split$`rsample::initial_time_split`,
               rsample::initial_time_split)
})

# TODO
# After you define a fit method for split, adapt this
# example to remove the fit object after split
## test_that("update a recipe after model fit", {
##   rec <- recipes::recipe(mpg ~ cyl, mtcars)
##   rec2 <- recipes::recipe(mpg ~ disp, mtcars)

##   lm_model <- parsnip::linear_reg()
##   lm_model <- parsnip::set_engine(lm_model, "lm")

##   workflow <- workflow()
##   workflow <- add_model(workflow, lm_model)
##   workflow <- add_recipe(workflow, rec)

##   workflow <- fit(workflow, data = mtcars)

##   # Should clear fitted model
##   workflow <- update_recipe(workflow, rec2)

##   expect_equal(workflow$pre$actions$recipe$recipe, rec2)

##   expect_equal(workflow$fit$actions$model$spec, lm_model)
##   expect_null(workflow$pre$mold)
## })

test_that("cannot add two split specifications", {
  workflow <- workflow()
  workflow <- add_split(workflow, rsample::initial_split)

  expect_error(add_split(workflow, rsample::initial_time_split),
               "A `split` action has already been added to this workflow")
})

test_that("add/update_split check if `...` are named", {
  workflow <- workflow()

  expect_error(
    add_split(workflow, rsample::initial_split, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )

  workflow <- add_split(workflow, rsample::initial_split)

  expect_error(
    update_split(workflow, rsample::initial_time_split, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )
  
})

test_that("Updating a split after removing one, warns", {
  workflow <- add_split(workflow(), rsample::initial_split)

  expect_warning(
    update_split(remove_split(workflow), rsample::initial_time_split),
    "The workflow does not have a split specification."
  )

})

test_that("Updating a split doesn't remove anything else", {

  # The recipe
  workflow <- add_recipe(workflow(), recipes::recipe(mpg ~ cyl, data = mtcars))
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- update_split(workflow, rsample::initial_time_split)
  expect_true(has_preprocessor_recipe(workflow))
  expect_true(has_preprocessor_split(workflow))

  # The CV fold
})

test_that("Removing a split doesn't remove anything else", {

  # The recipe
  workflow <- add_recipe(workflow(), recipes::recipe(mpg ~ cyl, data = mtcars))
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- remove_split(workflow)
  expect_true(has_preprocessor_recipe(workflow))

  # The CV fold
})

test_that("Name of split function is always saved as name in the list", {
  # For add_split
  workflow <- add_split(workflow(), rsample::initial_split)
  expect_true("rsample::initial_split" %in% names(workflow$pre$actions$split))

  # For update_split
  workflow <- add_split(workflow(), rsample::initial_time_split)
  workflow <- update_split(workflow, rsample::initial_split)
  expect_true("rsample::initial_split" %in% names(workflow$pre$actions$split))
})
