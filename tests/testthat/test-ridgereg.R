data("iris")

test_that("class is correct", {
  ridgereg_obj <- ridgereg$new(Petal.Length~Species, data=iris, lambda = 0)
  expect_true(class(ridgereg_obj)[1] == "ridgereg")
})


test_that("lm.ridge and ridgereg equal coefficents",{
  for (l in c(0, 1, 10)) {
    lm_ridge  <- MASS::lm.ridge(Petal.Length ~ Species, data = iris, lambda = l)
    ridge_obj <- ridgereg$new(Petal.Length ~ Species, data = iris, lambda = l)

    expect_equal(lm_ridge$coef, ridge_obj$coef()[-1], tolerance = 1e-2)
  }
})
