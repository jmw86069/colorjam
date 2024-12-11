
test_that("mean_angle", {
   # two values
   ma_120_45 <- mean_angle(c(120, 45));
   expect_equal(
      ma_120_45[["deg"]],
      82.5)
   expect_equal(
      round(ma_120_45[["radius"]], digits=3),
      0.793)
   expect_equal(
      round(ma_120_45[["radius2"]], digits=3),
      0.894)

   # four values
   ma_4 <- mean_angle(c(120, 45, 20, 300));
   expect_equal(
      round(ma_4[["deg"]], digits=3),
      32.5)
   expect_equal(
      round(ma_4[["radius"]], digits=3),
      0.488)
   expect_equal(
      round(ma_4[["radius2"]], digits=3),
      0.689)
})

test_that("blend_colors", {
   #
   expect_equal(
      blend_colors(c("red", "gold")),
      "#E69A62FF")
   expect_equal(
      blend_colors(c("blue", "gold")),
      "#10A376FF")
   expect_equal(
      blend_colors(c("dodgerblue", "firebrick3")),
      "#BE52B5FF")

   # three colors with transparency
   expect_equal(
      blend_colors(c("#0000FF55", "#FFFF0099", "#FF0000FF")),
      "#D28483FF")
})

test_that("combine_alphas", {
   # two values
   expect_equal(
      combine_alphas(c(0.5, 0.5)),
      0.75)
   # three values
   expect_equal(
      combine_alphas(c(0.5, 0.5, 0.5)),
      0.875)
   # three values with NA
   expect_equal(
      combine_alphas(c(0.5, 0.5, NA, 0.5)),
      0.875)
   # all NA
   expect_equal(
      combine_alphas(c(NA_integer_, NA_integer_)),
      integer(0))
   # NULL input
   expect_equal(
      combine_alphas(NULL),
      integer(0))

   # two values, max_alpha=100
   expect_equal(
      combine_alphas(c(50, 50), max_alpha=100),
      75)
   # three values, max_alpha=100
   expect_equal(
      combine_alphas(c(50, 50, 50), max_alpha=100),
      87.5)

})
