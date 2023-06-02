# Returns error, but it works correct in the app!

test_that("the correct images show", {
  test_res <- list()
  for (i in 2:3){
    test_res[[1]] <- list(found = TRUE, memo= paste0("nr", i))
    test_res[[4]] <- list(found = TRUE, memo= paste0("nr", i))
    test_res[[i+3]] <- list(found = FALSE, memo= paste0("nr", i))
  }
  list_res <- rep(list(test_res),6)

                   which_show(list_res, 1)
})
