Sys.setenv("OMP_THREAD_LIMIT" = 2)

testfiles = list.files("testthat", pattern = ".R", full.names = T)

system.time(
  lapply(testfiles, \(f) {
    source(f, chdir = T)
  }))
#   user  system elapsed 
# 16.211   0.969  15.509 

skip_on_cran = function() {
  skip("Test on CRAN")
}
system.time(
  lapply(testfiles, \(f) {
    source(f, chdir = T)
  }))
#   user  system elapsed 
# 11.028   0.778  11.746 

# Find heavy tests
times = lapply(testfiles, \(f) {
  system.time(source(f, chdir = T, ))
})
names(times) <- gsub("testthat/", "", testfiles)

dplyr::bind_rows(times, .id = "file") |> 
  dplyr::mutate(ratio = as.numeric(elapsed/user.self)) |> 
  dplyr::arrange(dplyr::desc(elapsed))

profvis::profvis(source("testthat/test-spatial.R"))
profvis::profvis(source("testthat/test-read-gtfs.R"))
profvis::profvis(source("testthat/test-travel_times.R"))
profvis::profvis(source("testthat/test-utils.R"))
