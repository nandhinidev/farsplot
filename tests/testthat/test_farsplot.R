context('Data Input')

test_that('Testing for compatibility and missing files',{
  tstr = make_filename(2018)
  expect_match(make_filename(2018),"accident_2018.csv.bz2",all=TRUE)
  expect_that(fars_read(tstr),throws_error())
})
