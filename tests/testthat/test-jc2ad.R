test_that("文字列から日付を読み取る", {

  expect_equal(fetch_date_from_string("令和５年７月２４日"),as.Date("2023/7/24"))
  expect_equal(fetch_date_from_string("令和元年７月"),as.Date("2019/7/1"))
  expect_equal(fetch_date_from_string("令和元年月"),NA)
  expect_equal(fetch_date_from_string("平成 年 月"),NA)
  expect_equal(fetch_date_from_string("平成 年月昭和5年７月１日"),NA)


})




test_that("和暦文字列を西暦文字列へ", {
  reiwa <- sub_jc2ad_strconv_func("令和",2019)
  heisei <- sub_jc2ad_strconv_func("平成",1989)

  expect_equal(reiwa("令和2年"),"2020年")
  expect_equal(reiwa("令和元年"),"2019年")

  expect_equal(heisei("令和元年"),"令和元年")
  expect_equal(heisei("平成元年"),"1989年")

  expect_equal("令和２年" %>% heisei() %>% reiwa(),"2020年")
  expect_equal("令和２年" %>% reiwa() %>% heisei(),"2020年")

})

test_that("西暦文字列へ変換", {

  expect_equal(sub_jc2ad_string("時点令和元年１月１日"),"時点2019年１月１日")
  expect_equal(sub_jc2ad_string("時点 令和元年１月１日"),"時点 2019年１月１日")
  expect_equal(sub_jc2ad_string("平成元年１月１日"),"1989年１月１日")
  expect_equal(sub_jc2ad_string("昭和４３年１月１日"),"1968年１月１日")
  expect_equal(sub_jc2ad_string("昭和四十3年１月１日"),"昭和四十3年１月１日")

  test_line <- "令和５年11月１日現在　　大 阪 府 市 区 町 村 別 世 帯 数 お よ び 人 口　（公表日：令和５年12月1日）"
  exp_line <- "2023年11月１日現在　　大 阪 府 市 区 町 村 別 世 帯 数 お よ び 人 口　（公表日：令和５年12月1日）"
  expect_equal(sub_jc2ad_string(test_line),exp_line)
  })
