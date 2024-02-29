#' fetch date from strng
#'
#' @param input_str string
#'
#' @return Date
#' @export
fetch_date_from_string <- function(input_str){

  tmp_str <-
    input_str %>%
    sub_jc2ad_string() %>%
    stringi::stri_trans_general("fullwidth-halfwidth") %>%
    #stringr::str_extract("[0-9]{4}年[0-9]+月[0-9]+日")
    stringr::str_extract("[0-9]{4}\u5e74[0-9]+\u6708[0-9]+\u65e5")
  if(is.na(tmp_str)){
    tmp_str <-
      input_str %>%
      sub_jc2ad_string() %>%
      stringi::stri_trans_general("fullwidth-halfwidth") %>%
      #stringr::str_extract("[0-9]{4}年[0-9]+月")
      stringr::str_extract("[0-9]{4}\u5e74[0-9]+\u6708")
    if(is.na(tmp_str)){
      return(NA)
    }
    #tmp_str <- paste0(tmp_str,"1日")
    tmp_str <- paste0(tmp_str,"1\u65e5")
  }

  date_tmp <- tmp_str %>%
    #stringr::str_match("([0-9]+)年([0-9]+)月([0-9]+)日")
    stringr::str_match("([0-9]+)\u5e74([0-9]+)\u6708([0-9]+)\u65e5")
  ans_date <-
    lubridate::as_date(sprintf("%s/%s/%s",
                       date_tmp[,2],
                       date_tmp[,3],
                       date_tmp[,4]))

  return(ans_date)
}

sub_jc2ad_string <- function(input_str){

  #reiwa2ad <- sub_jc2ad_strconv_func("令和",2019)
  reiwa2ad <- sub_jc2ad_strconv_func("\u4ee4\u548c",2019)

  #heisei2ad <- sub_jc2ad_strconv_func("平成",1989)
  heisei2ad <- sub_jc2ad_strconv_func("\u5e73\u6210",1989)

  #syouwa2ad <- sub_jc2ad_strconv_func("昭和",1926)
  syouwa2ad <- sub_jc2ad_strconv_func("\u662d\u548c",1926)

  gengou <- input_str %>%
    #stringr::str_extract("(平成|令和|昭和)(.|..)年")
    stringr::str_extract("(\u5e73\u6210|\u4ee4\u548c|\u662d\u548c)(.|..)\u5e74")
  ans <- input_str %>%
    #stringr::str_replace("(平成|令和|昭和)(.|..)年",
    stringr::str_replace("(\u5e73\u6210|\u4ee4\u548c|\u662d\u548c)(.|..)\u5e74",
                         gengou %>% reiwa2ad() %>% heisei2ad() %>% syouwa2ad())

  return(ans)

}

# 元号部分の文字列を西暦の４桁文字列に変換する関数を作る関数
# 変換関数は元号文字列にマッチしない場合、何もしない
sub_jc2ad_strconv_func <- function(gengou, first_year){

  # "元号([0-9]+)年"でマッチする正規表現
  #pattern <- paste0(gengou, "([0-9]+)年")
  pattern <- paste0(gengou, "([0-9]+)\u5e74")

  ans_func <- function(input_string){

    target <-
      input_string %>%
      as.character() %>%
      stringi::stri_trans_general("fullwidth-halfwidth") %>%
      #stringr::str_replace("元年", "1年")
      stringr::str_replace("\u5143\u5e74", "1\u5e74")

    tmp <- stringr::str_match(target, pattern)
    ans <- ifelse(is.na(tmp[,1]), input_string,
                  #sprintf("%d年", as.integer(tmp[,2]) + first_year - 1))
                  sprintf("%d\u5e74", as.integer(tmp[,2]) + first_year - 1))
    return(ans)
  }
}

