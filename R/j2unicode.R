#' japanese charactor into escaped unicode
#'
#' @param str_vector vector of strings
#'
#' @return vector of escaped unicode strings
#' @export
jp2unicode <- function(str_vector){

  str_vector
  ans <- NULL
  for(i in seq_along(str_vector)){
    ans <- c(ans,sub_japanese2unicode_filter(str_vector[i]))
  }

  return(ans)
}

#' jp2unicode in clipbord
#'
#' @export
jp2uni_clip_convert <- function(){
  clipr::read_clip() %>% jp2unicode() %>% clipr::write_clip()
}



# 元のアイディア
# https://qiita.com/zakkiiii/items/eb86fe6ff9c8e5b7715a
# 原則:エスケープすると、日本語以外の改行等も全てエスケープされる。
# エスケープされた文字のうち"u"が付くもの（日本語等）に着目して、
# uの付くものだけを、エスケープ文字（\）を倍にする
# その後、アンエスケープをすると、日本語以外の文字は、
# エスケープが外れ、日本語等のみが普通のエスケープ状態になる。
#
# コメント部分を除く処理では、
# "#"の後ろでエスケープされている日本語等（u）のつくものは、
# 一旦、"\\\\u"を"\\\\_u"に変換し、
# 上述のuが付いたもののエスケープもじを倍にする処理にかからないようにした上で、
# その処理が終わった後に"\\\\u"に戻す処理を行う。
# これによりアンエスケープのときに、日本語文字に戻ることになる。
sub_japanese2unicode_filter <- function(input_str){

  encode_tmp_str <-
    input_str %>%
    stringi::stri_escape_unicode()

  while(TRUE){

    finish <- stringr::str_detect(encode_tmp_str,
                               pattern = "#.*\\\\u[0-9a-fA-F]{4}")
    if (finish == 0){
      break
    }

    encode_tmp_str <-
      stringi::stri_replace_all_regex(encode_tmp_str,
                                      pattern = "#(.*)\\\\u([0-9a-fA-F]{4})",
                                      replacement = "#$1\\\\_u$2")
  }

  ans <-
    encode_tmp_str %>%

    stringi::stri_replace_all_regex(
      pattern = "\\\\\\\\u([0-9a-fA-F]{4})",
      replacement = "\\\\u$1") %>%

    stringi::stri_replace_all_regex(
      pattern = "\\\\u([0-9a-fA-F]{4})",
      replacement = "\\\\\\\\u$1") %>%

    stringi::stri_replace_all_regex(
      pattern = "\\\\_u([0-9a-fA-F]{4})",
      replacement = "\\\\u$1") %>%

    stringi::stri_unescape_unicode()

  return(ans)
}

