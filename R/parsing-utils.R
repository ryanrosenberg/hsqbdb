#' Title
#'
#' @param url 
#'
#' @return
#' @export
#'
#' @examples
get_game_lines_yf <- function(url){
  game_lines <- url %>% 
    rvest::read_html() %>% 
    rvest::html_elements("h3 , h2") %>% 
    rvest::html_text2() %>% 
    trimws() %>% 
    purrr::discard(~stringr::str_detect(., "by forfeit")) %>% 
    purrr::discard(~stringr::str_detect(., 
                                        "Sponsored by the Partnership for Academic Competition Excellence")) %>% 
    dplyr::tibble(team = .) %>% 
    dplyr::mutate(round = stringr::str_extract(team, "^Round\\s\\d+$"), .before = 1) %>% 
    tidyr::fill(round, .direction = "down") %>% 
    dplyr::filter(stringr::str_detect(team, "^Round\\s\\d+$", negate = T)) %>% 
    dplyr::group_by(round) %>% 
    dplyr::mutate(game_num = dplyr::row_number(), .after = round) %>% 
    dplyr::ungroup()
  
  return(game_lines)
}

#' Title
#'
#' @param url 
#' @param powers 
#'
#' @return
#' @export
#'
#' @examples
get_game_boxes_yf <- function(url, powers){
  html_file <- url %>% 
    rvest::read_html() %>% 
    rvest::html_elements("table") %>% 
    rvest::html_table()
  
  boxes <- html_file %>% 
    purrr::keep(~dim(.)[2] == 11) %>% 
    purrr::discard(~dim(.)[1] == 1) 
  
  if (length(boxes) <= 1 & powers == 'F'){
    boxes <- html_file %>% 
      purrr::keep(~dim(.)[2] == 13) %>% 
      purrr::discard(~dim(.)[1] == 1) %>% 
      purrr::map(~select(., -X3, -X10))
  } else if (length(boxes) <= 1 & powers == 'T') {
    boxes <- html_file %>% 
      purrr::keep(~dim(.)[2] == 13) %>% 
      purrr::discard(~dim(.)[1] == 1)
  } else {
    boxes <- html_file %>% 
      purrr::keep(~dim(.)[2] == 11) %>% 
      purrr::discard(~dim(.)[1] == 1) 
  }
  
  return(boxes)
  
}
