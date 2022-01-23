get_game_lines_yf <- function(url){
  game_lines <- url %>% 
    read_html() %>% 
    html_elements("h3 , h2") %>% 
    html_text2() %>% 
    trimws() %>% 
    discard(~str_detect(., "by forfeit")) %>% 
    discard(~str_detect(., 
                        "Sponsored by the Partnership for Academic Competition Excellence")) %>% 
    tibble(team = .) %>% 
    mutate(round = str_extract(team, "^Round\\s\\d+$"), .before = 1) %>% 
    fill(round, .direction = "down") %>% 
    filter(str_detect(team, "^Round\\s\\d+$", negate = T)) %>% 
    group_by(round) %>% 
    mutate(game_num = row_number(), .after = round) %>% 
    ungroup()
  
  return(game_lines)
}

get_game_boxes_yf <- function(url, powers){
  html_file <- url %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table()
  
  boxes <- html_file %>% 
    keep(~dim(.)[2] == 11) %>% 
    discard(~dim(.)[1] == 1) 
  
  if (length(boxes) <= 1 & powers == 'F'){
    boxes <- html_file %>% 
      keep(~dim(.)[2] == 13) %>% 
      discard(~dim(.)[1] == 1) %>% 
      map(~select(., -X3, -X10))
  } else if (length(boxes) <= 1 & powers == 'T') {
    boxes <- html_file %>% 
      keep(~dim(.)[2] == 13) %>% 
      discard(~dim(.)[1] == 1)
  } else {
    boxes <- html_file %>% 
      keep(~dim(.)[2] == 11) %>% 
      discard(~dim(.)[1] == 1) 
  }
  
  return(boxes)
    
}
