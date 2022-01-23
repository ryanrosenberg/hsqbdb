#' Title
#'
#' @param url 
#' @param powers 
#'
#' @return
#' @export
#'
#' @examples
parse_tournament_yf <- function(url, powers){
  game_lines <- get_game_lines_yf(url) %>% 
    split(1:nrow(.))
  
  game_boxes <- get_game_boxes_yf(url, powers)
  
  player_stats <- purrr::map2_dfr(game_lines, game_boxes, parse_box_yf, powers)
  team_stats <- purrr::map2_dfr(game_lines, game_boxes, parse_line_yf, powers)
  
  return(list(player_stats = player_stats,
              team_stats = team_stats))
}

#' Title
#'
#' @param line 
#' @param box 
#' @param powers 
#'
#' @return
#' @export
#'
#' @examples
parse_box_yf <- function(line, box, powers){
  if(powers){
    team_1_name <- box[[1,1]]
    team_2_name <- box[[1,8]]
    
    team_1_box <- box[2:nrow(box), 1:6] %>% 
      purrr::set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(team = team_1_name, opponent = team_2_name, .before = 1)
    
    team_2_box <- box[2:nrow(box), 8:13] %>% 
      purrr::set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(team = team_2_name, opponent = team_1_name, .before = 1)
    
    player_lines <- dplyr::bind_rows(team_1_box, team_2_box) %>% 
      dplyr::mutate(dplyr::across(powers:pts, as.numeric)) %>% 
      dplyr::mutate(round = line$round, game_num = line$game_num, .before = 1) %>% 
      dplyr::filter(player != "Total")
  }
  
  else {
    team_1_name <- box[[1,1]]
    team_2_name <- box[[1,7]]
    
    team_1_box <- box[2:nrow(box), 1:5] %>% 
      purrr::set_names(c("player", "tuh", "tens", "negs", "pts")) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(team = team_1_name, opponent = team_2_name, .before = 1)
    
    team_2_box <- box[2:nrow(box), 7:11] %>% 
      purrr::set_names(c("player", "tuh", "tens", "negs", "pts")) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(team = team_2_name, opponent = team_1_name, .before = 1)
    
    player_lines <- dplyr::bind_rows(team_1_box, team_2_box) %>% 
      dplyr::mutate(dplyr::across(tens:pts, as.numeric)) %>% 
      dplyr::mutate(powers = NA, .before = tens) %>% 
      dplyr::mutate(round = line$round, game_num = line$game_num, .before = 1) %>% 
      dplyr::filter(player != "Total")
  }
  
  return(player_lines)
}

#' Title
#'
#' @param line 
#' @param box 
#' @param powers 
#'
#' @return
#' @export
#'
#' @examples
parse_line_yf <- function(line, box, powers){
  if (powers){
    team_1_name <- box[[1,1]]
    team_2_name <- box[[1,8]]
    
    team_1_box <- box[2:nrow(box), 1:6] %>% 
      purrr::set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(team = team_1_name, opponent = team_2_name, .before = 1)
    
    team_2_box <- box[2:nrow(box), 8:13] %>% 
      purrr::set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(team = team_2_name, opponent = team_1_name, .before = 1)
    
    box_tidy <- dplyr::bind_rows(team_1_box, team_2_box) %>% 
      dplyr::filter(player == "Total") %>% 
      dplyr::select(-player, -tuh)
  }
  
  else {
    team_1_name <- box[[1,1]]
    team_2_name <- box[[1,7]]
    
    team_1_box <- box[2:nrow(box), 1:5] %>% 
      purrr::set_names(c("player", "tuh", "tens", "negs", "pts")) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(team = team_1_name, opponent = team_2_name, .before = 1)
    
    team_2_box <- box[2:nrow(box), 7:11] %>% 
      purrr::set_names(c("player", "tuh", "tens", "negs", "pts")) %>% 
      tidyr::drop_na() %>% 
      dplyr::mutate(team = team_2_name, opponent = team_1_name, .before = 1)
    
    box_tidy <- dplyr::bind_rows(team_1_box, team_2_box) %>% 
      dplyr::filter(player == "Total") %>% 
      dplyr::select(-player, -tuh)
  }
  
  line_tidy <- line %>% 
    tidyr::separate(team, c("winner", "loser"), sep = '(?<=\\d),\\s') %>% 
    tidyr::gather(result, team, winner, loser) %>% 
    dplyr::mutate(team = stringr::str_remove(team, "\\s\\(OT\\)"),
           result = ifelse(result == 'winner', 'W', 'L')) %>% 
    tidyr::separate(team, c("team", "total_pts"), sep = "\\s(?=[-0-9]+$)") %>% 
    dplyr::mutate(team = trimws(team),
           total_pts = as.numeric(total_pts)) %>% 
    dplyr::mutate(opponent = rev(team), .after = team) 
  
  if (powers) {
    full_line <- box_tidy %>% 
      dplyr::left_join(line_tidy,
                by = c("team", "opponent")) %>% 
      dplyr::rename(tu_pts = pts) %>% 
      dplyr::mutate(tu_pts = as.numeric(tu_pts),
             bonuses_heard = powers + tens,
             bonus_points = total_pts - tu_pts,
             .before = total_pts)
  }
  
  else {
    full_line <- box_tidy %>% 
      dplyr::left_join(line_tidy,
                by = c("team", "opponent")) %>% 
      dplyr::rename(tu_pts = pts) %>% 
      dplyr::mutate(tu_pts = as.numeric(tu_pts),
             bonuses_heard = tens,
             bonus_points = total_pts - tu_pts,
             .before = total_pts) %>% 
      dplyr::mutate(powers = NA, .before = tens)
  }
  
  return(full_line)
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
parse_tournament_sqbs <- function(url, powers){
  lines <- url %>% 
    rvest::read_html() %>% 
    rvest::html_elements("font") %>% 
    rvest::html_text(trim = T) %>% 
    purrr::discard(~stringr::str_detect(., "by forfeit")) %>% 
    dplyr::tibble(team = .) %>% 
    dplyr::mutate(round = stringr::str_extract(team, "^Round\\s\\d+"), .before = 1) %>% 
    tidyr::fill(round, .direction = "down") %>% 
    filter(stringr::str_detect(team, "^Round\\s\\d+", negate = T)) %>% 
    dplyr::group_by(round) %>% 
    dplyr::mutate(type = ifelse(stringr::str_detect(team, ':'), 'box', 'line'), 
           game_num = rep(1:(n()/2), each = 2), .after = round) %>%
    tidyr::spread(type, team) %>% 
    dplyr::ungroup()
  
  all <- lines %>% 
    tidyr::separate(box, c("team1_name", "team1", 
                    "team2_name", "team2", 
                    "remove", "bonuses"), 
             sep = ':|(\r\n)|(\n)') %>% 
    dplyr::select(-remove) %>% 
    purrr::map_df(trimws) %>% 
    dplyr::mutate(dplyr::across(c(team1_name, team2_name), ~stringr::str_remove_all(., "\\s\\(D2\\)"))) %>% 
    dplyr::mutate(dplyr::across(c(team1_name, team2_name), ~stringr::str_remove_all(., "\\s\\(UG\\)"))) %>% 
    dplyr::mutate(dplyr::across(c(team1_name, team2_name), ~stringr::str_remove_all(., "\\s\\(DII\\)"))) %>% 
    dplyr::mutate(dplyr::across(c(team1, team2), ~stringr::str_remove_all(., "\\s\\(UG\\)"))) %>% 
    dplyr::mutate(dplyr::across(c(team1, team2), ~stringr::str_remove_all(., "\\s\\(D2\\)"))) %>% 
    dplyr::mutate(dplyr::across(c(team1, team2), ~stringr::str_remove_all(., "\\s\\(DII\\)")))
  
  parse_game <- function(round, game_num, line, 
                         team1_name, team1, team2_name, team2, bonuses){
    team1_box <- parse_players_line(team1, powers) %>% 
      dplyr::mutate(round = round, game_num = game_num,
             team = team1_name, opponent = team2_name, .before = 1)
    team2_box <- parse_players_line(team2, powers) %>% 
      dplyr::mutate(round = round, game_num = game_num,
             team = team2_name, opponent = team1_name, .before = 1)
    
    if (powers) {
      player_stats <- bind_rows(team1_box, team2_box) %>% 
        dplyr::mutate(dplyr::across(powers:pts, as.numeric)) %>% 
        dplyr::mutate(tuh = NA, .before = powers)
      
      suppressMessages(
      team_stats <- line %>% 
        stringr::str_split(",\\s") %>% 
        purrr::pluck(1) %>% 
        dplyr::tibble() %>% 
        purrr::set_names("team") %>% 
        dplyr::mutate(team = stringr::str_remove_all(team, "\\sOT$")) %>% 
        tidyr::separate(team, c("team", "total_pts"), sep = "\\s(?=[-0-9]+$)") %>% 
        dplyr::mutate(team = stringr::str_remove_all(team, "\\s\\(UG\\)"),
               team = stringr::str_remove_all(team, "\\s\\(D2\\)"),
               team = stringr::str_remove_all(team, "\\s\\(DII\\)")) %>% 
        dplyr::mutate(opponent = rev(team)) %>% 
        dplyr::left_join(player_stats %>% 
                           dplyr::group_by(round, game_num, team) %>% 
                           dplyr::summarize(powers = sum(powers),
                              tens = sum(tens),
                              negs = sum(negs),
                              tu_pts = sum(pts),
                              bonuses_heard = sum(powers) + sum(tens)),
                  by = c("team")) %>% 
        dplyr::mutate(total_pts = as.numeric(total_pts)) %>% 
        dplyr::mutate(bonus_points = total_pts - tu_pts, .after = bonuses_heard) %>% 
        dplyr::relocate(total_pts, .after = everything()) %>% 
        dplyr::mutate(dplyr::across(powers:total_pts, as.numeric))
      )
    }
    
    else {
      player_stats <- dplyr::bind_rows(team1_box, team2_box) %>% 
        dplyr::mutate(dplyr::across(tens:pts, as.numeric)) %>% 
        dplyr::mutate(tuh = NA, powers = NA, .before = tens)
      
      suppressMessages(
      team_stats <- line %>% 
        stringr::str_split(",\\s") %>% 
        purrr::pluck(1) %>% 
        dplyr::tibble() %>% 
        purrr::set_names("team") %>% 
        dplyr::mutate(team = stringr::str_remove(team, "\\sOT$")) %>% 
        tidyr::separate(team, c("team", "total_pts"), sep = "\\s(?=[-0-9]+$)") %>% 
        dplyr::mutate(team = stringr::str_remove_all(team, "\\s\\(UG\\)"),
               team = stringr::str_remove_all(team, "\\s\\(D2\\)"),
               team = stringr::str_remove_all(team, "\\s\\(DII\\)")) %>% 
        dplyr::mutate(opponent = rev(team)) %>% 
        dplyr::left_join(player_stats %>% 
                           dplyr::group_by(round, game_num, team) %>% 
                           dplyr::summarize(tens = sum(tens),
                                               negs = sum(negs),
                                               tu_pts = sum(pts),
                                               bonuses_heard = sum(tens)),
                                   by = c("team")) %>% 
        dplyr::mutate(total_pts = as.numeric(total_pts)) %>% 
        dplyr::mutate(bonus_points = total_pts - tu_pts, .after = bonuses_heard) %>% 
        dplyr::relocate(total_pts, .after = everything()) %>% 
        dplyr::mutate(dplyr::across(tens:total_pts, as.numeric)) %>% 
        dplyr::mutate(powers = NA, .before = tens)
      )
    }
    
    return(list(player_stats = player_stats,
                team_stats = team_stats))
  }
  
  parse_players_line <- function(player_line, powers){
    if (powers == 'T'){
      cols <- c("player", "powers", "tens", "negs", "pts")
    }
    else {
      cols <- c("player", "tens", "negs", "pts")
    }
    player_line %>% 
      stringr::str_split(",\\s") %>% 
      purrr::pluck(1) %>% 
      dplyr::tibble() %>% 
      purrr::set_names("temp") %>% 
      tidyr::separate(temp, cols, sep = '\\s(?=[-0-9])')
  }         
  
  all_stats <- purrr::pmap(all, parse_game)
  player_stats <- purrr::map_df(all_stats, "player_stats")
  team_stats <- purrr::map_df(all_stats, "team_stats")
  
  return(list(player_stats = player_stats,
              team_stats = team_stats))
}


#' Title
#'
#' @param year 
#' @param set 
#' @param site 
#' @param url 
#' @param powers 
#' @param difficulty 
#'
#' @return
#' @export
#'
#' @examples
read_tournament_entry <- function(year, set, site, url, powers, difficulty){
  print(glue::glue('Scraping the {site} site of {year} {set}'))
  tryCatch(test_read <- rvest::read_html(url),
           error = function(c) "Error in reading the HTML file")
  
  num_tables <- test_read %>% 
    rvest::html_elements("table") %>% 
    rvest::html_table() %>% 
    length()
  
  if(num_tables < 4){
    stats <- parse_tournament_sqbs(url, powers) %>% 
      purrr::map(~dplyr::mutate(., year = year, set = set, 
                  difficulty = difficulty, site = site, .before = 1))
    return(stats)
  }
  
  else {
    stats <- parse_tournament_yf(url, powers) %>% 
      purrr::map(~dplyr::mutate(., year = year, set = set, 
                  difficulty = difficulty, site = site, .before = 1))
    return(stats)
  }
  
}
