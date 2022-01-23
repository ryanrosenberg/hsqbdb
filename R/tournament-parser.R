parse_tournament_yf <- function(url, powers){
  game_lines <- get_game_lines_yf(url) %>% 
    split(1:nrow(.))
  
  game_boxes <- get_game_boxes_yf(url, powers)
  
  player_stats <- map2_dfr(game_lines, game_boxes, parse_box_yf, powers)
  team_stats <- map2_dfr(game_lines, game_boxes, parse_line_yf, powers)
  
  return(list(player_stats = player_stats,
              team_stats = team_stats))
}

parse_box_yf <- function(line, box, powers){
  if(powers){
    team_1_name <- box[[1,1]]
    team_2_name <- box[[1,8]]
    
    team_1_box <- box[2:nrow(box), 1:6] %>% 
      set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>% 
      drop_na() %>% 
      mutate(team = team_1_name, opponent = team_2_name, .before = 1)
    
    team_2_box <- box[2:nrow(box), 8:13] %>% 
      set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>% 
      drop_na() %>% 
      mutate(team = team_2_name, opponent = team_1_name, .before = 1)
    
    player_lines <- bind_rows(team_1_box, team_2_box) %>% 
      mutate(across(powers:pts, as.numeric)) %>% 
      mutate(round = line$round, game_num = line$game_num, .before = 1) %>% 
      filter(player != "Total")
  }
  
  else {
    team_1_name <- box[[1,1]]
    team_2_name <- box[[1,7]]
    
    team_1_box <- box[2:nrow(box), 1:5] %>% 
      set_names(c("player", "tuh", "tens", "negs", "pts")) %>% 
      drop_na() %>% 
      mutate(team = team_1_name, opponent = team_2_name, .before = 1)
    
    team_2_box <- box[2:nrow(box), 7:11] %>% 
      set_names(c("player", "tuh", "tens", "negs", "pts")) %>% 
      drop_na() %>% 
      mutate(team = team_2_name, opponent = team_1_name, .before = 1)
    
    player_lines <- bind_rows(team_1_box, team_2_box) %>% 
      mutate(across(tens:pts, as.numeric)) %>% 
      mutate(powers = NA, .before = tens) %>% 
      mutate(round = line$round, game_num = line$game_num, .before = 1) %>% 
      filter(player != "Total")
  }
  
  return(player_lines)
}

parse_line_yf <- function(line, box, powers){
  if (powers){
    team_1_name <- box[[1,1]]
    team_2_name <- box[[1,8]]
    
    team_1_box <- box[2:nrow(box), 1:6] %>% 
      set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>% 
      drop_na() %>% 
      mutate(team = team_1_name, opponent = team_2_name, .before = 1)
    
    team_2_box <- box[2:nrow(box), 8:13] %>% 
      set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>% 
      drop_na() %>% 
      mutate(team = team_2_name, opponent = team_1_name, .before = 1)
    
    box_tidy <- bind_rows(team_1_box, team_2_box) %>% 
      filter(player == "Total") %>% 
      select(-player, -tuh)
  }
  
  else {
    team_1_name <- box[[1,1]]
    team_2_name <- box[[1,7]]
    
    team_1_box <- box[2:nrow(box), 1:5] %>% 
      set_names(c("player", "tuh", "tens", "negs", "pts")) %>% 
      drop_na() %>% 
      mutate(team = team_1_name, opponent = team_2_name, .before = 1)
    
    team_2_box <- box[2:nrow(box), 7:11] %>% 
      set_names(c("player", "tuh", "tens", "negs", "pts")) %>% 
      drop_na() %>% 
      mutate(team = team_2_name, opponent = team_1_name, .before = 1)
    
    box_tidy <- bind_rows(team_1_box, team_2_box) %>% 
      filter(player == "Total") %>% 
      select(-player, -tuh)
  }
  
  line_tidy <- line %>% 
    separate(team, c("winner", "loser"), sep = '(?<=\\d),\\s') %>% 
    gather(result, team, winner, loser) %>% 
    mutate(team = str_remove(team, "\\s\\(OT\\)"),
           result = ifelse(result == 'winner', 'W', 'L')) %>% 
    separate(team, c("team", "total_pts"), sep = "\\s(?=[-0-9]+$)") %>% 
    mutate(team = trimws(team),
           total_pts = as.numeric(total_pts)) %>% 
    mutate(opponent = rev(team), .after = team) 
  
  if (powers) {
    full_line <- box_tidy %>% 
      left_join(line_tidy,
                by = c("team", "opponent")) %>% 
      rename(tu_pts = pts) %>% 
      mutate(tu_pts = as.numeric(tu_pts),
             bonuses_heard = powers + tens,
             bonus_points = total_pts - tu_pts,
             .before = total_pts)
  }
  
  else {
    full_line <- box_tidy %>% 
      left_join(line_tidy,
                by = c("team", "opponent")) %>% 
      rename(tu_pts = pts) %>% 
      mutate(tu_pts = as.numeric(tu_pts),
             bonuses_heard = tens,
             bonus_points = total_pts - tu_pts,
             .before = total_pts) %>% 
      mutate(powers = NA, .before = tens)
  }
  
  return(full_line)
}

parse_tournament_sqbs <- function(url, powers){
  lines <- url %>% 
    read_html() %>% 
    html_elements("font") %>% 
    html_text(trim = T) %>% 
    discard(~str_detect(., "by forfeit")) %>% 
    tibble(team = .) %>% 
    mutate(round = str_extract(team, "^Round\\s\\d+"), .before = 1) %>% 
    fill(round, .direction = "down") %>% 
    filter(str_detect(team, "^Round\\s\\d+", negate = T)) %>% 
    group_by(round) %>% 
    mutate(type = ifelse(str_detect(team, ':'), 'box', 'line'), 
           game_num = rep(1:(n()/2), each = 2), .after = round) %>%
    spread(type, team) %>% 
    ungroup()
  
  all <- lines %>% 
    separate(box, c("team1_name", "team1", 
                    "team2_name", "team2", 
                    "remove", "bonuses"), 
             sep = ':|(\r\n)|(\n)') %>% 
    select(-remove) %>% 
    map_df(trimws) %>% 
    mutate(across(c(team1_name, team2_name), ~str_remove_all(., "\\s\\(UG\\)"))) %>% 
    mutate(across(c(team1_name, team2_name), ~str_remove_all(., "\\s\\(D2\\)"))) %>% 
    mutate(across(c(team1_name, team2_name), ~str_remove_all(., "\\s\\(DII\\)"))) %>% 
    mutate(across(c(team1, team2), ~str_remove_all(., "\\s\\(UG\\)"))) %>% 
    mutate(across(c(team1, team2), ~str_remove_all(., "\\s\\(D2\\)"))) %>% 
    mutate(across(c(team1, team2), ~str_remove_all(., "\\s\\(DII\\)")))
  
  parse_game <- function(round, game_num, line, 
                         team1_name, team1, team2_name, team2, bonuses){
    team1_box <- parse_players_line(team1, powers) %>% 
      mutate(round = round, game_num = game_num,
             team = team1_name, opponent = team2_name, .before = 1)
    team2_box <- parse_players_line(team2, powers) %>% 
      mutate(round = round, game_num = game_num,
             team = team2_name, opponent = team1_name, .before = 1)
    
    if (powers) {
      player_stats <- bind_rows(team1_box, team2_box) %>% 
        mutate(across(powers:pts, as.numeric)) %>% 
        mutate(tuh = NA, .before = powers)
      
      suppressMessages(
      team_stats <- line %>% 
        str_split(",\\s") %>% 
        pluck(1) %>% 
        tibble() %>% 
        set_names("team") %>% 
        mutate(team = str_remove_all(team, "\\sOT$")) %>% 
        separate(team, c("team", "total_pts"), sep = "\\s(?=[-0-9]+$)") %>% 
        mutate(team = str_remove_all(team, "\\s\\(UG\\)"),
               team = str_remove_all(team, "\\s\\(D2\\)"),
               team = str_remove_all(team, "\\s\\(DII\\)")) %>% 
        mutate(opponent = rev(team)) %>% 
        left_join(player_stats %>% 
                    group_by(round, game_num, team) %>% 
                    summarize(powers = sum(powers),
                              tens = sum(tens),
                              negs = sum(negs),
                              tu_pts = sum(pts),
                              bonuses_heard = sum(powers) + sum(tens)),
                  by = c("team")) %>% 
        mutate(total_pts = as.numeric(total_pts)) %>% 
        mutate(bonus_points = total_pts - tu_pts, .after = bonuses_heard) %>% 
        relocate(total_pts, .after = everything()) %>% 
        mutate(across(powers:total_pts, as.numeric))
      )
    }
    
    else {
      player_stats <- bind_rows(team1_box, team2_box) %>% 
        mutate(across(tens:pts, as.numeric)) %>% 
        mutate(tuh = NA, powers = NA, .before = tens)
      
      suppressMessages(
      team_stats <- line %>% 
        str_split(",\\s") %>% 
        pluck(1) %>% 
        tibble() %>% 
        set_names("team") %>% 
        mutate(team = str_remove(team, "\\sOT$")) %>% 
        separate(team, c("team", "total_pts"), sep = "\\s(?=[-0-9]+$)") %>% 
        mutate(team = str_remove_all(team, "\\s\\(UG\\)"),
               team = str_remove_all(team, "\\s\\(D2\\)"),
               team = str_remove_all(team, "\\s\\(DII\\)")) %>% 
        mutate(opponent = rev(team)) %>% 
        left_join(player_stats %>% 
                                     group_by(round, game_num, team) %>% 
                                     summarize(tens = sum(tens),
                                               negs = sum(negs),
                                               tu_pts = sum(pts),
                                               bonuses_heard = sum(tens)),
                                   by = c("team")) %>% 
        mutate(total_pts = as.numeric(total_pts)) %>% 
        mutate(bonus_points = total_pts - tu_pts, .after = bonuses_heard) %>% 
        relocate(total_pts, .after = everything()) %>% 
        mutate(across(tens:total_pts, as.numeric)) %>% 
        mutate(powers = NA, .before = tens)
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
      str_split(",\\s") %>% 
      pluck(1) %>% 
      tibble() %>% 
      set_names("temp") %>% 
      separate(temp, cols, sep = '\\s(?=[-0-9])')
  }         
  
  all_stats <- pmap(all, parse_game)
  player_stats <- map_df(all_stats, "player_stats")
  team_stats <- map_df(all_stats, "team_stats")
  
  return(list(player_stats = player_stats,
              team_stats = team_stats))
}


read_tournament_entry <- function(year, set, site, url, powers, difficulty){
  print(glue::glue('Scraping the {site} site of {year} {set}'))
  tryCatch(test_read <- read_html(url),
           error = function(c) "Error in reading the HTML file")
  
  num_tables <- test_read %>% 
    html_elements("table") %>% 
    html_table() %>% 
    length()
  
  if(num_tables < 4){
    stats <- parse_tournament_sqbs(url, powers) %>% 
      map(~mutate(., year = year, set = set, 
                  difficulty = difficulty, site = site, .before = 1))
    return(stats)
  }
  
  else {
    stats <- parse_tournament_yf(url, powers) %>% 
      map(~mutate(., year = year, set = set, 
                  difficulty = difficulty, site = site, .before = 1))
    return(stats)
  }
  
}
