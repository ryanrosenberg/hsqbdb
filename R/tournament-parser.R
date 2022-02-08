#' Wrapper function for scraping a YellowFruit tournament
#'#' Wrapper function for scraping a YellowFruit tournament
#'
#' @param url A link to a YellowFruit stats report
#' @param powers Whether or not the tournament has powers ('T' or 'F')
#'
#' @return
#' Returns a named list of two data frames, one containing player stats
#' (conveniently named "player_stats") and the other team stats ("team_stats").
#'
parse_tournament_yf <- function(url, powers){
  game_lines <- get_game_lines_yf(url) %>%
    split(1:nrow(.))

  game_boxes <- get_game_boxes_yf(url, powers)

  player_stats <- purrr::pmap_dfr(list(
    game_lines,
    game_boxes,
    1:length(game_lines)
  ),
  parse_box_yf, powers)
  team_stats <- purrr::pmap_dfr(list(
    game_lines,
    game_boxes,
    1:length(game_lines)
  ),
  parse_line_yf, powers)

  return(list(player_stats = player_stats,
              team_stats = team_stats))
}

#' Parse the players box score from a YellowFruit tournament
#'
#' @param line The game score line
#' @param box The player box score
#' @param powers Whether or not the tournament has powers ('T' or 'F')
#'
#' @return
#' Returns a tidy data frame of player stats, with columns for round, game_num,
#' team, opponent, player, tossups heard ("tuh", if the tournament did not
#' record player tuh this will be NA), powers (if the tournament does not have
#' powers this will be NA), tens, negs, and pts.
#'
parse_box_yf <- function(line, box, game_id, powers){
  if(powers){
    team_1_name <- box[[1,1]] %>%
      process_issues()
    team_2_name <- box[[1,8]] %>%
      process_issues()

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
    team_1_name <- box[[1,1]] %>%
      process_issues()

    team_2_name <- box[[1,7]] %>%
      process_issues()

    team_1_box <- box[2:nrow(box), 1:5] %>%
      purrr::set_names(c("player", "tuh", "tens", "negs", "pts")) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(team = trimws(team_1_name), opponent = trimws(team_2_name),
                    .before = 1)

    team_2_box <- box[2:nrow(box), 7:11] %>%
      purrr::set_names(c("player", "tuh", "tens", "negs", "pts")) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(team = trimws(team_2_name), opponent = trimws(team_1_name),
                    .before = 1)

    player_lines <- dplyr::bind_rows(team_1_box, team_2_box) %>%
      dplyr::mutate(dplyr::across(tens:pts, as.numeric)) %>%
      dplyr::mutate(powers = NA, .before = tens) %>%
      dplyr::mutate(game_id = game_id, round = line$round,
                    game_num = line$game_num, .before = 1) %>%
      dplyr::filter(player != "Total")
  }

  return(player_lines)
}

#' Parse team stats from a YellowFruit tournament
#'
#' @param line The game score line
#' @param box The player box score
#' @param powers Whether or not the tournament has powers ('T' or 'F')
#'
#' @return
#' Returns a tidy data frame of team stats, with columns for round, game_num,
#' team, opponent, tossups heard ("tuh"), powers (if the tournament does not have
#' powers this will be NA), tens, negs, bonuses_heard, bonus_pts, and total_pts.
#'
parse_line_yf <- function(line, box, game_id, powers){
  if (powers){
    team_1_name <- box[[1,1]] %>%
      process_issues()

    team_2_name <- box[[1,8]] %>%
      process_issues()

    team_1_box <- box[2:nrow(box), 1:6] %>%
      purrr::set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(team = team_1_name, opponent = team_2_name, .before = 1)

    team_2_box <- box[2:nrow(box), 8:13] %>%
      purrr::set_names(c("player", "tuh", "powers", "tens", "negs", "pts")) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(team = team_2_name, opponent = team_1_name, .before = 1)

    box_tidy <- dplyr::bind_rows(team_1_box, team_2_box) %>%
      dplyr::mutate(dplyr::across(powers:pts, as.numeric)) %>%
      dplyr::group_by(team, opponent) %>%
      dplyr::summarize(powers = sum(powers),
                       tens = sum(tens),
                       negs = sum(negs),
                       pts = sum(pts))
  }

  else {
    team_1_name <- box[[1,1]] %>%
      process_issues()

    team_2_name <- box[[1,7]] %>%
      process_issues()

    team_1_box <- box[2:nrow(box), 1:5] %>%
      purrr::set_names(c("player", "tuh", "tens", "negs", "pts")) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(team = trimws(team_1_name), opponent = trimws(team_2_name),
                    .before = 1)

    team_2_box <- box[2:nrow(box), 7:11] %>%
      purrr::set_names(c("player", "tuh", "tens", "negs", "pts")) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(team = trimws(team_2_name), opponent = trimws(team_1_name),
                    .before = 1)

    box_tidy <- dplyr::bind_rows(team_1_box, team_2_box) %>%
      dplyr::mutate(dplyr::across(tens:pts, as.numeric)) %>%
      dplyr::group_by(team, opponent) %>%
      dplyr::summarize(tens = sum(tens),
                       negs = sum(negs),
                       pts = sum(pts))
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
                    bonus_pts = total_pts - tu_pts,
                    .before = total_pts) %>%
      dplyr::mutate(game_id, .before = 1)
  }

  else {
    full_line <- box_tidy %>%
      dplyr::left_join(line_tidy,
                       by = c("team", "opponent")) %>%
      dplyr::rename(tu_pts = pts) %>%
      dplyr::mutate(tu_pts = as.numeric(tu_pts),
                    bonuses_heard = tens,
                    bonus_pts = total_pts - tu_pts,
                    .before = total_pts) %>%
      dplyr::mutate(powers = NA, .before = tens) %>%
      dplyr::mutate(game_id, .before = 1)
  }

  return(full_line)
}

#' Wrapper function for scraping SQBS tournament stats
#'
#' @param url A link to a YellowFruit stats report
#' @param powers Whether or not the tournament has powers ('T' or 'F')
#'
#' @return
#' Returns a named list of two data frames, one containing player stats
#' (conveniently named "player_stats") and the other team stats ("team_stats").
#'
parse_tournament_sqbs <- function(url, powers){
  lines <- url %>%
    rvest::read_html() %>%
    rvest::html_elements("font") %>%
    rvest::html_text(trim = T) %>%
    process_issues() %>%
    purrr::discard(~stringr::str_detect(., "by forfeit")) %>%
    purrr::discard(~stringr::str_detect(., ":$")) %>%
    dplyr::tibble(team = .) %>%
    dplyr::mutate(round = stringr::str_extract(team, "^Round\\s\\d+"), .before = 1) %>%
    tidyr::fill(round, .direction = "down") %>%
    dplyr::filter(stringr::str_detect(team, "^Round\\s\\d+", negate = T),
                  stringr::str_detect(team, "\\?\\?\\?", negate = T),
                  stringr::str_detect(team, "viz\\. in prelim bracket", negate = T)) %>%
    dplyr::group_by(round) %>%
    dplyr::mutate(type = ifelse(stringr::str_detect(team, '(?<!Final):'), 'box', 'line'),
                  game_num = rep(1:(dplyr::n()/2), each = 2), .after = round) %>%
    tidyr::spread(type, team) %>%
    dplyr::ungroup()

  all <- lines %>%
    tidyr::separate(box, c("team1_name", "team1",
                           "team2_name", "team2",
                           "remove", "bonuses"),
                    sep = ':|(\r\n)(?!\\t)|(\n)(?!\\t)|(\r\\s)(?!\\t)') %>%
    dplyr::select(-remove) %>%
    purrr::map_df(trimws) %>%
    dplyr::mutate(dplyr::across(c(team1_name, team2_name), ~stringr::str_remove_all(., "\\s\\(D2\\)"))) %>%
    dplyr::mutate(dplyr::across(c(team1_name, team2_name), ~stringr::str_remove_all(., "\\s\\(UG\\)"))) %>%
    dplyr::mutate(dplyr::across(c(team1_name, team2_name), ~stringr::str_remove_all(., "\\s\\(DII\\)"))) %>%
    dplyr::mutate(dplyr::across(c(team1, team2), ~stringr::str_remove_all(., "\\s\\(UG\\)"))) %>%
    dplyr::mutate(dplyr::across(c(team1, team2), ~stringr::str_remove_all(., "\\s\\(D2\\)"))) %>%
    dplyr::mutate(dplyr::across(c(team1, team2), ~stringr::str_remove_all(., "\\s\\(DII\\)"))) %>%
    dplyr::mutate(game_id = row_number(), .before = 1)

  all_stats <- purrr::pmap(all, parse_game_sqbs)
  player_stats <- purrr::map_df(all_stats, "player_stats")
  team_stats <- purrr::map_df(all_stats, "team_stats")

  return(list(player_stats = player_stats,
              team_stats = team_stats))
}


#' Scrape tournament stats report
#'
#' @param year The competition year the tournament took place. Preferred format
#' is 'XX-XX' (e.g. '20-21' or '16-17').
#' @param set The set the tournament used. If set has multiple iterations,
#' there is no need to distinguish them (i.e. 'Penn Bowl' should be used instead
#' of '2009 Penn Bowl').
#' @param site The site of the tournament. Should be selected to distinguish
#' between multiple mirrors of the same set, even if they had the nominal same
#' site (e.g. two online mirrors, or a school hosting a high school and a college
#' mirror).
#' @param url A link to a tournament stats report in either SQBS or YellowFruit
#' format. Should be in string format.
#' @param powers Whether or not the tournament has powers ('T' or 'F')
#' @param difficulty The difficulty of the tournament (one of "easy", "medium",
#' "regionals", or "nationals").
#'
#' @return
#' Returns a named list of two data frames, one containing player stats
#' (conveniently named "player_stats") and the other team stats ("team_stats").
#'
#' @export
#'
#' @examples
#' read_tournament_entry(
#' url = 'https://hsquizbowl.org/db/tournaments/7030/stats/combined/games/',
#' year = '20-21',
#' set = 'ACF Nationals',
#' site = 'Northwestern',
#' difficulty = 'nationals',
#' powers = 'F'
#' )
#'
#' read_tournament_entry(
#' url = 'https://hsquizbowl.org/db/tournaments/7112/stats/combined/games/',
#' difficulty = 'medium'
#' )
#'
read_tournament_entry <- function(year = 'Unknown year',
                                  set = 'Unknown set',
                                  site = 'Unknown site',
                                  url,
                                  powers = 'T',
                                  difficulty = c("easy",
                                                 "medium",
                                                 "regionals",
                                                 "nationals")){
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
                                difficulty = difficulty, site = site, .before = 1)) %>%
      purrr::map(post_processing)
    return(stats)
  }

  else {
    stats <- parse_tournament_yf(url, powers) %>%
      purrr::map(~dplyr::mutate(., year = year, set = set,
                                difficulty = difficulty, site = site, .before = 1)) %>%
      purrr::map(post_processing)
    return(stats)
  }

}
