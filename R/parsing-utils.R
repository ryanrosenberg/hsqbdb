process_issues <- function(html){
  clean <- html %>%
    str_remove_all(', Jr\\.') %>%
    str_remove_all('\\s\\([^\\)]+\\)') %>%
    str_remove_all('\\s(?=:)')

  return(clean)
}

#' Get game score lines from a YellowFruit tournament
#'
#' @param url A link to a tournament stats report in YellowFruit format.
#'
#' @return
#' Returns a data frame with columns containing the game's round, game_num
#' (i.e. room), and score line (e.g. Team A 300, Team B 230).
#'
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

#' Get player box scores from a YellowFruit tournament
#'
#' @param url A link to a tournament stats report in YellowFruit format.
#' @param powers Whether or not the tournament has powers ('T' or 'F')
#'
#' @return
#' Returns a data frame containing player box scores in the same shape as a
#' YellowFruit web report
#' (i.e. https://hsquizbowl.org/db/tournaments/7112/stats/combined/games/).
#'
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

#' Parses player score lines from an SQBS tournament
#'
#' @param player_line Line of player scores
#' @param powers Whether or not the tournament has powers ('T' or 'F')
#'
#' @return
#' Returns a tidy data frame with columns for player, powers (if the tournament
#' has powers), tens, negs, and pts.
#'
parse_players_line_sqbs <- function(player_line){
  temp <- player_line %>%
    stringr::str_split(",\\s") %>%
    purrr::pluck(1) %>%
    dplyr::tibble() %>%
    purrr::set_names("temp") %>%
    tidyr::separate(temp, c("player", "score_line"),
                    sep = '(?<=\\D)\\s(?=[-0-9])') %>%
    tidyr::separate(score_line, c("powers", "tens", "negs", "pts"),
                    sep = '\\s(?=[-0-9])', fill = 'left')

  return(temp)
}

#' Parse game from an SQBS tournament
#'
#' @param round The round of the tournament
#' @param game_num An integer indicating the game's order within the round on
#' the stats report
#' @param line The game score line
#' @param team1_name The name of the team listed first
#' @param team1 The player score line of the team listed first
#' @param team2_name The name of the team listed second
#' @param team2 The player score line of the team listed second
#' @param bonuses The bonus stat line
#'
#' @return
#' Returns a named list of two data frames, one containing player stats
#' (conveniently named "player_stats") and the other team stats ("team_stats").
#'
parse_game_sqbs <- function(round, game_num, line,
                            team1_name, team1, team2_name, team2, bonuses){
  team1_box <- parse_players_line_sqbs(team1) %>%
    dplyr::mutate(round = round, game_num = game_num,
                  team = team1_name, opponent = team2_name, .before = 1)
  team2_box <- parse_players_line_sqbs(team2) %>%
    dplyr::mutate(round = round, game_num = game_num,
                  team = team2_name, opponent = team1_name, .before = 1)

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
      dplyr::mutate(team = trimws(team),
                    team = stringr::str_remove_all(team, "\\s\\(UG\\)"),
                    team = stringr::str_remove_all(team, "\\s\\(D2\\)"),
                    team = stringr::str_remove_all(team, "\\s\\(DII\\)")) %>%
      dplyr::mutate(opponent = rev(team)) %>%
      dplyr::left_join(player_stats %>%
                         dplyr::group_by(round, game_num, team) %>%
                         mutate(powers_safe = ifelse(is.na(powers), 0, powers)) %>%
                         dplyr::summarize(powers = sum(powers),
                                          tens = sum(tens),
                                          negs = sum(negs),
                                          tu_pts = sum(pts),
                                          bonuses_heard = sum(powers_safe) +
                                            sum(tens)),
                       by = c("team")) %>%
      dplyr::mutate(total_pts = as.numeric(total_pts)) %>%
      dplyr::mutate(bonus_points = total_pts - tu_pts,
                    .after = bonuses_heard) %>%
      dplyr::relocate(total_pts, .after = everything()) %>%
      dplyr::mutate(dplyr::across(powers:total_pts, as.numeric))
  )

  return(list(player_stats = player_stats,
              team_stats = team_stats))
}
