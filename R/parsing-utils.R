#' Utility function to handle random stats issues
#'
#' @param html Arbitrary string
#'
#' @return
#' Returns cleaned string
#'
process_issues <- function(html){
  clean <- html %>%
    stringr::str_remove_all(', Jr\\.') %>%
    stringr::str_remove_all(',\\s\\s0\\s0\\s0\\s0') %>%
    stringr::str_remove_all(',\\s\\s0\\s0\\s0') %>%
    stringr::str_remove_all('Division II Final:\\s') %>%
    stringr::str_remove_all('Undergraduate Final:\\s') %>%
    stringr::str_remove_all('Overall Final:\\s') %>%
    stringr::str_remove_all('Undergraduate 3rd place Tiebreaker:\\s') %>%
    stringr::str_remove_all('Division II Championship:\\s') %>%
    stringr::str_remove_all('Overall 2nd place Tiebreaker, Leg 1:\\s') %>%
    stringr::str_remove_all('Overall 2nd place Tiebreaker, Leg 2:\\s') %>%
    stringr::str_remove_all('\\s\\?\\?\\?\\?') %>%
    stringr::str_replace_all('\\(D2(?=\\s)', '(D2)') %>%
    stringr::str_replace_all('\\(Sr\\.\\]', '(Sr.)') %>%
    stringr::str_replace_all('Brandeis: The Wandering Jew', 'Brandeis') %>%
    stringr::str_replace_all('Kay Li,', 'Kay Li') %>%
    stringr::str_replace_all("\\*(?=\\s[-\\d])", ' (UG)') %>%
    stringr::str_replace_all("\\*(?=:)", ' (UG)') %>%
    stringr::str_replace_all("\\#(?=\\s[-\\d])", ' (DII)') %>%
    stringr::str_replace_all("\\#(?=:)", ' (DII)') %>%
    stringr::str_replace_all("It's Aidan, bitches!", 'Aidan Mehigan') %>%
    stringr::str_replace_all("Aadith M,", 'Aadith M.') %>%
    stringr::str_replace_all("Alexias I, Penguin Mascot", 'Alexias I Penguin Mascot') %>%
    stringr::str_replace_all('(?<=,)\\s\\s(?=[\\s\\d]+\r\n)', ' Unknown player ') %>%
    stringr::str_replace_all('Shashank \\(D20', 'Shashank') %>%
    stringr::str_replace_all('Chicago C:\\s\\s0 2 0 20, MSU A 2 3 0 60, Marianna 0 0 0 0, Austin 0 1 0 10',
                             'Chicago C: Brett 0 2 0 20, Marianna 2 3 0 60, Samir 0 0 0 0, Baidoo 0 1 0 10') %>%
    stringr::str_replace_all('Chicago C:\\s\\s0 0 0 0, MSU A 1 4 0 55, Marianna 0 2 0 20, Austin 1 1 0 25',
                             'Chicago C: Brett 0 0 0 0, Marianna 1 4 0 55, Samir 0 2 0 20, Baidoo 1 1 0 25') %>%
    stringr::str_replace_all('Maryland B 335, UVA 20', 'Maryland B 335, UVA 220') %>%
    stringr::str_replace_all('Heather Brooks\nHeather Brooks\nHeather Brooks', 'Heather Brooks') %>%
    stringr::str_replace_all('(?<=UF Vicu).(?=a)', 'ñ') %>%
    stringr::str_replace_all('(?<=zur).(?=ckhaltend)', 'ü') %>%
    stringr::str_replace_all(' - ', '-') %>%
    stringr::str_replace_all('(?<=\\s)D2$', '\\(D2\\)') %>%
    stringr::str_replace_all('(?<=\\s)D2(?=\\s)', '\\(D2\\)') %>%
    stringr::str_replace_all('(?<=\\s)D2(?=:)', '\\(D2\\)') %>%
    stringr::str_replace_all('(?<=\\s)DII(?=:)', '\\(DII\\)') %>%
    stringr::str_replace_all('(?<=\\s)DII$', '\\(DII\\)') %>%
    stringr::str_replace_all('(?<=\\s)DII(?=\\s)', '\\(DII\\)') %>%
    stringr::str_replace_all('(?<=\\s)UG(?=\\s)', '\\(UG\\)') %>%
    stringr::str_replace_all('\\(null\\)', 'null') %>%
    stringr::str_remove_all('\\s\\([^\\)]+\\)') %>%
    stringr::str_remove_all('\\(#\\d\\)\\s') %>%
    stringr::str_remove_all('-UG') %>%
    stringr::str_remove_all('-DII') %>%
    stringr::str_remove_all('\\sUG(?=:)') %>%
    stringr::str_remove_all('\\sDII(?=:)') %>%
    stringr::str_remove_all('\\sD2(?=:)') %>%
    stringr::str_remove_all('\\s(?=:)') %>%
    stringr::str_replace_all('UC  Davis', 'UC Davis')

  return(clean)
}

#' Utility function to handle random stats issues
#'
#' @param df Arbitrary data frame
#'
#' @return
#' Returns cleaned data frame
#'
post_processing <- function(df){
  clean <- df %>%
    dplyr::filter(paste(year, set, site) != '11-12 MUT Valencia College' |
                    team != "NFCC" | opponent != "NFCC") %>%
    dplyr::filter(paste(year, set, site) != '12-13 Collegiate Novice Rice' |
                    str_detect(team, "Rice", negate = T) |
                    str_detect(opponent, "Rice", negate = T)) %>%
    dplyr::filter(paste(year, set, site) != '13-14 Collegiate Novice Virginia Tech' |
                    !(team %in% c('Liberty B', 'Wake Forest')) |
                    !(opponent %in% c('Liberty B', 'Wake Forest'))) %>%
    dplyr::filter(paste(year, set, site) != '13-14 MUT Valencia College' |
                    !(team %in% c('Daytona State', 'Valencia Gold')) |
                    !(opponent %in% c('Daytona State', 'Valencia Gold'))) %>%
    dplyr::filter(paste(year, set, site) != '14-15 ACF Nationals Michigan' |
                    !(team %in% c('UCF', 'UCLA')) |
                    !(opponent %in% c('UCF', 'UCLA'))) %>%
    dplyr::filter(paste(year, set, site) != '14-15 PADAWAN Chicago' |
                    !(team %in% c('Northwestern', 'Chicago A')) |
                    !(opponent %in% c('Northwestern', 'Chicago A'))) %>%
    dplyr::filter(paste(year, set, site) != '14-15 Penn Bowl Minnesota' |
                    !(team %in% c('Minnesota B', 'Carleton')) |
                    !(opponent %in% c('Minnesota B', 'Carleton'))) %>%
    dplyr::mutate(set = dplyr::case_when(paste(year, set, site) == "17-18 SCT Georgia" &
                                           team %in% c("Alabama", "Wofford A",
                                                       "South Carolina A", "Georgia Tech A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "17-18 SCT Georgia" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "17-18 SCT Georgetown" &
                                           team %in% c("Virginia A", "Maryland A",
                                                       "Johns Hopkins A", "Penn A",
                                                       "Maryland C", "Gettysburg College",
                                                       "Delaware A", "Maryland B") ~
                                           "DI SCT",
                                         paste(year, set, site) == "17-18 SCT Georgetown" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "17-18 SCT Stanford" &
                                           team %in% c("UC Berkeley A", "UC Berkeley B",
                                                       "UC Berkeley C", "Sacramento State",
                                                       "Stanford A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "17-18 SCT Stanford" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "16-17 SCT Virginia" &
                                           team %in% c("Duke A", "Maryland A",
                                                       "Johns Hopkins A", "Wofford A",
                                                       "Georgetown A", "William & Mary A",
                                                       "North Carolina State", "Maryland B") ~
                                           "DI SCT",
                                         paste(year, set, site) == "17-18 SCT Georgetown" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "16-17 SCT Yale" &
                                           team %in% c("Columbia A", "Penn A",
                                                       "MIT A", "Harvard A",
                                                       "Brown A", "NYU A",
                                                       "Princeton A", "MIT B",
                                                       "Columbia B", "Columbia C",
                                                       "Yale", "Penn B") ~
                                           "DI SCT",
                                         paste(year, set, site) == "16-17 SCT Yale" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "15-16 SCT Central Oklahoma" &
                                           team %in% c("Missouri A", "Truman State A",
                                                       "Kansas State A", "Central Oklahoma") ~
                                           "DI SCT",
                                         paste(year, set, site) == "15-16 SCT Central Oklahoma" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "15-16 SCT VCU" &
                                           team %in% c("Maryland A", "Penn A",
                                                       "Duke A", "North Carolina",
                                                       "William & Mary", "Virginia A",
                                                       "Maryland B", "Georgetown University A",
                                                       "Liberty A", "Virginia Tech A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "15-16 SCT VCU" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "15-16 SCT Brown" &
                                           team %in% c("Columbia A", "Harvard A",
                                                       "Yale A", "Columbia B",
                                                       "MIT A", "Amherst A",
                                                       "Dartmouth", "MIT B",
                                                       "Columbia C", "NYU A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "15-16 SCT Brown" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "15-16 SCT Youngstown State" &
                                           team %in% c("Michigan A", "Michigan B",
                                                       "Michigan State A", "Kenyon",
                                                       "Pitt A", "Carnegie Mellon A",
                                                       "Case Western A", "Wright State A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "15-16 SCT Youngstown State" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "14-15 SCT UC Berkeley" &
                                           team %in% c("Stanford A", "UC Berkeley B",
                                                       "UC Berkeley A", "UC Berkeley C") ~
                                           "DI SCT",
                                         paste(year, set, site) == "14-15 SCT UC Berkeley" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "14-15 SCT Georgia Tech" &
                                           team %in% c("Georgia Tech", "Vanderbilt",
                                                       "South Carolina", "Western Kentucky A",
                                                       "Louisville A", "Alabama A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "14-15 SCT Georgia Tech" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "13-14 SCT Kentucky" &
                                           team %in% c("Wright State A", "Louisville B",
                                                       "Alabama A", "Western Kentucky A",
                                                       "Louisville A", "Centre A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "13-14 SCT Kentucky" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "13-14 SCT Case Western" &
                                           team %in% c("Ohio State A", "Ohio State B",
                                                       "Michigan State A", "Kenyon A",
                                                       "Michigan State B", "Carnegie Mellon A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "13-14 SCT Case Western" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "13-14 SCT Virginia Tech" &
                                           team %in% c("Virginia A", "North Carolina A",
                                                       "Maryland A", "Reynolds CC",
                                                       "Maryland B", "William & Mary",
                                                       "VCU", "Virginia B",
                                                       "South Carolina") ~
                                           "DI SCT",
                                         paste(year, set, site) == "13-14 SCT Virginia Tech" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "13-14 SCT RPI" &
                                           team %in% c("Penn A", "Delaware",
                                                       "Vassar A", "Cornell A",
                                                       "Penn B", "RPI A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "13-14 SCT RPI" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "13-14 SCT Minnesota" &
                                           team %in% c("Minneapolis CTC", "Carleton College A",
                                                       "Marquette", "St. Thomas A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "13-14 SCT Minnesota" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "12-13 SCT Ohio State" &
                                           team %in% c("Miami-Ohio A", "Case Western A",
                                                       "Michigan A", "Michigan State A",
                                                       "Michigan B", "Carnegie Mellon A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "12-13 SCT Ohio State" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "11-12 SCT Princeton" &
                                           team %in% c("Columbia", "Cornell A",
                                                       "Maryland A", "Penn A",
                                                       "Maryland B", "RPI A") ~
                                           "DI SCT",
                                         paste(year, set, site) == "11-12 SCT Princeton" ~ "DII SCT",
                                         T ~ set),
                  set = dplyr::case_when(paste(year, set, site) == "08-09 SCT MIT" &
                                           team %in% c("Brown", "Princeton",
                                                       "Harvard A", "Dartmouth A",
                                                       "Harvard B", "Dartmouth B",
                                                       "Brandeis", "Harvard C",
                                                       "Rutgers", "Harvard D",
                                                       "NYU", "Yale") &
                                           opponent %in% c("Brown", "Princeton",
                                                           "Harvard A", "Dartmouth A",
                                                           "Harvard B", "Dartmouth B",
                                                           "Brandeis", "Harvard C",
                                                           "Rutgers", "Harvard D",
                                                           "NYU", "Yale") ~
                                           "DI SCT",
                                         paste(year, set, site) == "08-09 SCT MIT" ~ "DII SCT",
                                         T ~ set),
                  difficulty = ifelse(set == 'DII SCT' & difficulty == 'regionals',
                                      'easy', difficulty))

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
    process_issues() %>%
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

  player_stats <- dplyr::bind_rows(team1_box, team2_box) %>%
    dplyr::mutate(dplyr::across(powers:pts, as.numeric)) %>%
    dplyr::mutate(tuh = NA, .before = powers)

  suppressMessages(
    team_stats <- line %>%
      stringr::str_split("(?<=\\d),\\s") %>%
      purrr::pluck(1) %>%
      dplyr::tibble() %>%
      purrr::set_names("team") %>%
      dplyr::mutate(team = stringr::str_remove_all(team, "\\sOT$"),
                    team = stringr::str_remove_all(team, "\\sTie$")) %>%
      tidyr::separate(team, c("team", "total_pts"), sep = "\\s(?=[-0-9]+$)") %>%
      dplyr::mutate(team = trimws(team),
                    team = stringr::str_remove_all(team, "\\s\\(UG\\)"),
                    team = stringr::str_remove_all(team, "\\s\\(D2\\)"),
                    team = stringr::str_remove_all(team, "\\s\\(DII\\)")) %>%
      dplyr::mutate(opponent = rev(team)) %>%
      dplyr::left_join(player_stats %>%
                         dplyr::group_by(round, game_num, team) %>%
                         dplyr::mutate(powers_safe = ifelse(is.na(powers), 0, powers)) %>%
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
