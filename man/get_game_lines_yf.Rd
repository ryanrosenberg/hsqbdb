% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsing-utils.R
\name{get_game_lines_yf}
\alias{get_game_lines_yf}
\title{Get game score lines from a YellowFruit tournament}
\usage{
get_game_lines_yf(url)
}
\arguments{
\item{url}{A link to a tournament stats report in YellowFruit format.}
}
\value{
Returns a data frame with columns containing the game's round, game_num
(i.e. room), and score line (e.g. Team A 300, Team B 230).
}
\description{
Get game score lines from a YellowFruit tournament
}
