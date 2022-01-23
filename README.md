
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hsqbdb

<!-- badges: start -->
<!-- badges: end -->

This package provides functions for scraping statistics from entries in
the hsquizbowl.org tournament database. Currently only supports scraping
and not search.

## Installation

You can install the released version of hsqbdb with:

``` r
devtools::install_github("ryanrosenberg/hsqbdb")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hsqbdb)

read_tournament_entry(url = 'https://hsquizbowl.org/db/tournaments/7030/stats/combined/games/',
                      year = '20-21',
                      set = 'ACF Nationals',
                      site = 'Northwestern',
                      difficulty = 'nationals',
                      powers = F)
#> Scraping the Northwestern site of 20-21 ACF Nationals
#> $player_stats
#> # A tibble: 1,356 x 14
#>    year  set     difficulty site    round  game_num team  opponent player  tuh  
#>    <chr> <chr>   <chr>      <chr>   <chr>     <int> <chr> <chr>    <chr>   <chr>
#>  1 20-21 ACF Na~ nationals  Northw~ Round~        1 Stan~ Chicago~ Tim Mo~ 20   
#>  2 20-21 ACF Na~ nationals  Northw~ Round~        1 Stan~ Chicago~ Natan ~ 20   
#>  3 20-21 ACF Na~ nationals  Northw~ Round~        1 Stan~ Chicago~ Ethan ~ 20   
#>  4 20-21 ACF Na~ nationals  Northw~ Round~        1 Stan~ Chicago~ Eric W~ 20   
#>  5 20-21 ACF Na~ nationals  Northw~ Round~        1 Chic~ Stanford John M~ 20   
#>  6 20-21 ACF Na~ nationals  Northw~ Round~        1 Chic~ Stanford Sam Mo~ 20   
#>  7 20-21 ACF Na~ nationals  Northw~ Round~        1 Chic~ Stanford Wonyou~ 20   
#>  8 20-21 ACF Na~ nationals  Northw~ Round~        1 Chic~ Stanford Walker~ 20   
#>  9 20-21 ACF Na~ nationals  Northw~ Round~        2 Illi~ Harvard~ Iain C~ 20   
#> 10 20-21 ACF Na~ nationals  Northw~ Round~        2 Illi~ Harvard~ Mitch ~ 20   
#> # ... with 1,346 more rows, and 4 more variables: powers <lgl>, tens <dbl>,
#> #   negs <dbl>, pts <dbl>
#> 
#> $team_stats
#> # A tibble: 346 x 16
#>    year  set    difficulty site   team  opponent powers  tens  negs tu_pts round
#>    <chr> <chr>  <chr>      <chr>  <chr> <chr>    <lgl>  <int> <int>  <dbl> <chr>
#>  1 20-21 ACF N~ nationals  North~ Stan~ Chicago~ NA        13     2    120 Roun~
#>  2 20-21 ACF N~ nationals  North~ Chic~ Stanford NA         4     1     35 Roun~
#>  3 20-21 ACF N~ nationals  North~ Illi~ Harvard~ NA        14     3    125 Roun~
#>  4 20-21 ACF N~ nationals  North~ Harv~ Illinois NA         5     0     50 Roun~
#>  5 20-21 ACF N~ nationals  North~ Nort~ Ohio St~ NA         8     2     70 Roun~
#>  6 20-21 ACF N~ nationals  North~ Ohio~ Northwe~ NA        10     3     85 Roun~
#>  7 20-21 ACF N~ nationals  North~ Vand~ Maryland NA         8     1     75 Roun~
#>  8 20-21 ACF N~ nationals  North~ Mary~ Vanderb~ NA        11     2    100 Roun~
#>  9 20-21 ACF N~ nationals  North~ Colu~ Yale     NA        12     1    115 Roun~
#> 10 20-21 ACF N~ nationals  North~ Yale  Columbia NA         8     2     70 Roun~
#> # ... with 336 more rows, and 5 more variables: game_num <int>, result <chr>,
#> #   bonuses_heard <int>, bonus_points <dbl>, total_pts <dbl>
```
