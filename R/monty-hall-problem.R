#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'  Contestant selects a door out of three available.
#'
#' @description
#'   `select_door` allows the user to makes a choice of a door from 1 to 3,
#'   representing the three doors.
#'
#' @details
#'   First, the variable `doors` is created, holding the
#'   numbers 1 through 3. Then one of the numbers is
#'   selected randomly and stored in a variable `a.pick`
#'   which is returned when the function is called.
#'
#' @param
#'   ... no arguments are used by the function.
#'
#' @return
#'   The function returns a vector (length = 1) with numerical value
#'   between 1 and 3 that indicates with door the contestant has chosen.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a door with a goat behind it.
#'
#' @description
#'   `open_goat_door()` generates a number between 1 and 3
#'   representing the door which the host opens with a goat behind it
#'   and was not the door originally selected by the contestant.
#'
#' @details
#'   This function uses two `if` statements to first determine
#'   if the contestant chose a door with a car behind it. If the original
#'   selection is a car, then the function randomly select one of the goat doors.
#'   If the contestant's original choice was a goat door, then the other goat door
#'   is returned.
#'
#' @param
#'   `'game` and `a.pick` are passed into the function.
#'   The `game` vector stores the door number and whether there is a
#'   goat or car associated with it. '
#'   `a.pick` is a numeric vector (length=1) that stores the contestant's
#'   original door choice (number between 1-3)
#'
#' @return
#'   The function returns numeric value representing an
#'   open door with a goat behind it.
#'
#' @examples
#'   open_goat_door( game, a.pick )
#'   open_goat_door( c("car", "goat", "goat"), 1 )
#'
#' @export
#'
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Contestant chooses to stay with original choice or change doors.
#'
#' @description
#'   `change_door()` generates the contestant's final choice.
#'   They can either stay with their original door chosen, or switch
#'   to the last unopened door.
#'
#' @details
#'   This function uses if statements that determine if the
#'   contestant stayed or changed doors. If the contestant decides to stay
#'   then the contestant's original door choice is returned as the final pick.
#'   If the contestant decides not to stay, then the final
#'   unopened door is returned as the final pick instead.
#'
#' @param
#'   `stay` - This tells the function if the contestant decides to stay
#'   with their initial pick or not. This is a TRUE/FALSE value.
#'   `opened.door` - This tells the function which goat door was opened.
#'   `a.pick` - This tells the function what the contestant's
#'   initial door pick was.
#'
#' @return
#'   The functions returns a number between 1 and 3 that represents
#'   the contestants final door pick.
#'
#' @examples
#'   change_door( stay=T, opened.door, a.pick )
#'   change_door( stay=F, 3, 1)
#'
#' @export
#'
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}


#' @title
#'   Determines if the contestant is a winner or not.
#'
#' @description
#'   `determine_winner()` uses the contestant's final door choice
#'   and combines it the knowledge of where the car is to generate
#'   whether the contestant won or lost.
#'
#' @details
#'  The function checks if the contestant's final choice was the car or the
#'  goat. It returns the string `WIN` if it was the car
#'  If it was the goat, it returns the string `LOSE`.
#'
#' @param
#'   `final.pick` - The contestant's final door choice - number 1 through 3.
#'   `game` - Vector that holds the door position of the goats
#'   and car.
#'
#' @return
#'   The function returns either the string `WIN` or `LOSE` depending
#'   on whether the contestant chose the car door or the
#'   goat door.
#'
#' @examples
#'   determine_winner()
#'
#' @export
#'
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}


#' @title
#'   Plays a single game
#'
#' @description
#'   `play_game()` generates a full game with both outcomes from either
#'   staying with the initial door, or switching doors.
#'
#' @details
#'   The function first creates a new game with the
#'   contestant's first door pick. It then opens a goat door for the h.
#'   A final pick for staying vs not staying is created.
#'   Then, it determines which the two final picks was the winner and loser. Finally,
#'   the results are returned as a data frame.
#'
#' @param
#'   ... no arguments are used by the function.
#'
#' @return
#'   The function returns a data frame showing the results of
#'   a single game with the winning and losing outcomes from
#'   staying or not staying.
#'
#' @examples
#'   play_game()
#'
#' @export
#'
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}


#' @title
#'   Runs the game a given amount of times and shows results.
#'
#' @description
#'   `play_n_games()` plays the game a number of
#'   times based on the numerical parameter passed to it.
#'   Proportional results are then shown.
#'
#' @details
#'   The function uses a `for` loop that continues based on
#'   the number parameter passed into it. The outcome are recorded in a 
#'   results list. A table showing the proportional results is then created
#'   and returned. 
#'
#' @param
#'   `n` - Input to determine the number of times the game is run.
#'
#' @return
#'   The function returns a results table showing the proportional
#'   results of `n` runs of the game.
#'
#' @examples
#'   play_n_games(5)
#'   play_n_games(90)
#'
#' @export
#'
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
