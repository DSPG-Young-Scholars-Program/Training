# Install the RVerbalExpressions package if necessary
devtools::install_github("VerbalExpressions/RVerbalExpressions")

# Load library
library(RVerbalExpressions)


# We’ve got multiple texts with numbers and we’d like to extract the names from them
strings = c('123Abdul233','233Raja434','223Ethan Hunt444')

# Get letters
expr =  rx_alpha()
stringr::str_extract_all(strings,expr) 

# Get numbers
expr =  rx_digit() 
stringr::str_extract_all(strings,expr) 

# Get names
expr =  rx_alpha()  %>%  rx_word() %>% rx_alpha() 
stringr::str_extract_all(strings,expr) 

# More advanced building of a regex statement
# constructing an expression for a url validator
x <- rx_start_of_line() %>% 
  rx_find('http') %>% 
  rx_maybe('s') %>% 
  rx_find('://') %>% 
  rx_maybe('www.') %>% 
  rx_anything_but(' ') %>% 
  rx_end_of_line()

# print the expression
x

# test for a match
grepl(x, "https://www.google.com")
