require(tidyverse)

fruit

fruit[str_detect(fruit,'a')]
str_which(fruit,'a')

# detect match -------
fruit %>% str_detect('a')
fruit %>% str_which('a')
fruit %>% str_count('a')
fruit %>% str_locate('a')
fruit %>% .[4] %>% str_locate_all('a')

# subset -----
fruit %>% str_sub(2,3)
fruit %>% str_subset('a')
# same with 
fruit[fruit %>% str_detect('a')]
fruit %>% str_extract('[aieou]')
fruit %>% str_extract_all('[aieou]')
fruit %>% str_match('(a|ho)*(b)')
fruit %>% str_match_all('(a|ho)*(b)')

# length ----
fruit %>% str_length()
fruit %>% str_pad(18)
fruit %>% str_trunc(5)
'  fruit  ' %>% str_trim(side = 'left') # both, left, right

# mutate -------
fruit2 = fruit
fruit2 %>% str_sub(2,3)
str_sub(fruit2, 2,3) = 'xy'
fruit2 %>% str_replace('ate', 'eta')
fruit2 %>% str_replace_all('ate', 'eta')
fruit2 %>% str_to_lower()
fruit2 %>% str_to_upper()
fruit2 %>% str_to_title()

# join split -----
fruit2 %>% str_c(fruit2)
fruit2 %>% str_c('_tail')
fruit2 %>% str_c(collapse = '')
fruit2 %>% str_dup(4)
fruit2 %>% str_split_fixed('a', 3)
fruit2 %>% str_split('a',3)

glue::glue('Pi is {pi}')
glue::glue('{fruit2}', sep = '') # a bit like python in this one

# order -------
fruit[fruit %>% str_order(decreasing = T)]
# same with
fruit %>% str_sort(decreasing = T)

# helper -------
fruit %>% str_conv('ISO-8859-1')
fruit %>% str_conv('UTF-8')
fruit %>% str_view('a')
fruit %>% str_view_all('a')
fruit %>% str_wrap(5)

# regex
see = function(x) str_view_all('abc ABC 123\t.!?\\(){}\n', x)
see('a')
see('\\.')
see('\\!')
see('\\\\')
see('\\(')
see('\\n')
see('\\t')
see('\\s')
see('\\d')
see('\\D')
see('\\w')
see('\\W')
see('\\b')
see('[:digit:]')
see('[:alpha:]')
see('[:lower:]')
see('[:upper:]')
see('[:alnum:]')
see('[:punct:]')
see('[:graph:]')
see('[:space:]')
see('[:blank:]')
see('.')

# interpretation -----
str_detect("I", regex("i", TRUE))
str_detect('\u0130',fixed('i'))
str_detect("\u0130", coll("i", TRUE, locale = "tr"))
str_split(sentences, boundary("word"))

# alternative ------
alt <- function(rx) str_view_all("abcde", rx)
alt("ab|d") 
alt("[abe]") 
alt("[^abe]")
alt("[a-c]") 

# quantifier -----
quant <- function(rx) str_view_all(".a.aa.aaa", rx)
quant("a?")                         
quant("a*")
quant("a+")
quant("a{2}")
quant("a{2,}")
quant("a{2,4}")

# anchor -----
anchor <- function(rx) str_view_all("aaa", rx)
anchor("^a")
anchor("a$")

# look around ------
look <- function(rx) str_view_all("bacad", rx)
look("a(?=c)") 
look("a(?!c)")
look("(?<=b)a") 
look("(?<!b)a")

# group ----
ref <- function(rx) str_view_all("abbaab", rx)
alt("(ab|d)e")
ref("(a)(b)\\2\\1")

