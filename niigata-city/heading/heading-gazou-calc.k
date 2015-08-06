-*- koshu -*-

|== HEADING : csv
  --order

|== TERM : all-terms
  | hier /pos -to /term
  | add /names ( sort   of to-set /term/name )
        /n     ( length of /term/name )
  | pick /pos /n /names
  --order

all-terms : term /1 1
  | join ( term /2 2 )
  | join ( term /3 3 )
  | join ( term /4 4 )
  | join ( term /5 5 )
  | join ( term /6 6 )
  | join ( term /7 7 )
  | join ( term /8 8 )
  | join ( term /9 9 )
  | join ( term /10 10 )
  | join ( term /11 11 )
  | join ( term /12 12 )
  | join ( term /13 13 )
  | join ( term /14 14 )
  | join ( term /15 15 )
  | join ( term /16 16 )
  | join ( term /17 17 )
  | join ( term /18 18 )
  | join ( term /19 19 )

term : csv | pick @'1 | move @'1 -to /name | add /pos @'2

csv : source CSV /1 /2 /3 /4 /5 /6 /7 /8 /9 /10 /11 /12 /13 /14 /15 /16 /17 /18 /19
