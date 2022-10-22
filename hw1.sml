(* is_older *)
(* evaluates to true if first argument is a date that comes before the second argument *)
(* val test1 = is_older ((1,2,3),(2,3,4)) = true *)
fun is_older (x : int*int*int, y : int*int*int) =
  (#3 x) < (#3 y) orelse (#2 x) < (#2 y) orelse (#1 x) < (#1 y)

(* number_in_month *)
(* takes a list of dates and a month; returns how many dates in the list are in the given month *)
(* val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1 *)
fun number_in_month (dates: (int * int * int) list, month: int) =
  let fun count (date_list: (int * int * int) list) =
    if null date_list
    then 0
    else if (#2 (hd date_list)) = month
    then 1 + count(tl date_list)
    else 0 + count(tl date_list)
  in
    count(dates)
  end

(* number_in_months *)
(* val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3 *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
  let fun count (month_list: int list) =
    if null month_list
    then 0
    else number_in_month(dates, (hd month_list)) + count((tl month_list))
  in
    count(months)
  end

(* dates_in_month *)
(* val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)] *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
  let fun find_dates (date_list: (int * int * int) list) =
    if null date_list
    then []
    else if (#2 (hd date_list)) = month
    then (hd date_list) :: find_dates(tl date_list)
    else find_dates(tl date_list)
  in
    find_dates(dates)
  end

(* dates_in_months *)
(* val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
  let fun find_dates (month_list: int list) =
    if null month_list
    then []
    else dates_in_month(dates, (hd month_list)) @ find_dates(tl month_list)
  in
    find_dates(months)
  end

(* get_nth *)
(* val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there" *)
fun get_nth (x,y) = true

(* date_to_string *)
(* val test7 = date_to_string (2013, 6, 1) = "June 1, 2013" *)
fun date_to_string (x,y) = true

(* number_before_reaching_sum *)
(* val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3 *)
fun number_before_reaching_sum (x,y) = true

(* what_month *)
(* val test9 = what_month 70 = 3 *)
fun what_month (x,y) = true

(* month_range *)
(* val test10 = month_range (31, 34) = [1,2,2,2] *)
fun month_range (x,y) = true

(* oldest *)
(* val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) *)
fun oldest (x,y) = true
