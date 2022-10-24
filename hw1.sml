(* is_older *)
(* evaluates to true if first argument is a date that comes before the second argument *)
(* val test1 = is_older ((1,2,3),(2,3,4)) = true *)
(* val test2 = is_older ((1990,2,3),(2002,3,4)) = true *)
(* val test2 = is_older ((2002,2,3),(2002,3,4)) = true *)
(* val test2 = is_older ((2002,5,3),(2002,3,4)) = false *)
fun is_older (x: int*int*int, y: int*int*int) =
  (#1 x) < (#1 y)
  orelse (#1 x) = (#1 y) andalso (#2 x) < (#2 y)
  orelse (#1 x) = (#1 y) andalso (#2 x) = (#2 y) andalso (#3 x) < (#3 y)

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
(* takes list of strings and int n; returns the nth element of the list where head is 1st *)
(* val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there" *)
fun get_nth (strings: string list, index: int ) =
  let fun nth(string_list: string list, idx: int) =
    if idx = index
    then hd string_list
    else nth((tl string_list), idx + 1)
  in
    nth(strings, 1)
  end

(* date_to_string *)
(* val test7 = date_to_string (2013, 6, 1) = "June 1, 2013" *)
fun date_to_string (date: int*int*int) =
  let val months = [
      "January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"
    ]
  in
    get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* number_before_reaching_sum *)
(* val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3 *)
fun number_before_reaching_sum (sum: int, numbers: int list) =
  let fun find_num (index: int, total: int, nums: int list) =
    if (total + hd(nums)) >= sum
    then index - 1
    else find_num(index + 1, total + hd(nums), tl(nums))
  in
    find_num(1, 0, numbers)
  end


(* what_month *)
(* val test9 = what_month 70 = 3 *)
fun what_month (day: int) =
  let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, months) + 1
  end

(* month_range *)
(* val test10 = month_range (31, 34) = [1,2,2,2] *)
fun month_range (day1: int, day2: int) =
  let fun make_range(day: int) =
    if day >= day2
    then [what_month(day2)]
    else what_month(day) :: make_range(day + 1)
  in
    make_range(day1)
  end

(* oldest *)
(* val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) *)
fun oldest (dates: (int*int*int) list) =
  if null dates
  then NONE
  else
    let fun max_nonempty (date_list: (int*int*int) list) =
      if null (tl date_list)
      then hd date_list
      else let val tl_ans = max_nonempty(tl date_list)
    in
      if (is_older((hd date_list), tl_ans))
      then hd date_list
      else tl_ans
    end
  in
    SOME (max_nonempty dates)
  end
