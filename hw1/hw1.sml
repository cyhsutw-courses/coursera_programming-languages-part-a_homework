fun is_older(x: int*int*int, y: int*int*int) =
  if #1(x) < #1(y)
  then true
  else
    if #1(x) > #1(y)
      then false
      else
        if #2(x) < #2(y)
        then true
        else
          if #2(x) > #2(y)
          then false
          else #3(x) < #3(y)

fun number_in_month(xs: (int * int * int) list, y: int) =
  if null xs
  then 0
  else number_in_month(tl(xs), y) + (if #2(hd(xs)) = y then 1 else 0)

fun number_in_months(xs: (int * int * int) list, ys: int list) =
  if null ys
  then 0
  else number_in_month(xs, hd(ys)) + number_in_months(xs, tl(ys))


fun dates_in_month(xs: (int * int * int) list, y: int) =
  if null xs
  then []
  else
    if #2(hd(xs)) = y
    then hd(xs) :: dates_in_month(tl(xs), y)
    else dates_in_month(tl(xs), y)

fun dates_in_months(xs: (int * int * int) list, ys: int list) =
  if null ys
  then []
  else dates_in_month(xs, hd(ys)) @ dates_in_months(xs, tl(ys))

fun get_nth(xs: string list, y: int) =
  if y = 1
  then hd(xs)
  else get_nth(tl(xs), y - 1)


fun date_to_string(x: int * int * int) =
  let
    val mos = ["January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November", "December"]
  in
    get_nth(mos, #2(x)) ^ " " ^ Int.toString(#3(x)) ^ ", " ^ Int.toString(#1(x))
  end

fun number_before_reaching_sum(sum: int, xs: int list) =
  let
    val remaining = sum - hd(xs)
  in
    if remaining <= 0
    then 0
    else 1 + number_before_reaching_sum(remaining, tl(xs))
  end

fun what_month(day: int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30,
                         31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_in_month) + 1
  end

fun month_range(from: int, to: int) =
  if to < from
  then []
  else
    what_month(from) :: month_range(from + 1, to)

fun oldest(xs: (int * int * int) list) =
  if null xs
  then NONE
  else
    let
      fun oldest_nonempty(xs: (int * int * int) list) =
        if null(tl(xs))
        then hd(xs)
        else
          let
            val suboldest_nonempty = oldest_nonempty(tl(xs))
          in
            if is_older(hd(xs), suboldest_nonempty)
            then hd(xs)
            else suboldest_nonempty
          end
      in
        SOME(oldest_nonempty(xs))
      end
