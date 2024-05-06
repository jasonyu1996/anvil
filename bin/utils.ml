let rec int_log2 (v : int) : int =
  if v < 1 then
    0
  else
    1 + (int_log2 (v / 2))

