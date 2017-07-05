let rec forloop f n m =
  if n <= m then
    begin
      f n;
      forloop f (n + 1) m
    end
