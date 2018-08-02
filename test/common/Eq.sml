structure Eq =
struct
  fun pair (x, y) ((a, b), (c, d)) = x (a, c) andalso y (b, d)
end
