x <- 2
x
class(x)
is.numeric(x)
i <- 5L
i
class(i)
is.integer(i)
is.numeric(i)
5L/2L
class(5L/2L)
x <- "data"
x
class(x)
y <- factor("data")
y

nchar(x)
nchar("Hello")
nchar(3)
nchar(345)
nchar(y)

date1 <- as.Date("1984-12-07")
date1
class(date1)
as.numeric(date1)
date2 <- as.POSIXct("1984-12-07 15:59")
date2
class(date2)
as.numeric(date2)

TRUE
FALSE
TRUE * 5
FALSE * 5
k <- TRUE
class(k)
is.logical(k)
T
T <- 7
T
class(T)

2 == 3
2 != 3
2 < 3
2 <= 3
"date" == "stats"
"data" < "stats"
