mydata <- read.table("input1.txt")

calculate.fuel <- function(mass) {
  fuel <- (mass %/% 3) - 2
  fuel * (fuel > 0)
}

calculate.total.fuel <- function(mass) {
  total.fuel <- integer(length(mass))
  fuel <- mass
  while (any(fuel > 0)) {
    fuel <- calculate.fuel(fuel)
    total.fuel <- total.fuel + fuel
  }
  print(sum(total.fuel))
}

calculate.total.fuel(mydata)

#> system.time(source("ex1b.r"))
#[1] 4940279
#   user  system elapsed 
#  0.015   0.000   0.016 
