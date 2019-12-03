load("input1.txt")

function fuel = calculateFuel1(mass)
  x = idivide(mass, 3, "floor") -2;
  fuel = x.*(x > 0);
end

function total_fuel = calculateFuel(mass)
  n = size(mass);
  fuel = mass;
  total_fuel = zeros(n);
  do
    fuel = calculateFuel1(fuel);
    total_fuel += fuel;    
  until (all(fuel == 0))
 end
 
sum(calculateFuel(input1))