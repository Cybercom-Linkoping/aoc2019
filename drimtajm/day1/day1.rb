require 'benchmark'
modules = File.read("input1.txt").split.map(&:to_i)

def calculate_fuel(mass)
  fuel = mass.div(3) - 2
  (fuel > 0 ? fuel : 0)
end

def calculate_total_fuel(modules)
  total_fuel = 0
  for f in modules
    while f > 0
      f = calculate_fuel(f)
      total_fuel += f
    end
  end
  total_fuel
end

time = Benchmark.measure {
  puts calculate_total_fuel modules
}
puts time.real
