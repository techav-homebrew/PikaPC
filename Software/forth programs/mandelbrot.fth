decimal

variable I3
variable R3
variable Z1
variable Z2
variable N
variable A
variable B

: mandel
  21 0 do
    95 i * -1000 + I3 !
    59 0 do
      i 50 * -2000 + dup R3 ! Z1 !
      I3 @ Z2 !
      0 N !
      30 0 do
        Z1 @ dup * 1000 / A ! Z2 @ dup * 1000 / B !
        A @ B @ + 4000 - 0x7fffffff > if 
        2 Z1 @ * Z2 @ * 1000 / I3 @ + Z2 !
        A @ B @ - R3 @ + Z1 !
        N @ 1 + N !
        then
      loop
      62 N @ - emit
    loop
    cr
  loop
;

: run begin mandel cr 1 0= until ;
