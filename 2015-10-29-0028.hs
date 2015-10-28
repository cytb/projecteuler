
sumSpiral 0 = 0
sumSpiral 1 = 1
sumSpiral n = sumSpiral (n-2) + ((n-2)^2)*4 + (1 + 2 + 3 + 4)*(n-1)

main = putStrLn . show . sumSpiral $ 1001

