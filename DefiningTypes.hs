data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show)

daysInMonth :: Month -> Integer -> Integer
daysInMonth January  _ = 31
daysInMonth February year
    | mod year 4 == 0  = 29
    | otherwise         = 28
daysInMonth March    _ = 31
daysInMonth April    _ = 30
daysInMonth May      _ = 31
daysInMonth June     _ = 30
daysInMonth July     _ = 31
daysInMonth August   _ = 31
daysInMonth September _ = 30
daysInMonth October  _ = 31
daysInMonth November _ = 30
daysInMonth December _ = 31

data Date y m d = Date {year :: y, month :: m, day :: d}

validDate :: Date Integer Month Integer -> Bool
validDate date = day date > 0 && day date <= daysInMonth (month date) (year date)