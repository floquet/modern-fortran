module TestMod
contains
  function Add(val1, val2) result(sum1)
    real val1, val2
    sum1 = val1 + val2
  end function
end module
