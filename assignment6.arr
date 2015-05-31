data ExprC:
  | numC(n :: Number)
  | idC(s :: String)
  | booleanC(b :: Boolean)
  | appC(func :: ExprC, args :: List )
  | ifC(test :: ExprC, iff :: ExprC, then :: ExprC)
  | binopC(sym :: String, l :: ExprC, r :: ExprC)
  | lamC(args :: List, body :: ExprC)
end

data Value:
  | numV(n :: Number)
  | booleanV(b :: Boolean)
  | closV(args :: List, body :: ExprC, env :: List)
end

data Binding:
  | bind(name :: String, val :: Value)
end
