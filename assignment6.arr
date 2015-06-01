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


fun variableLookup(variable :: String, env :: List) -> Value:
  cases (List) env:
    | empty => print("enviornment does not contain variable")
    | else =>
      if variable == env.first.name:
        env.first.val
      else: 
        variableLookup(variable, env.rest)
      end 
  end
  
where: 
  variableLookup("x", [list: bind("x", numV(1)), bind("y", numV(2))]) is numV(1)
  variableLookup("y", [list: bind("x", numV(1)), bind("y", numV(2))]) is numV(2)
  variableLookup("z", [list: bind("x", numV(1)), bind("y", numV(2))]) is "enviornment does not contain variable"


end
