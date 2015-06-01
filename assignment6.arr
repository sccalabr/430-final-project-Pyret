data ExprC:
  | numC(n :: Number)
  | idC(s :: String)
  | bool(b :: Boolean)
  | appC(func :: ExprC, args :: List )
  | ifC(test :: ExprC, iff :: ExprC, then :: ExprC)
  | binopC(sym :: String, l :: ExprC, r :: ExprC)
  | lamC(args :: List, body :: ExprC)
end

data Value:
  | numV(n :: Number)
  | boolV(b :: Boolean)
  | closV(args :: List, body :: ExprC, env :: List)
end

data Binding:
  | bind(name :: String, val :: Value)
end

fun variableLookup(variable :: String, env :: List) -> Value:
  cases (List) env:
    | empty => raise("enviornment does not contain variable")
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
  variableLookup("z", [list: bind("x", numV(1)), bind("y", numV(2))]) raises "enviornment does not contain variable"
end

fun check-binop(symbol :: String) -> Boolean:
  if (symbol == "+")
    or (symbol == "-")
    or (symbol == "*")
    or (symbol == "/")
    or (symbol == "<=")
    or (symbol == "eq"):
    true
  else:
    false
  end
where:
check-binop("+") is true
check-binop("ere") is false
check-binop("*") is true
end



fun interp(e :: ExprC, env :: Binding) -> Value:
  cases (ExprC) e:
    | numC(n) => numV(1)
    | idC(s) => numV(1)
    | bool(b) => numV(1)
    | binop(s, l, r) => numV(1)
    | appC(f, a)=> numV(1)
    | lamC(a, b)=> numV(1)

  end
end
