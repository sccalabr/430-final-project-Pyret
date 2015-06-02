import s-exp as S
data ExprC:
  | numC(n :: Number)
  | idC(s :: String)
  | boolC(b :: Boolean)
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

fun num-pl(l :: Value, r :: Value) -> Value:
  cases (Value) l:
    | numV(n) => 
      cases (Value) r:
        | numV(n2) =>
          numV(l.n + r.n)
        | boolV(b) => raise("invalid input")
        | cloV(args, body, env) => raise("invalid input")
      end
    | boolV(b) => raise("invalid input")
    | cloV(args, body, env) => raise("invalid input")
  end  
  
where:
  num-pl(numV(12), numV(2)) is numV(14)
  num-pl(boolV(true), numV(4)) raises("invalid input")
  num-pl(numV(4), boolV(true)) raises("invalid input")
end

fun num-mi(l :: Value, r :: Value) -> Value:
  cases (Value) l:
    | numV(n) => 
      cases (Value) r:
        | numV(n2) =>
          numV(l.n - r.n)
        | boolV(b) => raise("invalid input")
        | cloV(args, body, env) => raise("invalid input")
      end
    | boolV(b) => raise("invalid input")
    | cloV(args, body, env) => raise("invalid input")
  end  
  
where:
  num-mi(numV(12), numV(2)) is numV(10)
  num-mi(boolV(true), numV(4)) raises("invalid input")
  num-mi(numV(4), boolV(true)) raises("invalid input")
end

fun num-mu(l :: Value, r :: Value) -> Value:
  cases (Value) l:
    | numV(n) => 
      cases (Value) r:
        | numV(n2) =>
          numV(l.n * r.n)
        | boolV(b) => raise("invalid input")
        | cloV(args, body, env) => raise("invalid input")
      end
    | boolV(b) => raise("invalid input")
    | cloV(args, body, env) => raise("invalid input")
  end  
  
where:
  num-mu(numV(12), numV(2)) is numV(24)
  num-mu(boolV(true), numV(4)) raises("invalid input")
  num-mu(numV(4), boolV(true)) raises("invalid input")
end

fun num-di(l :: Value, r :: Value) -> Value:
  cases (Value) l:
    | numV(n) => 
      cases (Value) r:
        | numV(n2) =>
          numV(l.n / r.n)
        | boolV(b) => raise("invalid input")
        | cloV(args, body, env) => raise("invalid input")
      end
    | boolV(b) => raise("invalid input")
    | cloV(args, body, env) => raise("invalid input")
  end  
  
where:
  num-di(numV(12), numV(2)) is numV(6)
  num-di(boolV(true), numV(4)) raises("invalid input")
  num-di(numV(4), boolV(true)) raises("invalid input")
end


fun extendEnviornment(newBinds :: List, env :: List) -> List:
  cases (List) newBinds:
    | empty => env
    | link(f, r) => 
      link(f, extendEnviornment(r, env))
  end
  
where:
  extendEnviornment([list: bind("x", numV(1)), bind("y", numV(2))], 
    [list: bind("z", numV(3))])  
    is [list: bind("x", numV(1)), bind("y", numV(2)), bind("z", numV(3))]  
  
  
end

fun evalArguments(param :: List, args :: List, env :: List) -> List:
  cases (List) args:
    | empty => [list: ]
    | link(f1, r1) =>
      cases (List) param:
        | link(f2, r2) =>
            link(bind(f2, interp(f1, env)), evalArguments(r2, r1, env))
      end
  end

where:
  evalArguments([list: "x", "y"], [list: numC(1), numC(2)], [list: ]) is
  [list: bind("x", numV(1)), bind("y", numV(2))]  
end


fun interp(e :: ExprC, env :: List) -> Value:
  cases (ExprC) e:
    | numC(n) => numV(n)
    | idC(s) => variableLookup(s, env) 
    | boolC(b) => boolV(b)
    | ifC(test, iff, then) => 
      if test.b == true:
        interp(iff, env)
      else: 
        interp(then, env)
      end
    | binopC(s, l, r) =>
      if (s == "+"):
        num-pl(interp(l, env), interp(r, env))
      else if (s == "-"):
        num-mi(interp(l, env), interp(r, env))
      else if (s == "*"):
        num-mu(interp(l, env), interp(r, env))
      else if (s == "/"):
        num-di(interp(l, env), interp(r, env))  
      else:
        raise("invalid input")
      end
    | appC(f, args)=> 
      cases (Value) interp( f, env):
        | closV(param, body, cloEnv) => 
          interp(body, extendEnviornment(evalArguments(param, args, env), env))
        | else => raise("funDef not eval to closure")
      end  
    | lamC(a, b)=> closV(a, b, env)
  end
  
where:
  interp(numC(1), [list: ]) is numV(1)
  interp(boolC(true), [list: ]) is boolV(true)
  interp(idC("x"), [list: bind("x", numV(1)), bind("y", numV(2))]) is numV(1)
  interp(numC(2), [list: bind("x", numV(1)), bind("y", numV(2))]) is numV(2)
  interp(ifC(boolC(true), numC(5), numC(6)), [list: bind("x", numV(1)), bind("y", numV(2))]) is numV(5)
  interp(ifC(boolC(false), numC(5), numC(6)), [list: bind("x", numV(1)), bind("y", numV(2))]) is numV(6)
  interp(binopC("+", numC(5), numC(6)), [list: ]) is numV(11)
  interp(binopC("+", numC(-12), numC(6)), [list: bind("a", numV(1)), bind("b", numV(2))]) is numV(-6)
  interp(binopC("-", numC(5), numC(6)), [list: ]) is numV(-1)
  interp(binopC("-", numC(-12), numC(6)), [list: bind("a", numV(1)), bind("b", numV(2))]) is numV(-18)
  interp(binopC("*", numC(5), numC(6)), [list: ]) is numV(30)
  interp(binopC("*", numC(-12), numC(6)), [list: bind("a", numV(1)), bind("b", numV(2))]) is numV(-72)
  interp(binopC("/", numC(30), numC(6)), [list: ]) is numV(5)
  interp(binopC("/", numC(-12), numC(6)), [list: bind("a", numV(1)), bind("b", numV(2))]) is numV(-2)
  
  interp(appC(lamC([list: "x", "y"], binopC("+", idC("x"), idC("y"))), [list: numC(5), numC(10)]), [list: ])
  
end  

fun parse(s :: S.S-Exp) -> ExprC:
  cases (S.S-Exp) s:
    | s-num(n) => numC(n)
    | else =>
      raise("parse: not number or list")
  end
end
