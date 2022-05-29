class Expr:
  def __init__(self, description, *args):
    assert isinstance(description, str)
    for arg in args:
      assert isinstance(arg, (Expr, int, float, complex))
    self.arguments = tuple(arg if isinstance(arg, Expr) else Const(arg) for arg in args)
    self.description = description

  def __str__(self):
    return "(" + self.description + ", " + ", ".join([str(arg) for arg in self.arguments]) + ")"
  
  __repr__ = __str__

  def __hash__(self):
    return hash((self.description, *self.arguments))
  
  def __eq__(self, other):
    assert isinstance(other, Expr)
    return self.arguments == other.arguments and self.description == other.description

  def dependencies(self):
    dependencies = set()
    for arg in self.arguments:
      dependencies = dependencies.union(arg.dependencies())
    return dependencies
  
  def __add__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Add(self, other)

  def __radd__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Add(other, self)

  def __sub__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Add(self, Neg(other))

  def __rsub__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Add(other, Neg(self))

  def __mul__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Mul(self, other)

  def __rmul__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Mul(other, self)

  def __truediv__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Div(self, other)

  def __rtruediv__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Div(other, self)

  def __pow__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Pow(self, other)

  def __rpow__(self, other):
    assert isinstance(other, (Expr, int, float, complex))
    return Pow(other, self)

  def __neg__(self):
    return Neg(self)
  
  def simplifiedArguments(self):
    return [arg.simplified() for arg in self.arguments]
  
  def simplified(self):
    return self

  def factors(self):
    return set([self])



class Var(Expr):
  def __init__(self, name):
    assert isinstance(name, str)
    super().__init__("var")
    self.name = name
  
  def __hash__(self):
    return hash(self.name)

  def __eq__(self, other):
    assert isinstance(other, Var)
    return self.name == other.name

  def __str__(self):
    return self.name
  __repr__ = __str__

  def dependencies(self):
    return set([self])

  def derivative(self, var):
    assert isinstance(var, Var)
    return int(self.name == var.name)



class Const(Expr):
  def __init__(self, value, name = None):
    assert name != None or value != None
    assert isinstance(name, str) or name == None
    assert isinstance(value, (int, float, complex)) or value == None
    super().__init__("const")
    self.name = name
    self.value = value
  
  def __hash__(self):
    return hash((self.name, self.value))
  
  def __eq__(self, other):
    assert isinstance(other, Const)
    return self.name == other.name and self.value == other.value

  def __str__(self):
    return self.name if self.name != None else str(self.value)
  __repr__ = __str__

  def derivative(self, var):
    assert isinstance(var, Var)
    return Const(0)

  def __neg__(self):
    if self.name == None:
      return Const(- self.value)
    else:
      return Neg(self)
  
  def factors(self):
    if isinstance(self.value, int):
      factors = []
      for i in range(1, self.value + 1):
        if self.value % i == 0:
          factors.append(i)
      return set([Const(i) for i in factors])
    return set([self])



class Add(Expr):
  def __init__(self, *args):
    super().__init__("+", *args)

  def derivative(self, var):
    assert isinstance(var, Var)
    return Add(*[arg.derivative(var) for arg in self.arguments])
  
  def simplified(self):
    args = []
    const = 0
    for arg in self.simplifiedArguments():
      if isinstance(arg, Const):
        if arg.value != None:
          const += arg.value
        else:
          args.append(arg)
      elif isinstance(arg, Add):
        for secArg in arg.arguments:
          if isinstance(secArg, Const):
            if secArg.value != None:
              const += secArg.value
            else:
              args.append(secArg)
          else:
            args.append(secArg)
      else:
        args.append(arg)
    if const != 0:
      args.append(Const(const))
    if len(args) == 0:
      return Const(0)
    if len(args) == 1:
      return args[0]
    return Add(*args)

  def factors(self):
    if len(self.arguments) == 0:
      return set()
    return self.arguments[0].factors().intersection(*[arg.factors() for arg in self.arguments])
    


class Mul(Expr):
  def __init__(self, *args):
    super().__init__("*", *args)

  def derivative(self, var):
    assert isinstance(var, Var)
    terms = []
    for i, arg in enumerate(self.arguments):
      terms.append(Mul(*self.arguments[:i], arg.derivative(var), *self.arguments[i + 1:]))
    return Add(*terms)

  def simplified(self):
    args = []
    const = 1
    for arg in self.simplifiedArguments():
      if isinstance(arg, Const):
        if arg.value != None:
          const *= arg.value
        else:
          args.append(arg)
      elif isinstance(arg, Mul):
        for secArg in arg.arguments:
          if isinstance(secArg, Const):
            if secArg.value != None:
              const *= secArg.value
            else:
              args.append(secArg)
          else:
            args.append(secArg)
      else:
        args.append(arg)
    if const == 0:
      return Const(0)
    if const != 1:
      args.append(Const(const))
    if len(args) == 0:
      return Const(1)
    if len(args) == 1:
      return args[0]
    return Mul(*args)

  def factors(self):
    if len(self.arguments) == 0:
      return set()
    return self.arguments[0].factors().union(*[arg.factors() for arg in self.arguments])



class Div(Expr):
  def __init__(self, *args):
    assert len(args) == 2
    super().__init__("/", *args)

  def derivative(self, var):
    assert isinstance(var, Var)
    return (self.arguments[1] * self.arguments[0].derivative(var) - self.arguments[0] * self.arguments[1].derivative(var)) / Pow(self.arguments[1], 2)



class Pow(Expr):
  def __init__(self, *args):
    assert len(args) == 2
    super().__init__("^", *args)

  def derivative(self, var):
    assert isinstance(var, Var)
    return Exp(Log(self.arguments[0]) * self.arguments[1]).derivative(var)



class Neg(Expr):
  def __init__(self, *args):
    assert len(args) == 1
    super().__init__("neg", *args)

  def derivative(self, var):
    assert isinstance(var, Var)
    return Neg(self.arguments[0].derivative(var))



class Func(Expr):
  def __init__(self, name, argument):
    assert isinstance(name, str)
    super().__init__(name, argument)

  def derivative(self, var):
    assert isinstance(var, Var)
    return Func(self.description + "'", *self.arguments) * self.arguments[0].derivative(var)



class Sin(Func):
  def __init__(self, argument):
    super().__init__("sin", argument)

  def derivative(self, var):
    assert isinstance(var, Var)
    return Cos(*self.arguments) * self.arguments[0].derivative(var)



class Cos(Func):
  def __init__(self, argument):
    super().__init__("cos", argument)

  def derivative(self, var):
    assert isinstance(var, Var)
    return - Sin(*self.arguments) * self.arguments[0].derivative(var)



class Exp(Func):
  def __init__(self, argument):
    super().__init__("exp", argument)

  def derivative(self, var):
    assert isinstance(var, Var)
    return Exp(*self.arguments) * self.arguments[0].derivative(var)



class Log(Func):
  def __init__(self, argument):
    super().__init__("log", argument)

  def derivative(self, var):
    assert isinstance(var, Var)
    return self.arguments[0].derivative(var) / self.arguments[0]