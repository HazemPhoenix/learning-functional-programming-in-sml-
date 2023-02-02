val x = {str = "hello", tuple= (1,2,3) , int = 2}

datatype mytype = TwoInts of int * int
                | str of string 
                | pizza


fun f x = 
    case x of 
        TwoInts(i1, i2) => i1 + i2
    |   str s => String.size s
    |   pizza => 7


datatype suit = Club   
            |   Diamond
            |   Spade
            |   Heart


datatype exp = Constant of int
            |  Negate   of exp 
            |  Add      of exp * exp
            |  Multiply of exp * exp


val x= Constant 1 

fun eval e = 
    case e of 
        Constant i      => i
    |   Negate e2       => ~ (eval e2)
    |   Add(e1,e2)      => (eval e1) + (eval e2)
    |   Multiply(e1,e2) => (eval e1) * (eval e2)


fun max_constant e =
    case e of 
        Constant i      => i
    |   Negate e1       => max_constant e1
    |   Add(e1,e2)      => Int.max(max_constant e1, max_constant e2)
    |   Multiply(e1,e2) => Int.max(max_constant e1, max_constant e2)


val test = Add(Constant 5, Multiply(Multiply(Constant 10, Constant 7), Constant 2))
val thirty_five = max_constant test 

Cons(4, Empty)

fun sum_tree tr =
    case tr of 
        Leaf i = i
        Node(i, lft, rgt) => i + sum_tree lft + sum_tree rgt

fun sum_leaves tr =
    case tr of 
        Leaf i = i 
        Node(i, lft, rgt) => sum_leaves lft + sum_leaves rgt

fun num_leaves tr =
    case tr of 
        Leaf i => 1 
        Node(i, lft, rgt) => num_leaves lft + num_leaves rgt 

val name = {first="Hazem", second="Abdulmoneim", third="Mohamed"}

fun fullname{first=f, second=s, third=t} = 
    f ^ " " ^ s ^ " " ^ t

fullname name 

fun sum_triple(x, y, z) = 
    x + y + z


sum_triple(1,2,3)