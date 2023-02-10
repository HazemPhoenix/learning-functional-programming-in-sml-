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

exception ListLengthMismatch

fun zip3 list_triple =
    case list_triple of 
        ([],[],[]) => []
      | (x::xs, y::ys, z::zs) => (x,y,z) :: zip3(xs,ys,zs)
      | _                     => raise ListLengthMismatch

zip3([1,3,5],[2,4,6],[3,5,7])

fun unzip3 lst = 
    case lst of 
        [] => ([],[],[])
      | (x,y,z)::tl => let val (l1,l2,l3) = unzip3 tl 
                       in (x::l1,y::l2,z::l3) 
                       end

unzip3[(1,2,3),(3,4,5),(5,6,7)]

fun nondecreasing xs =
    case xs of 
        []     => true 
       | x::[] => true 
       | x::y::rest => x <= y 
                       andalso nondecreasing(y::rest)

nondecreasing[1,2,4,3,5]

fun fact n =
    let 
        fun helper(n, acc) = 
            if(n = 0) then acc
            else helper(n - 1, acc * n)
    in 
        helper(n, 1)
    end


fun n_times(f, n, x) =
    if n = 0 then x
    else f (n_times(f, n-1, x))


fun triple_n_times(n, x) = n_times(fn a => 3 * a, n, x)

fun map(f, xs) = 
    case xs of 
        [] => []
        | x::xs' => (f x) :: map(f, xs')

(*(a' -> b') * a' list -> b' list*)

map(fn a => 2 * a, [1,2,3,4])


fun filter(f, xs) =
    case xs of 
        [] => []
        | x::xs' => if f x 
                    then x :: filter(f, xs')
                    else filter(f, xs')

(*(a' -> boolean) * a' list -> a' list*)

fun is_more_than_three x = x > 3 
fun is_less_than_three x = x < 3 

val less = filter(fn x => x < 3, [1,2,3,4,5,6])
val more = filter(fn x => x > 3, [1,2,3,4,5,6])


fun fold f acc xs = 
    case xs of 
        [] => acc
        | x::xs' => fold f (f(acc, x)) xs'
        

(*
fun range x y = 
    if x = y then [x]
    else if x > y then x :: range (x - 1) y
    else x :: range (x + 1) y
            *)

fun range(x, y) = 
    if x = y then [x]
    else if x > y then x :: range(x - 1, y)
    else x :: range(x + 1, y)

fun curry f = fn x => fn y => f(x, y)
(curry range) 3 7

fun sum xs = fold (fn(a,b) => a + b) 0 xs
fun multiply xs = fold (fn(a,b) => a * b) 1 xs
fun all_positive xs = fold ((fn(a,b) => a andalso b >= 0)) true xs

val sum2 = fold (fn(a,b) => a + b) 0
sum2 [1,2,3]

val multiply2 = fold (fn(a,b) => a * b) 1
multiply2[1,2,3,4,5]

val all_positive2 = fold (fn (a,b) => a andalso b >= 0) true
all_positive2[1,2,~3,4,5]

(*fun sorted3 x = fn y => fn z => z >= y andalso y >= x*)
fun sorted3 x y z = z >= y andalso y >= x

val test_sort1 = sorted3 2 4 6
val test_sort2 = sorted3 2 6 4
val fewer_args = sorted3 2 4 
fewer_args 6
fewer_args 3

fun range x y = 
    if x = y then [x]
    else if x > y then x :: range (x - 1) y
    else x :: range (x + 1) y

range 10 2
range 2 10

val count_from1 = range 1 
count_from1 20


fun no_mutual_recursion xs = 
    case xs of 
        [] => true 
        | x::[] => false
        | x::y::xy => if x < y 
                        then no_mutual_recursion xy
                        else false


fun mutual_recursion xs = 
    let fun s_need_one xs =
            case xs of 
            [] => true
            |1::xs' => s_need_two xs'
            |_ => false
        and s_need_two xs = 
            case xs of 
            [] => false 
            |2::xs' => s_need_one xs'
            |_ => false 
    in 
        s_need_one xs
    end

val test1 = mutual_recursion [] = no_mutual_recursion []
val test2 = mutual_recursion [1] = no_mutual_recursion [1]
val test3 = mutual_recursion [1,2] = no_mutual_recursion [1,2]
val test4 = mutual_recursion [1,2,1] = no_mutual_recursion [1,2,1]
val test5 = mutual_recursion[1,2,1,2] = no_mutual_recursion [1,2,1,2]
val test6 = mutual_recursion[1,2,1,3,2] = no_mutual_recursion [1,2,1,3,2]


signature MYLIST =
    sig
        val MyMap: ('a -> 'b) -> 'a  list -> 'b list 
        val MyFold: ('a  * 'b -> 'a ) -> 'a  -> 'b list -> 'a 
        val MyFilter: ('a  -> bool) -> 'a  list -> 'a  list
    end

structure MyList :> MYLIST = 
    struct 
        fun MyMap f xs = 
            case xs of 
                [] => []
                |x::xs' => (f x) :: MyMap f xs' 

        fun MyFold f acc xs  =
            case xs of 
                [] => acc
                |x::xs' =>  MyFold f (f(acc, x)) xs' 

        fun MyFilter f xs = 
            case xs of 
                [] => []
                |x::xs' => case f x of 
                            true => x :: MyFilter f xs'
                            |_   => MyFilter f xs'
    end 


val double_list =  MyList.MyMap (fn x => x * 2)
double_list [1,2,3,4]

val sum_list = MyList.MyFold (fn(x, y) => x + y) 0 
sum_list [1,2,3,4]

val only_even = MyList.MyFilter (fn x => x mod 2 = 0)
only_even [1,2,3,4,5,6,7,8,9,10]