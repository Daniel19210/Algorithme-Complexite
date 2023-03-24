(*let () = print_endline "Hello, World!"*)
print_string "\n";;

open Gsl;;

let m = Matrix.create ?init:(Some 1.) 2 2 in
print_float(Matrix.get m 0 0) ;
print_float(Matrix.get m 0 1) ;
print_float(Matrix.get m 1 0) ;
print_float(Matrix.get m 1 1) ; print_string "\n";;

print_string "Hello, World\n";;
