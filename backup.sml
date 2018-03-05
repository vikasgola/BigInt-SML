use "bignat.sml";

functor BigInt (Bn:BIGNAT):
sig
type bigint
exception division_by_zero
val bigzero: bigint                         (* done *)
val normalize : bigint -> bigint            (* done *)
val bigint: int -> bigint                   (* done *)
val fromString : string -> bigint option       (* done *)
val toString : bigint -> string             (* done *)
val int : bigint -> int option              (* done *)
val ~~ : bigint -> bigint                   (* done *)
val abs : bigint -> bigint                  (* done *)
val ++ : bigint * bigint -> bigint          
val succ : bigint -> bigint                 (* done *)
val min : bigint * bigint -> bigint         (* done *)
val max : bigint * bigint -> bigint         (* done *)
val sign : bigint -> int                    
val sameSign : bigint * bigint -> bool      (* done *)
(* val ** : bigint * bigint -> bigint *)
val compare : bigint * bigint -> order      (* done *)
val << : bigint * bigint -> bool            (* done *)
val <<= : bigint * bigint -> bool           (* done *)
val >> : bigint * bigint -> bool            (* done *)
val >>= : bigint * bigint -> bool           (* done *)
val == : bigint * bigint -> bool            (* done *)
val len : bigint -> int                     (* done *)
val lenCompare : bigint * bigint -> order    (* done *)
val lenLt : bigint * bigint -> bool         (* done *)
val lenLeq : bigint * bigint -> bool        (* done *)
val lenGt : bigint * bigint -> bool         (* done *)
val lenGeq : bigint * bigint -> bool        (* done *)
val lenEq : bigint * bigint -> bool         (* done *)
(* val -- : bigint * bigint -> bigint *)
val pred : bigint -> bigint                  (*done*)
(* val %% : bigint * bigint -> bigint * bigint
val div : bigint * bigint -> bigint
val mod : bigint * bigint -> bigint
val quo : bigint * bigint -> bigint
val rem : bigint * bigint -> bigint *)
end =
struct
    exception division_by_zero

    type bigint = string;
    type order = bigint * bigint;

    val bigzero : bigint = BIGNAT.zero;

    fun toString(n : bigint ): string = BIGNAT.toString(n);

    fun bigint(n) : bigint = BIGNAT.toString(Int.toString(n));

    fun fromString(n : string ) : bigint option = if(n = " ") then NONE
                                            else SOME n;
    
    fun normalize(n: bigint): bigint = BIGNAT.normalize(n) ; 

    fun int(n : bigint) : int option = Int.fromString(n);

    fun op ~~ (n) : bigint = if(substring(n, 0 ,1) = "-") then substring(n,1,size(n))
                                else "-"^n
    fun abs(n) : bigint = if(substring(n, 0 ,1) = "-") then substring(n,1,size(n))
                            else n;

    fun succ(n : bigint ): bigint = BIGNAT.succ(n);

    fun pred(n : bigint ): bigint = BIGNAT.pred(n);
    

    fun min(a: bigint ,b: bigint) = BIGNAT.min(a,b);

    fun max(a: bigint ,b: bigint) = BIGNAT.max(a,b);

    fun sameSign(a: bigint ,b: bigint) = if( substring(a, 0 ,1) = "-" andalso substring(b, 0 ,1) = "-" ) then true
                                            else if(substring(a, 0 ,1) <> "-" andalso substring(b, 0 ,1) <> "-") then true
                                            else false

    fun compare(a: bigint ,b: bigint) = BIGNAT.compare(a,b);

    infix <<;
    fun op << (a: bigint ,b: bigint): bool =(a << b);

    infix <<=;
    fun op <<= (a: bigint ,b: bigint): bool =(a <<= b);

    infix >>;
    fun op >> (a: bigint ,b: bigint): bool =(a >> b);

    infix >>=;
    fun op >>= (a: bigint ,b: bigint): bool =(a >>= b);

    infix ==;
    fun op == (a: bigint ,b: bigint): bool =(a == b);

    fun len(a: bigint) = BIGNAT.len(a);

    fun lenCompare(a: bigint ,b: bigint) : order = lenCompare(a,b);

    fun lenLt(a: bignat, b: bignat) = if(len(a) < len(b)) then true
                                        else false

    fun lenGt(a: bignat, b: bignat) = if(len(a) > len(b)) then true
                                                else false
    
    fun lenLeq(a: bignat, b: bignat) = if(len(a) < len(b) orelse len a = len b ) then true
                                        else false

    fun lenGeq(a: bignat, b: bignat) = if(len(a) > len(b) orelse len a = len b ) then true
                                        else false

    fun lenEq(a: bignat, b: bignat) = if(len a = len b ) then true
                                        else false

    fun sign(n : bigint) = if(hd( BIGNAT.toList(n)) = "-") then 0
                            else 1
    infix ++;
    fun op ++(a: bigint, b: bigint) : bigint = if(substring(a,0,1) <> "-" andalso substring(b,0,1) <> "-") then BIGNAT.++(a,b)
                                        else if(substring(a,0,1) = "-" andalso substring(b,0,1) = "-") then "-"^BIGNAT.++(substring(b,1,size(b)-1 ),substring(a,1,size(a)-1 ))
                                        else if(substring(b,0,1) = "-") then if( BIGNAT.>>(a ,substring(b,1,size(b)- 1)) ) then BIGNAT.--(a,substring(b,1,size(b) - 1 ))
                                                                                else "-"^BIGNAT.--(substring(b,1,size(b) - 1),a)
                                        else if( BIGNAT.>>(b ,substring(a,1,size(a)- 1)) ) then "-"^BIGNAT.--(b,substring(a,1,size(a) - 1 ))
                                                                                else BIGNAT.--(substring(a,1,size(a) - 1),b)

    infix --;
    fun op --(a: bigint, b: bigint): bigint = if(substring(b,0,1) <> "-") then a++("-"^b)
                                                else a++substring(b,1,size(b)-1);
end

structure Bigint = BigInt(Bignat);

open Bigint;
