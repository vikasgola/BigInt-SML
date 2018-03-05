use "bignat.sml";

functor BigInt (Bn:BIGNAT):
sig
type bigint
exception division_by_zero
val bigzero: bigint                         (* done *)
val normalize : bigint -> bigint            (* done *)
val bigint: int -> bigint                   (* done *)
val fromString : string -> bigint      (* done *)
val toString : bigint -> string             (* done *)
val int : bigint -> int option              (* done *)
val ~~ : bigint -> bigint                   (* done *)
val abs : bigint -> bigint                  (* done *)
val ++ : bigint * bigint -> bigint          (* done *)
val succ : bigint -> bigint                 (* done *)
val min : bigint * bigint -> bigint         (* done *)
val max : bigint * bigint -> bigint         (* done *)
val sign : bigint -> int                    (* done *)
val sameSign : bigint * bigint -> bool      (* done *)
(* val ** : bigint * bigint -> bigint *)
val compare : bigint * bigint -> Bn.order      (* done *)
val << : bigint * bigint -> bool            (* done *)
val <<= : bigint * bigint -> bool           (* done *)
val >> : bigint * bigint -> bool            (* done *)
val >>= : bigint * bigint -> bool           (* done *)
val == : bigint * bigint -> bool            (* done *)
val len : bigint -> int                     (* done *)
val lenCompare : bigint * bigint -> Bn.order    (* done *)
val lenLt : bigint * bigint -> bool         (* done *)
val lenLeq : bigint * bigint -> bool        (* done *)
val lenGt : bigint * bigint -> bool         (* done *)
val lenGeq : bigint * bigint -> bool        (* done *)
val lenEq : bigint * bigint -> bool         
(* val -- : bigint * bigint -> bigint *)
val pred : bigint -> bigint                 (*done*) 
 (* val %% : bigint * bigint -> bigint * bigint
val div : bigint * bigint -> bigint
val mod : bigint * bigint -> bigint
val quo : bigint * bigint -> bigint
val rem : bigint * bigint -> bigint  *)
end =
struct
    exception division_by_zero

    type bigint = Bn.bignat;
    type order = bigint * bigint;

    val bigzero : bigint = Bn.zero;

    fun toString(n : bigint ) = Bn.toString(n);

    fun bigint(n) : bigint = Bn.fromString(Int.toString(n));

    fun fromString(n : string ) : bigint = Bn.fromString(n);
    
    fun normalize(n: bigint): bigint = Bn.normalize(n) ; 

    fun int(n : bigint) : int option = Int.fromString(Bn.toString(n) );

    fun op ~~ (n : bigint) : bigint = if(substring(Bn.toString(n), 0 ,1) = "-") then Bn.fromString(substring(Bn.toString(n),1,size(Bn.toString(n))))
                                else Bn.fromString("-"^Bn.toString(n))
    fun abs(n) : bigint = if(substring(Bn.toString(n) , 0 ,1) = "-") then Bn.fromString(substring(Bn.toString(n),1,size(Bn.toString(n))))
                            else n;

    fun succ(n : bigint ): bigint = Bn.succ(n);

    fun pred(n : bigint ): bigint = Bn.pred(n);
    

    fun min(a: bigint ,b: bigint) = Bn.min(a,b);

    fun max(a: bigint ,b: bigint) = Bn.max(a,b);

    fun sameSign(a: bigint ,b: bigint) = if( substring(Bn.toString(a) , 0 ,1) = "-" andalso substring(Bn.toString(b) , 0 ,1) = "-" ) then true
                                            else if(substring(Bn.toString(a), 0 ,1) <> "-" andalso substring(Bn.toString(b), 0 ,1) <> "-") then true
                                            else false

    fun compare(a: bigint ,b: bigint) = Bn.compare(a,b);

    infix <<;
    fun op << (a: bigint ,b: bigint): bool =(Bn.<< (Bn.fromString(toString a),Bn.fromString(toString b)));

    infix <<=;
    fun op <<= (a: bigint ,b: bigint): bool =(Bn.<<= (Bn.fromString(toString a),Bn.fromString(toString b)));

    infix >>;
    fun op >> (a: bigint ,b: bigint): bool =(Bn.>> (Bn.fromString(toString a),Bn.fromString(toString b)));

    infix >>=;
    fun op >>= (a: bigint ,b: bigint): bool =(Bn.>>= (Bn.fromString(toString a),Bn.fromString(toString b)));

    infix ==;
    fun op == (a: bigint ,b: bigint): bool =(Bn.== (Bn.fromString(toString a),Bn.fromString(toString b)));

    fun len(a: bigint) = Bn.len(a);

    fun lenCompare(a: bigint ,b: bigint) : order = Bn.lenCompare(a,b);

    fun lenLt(a: bigint, b: bigint) = if(len( a) < len(b)) then true
                                        else false

    fun lenGt(a: bigint, b: bigint) = if(len(a) > len(b)) then true
                                                else false
    
    fun lenLeq(a: bigint, b: bigint) = if(len(a) < len(b) orelse len a = len b ) then true
                                        else false

    fun lenGeq(a: bigint, b: bigint) = if(len(a) > len(b) orelse len a = len b ) then true
                                        else false

    fun lenEq(a: bigint, b: bigint) = if(len a = len b ) then true
                                        else false

    fun sign(n : bigint) = if(substring(Bn.toString(n),0,1) = "-") then 0
                            else 1
    infix ++;
    fun op ++(a: bigint, b: bigint) : bigint = if(substring(toString(a),0,1) <> "-" andalso substring(toString(b),0,1) <> "-") then fromString(Bn.toString(Bn.++(Bn.fromString(toString(a)),Bn.fromString(toString(b)))))
                                                else if(substring(toString(a),0,1) = "-" andalso substring(toString(b),0,1) = "-") then fromString("-"^toString(fromString(substring(toString(b),1,size(toString(b))-1)) ++ fromString(substring(toString(a),1,size(toString(a))-1))))
                                                else if(substring(toString(b),0,1) = "-") then if(a >> fromString(substring(toString(b),1,size(toString(b))-1)) ) then fromString(Bn.toString(Bn.--(Bn.fromString(toString a) , Bn.fromString(substring(toString(b),1,size(toString(b))-1))) ))
                                                                                                    else fromString("-"^Bn.toString(Bn.--(Bn.fromString(substring(Bn.toString(b),1,size(Bn.toString(b))-1)),Bn.fromString(toString(a))) ))
                                                else if( b >> fromString(substring(toString(a),1,size(toString(a))-1) ) ) then fromString(Bn.toString(Bn.--(Bn.fromString(toString b),Bn.fromString(substring(toString(a),1,size(toString(a))-1))) ))
                                                                                else fromString("-"^Bn.toString(Bn.--(Bn.fromString(substring(toString(a),1,size(toString(a))-1)),Bn.fromString(toString b))))

    infix --;
    fun op --(a: bigint, b: bigint): bigint = if(substring(toString(b),0,1) <> "-") then a ++(fromString("-"^toString(b)))
                                                else a ++ fromString(substring(toString(b),1,size(toString(b))-1));
end

structure Bigint = BigInt(Bignat);

open Bigint;
infix ++;
infix --


