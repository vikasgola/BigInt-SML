# BigInt-SML
Big Natural numbers and Big Integers as string

# Availablity

    val bigzero: bigint                         
    val normalize : bigint -> bigint            
    val bigint: int -> bigint                   
    val fromString : string -> bigint      
    val toString : bigint -> string             
    val int : bigint -> int option              
    val ~~ : bigint -> bigint                   
    val abs : bigint -> bigint                  
    val ++ : bigint * bigint -> bigint          
    val succ : bigint -> bigint                 
    val min : bigint * bigint -> bigint         
    val max : bigint * bigint -> bigint         
    val sign : bigint -> int                    
    val sameSign : bigint * bigint -> bool      
    val ** : bigint * bigint -> bigint          
    val compare : bigint * bigint -> Bn.order      
    val << : bigint * bigint -> bool            
    val <<= : bigint * bigint -> bool           
    val >> : bigint * bigint -> bool            
    val >>= : bigint * bigint -> bool           
    val == : bigint * bigint -> bool            
    val len : bigint -> int                     
    val lenCompare : bigint * bigint -> Bn.order    
    val lenLt : bigint * bigint -> bool         
    val lenLeq : bigint * bigint -> bool        
    val lenGt : bigint * bigint -> bool         
    val lenGeq : bigint * bigint -> bool        
    val lenEq : bigint * bigint -> bool         
    val -- : bigint * bigint -> bigint          
    val pred : bigint -> bigint                
