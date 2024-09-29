(prolog? (use-module (library clpz)))

(datatype nat
   var(N);
   #>=(N 0);
   =========
   N : nat;

   if (integer? N)
   if (>= N 0)
   _______________
   N : nat;

   _______________________________________
   (integer? N) : verified >> N : integer;

   __________________________
   N : integer >> N : number;

   _______________________
   N : nat >> N : integer;

   (nat? N) : verified;
   ____________________
   N : nat;)

(define nat?
  { A --> boolean }
  X -> (>= X 0) where (integer? X)
  _ -> false)
