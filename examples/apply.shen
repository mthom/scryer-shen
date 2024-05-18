(defprolog h-list-to-function-sig
  []     R         R <--;
  [T]    -->(T R)  R <--;
  [T|Ts] -->(T Rs) R <-- (h-list-to-function-sig Ts Rs R);)

(datatype heterogeneous-lists
   ____________________________
   [] : (h-list Ts) >> Ts ~ [];

   _______________________________________________________________
   [X|Xs] : (h-list Ts) >> X : T, Xs : (h-list Tss), Ts ~ [T|Tss];

   _________________
   [] : (h-list []);

   X : T;
   Xs : (h-list Ts);
   _________________________
   [X|Xs] : (h-list [T|Ts]);)

(datatype heterogeneously-mappable-functions
  F : (--> R);
  ======================
  F : (h-mappable [] R);

  F : (T --> R);
  =======================
  F : (h-mappable [T] R);

  F : (T1 --> (h-mappable [T2|Ts] R));
  ===================================
  F : (h-mappable [T1 T2|Ts] R);

  F : (A --> B);
  (h-list-to-function-sig [T|Ts] (A --> B) R);
  ____________________________________________
  F : (h-mappable [T|Ts] R);)

(define apply
  { (h-mappable [T|Ts] R) --> (h-list [T|Ts]) --> R }
  F [X]        -> (F X)
  F [X1 X2|Xs] -> (apply (F X1) [X2|Xs]))
