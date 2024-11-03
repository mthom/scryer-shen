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

(defprolog h-list-to-function-sig
  [] R R <--;
  [T1 T2|Ts] -->(T1 Rs) R <-- (h-list-to-function-sig [T2|Ts] Rs R);
  [T] -->(T R) R <--;)

(prolog? (use-module (library dif)))

(datatype heterogeneously-mappable-functions
  F : (--> R);
  ======================
  F : (h-mappable [] R);

  (dif R -->(_ _));
  F : (T --> R);
  =======================
  F : (h-mappable [T] R);

  (dif R -->(_ _));
  F : (T1 --> (h-mappable [T2|Ts] R));
  ===================================
  F : (h-mappable [T1 T2|Ts] R);

  F : (A --> B);
  (h-list-to-function-sig [T|Ts] (A --> B) R);
  ____________________________________________
  F : (h-mappable [T|Ts] R);)

(define apply
  { (h-mappable Ts R) --> (h-list Ts) --> R }
  F []         -> (F)
  F [X]        -> (F X)
  F [X1 X2|Xs] -> (apply (F X1) [X2|Xs]))

(define apply1
  { (h-mappable [T|Ts] R) --> (h-list [T|Ts]) --> R}
  F [X]        -> (F X)
  F [X1 X2|Xs] -> (apply1 (F X1) [X2|Xs]))
