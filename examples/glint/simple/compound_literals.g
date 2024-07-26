;; A compound literal with a single expression should be replaced with the
;; single expression. Most of the time, you shouldn't need to do this.
a :int !{ .ignored 69 };
b :: int !{ 69 };

foo_t : struct { x:int y:int };
sum_t : sum { number:cint length:uint };

;; A compound literal should work as an argument when calling a type
;; expression.
{
  foo :: foo_t !{ 69, 420 };
  bar :: foo_t !{ .y 69, .x 420 };
  ;; foo.x = 69
  ;; bar.x = 420

  ;; would error due to "z" not existing as a member in "foo_t".
  ;; baz :: foo_t !{ .z 69, .x 420 };
};

{
  ;; and also for sum types, but with different semantics (member name
  ;; required, multiple expressions disallowed)
  baz :: sum_t !{ .number 69 };
  ;; has baz.number = true
  ;; baz.number = 69

  bar : sum_t;
  bar.number := 69;

  ;; Would error due to "z" not existing as a member in "sum_t".
  ;; foo :: sum_t !{ .z 69 };

  ;; Would error due to the member not having a name.
  ;; foo :: sum_t !{ 69 };

  ;; Would error due to there not being a member.
  ;; foo :: sum_t !{};

  ;; Would error due to multiple member expressions in sum type compound.
  ;; literal initialisation expression.
  ;; foo :: sum_t !{ .number 69, .length 420 };
};

{
  ;; and also for fixed array types
  foo :: [byte 4] !{ 69, 420, 69, 420 };
;;   ;; and also for dynamic array types
;;   bar :: [byte] !{ 69, 420, 69, 420 };
;;   ;; and also for view types (ptr and len)
;;   ;; note that fixed and dynamic array types are implicitly convertible to
;;   ;; views, so this will mostly be done to construct a view from some non-
;;   ;; native data like a C string, or something.
;;   foobar :: [byte view] !{ bar.data, bar.size };
;;   -bar;
  @foo[0];
};
