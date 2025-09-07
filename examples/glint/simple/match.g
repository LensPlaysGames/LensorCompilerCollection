foo : sum {
  x:int;
  y:int;
};

bar : foo;
bar.x := 69;
;; bar.y := 69;
out : int;
match bar {
  .x: out := bar.x;
  .y: out := 42;
};
out;
