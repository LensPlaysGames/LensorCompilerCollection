;; Yes, it's backwards
format : [byte](x : int) = {
  out : [byte];

  while x, {
    out += byte 48 + x % 10;
    x /= 10;
  };

  out += byte 0;

  out;
};

print 42;

foo : struct {
  x : int;
};

;; There are currently issues with LCC regarding returning structs that
;; fit in registers from a temporary created from a returned call
;; expression. So, we create the temporary ourselves within Glint.
format : [byte](x : foo) = {
  out :: format x.x;
  out;
};

bar : foo;
bar.x := 42;

print bar;
