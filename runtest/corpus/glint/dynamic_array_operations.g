format : [byte](c : byte) {
  out : [byte];
  out += c;
  out += `\0`;
  return out;
};

foo : [byte];
;; append
foo += `4`;
foo += `2`;
;; prepend
;; foo ~= `4`;
;; append
foo += `\0`;

;; insert
foo[2] += `0`;

;; ranged for
for c in foo, {
  print c;
};

;; print
print foo;

-foo;
