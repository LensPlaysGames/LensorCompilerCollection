foo : [byte];
;; append
foo += `2`;
;; prepend
foo ~= `4`;

;; insert
foo[2] += `0`;

;; ranged for
for c in foo,
  print c, `\n`;

;; print
print foo, `\n`;

-foo;
